# ============================================================================
# 06_technical_qc_same_facility.R
# BMJ Health & Care Informatics additional technical QC / sensitivity analysis
# Figure 2 plausibility QC + OSRM snapping QC + same-facility analysis
# + facility geocoding quality-control diagnostics
# Version 1.4.0 (adds CSIS iConf/iLvl geocoding QC; routing core unchanged)
#
# This script is intentionally separate from the main v7.1 analysis pipeline.
# It does NOT overwrite the manuscript outputs from the main analysis.
#
# Required inputs:
#   policy_crosswalk_16km_osrm_master_v4.csv
#   japan_clinic_address_only_csv_matched.csv (fX/fY; iConf/iLvl used when present)
#
# Required for Sections B-D:
#   The SAME local OSRM server / road-network snapshot / car profile used for
#   the original national analysis, accessible through osrm_base_url below.
#
# Main outputs:
#   1) Figure 2 physical-plausibility QC
#   2) centroid, straight-line-nearest-facility, and PRIMARY time-minimising-
#      facility snapping distances
#   3) same-facility OSRM duration/distance for each unique centroid
#   4) centroid-only and BOTH-ENDPOINT snap-distance sensitivity tables for
#      the PRIMARY minimum-time analysis
#   5) same-facility cross-classification sensitivity table
#   6) Supplementary Figure S7 candidate restricted to routes with both
#      same-facility endpoint snap distances <=1 km
#   7) Figure 2 diagnostic restricted to PRIMARY routes with both endpoint
#      snap distances <=1 km
#   8) CSIS address-matching iConf/iLvl distributions and their relationship
#      to PRIMARY facility-origin network-snapping distance
#
# IMPORTANT:
#   Plausibility flags below are QC diagnostics, NOT exclusion rules and NOT
#   clinical thresholds. No observation is deleted from the primary analysis.
# ============================================================================

# Public repository configuration ---------------------------------------------
config_file <- Sys.getenv("HOMECARE_CONFIG", unset = file.path("config", "config.R"))
if (!file.exists(config_file)) {
  stop("Configuration file not found: ", config_file,
       "\nRun scripts from the repository root or set HOMECARE_CONFIG.")
}
source(config_file)
work_dir <- cfg$work_dir
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
setwd(work_dir)

# ----------------------------------------------------------------------------
# 0. Packages
# ----------------------------------------------------------------------------
required_packages <- c("data.table", "jsonlite", "curl", "ggplot2", "hexbin", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Required packages are not installed: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them first with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))."
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(curl)
  library(ggplot2)
  library(hexbin)
  library(scales)
})

options(scipen = 999)

# ----------------------------------------------------------------------------
# 1. User settings
# ----------------------------------------------------------------------------
file_crosswalk <- cfg$crosswalk_master_file
file_facilities <- cfg$facility_file

# Use the same OSRM server/snapshot/profile as the original national run.
# Original R/osrm settings were:
#   options(osrm.server = "http://127.0.0.1:5001/", osrm.profile = "car")
# For direct OSRM HTTP API calls, the car profile is addressed as "driving".
osrm_base_url <- sub("/+$", "", cfg$osrm_server)
osrm_profile <- "driving"

# One straight-line-nearest facility is used as the source, and its centroids
# are sent as destinations. 200 destinations/request keeps URLs manageable and
# is comfortably below the original max-table-size=5000 setting.
osrm_destination_chunk_size <- 200L
osrm_timeout_sec <- 120L
osrm_max_retries <- 4L
osrm_retry_wait_sec <- 2

thresholds <- cfg$thresholds
snap_sensitivity_m <- c(500, 1000)

figure_max_distance_km <- 30
figure_max_duration_min <- 120
supplementary_figure_s7_snap_display_cutoff_m <- 1000
figure2_primary_endpoint_display_cutoff_m <- 1000

out_root <- file.path(cfg$output_dir, "technical_qc")
out_qc <- file.path(out_root, "qc")
out_tables <- file.path(out_root, "tables")
out_figures <- file.path(out_root, "figures")
out_logs <- file.path(out_root, "logs")
for (d in c(out_root, out_qc, out_tables, out_figures, out_logs)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

checkpoint_file <- file.path(out_logs, "same_facility_osrm_checkpoint.csv")

# Reuse the completed v1/v1.2 same-facility checkpoint when available so that
# the 16,000+ grouped OSRM table requests do not need to be repeated.
legacy_same_facility_checkpoint_candidates <- character()

if (!file.exists(checkpoint_file)) {
  legacy_hit <- legacy_same_facility_checkpoint_candidates[
    file.exists(legacy_same_facility_checkpoint_candidates)
  ]
  if (length(legacy_hit) > 0L) {
    file.copy(legacy_hit[1], checkpoint_file, overwrite = FALSE)
    message("Copied existing same-facility checkpoint into technical-QC output folder: ", legacy_hit[1])
  }
}

# Checkpoint used only for PRIMARY time-minimising facilities whose snapping
# distance could not be reused from the straight-line-nearest-facility queries.
primary_facility_snap_checkpoint_file <- file.path(
  out_logs,
  "primary_time_min_facility_snap_checkpoint.csv"
)

legacy_primary_facility_snap_checkpoint <- ""

if (!file.exists(primary_facility_snap_checkpoint_file) &&
    nzchar(legacy_primary_facility_snap_checkpoint) &&
    file.exists(legacy_primary_facility_snap_checkpoint)) {
  file.copy(
    legacy_primary_facility_snap_checkpoint,
    primary_facility_snap_checkpoint_file,
    overwrite = FALSE
  )
  message(
    "Copied existing primary facility-origin snap checkpoint into technical-QC output folder."
  )
}

# ----------------------------------------------------------------------------
# 2. Helpers
# ----------------------------------------------------------------------------
make_join_key <- function(dt, digits = 7) {
  paste0(
    as.character(dt$mesh_id), "_",
    sprintf(paste0("%.", digits, "f"), as.numeric(dt$centroid_lon)), "_",
    sprintf(paste0("%.", digits, "f"), as.numeric(dt$centroid_lat))
  )
}

as_logical_16km <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

safe_pct <- function(num, den) {
  # Robust to scalar/vector combinations.
  # data.table::fifelse() requires yes/no to be length 1 or the same
  # length as test; summarise_cross() passes a 4-element numerator
  # (the four cross-classification categories) with a scalar denominator.
  n <- max(length(num), length(den))
  num2 <- rep_len(as.numeric(num), n)
  den2 <- rep_len(as.numeric(den), n)

  out <- rep(NA_real_, n)
  ok <- !is.na(den2) & den2 > 0
  out[ok] <- 100 * num2[ok] / den2[ok]
  out
}

fmt_coord <- function(x) sprintf("%.7f", as.numeric(x))

facility_key_from_coords <- function(lon, lat) {
  paste0(fmt_coord(lon), "_", fmt_coord(lat))
}

csis_iconf_label <- function(x) {
  x <- suppressWarnings(as.integer(x))
  fcase(
    x == 3L, "3: unique match at one address-hierarchy level",
    x == 4L, "4: multiple matches at two or more address-hierarchy levels",
    x == 5L, "5: unique match at two or more address-hierarchy levels",
    is.na(x), "Missing/unavailable",
    default = "Other/unclassified code"
  )
}

csis_ilvl_label <- function(x) {
  x <- suppressWarnings(as.integer(x))
  fcase(
    x == -1L, "-1: coordinates unknown",
    x == 0L, "0: address level unknown",
    x == 1L, "1: prefecture",
    x == 2L, "2: county/subprefecture",
    x == 3L, "3: municipality/special ward",
    x == 4L, "4: designated-city ward",
    x == 5L, "5: oaza (large section)",
    x == 6L, "6: chome/koaza (small section)",
    x == 7L, "7: block/lot",
    x == 8L, "8: house/building branch number",
    is.na(x), "Missing/unavailable",
    default = "Other/unclassified code"
  )
}

parse_table_response <- function(raw_response, n_destinations) {
  txt <- rawToChar(raw_response$content)
  obj <- jsonlite::fromJSON(txt, simplifyVector = TRUE)

  if (is.null(obj$code) || obj$code != "Ok") {
    stop("OSRM table response was not Ok: ", txt)
  }

  durations <- as.numeric(obj$durations[1, ])
  if (length(durations) != n_destinations) {
    stop("Unexpected OSRM duration vector length.")
  }

  distances <- rep(NA_real_, n_destinations)
  if (!is.null(obj$distances)) {
    distances <- as.numeric(obj$distances[1, ])
  }

  destination_snap_m <- rep(NA_real_, n_destinations)
  if (!is.null(obj$destinations) && "distance" %in% names(obj$destinations)) {
    destination_snap_m <- as.numeric(obj$destinations$distance)
  }

  source_snap_m <- NA_real_
  if (!is.null(obj$sources) && "distance" %in% names(obj$sources)) {
    source_snap_m <- as.numeric(obj$sources$distance[1])
  }

  list(
    duration_sec = durations,
    distance_m = distances,
    destination_snap_m = destination_snap_m,
    source_snap_m = source_snap_m
  )
}

osrm_table_one_source <- function(source_lon, source_lat, dest_dt) {
  coords <- c(
    paste0(fmt_coord(source_lon), ",", fmt_coord(source_lat)),
    paste0(fmt_coord(dest_dt$centroid_lon), ",", fmt_coord(dest_dt$centroid_lat))
  )

  destinations <- paste(seq_len(nrow(dest_dt)), collapse = ";")
  url <- paste0(
    osrm_base_url, "/table/v1/", osrm_profile, "/",
    paste(coords, collapse = ";"),
    "?sources=0&destinations=", destinations,
    "&annotations=duration,distance"
  )

  last_error <- NULL
  for (attempt in seq_len(osrm_max_retries)) {
    ans <- tryCatch({
      h <- curl::new_handle(timeout = osrm_timeout_sec)
      r <- curl::curl_fetch_memory(url, handle = h)
      if (r$status_code != 200L) {
        stop("HTTP status ", r$status_code, ": ", rawToChar(r$content))
      }
      parse_table_response(r, nrow(dest_dt))
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(ans)) return(ans)
    Sys.sleep(osrm_retry_wait_sec * attempt)
  }

  stop("OSRM request failed after retries: ", last_error)
}

check_osrm_server <- function(lon, lat) {
  url <- paste0(
    osrm_base_url, "/nearest/v1/", osrm_profile, "/",
    fmt_coord(lon), ",", fmt_coord(lat), "?number=1"
  )
  ok <- tryCatch({
    h <- curl::new_handle(timeout = 10)
    r <- curl::curl_fetch_memory(url, handle = h)
    if (r$status_code != 200L) return(FALSE)
    obj <- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = TRUE)
    !is.null(obj$code) && obj$code == "Ok"
  }, error = function(e) FALSE)
  ok
}

# Return OSRM network-snapping distance (metres) for one coordinate.
# This is used for PRIMARY time-minimising facility origins that were not
# already queried as straight-line-nearest facilities in the same-facility run.
osrm_nearest_snap_distance <- function(lon, lat) {
  url <- paste0(
    osrm_base_url, "/nearest/v1/", osrm_profile, "/",
    fmt_coord(lon), ",", fmt_coord(lat), "?number=1"
  )

  last_error <- NULL
  for (attempt in seq_len(osrm_max_retries)) {
    ans <- tryCatch({
      h <- curl::new_handle(timeout = osrm_timeout_sec)
      r <- curl::curl_fetch_memory(url, handle = h)
      if (r$status_code != 200L) {
        stop("HTTP status ", r$status_code, ": ", rawToChar(r$content))
      }

      obj <- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = TRUE)
      if (is.null(obj$code) || obj$code != "Ok" ||
          is.null(obj$waypoints) || nrow(obj$waypoints) < 1L ||
          !"distance" %in% names(obj$waypoints)) {
        stop("Unexpected OSRM nearest response: ", rawToChar(r$content))
      }

      as.numeric(obj$waypoints$distance[1])
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(ans) && is.finite(ans)) return(ans)
    Sys.sleep(osrm_retry_wait_sec * attempt)
  }

  stop("OSRM nearest request failed after retries: ", last_error)
}

make_cross <- function(within16, within_time) {
  fcase(
    within16 & within_time, "Both accessible",
    within16 & !within_time, "16 km only",
    !within16 & within_time, "Time only",
    !within16 & !within_time, "Neither",
    default = NA_character_
  )
}

summarise_cross <- function(dt, duration_col, analysis_name, population_col = NULL) {
  out <- rbindlist(lapply(thresholds, function(th) {
    within_time <- !is.na(dt[[duration_col]]) & dt[[duration_col]] <= th
    cls <- make_cross(dt$within_16km, within_time)

    if (is.null(population_col)) {
      z <- data.table(category = cls)[, .(value = .N), by = category]
      den <- nrow(dt)
      unit <- "Unique spatial centroids"
    } else {
      w <- as.numeric(dt[[population_col]])
      w[is.na(w)] <- 0
      z <- data.table(category = cls, w = w)[, .(value = sum(w)), by = category]
      den <- sum(w)
      unit <- population_col
    }

    all_cat <- data.table(category = c("Both accessible", "16 km only", "Time only", "Neither"))
    z <- merge(all_cat, z, by = "category", all.x = TRUE, sort = FALSE)
    z[is.na(value), value := 0]
    z[, `:=`(
      analysis = analysis_name,
      analysis_unit = unit,
      threshold_min = th,
      denominator = den,
      pct = safe_pct(value, den)
    )]
    z
  }))
  setcolorder(out, c("analysis", "analysis_unit", "threshold_min", "category", "value", "denominator", "pct"))
  out[]
}

summarise_accessibility <- function(dt, duration_col, label) {
  rbindlist(lapply(thresholds, function(th) {
    reachable <- !is.na(dt[[duration_col]]) & dt[[duration_col]] <= th
    data.table(
      analysis = label,
      threshold_min = th,
      n_centroids = nrow(dt),
      mesh_within_n = sum(reachable),
      mesh_within_pct = safe_pct(sum(reachable), nrow(dt)),
      pop75_denominator = sum(dt$pop_75plus_2025, na.rm = TRUE),
      pop75_within = sum(dt$pop_75plus_2025[reachable], na.rm = TRUE),
      pop75_within_pct = safe_pct(
        sum(dt$pop_75plus_2025[reachable], na.rm = TRUE),
        sum(dt$pop_75plus_2025, na.rm = TRUE)
      )
    )
  }))
}

# ----------------------------------------------------------------------------
# 2A. Facility geocoding QC (does not require OSRM server)
# ----------------------------------------------------------------------------
# The study facility file was geocoded using the University of Tokyo CSIS CSV
# Address Matching Service. When iConf/iLvl are present, this block reports the
# source-reported matching confidence/address level before any OSRM rerouting.
# These fields are not used to exclude facilities or to weight the primary
# analysis; they are diagnostic metadata only.
#
# CSIS reference for iConf/iLvl codes:
# https://geocode.csis.u-tokyo.ac.jp/home/csv-admatch/faq/

geocoding_qc_available <- FALSE
facility_geocode <- NULL

if (!file.exists(file_facilities)) {
  geocoding_status <- data.table(
    geocoding_qc_available = FALSE,
    status = "Facility input not found; CSIS iConf/iLvl QC was not run.",
    facility_file = file_facilities
  )
  fwrite(geocoding_status, file.path(out_qc, "qc_geocoding_status.csv"))
  if (isTRUE(cfg$study_mode)) {
    stop("Study mode requires the facility input for geocoding QC: ", file_facilities)
  }
} else {
  facility_header <- names(fread(file_facilities, nrows = 0))
  required_facility_cols <- c("fX", "fY")
  missing_required_facility_cols <- setdiff(required_facility_cols, facility_header)
  if (length(missing_required_facility_cols) > 0L) {
    stop(
      "Facility input is missing required coordinate columns: ",
      paste(missing_required_facility_cols, collapse = ", ")
    )
  }

  geocoding_fields_present <- all(c("iConf", "iLvl") %in% facility_header)

  if (!geocoding_fields_present) {
    geocoding_status <- data.table(
      geocoding_qc_available = FALSE,
      status = paste0(
        "Facility input does not contain both iConf and iLvl; ",
        "CSIS-specific geocoding QC was skipped."
      ),
      facility_file = file_facilities
    )
    fwrite(geocoding_status, file.path(out_qc, "qc_geocoding_status.csv"))
    if (isTRUE(cfg$study_mode)) {
      stop(
        "Study mode expects CSIS address-matching fields iConf and iLvl in: ",
        file_facilities
      )
    }
  } else {
    facility_geocode <- fread(
      file_facilities,
      select = c("fX", "fY", "iConf", "iLvl")
    )
    # Script 03 assigns clinic_id from the original facility-file row order
    # before filtering invalid coordinates. Reproduce that identifier exactly.
    facility_geocode[, clinic_id := .I]
    facility_geocode[, `:=`(
      fX = suppressWarnings(as.numeric(fX)),
      fY = suppressWarnings(as.numeric(fY)),
      iConf = suppressWarnings(as.integer(iConf)),
      iLvl = suppressWarnings(as.integer(iLvl))
    )]
    facility_geocode[, routing_eligible :=
      !is.na(fX) & !is.na(fY) &
      fX >= 120 & fX <= 155 &
      fY >= 20 & fY <= 50
    ]
    facility_geocode[, `:=`(
      iConf_label = csis_iconf_label(iConf),
      iLvl_label = csis_ilvl_label(iLvl)
    )]

    facility_geocode_routing <- facility_geocode[routing_eligible == TRUE]
    geocoding_qc_available <- TRUE

    if (isTRUE(cfg$study_mode) &&
        nrow(facility_geocode_routing) != cfg$expected_counts$facilities) {
      stop(
        "Routing-eligible facility count is ", nrow(facility_geocode_routing),
        "; expected ", cfg$expected_counts$facilities,
        " for the submission dataset."
      )
    }

    iconf_distribution <- facility_geocode_routing[, .(
      n_facilities = .N
    ), by = .(iConf, iConf_label)]
    iconf_distribution[, pct_facilities := safe_pct(
      n_facilities, sum(n_facilities)
    )]
    setorder(iconf_distribution, iConf)
    fwrite(
      iconf_distribution,
      file.path(out_qc, "qc_geocoding_iConf_distribution.csv")
    )

    ilvl_distribution <- facility_geocode_routing[, .(
      n_facilities = .N
    ), by = .(iLvl, iLvl_label)]
    ilvl_distribution[, pct_facilities := safe_pct(
      n_facilities, sum(n_facilities)
    )]
    setorder(ilvl_distribution, iLvl)
    fwrite(
      ilvl_distribution,
      file.path(out_qc, "qc_geocoding_iLvl_distribution.csv")
    )

    n_routing_facilities <- nrow(facility_geocode_routing)
    geocoding_quality_summary <- data.table(
      metric = c(
        "Facility rows in source file",
        "Routing-eligible facilities used by script 03 coordinate criteria",
        "iConf = 5: unique match at >=2 address-hierarchy levels",
        "iConf = 4: multiple matches at >=2 address-hierarchy levels",
        "iConf = 3: unique match at 1 address-hierarchy level",
        "iConf missing/other",
        "iLvl >= 7: block/lot level or finer",
        "iLvl >= 6: chome/koaza level or finer",
        "iLvl <= 5 among nonmissing codes",
        "iLvl missing/unknown/coordinates unknown (NA, 0, or -1)"
      ),
      n = c(
        nrow(facility_geocode),
        n_routing_facilities,
        sum(facility_geocode_routing$iConf == 5L, na.rm = TRUE),
        sum(facility_geocode_routing$iConf == 4L, na.rm = TRUE),
        sum(facility_geocode_routing$iConf == 3L, na.rm = TRUE),
        sum(is.na(facility_geocode_routing$iConf) |
              !(facility_geocode_routing$iConf %in% c(3L, 4L, 5L))),
        sum(facility_geocode_routing$iLvl >= 7L, na.rm = TRUE),
        sum(facility_geocode_routing$iLvl >= 6L, na.rm = TRUE),
        sum(!is.na(facility_geocode_routing$iLvl) &
              facility_geocode_routing$iLvl >= 1L &
              facility_geocode_routing$iLvl <= 5L),
        sum(is.na(facility_geocode_routing$iLvl) |
              facility_geocode_routing$iLvl %in% c(-1L, 0L))
      ),
      denominator = c(
        nrow(facility_geocode),
        nrow(facility_geocode),
        rep(n_routing_facilities, 8L)
      )
    )
    geocoding_quality_summary[, pct := safe_pct(n, denominator)]
    fwrite(
      geocoding_quality_summary,
      file.path(out_qc, "qc_geocoding_quality_summary.csv")
    )

    geocoding_status <- data.table(
      geocoding_qc_available = TRUE,
      status = "CSIS iConf/iLvl fields found and summarized; no exclusion was applied from these fields.",
      facility_file = file_facilities,
      n_source_facility_rows = nrow(facility_geocode),
      n_routing_eligible_facilities = n_routing_facilities,
      csis_code_reference = "https://geocode.csis.u-tokyo.ac.jp/home/csv-admatch/faq/"
    )
    fwrite(geocoding_status, file.path(out_qc, "qc_geocoding_status.csv"))
  }
}

# ----------------------------------------------------------------------------
# 3. Read / collapse to unique spatial centroids
# ----------------------------------------------------------------------------
if (!file.exists(file_crosswalk)) stop("Input file not found: ", file_crosswalk)

needed_cols <- c(
  "mesh_id", "shicode", "centroid_lon", "centroid_lat", "within_16km",
  "nearest_distance_km", "min_duration_min",
  "nearest_clinic_id", "nearest_clinic_lon", "nearest_clinic_lat",
  "nearest_clinic_lon_16km", "nearest_clinic_lat_16km",
  "pop_total_2025", "pop_75plus_2025", "pop_80plus_2025"
)

header <- names(fread(file_crosswalk, nrows = 0))
missing_cols <- setdiff(needed_cols, header)
if (length(missing_cols) > 0) {
  stop("Required columns missing from crosswalk: ", paste(missing_cols, collapse = ", "))
}

raw <- fread(file_crosswalk, select = needed_cols)
raw[, `:=`(
  mesh_id = as.character(mesh_id),
  shicode = as.character(shicode),
  centroid_lon = as.numeric(centroid_lon),
  centroid_lat = as.numeric(centroid_lat),
  nearest_distance_km = as.numeric(nearest_distance_km),
  min_duration_min = as.numeric(min_duration_min),
  nearest_clinic_id = suppressWarnings(as.integer(nearest_clinic_id)),
  within_16km = as_logical_16km(within_16km),
  nearest_clinic_lon = as.numeric(nearest_clinic_lon),
  nearest_clinic_lat = as.numeric(nearest_clinic_lat),
  nearest_clinic_lon_16km = as.numeric(nearest_clinic_lon_16km),
  nearest_clinic_lat_16km = as.numeric(nearest_clinic_lat_16km),
  pop_total_2025 = as.numeric(pop_total_2025),
  pop_75plus_2025 = as.numeric(pop_75plus_2025),
  pop_80plus_2025 = as.numeric(pop_80plus_2025)
)]
raw[, join_key := make_join_key(raw)]
raw[, source_row_id := .I]

spatial_cols <- c(
  "mesh_id", "centroid_lon", "centroid_lat", "within_16km",
  "nearest_distance_km", "min_duration_min",
  "nearest_clinic_id", "nearest_clinic_lon", "nearest_clinic_lat",
  "nearest_clinic_lon_16km", "nearest_clinic_lat_16km"
)

# Verify spatial values are invariant within duplicated administrative records.
dup <- raw[, .N, by = join_key][N > 1L]
if (nrow(dup) > 0) {
  inv <- raw[join_key %in% dup$join_key, lapply(.SD, uniqueN), by = join_key, .SDcols = spatial_cols]
  if (any(vapply(spatial_cols, function(z) any(inv[[z]] > 1L), logical(1)))) {
    stop("Spatial/routing variables differ within duplicated spatial centroids.")
  }
}

spatial <- raw[, c(list(first_source_row_id = min(source_row_id)), .SD[1]), by = join_key, .SDcols = spatial_cols]
pops <- raw[, .(
  pop_total_2025 = sum(pop_total_2025, na.rm = TRUE),
  pop_75plus_2025 = sum(pop_75plus_2025, na.rm = TRUE),
  pop_80plus_2025 = sum(pop_80plus_2025, na.rm = TRUE)
), by = join_key]
master <- merge(spatial, pops, by = "join_key", all.x = TRUE, sort = FALSE)
setorder(master, first_source_row_id)

if (isTRUE(cfg$study_mode) &&
    nrow(master) != cfg$expected_counts$unique_spatial_centroids) {
  warning("Unique-centroid count is ", nrow(master),
          "; expected ", cfg$expected_counts$unique_spatial_centroids,
          " for the submission dataset.")
}

# Straight-line-nearest facility identity represented by coordinates.
master[, straight_nearest_facility_key := facility_key_from_coords(
  nearest_clinic_lon_16km, nearest_clinic_lat_16km
)]

master[, primary_time_min_facility_key := fifelse(
  !is.na(nearest_clinic_lon) & !is.na(nearest_clinic_lat),
  facility_key_from_coords(nearest_clinic_lon, nearest_clinic_lat),
  NA_character_
)]

master[, time_min_facility_same_as_straight_nearest := fcase(
  is.na(nearest_clinic_lon) | is.na(nearest_clinic_lat), NA,
  abs(nearest_clinic_lon - nearest_clinic_lon_16km) < 1e-8 &
    abs(nearest_clinic_lat - nearest_clinic_lat_16km) < 1e-8, TRUE,
  default = FALSE
)]

# ----------------------------------------------------------------------------
# A. Figure 2 physical-plausibility QC (does not require OSRM server)
# ----------------------------------------------------------------------------
master[, implied_speed_lower_bound_kmh := fifelse(
  !is.na(min_duration_min) & min_duration_min > 0,
  nearest_distance_km / (min_duration_min / 60),
  NA_real_
)]

master[, `:=`(
  flag_zero_time_distance_gt1km = !is.na(min_duration_min) & min_duration_min == 0 & nearest_distance_km > 1,
  flag_distance_gt10_time_lt5 = !is.na(min_duration_min) & nearest_distance_km > 10 & min_duration_min < 5,
  flag_distance_gt16_time_lt5 = !is.na(min_duration_min) & nearest_distance_km > 16 & min_duration_min < 5,
  flag_distance_gt20_time_lt5 = !is.na(min_duration_min) & nearest_distance_km > 20 & min_duration_min < 5,
  flag_speed_gt120_dist_ge5 = !is.na(implied_speed_lower_bound_kmh) & nearest_distance_km >= 5 & implied_speed_lower_bound_kmh > 120,
  flag_speed_gt150_dist_ge5 = !is.na(implied_speed_lower_bound_kmh) & nearest_distance_km >= 5 & implied_speed_lower_bound_kmh > 150,
  flag_speed_gt200_dist_ge5 = !is.na(implied_speed_lower_bound_kmh) & nearest_distance_km >= 5 & implied_speed_lower_bound_kmh > 200
)]

flag_cols <- grep("^flag_", names(master), value = TRUE)
plausibility_summary <- rbindlist(lapply(flag_cols, function(z) {
  flag <- master[[z]] %in% TRUE
  data.table(
    flag = z,
    n_centroids = sum(flag),
    pct_centroids = safe_pct(sum(flag), nrow(master)),
    total_population_2025 = sum(master$pop_total_2025[flag], na.rm = TRUE),
    population_75plus_2025 = sum(master$pop_75plus_2025[flag], na.rm = TRUE),
    population_80plus_2025 = sum(master$pop_80plus_2025[flag], na.rm = TRUE)
  )
}))

fwrite(plausibility_summary, file.path(out_qc, "qc_figure2_physical_plausibility_summary.csv"))

candidate_flag <- Reduce(`|`, lapply(flag_cols, function(z) master[[z]] %in% TRUE))
plausibility_candidates <- master[candidate_flag, c(
  "join_key", "mesh_id", "centroid_lon", "centroid_lat",
  "nearest_distance_km", "min_duration_min", "implied_speed_lower_bound_kmh",
  "within_16km", "nearest_clinic_id", "nearest_clinic_lon", "nearest_clinic_lat",
  "nearest_clinic_lon_16km", "nearest_clinic_lat_16km",
  "time_min_facility_same_as_straight_nearest",
  "pop_total_2025", "pop_75plus_2025", "pop_80plus_2025",
  flag_cols
), with = FALSE]
setorder(plausibility_candidates, -nearest_distance_km, min_duration_min)
fwrite(plausibility_candidates, file.path(out_qc, "qc_figure2_physical_plausibility_candidates.csv"))

facility_identity_summary <- data.table(
  metric = c(
    "Unique spatial centroids",
    "Primary OSRM-reachable centroids with facility identity available",
    "Time-minimising facility coordinates equal straight-line-nearest facility coordinates",
    "Coordinates differ among reachable centroids"
  ),
  n = c(
    nrow(master),
    sum(!is.na(master$time_min_facility_same_as_straight_nearest)),
    sum(master$time_min_facility_same_as_straight_nearest %in% TRUE),
    sum(master$time_min_facility_same_as_straight_nearest %in% FALSE)
  )
)
facility_identity_summary[, denominator := c(
  nrow(master), nrow(master),
  sum(!is.na(master$time_min_facility_same_as_straight_nearest)),
  sum(!is.na(master$time_min_facility_same_as_straight_nearest))
)]
facility_identity_summary[, pct := safe_pct(n, denominator)]
fwrite(facility_identity_summary, file.path(out_qc, "qc_time_min_vs_straight_nearest_facility_identity.csv"))

# Diagnostic plot: same visual limits as current Figure 2, with implausible candidates overlaid.
fig_qc <- ggplot(
  master[
    !is.na(nearest_distance_km) & !is.na(min_duration_min) &
      nearest_distance_km <= figure_max_distance_km &
      min_duration_min <= figure_max_duration_min
  ],
  aes(nearest_distance_km, min_duration_min)
) +
  stat_binhex(bins = 70) +
  scale_fill_viridis_c(trans = "log10", name = "Mesh count") +
  geom_point(
    data = master[
      flag_distance_gt16_time_lt5 &
        nearest_distance_km <= figure_max_distance_km &
        min_duration_min <= figure_max_duration_min
    ],
    aes(nearest_distance_km, min_duration_min),
    inherit.aes = FALSE,
    shape = 21, fill = NA, stroke = 0.7, size = 2.4
  ) +
  geom_vline(xintercept = 16, linetype = "dashed") +
  geom_hline(yintercept = thresholds, linetype = "dotted") +
  coord_cartesian(xlim = c(0, figure_max_distance_km), ylim = c(0, figure_max_duration_min)) +
  labs(
    x = "Straight-line distance to nearest qualifying facility (km)",
    y = "OSRM-estimated minimum travel time (min)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_figures, "qc_figure2_flagged_long_fast_points.png"), fig_qc,
       width = 7, height = 6, units = "in", dpi = 300, bg = "white")

# Save a compact pre-OSRM summary.
pre_osrm_summary <- data.table(
  metric = c(
    "Unique centroids",
    "OSRM unreachable in primary data",
    "Straight-line-nearest and time-minimising facility coordinates identical",
    "Straight-line-nearest and time-minimising facility coordinates different",
    "Nearest distance >20 km and primary minimum time <5 min"
  ),
  value = c(
    nrow(master),
    sum(is.na(master$min_duration_min)),
    sum(master$time_min_facility_same_as_straight_nearest, na.rm = TRUE),
    sum(master$time_min_facility_same_as_straight_nearest %in% FALSE),
    sum(master$flag_distance_gt20_time_lt5, na.rm = TRUE)
  )
)
fwrite(pre_osrm_summary, file.path(out_qc, "qc_pre_osrm_summary.csv"))

message("Section A complete. Physical-plausibility QC outputs were written to: ", out_qc)

# ----------------------------------------------------------------------------
# B. Verify OSRM server and calculate snapping + same-facility routes
# ----------------------------------------------------------------------------
if (!check_osrm_server(master$centroid_lon[1], master$centroid_lat[1])) {
  stop(
    paste0(
      "The preliminary QC (Section A) completed successfully, but the OSRM server ",
      "was not reachable at ", osrm_base_url, ".\n",
      "Start the SAME OSRM dataset/profile used in the original national analysis, ",
      "then rerun this script. Existing Section A outputs will simply be overwritten."
    )
  )
}

message("OSRM server is reachable. Starting same-facility grouped table queries...")

# Create request plan. The maximum original group size in the submission data is
# modest, but groups are chunked to keep request URLs manageable.
setorder(master, straight_nearest_facility_key, join_key)
master[, facility_row_index := seq_len(.N), by = straight_nearest_facility_key]
master[, request_chunk := ceiling(facility_row_index / osrm_destination_chunk_size)]
master[, request_id := paste0(straight_nearest_facility_key, "__", request_chunk)]

request_plan <- unique(master[, .(
  request_id,
  straight_nearest_facility_key,
  request_chunk,
  source_lon = nearest_clinic_lon_16km,
  source_lat = nearest_clinic_lat_16km
)])
setorder(request_plan, straight_nearest_facility_key, request_chunk)

completed_ids <- character()
if (file.exists(checkpoint_file)) {
  checkpoint_existing <- fread(checkpoint_file, select = "request_id")
  completed_ids <- unique(checkpoint_existing$request_id)
  message("Resuming from checkpoint: ", length(completed_ids), " request chunks already completed.")
}

n_req <- nrow(request_plan)
for (ii in seq_len(n_req)) {
  req <- request_plan[ii]
  if (req$request_id %in% completed_ids) next

  dest <- master[request_id == req$request_id]
  ans <- osrm_table_one_source(req$source_lon, req$source_lat, dest)

  out <- dest[, .(
    request_id,
    join_key,
    mesh_id,
    centroid_lon,
    centroid_lat,
    nearest_distance_km,
    within_16km,
    primary_min_duration_min = min_duration_min,
    straight_nearest_facility_key,
    straight_nearest_facility_lon = nearest_clinic_lon_16km,
    straight_nearest_facility_lat = nearest_clinic_lat_16km,
    time_min_facility_same_as_straight_nearest,
    pop_total_2025,
    pop_75plus_2025,
    pop_80plus_2025
  )]

  out[, `:=`(
    same_facility_duration_min = ans$duration_sec / 60,
    same_facility_osrm_distance_km = ans$distance_m / 1000,
    centroid_snap_distance_m = ans$destination_snap_m,
    straight_nearest_facility_snap_distance_m = ans$source_snap_m
  )]

  fwrite(
    out,
    checkpoint_file,
    append = file.exists(checkpoint_file),
    col.names = !file.exists(checkpoint_file)
  )

  if (ii %% 100L == 0L || ii == n_req) {
    message(sprintf("OSRM request progress: %d / %d", ii, n_req))
  }
}

same <- fread(checkpoint_file)
# If a rerun ever produced duplicates after an interrupted write, keep one row per centroid.
same <- unique(same, by = "join_key")
if (nrow(same) != nrow(master)) {
  stop("Same-facility checkpoint does not contain exactly one row per unique centroid: ", nrow(same))
}

fwrite(same, file.path(out_qc, "qc_same_facility_osrm_all_unique_centroids.csv"))

# Merge back onto the full master in original order.
setkey(master, join_key)
setkey(same, join_key)
master2 <- same[master]

# ----------------------------------------------------------------------------
# C1. PRIMARY time-minimising facility-origin snapping QC
# ----------------------------------------------------------------------------
# The original Figure 2 uses:
#   x = distance to the straight-line-nearest facility
#   y = minimum OSRM time from ANY qualifying facility.
#
# Sections B/C of v1.2 quantified snapping of the mesh centroid and the
# straight-line-nearest facility. For the PRIMARY minimum-time result, however,
# the relevant route origin is the time-minimising facility. This section
# explicitly obtains that facility-origin snapping distance and then evaluates
# sensitivity using BOTH endpoints of the primary route.

# Build a reusable lookup from facilities already queried as the source in the
# same-facility analysis. A facility may occur in multiple request chunks, but
# the snapping distance should be invariant for the same coordinate.
straight_source_snap_qc <- same[
  !is.na(straight_nearest_facility_key) &
    !is.na(straight_nearest_facility_snap_distance_m),
  .(
    n_source_observations = .N,
    snap_min_m = min(straight_nearest_facility_snap_distance_m, na.rm = TRUE),
    snap_max_m = max(straight_nearest_facility_snap_distance_m, na.rm = TRUE),
    snap_distance_m = median(straight_nearest_facility_snap_distance_m, na.rm = TRUE)
  ),
  by = straight_nearest_facility_key
]

if (any(straight_source_snap_qc$snap_max_m - straight_source_snap_qc$snap_min_m > 1e-6)) {
  warning(
    "The same straight-line-nearest facility coordinate returned slightly ",
    "different source snapping distances across request chunks. Median values ",
    "are used for the reusable lookup; inspect qc_straight_source_snap_invariance.csv."
  )
}
fwrite(
  straight_source_snap_qc,
  file.path(out_qc, "qc_straight_source_snap_invariance.csv")
)

reused_primary_facility_snap <- straight_source_snap_qc[, .(
  primary_time_min_facility_key = straight_nearest_facility_key,
  primary_time_min_facility_snap_distance_m = snap_distance_m,
  snap_source = "reused_from_same_facility_table_source"
)]

# Unique PRIMARY time-minimising facilities among primary-reachable centroids.
primary_facilities <- unique(master2[
  !is.na(primary_min_duration_min) &
    !is.na(nearest_clinic_lon) &
    !is.na(nearest_clinic_lat),
  .(
    primary_time_min_facility_key,
    primary_time_min_facility_lon = nearest_clinic_lon,
    primary_time_min_facility_lat = nearest_clinic_lat
  )
])

if (anyDuplicated(primary_facilities$primary_time_min_facility_key)) {
  stop("PRIMARY time-minimising facility coordinate key is not unique after coordinate collapse.")
}

# Attach reusable source snapping distances.
primary_facilities[
  reused_primary_facility_snap,
  on = "primary_time_min_facility_key",
  `:=`(
    primary_time_min_facility_snap_distance_m =
      i.primary_time_min_facility_snap_distance_m,
    snap_source = i.snap_source
  )
]

# Load any previously completed nearest-query checkpoint.
queried_primary_snap <- NULL
if (file.exists(primary_facility_snap_checkpoint_file)) {
  queried_primary_snap <- fread(primary_facility_snap_checkpoint_file)
  queried_primary_snap <- unique(
    queried_primary_snap,
    by = "primary_time_min_facility_key"
  )

  primary_facilities[
    queried_primary_snap,
    on = "primary_time_min_facility_key",
    `:=`(
      primary_time_min_facility_snap_distance_m =
        fifelse(
          is.na(primary_time_min_facility_snap_distance_m),
          i.primary_time_min_facility_snap_distance_m,
          primary_time_min_facility_snap_distance_m
        ),
      snap_source =
        fifelse(
          is.na(snap_source),
          i.snap_source,
          snap_source
        )
    )
  ]
}

# Query only facilities that were never a straight-line-nearest source and are
# therefore still missing a snapping distance.
primary_facilities_missing <- primary_facilities[
  is.na(primary_time_min_facility_snap_distance_m)
]

message(
  "PRIMARY time-minimising facilities: ", nrow(primary_facilities),
  "; snapping distance reused from same-facility source queries: ",
  sum(!is.na(primary_facilities$primary_time_min_facility_snap_distance_m)),
  "; additional /nearest queries required: ",
  nrow(primary_facilities_missing)
)

if (nrow(primary_facilities_missing) > 0L) {
  for (ii in seq_len(nrow(primary_facilities_missing))) {
    z <- primary_facilities_missing[ii]

    snap_m <- osrm_nearest_snap_distance(
      z$primary_time_min_facility_lon,
      z$primary_time_min_facility_lat
    )

    out <- data.table(
      primary_time_min_facility_key = z$primary_time_min_facility_key,
      primary_time_min_facility_lon = z$primary_time_min_facility_lon,
      primary_time_min_facility_lat = z$primary_time_min_facility_lat,
      primary_time_min_facility_snap_distance_m = snap_m,
      snap_source = "direct_nearest_query"
    )

    fwrite(
      out,
      primary_facility_snap_checkpoint_file,
      append = file.exists(primary_facility_snap_checkpoint_file),
      col.names = !file.exists(primary_facility_snap_checkpoint_file)
    )

    if (ii %% 100L == 0L || ii == nrow(primary_facilities_missing)) {
      message(
        sprintf(
          "PRIMARY facility-origin snapping progress: %d / %d",
          ii,
          nrow(primary_facilities_missing)
        )
      )
    }
  }

  queried_primary_snap <- fread(primary_facility_snap_checkpoint_file)
  queried_primary_snap <- unique(
    queried_primary_snap,
    by = "primary_time_min_facility_key"
  )

  primary_facilities[
    queried_primary_snap,
    on = "primary_time_min_facility_key",
    `:=`(
      primary_time_min_facility_snap_distance_m =
        fifelse(
          is.na(primary_time_min_facility_snap_distance_m),
          i.primary_time_min_facility_snap_distance_m,
          primary_time_min_facility_snap_distance_m
        ),
      snap_source =
        fifelse(
          is.na(snap_source),
          i.snap_source,
          snap_source
        )
    )
  ]
}

if (anyNA(primary_facilities$primary_time_min_facility_snap_distance_m)) {
  fwrite(
    primary_facilities[is.na(primary_time_min_facility_snap_distance_m)],
    file.path(out_qc, "qc_primary_time_min_facility_snap_missing.csv")
  )
  stop(
    "Some PRIMARY time-minimising facilities still lack an OSRM snapping distance."
  )
}

fwrite(
  primary_facilities,
  file.path(out_qc, "qc_primary_facility_snap_lookup.csv")
)

# ----------------------------------------------------------------------------
# C1b. Relate PRIMARY facility-origin snapping to source geocoding quality
# ----------------------------------------------------------------------------
# This comparison is descriptive. iConf/iLvl are source geocoding metadata and
# are not assumed to be the cause of large OSRM network snaps. Large snapping
# can also reflect road-network representation, access geometry, islands, or
# ferry-connected locations.
if (isTRUE(geocoding_qc_available)) {
  primary_facility_use <- master2[
    !is.na(primary_min_duration_min) & !is.na(nearest_clinic_id),
    .(
      n_centroids_time_minimising = .N,
      pop75_time_minimising = sum(pop_75plus_2025, na.rm = TRUE),
      primary_time_min_facility_key = primary_time_min_facility_key[1]
    ),
    by = .(clinic_id = nearest_clinic_id)
  ]

  primary_facility_geocoding_detail <- merge(
    primary_facility_use,
    facility_geocode[, .(
      clinic_id,
      iConf,
      iConf_label,
      iLvl,
      iLvl_label,
      routing_eligible
    )],
    by = "clinic_id",
    all.x = TRUE,
    sort = FALSE
  )

  primary_facility_geocoding_detail <- merge(
    primary_facility_geocoding_detail,
    primary_facilities[, .(
      primary_time_min_facility_key,
      primary_time_min_facility_snap_distance_m,
      snap_source
    )],
    by = "primary_time_min_facility_key",
    all.x = TRUE,
    sort = FALSE
  )

  if (anyNA(primary_facility_geocoding_detail$primary_time_min_facility_snap_distance_m)) {
    warning(
      "Some primary time-minimising facility IDs could not be linked to the ",
      "coordinate-level snapping lookup; inspect qc_primary_facility_snap_geocoding_detail.csv."
    )
  }

  fwrite(
    primary_facility_geocoding_detail,
    file.path(out_qc, "qc_primary_facility_snap_geocoding_detail.csv")
  )

  geocode_snap_subsets <- list(
    "All unique PRIMARY time-minimising facilities" =
      rep(TRUE, nrow(primary_facility_geocoding_detail)),
    "PRIMARY facility-origin snap >500 m" =
      primary_facility_geocoding_detail$primary_time_min_facility_snap_distance_m > 500,
    "PRIMARY facility-origin snap >1000 m" =
      primary_facility_geocoding_detail$primary_time_min_facility_snap_distance_m > 1000
  )

  primary_facility_snap_vs_geocoding_quality <- rbindlist(
    lapply(names(geocode_snap_subsets), function(nm) {
      keep <- geocode_snap_subsets[[nm]] %in% TRUE
      z <- primary_facility_geocoding_detail[keep]
      den <- nrow(z)
      data.table(
        subset = nm,
        n_facilities = den,
        iConf_5_n = sum(z$iConf == 5L, na.rm = TRUE),
        iConf_5_pct = safe_pct(sum(z$iConf == 5L, na.rm = TRUE), den),
        iConf_4_n = sum(z$iConf == 4L, na.rm = TRUE),
        iConf_4_pct = safe_pct(sum(z$iConf == 4L, na.rm = TRUE), den),
        iConf_3_n = sum(z$iConf == 3L, na.rm = TRUE),
        iConf_3_pct = safe_pct(sum(z$iConf == 3L, na.rm = TRUE), den),
        iConf_missing_or_other_n = sum(is.na(z$iConf) | !(z$iConf %in% c(3L, 4L, 5L))),
        iConf_missing_or_other_pct = safe_pct(
          sum(is.na(z$iConf) | !(z$iConf %in% c(3L, 4L, 5L))), den
        ),
        iLvl_ge7_n = sum(z$iLvl >= 7L, na.rm = TRUE),
        iLvl_ge7_pct = safe_pct(sum(z$iLvl >= 7L, na.rm = TRUE), den),
        iLvl_ge6_n = sum(z$iLvl >= 6L, na.rm = TRUE),
        iLvl_ge6_pct = safe_pct(sum(z$iLvl >= 6L, na.rm = TRUE), den),
        iLvl_missing_unknown_n = sum(is.na(z$iLvl) | z$iLvl %in% c(-1L, 0L)),
        iLvl_missing_unknown_pct = safe_pct(
          sum(is.na(z$iLvl) | z$iLvl %in% c(-1L, 0L)), den
        ),
        median_facility_origin_snap_m = if (den > 0L) {
          median(z$primary_time_min_facility_snap_distance_m, na.rm = TRUE)
        } else NA_real_,
        max_facility_origin_snap_m = if (den > 0L) {
          max(z$primary_time_min_facility_snap_distance_m, na.rm = TRUE)
        } else NA_real_
      )
    }),
    fill = TRUE
  )

  fwrite(
    primary_facility_snap_vs_geocoding_quality,
    file.path(out_qc, "qc_primary_facility_snap_vs_geocoding_quality.csv")
  )
}

# Attach the PRIMARY facility-origin snap to every centroid.
master2[
  primary_facilities,
  on = "primary_time_min_facility_key",
  `:=`(
    primary_time_min_facility_snap_distance_m =
      i.primary_time_min_facility_snap_distance_m,
    primary_time_min_facility_snap_source =
      i.snap_source
  )
]

# Reachable primary routes should now have snapping distances for BOTH endpoints.
missing_primary_endpoint_snap <- master2[
  !is.na(primary_min_duration_min) &
    (
      is.na(centroid_snap_distance_m) |
      is.na(primary_time_min_facility_snap_distance_m)
    )
]

fwrite(
  missing_primary_endpoint_snap,
  file.path(out_qc, "qc_primary_routes_missing_endpoint_snap.csv")
)

if (nrow(missing_primary_endpoint_snap) > 0L) {
  stop(
    "At least one PRIMARY-reachable route lacks centroid or facility-origin ",
    "snapping distance. See qc_primary_routes_missing_endpoint_snap.csv."
  )
}

master2[, primary_endpoint_snap_max_m := pmax(
  centroid_snap_distance_m,
  primary_time_min_facility_snap_distance_m,
  na.rm = FALSE
)]

# ----------------------------------------------------------------------------
# C2. Snapping QC and sensitivity of PRIMARY minimum-time results
# ----------------------------------------------------------------------------
snap_quantiles <- data.table(
  statistic = c("min", "p25", "median", "p75", "p95", "p99", "p99.9", "max"),
  centroid_snap_distance_m = as.numeric(quantile(
    master2$centroid_snap_distance_m,
    probs = c(0, .25, .5, .75, .95, .99, .999, 1),
    na.rm = TRUE,
    names = FALSE
  )),
  straight_nearest_facility_snap_distance_m = as.numeric(quantile(
    master2$straight_nearest_facility_snap_distance_m,
    probs = c(0, .25, .5, .75, .95, .99, .999, 1),
    na.rm = TRUE,
    names = FALSE
  )),
  primary_time_min_facility_snap_distance_m = as.numeric(quantile(
    master2$primary_time_min_facility_snap_distance_m,
    probs = c(0, .25, .5, .75, .95, .99, .999, 1),
    na.rm = TRUE,
    names = FALSE
  )),
  primary_route_max_endpoint_snap_distance_m = as.numeric(quantile(
    master2$primary_endpoint_snap_max_m,
    probs = c(0, .25, .5, .75, .95, .99, .999, 1),
    na.rm = TRUE,
    names = FALSE
  ))
)
fwrite(snap_quantiles, file.path(out_qc, "qc_snap_distance_quantiles.csv"))

snap_cutoffs <- c(100, 250, 500, 1000, 2000, 5000, 10000)
snap_counts <- rbindlist(lapply(snap_cutoffs, function(cut) {
  data.table(
    cutoff_m = cut,
    centroid_n_above = sum(master2$centroid_snap_distance_m > cut, na.rm = TRUE),
    centroid_pct_above = safe_pct(sum(master2$centroid_snap_distance_m > cut, na.rm = TRUE), nrow(master2)),
    pop75_above = sum(master2$pop_75plus_2025[master2$centroid_snap_distance_m > cut], na.rm = TRUE),
    pop75_pct_above = safe_pct(
      sum(master2$pop_75plus_2025[master2$centroid_snap_distance_m > cut], na.rm = TRUE),
      sum(master2$pop_75plus_2025, na.rm = TRUE)
    ),
    straight_nearest_facility_n_above = sum(master2$straight_nearest_facility_snap_distance_m > cut, na.rm = TRUE),
    primary_time_min_facility_n_above = sum(master2$primary_time_min_facility_snap_distance_m > cut, na.rm = TRUE),
    primary_either_endpoint_n_above = sum(master2$primary_endpoint_snap_max_m > cut, na.rm = TRUE),
    primary_either_endpoint_pct_above = safe_pct(
      sum(master2$primary_endpoint_snap_max_m > cut, na.rm = TRUE),
      nrow(master2)
    ),
    primary_either_endpoint_pop75_above = sum(
      master2$pop_75plus_2025[master2$primary_endpoint_snap_max_m > cut],
      na.rm = TRUE
    ),
    primary_either_endpoint_pop75_pct_above = safe_pct(
      sum(
        master2$pop_75plus_2025[master2$primary_endpoint_snap_max_m > cut],
        na.rm = TRUE
      ),
      sum(master2$pop_75plus_2025, na.rm = TRUE)
    )
  )
}))
fwrite(snap_counts, file.path(out_qc, "qc_snap_distance_cutoff_counts.csv"))

# Link the original Figure 2 plausibility flags to observed centroid snapping.
master2[, implied_speed_lower_bound_kmh := fifelse(
  !is.na(primary_min_duration_min) & primary_min_duration_min > 0,
  nearest_distance_km / (primary_min_duration_min / 60),
  NA_real_
)]
master2[, flag_distance_gt20_time_lt5 :=
  !is.na(primary_min_duration_min) & nearest_distance_km > 20 & primary_min_duration_min < 5
]
master2[, flag_speed_gt150_dist_ge5 :=
  !is.na(implied_speed_lower_bound_kmh) & nearest_distance_km >= 5 & implied_speed_lower_bound_kmh > 150
]

snap_plausibility <- rbindlist(list(
  master2[, .(
    flag = "nearest distance >20 km and primary minimum time <5 min",
    n_flagged = sum(flag_distance_gt20_time_lt5),
    centroid_snap_gt500_n = sum(flag_distance_gt20_time_lt5 & centroid_snap_distance_m > 500, na.rm = TRUE),
    centroid_snap_gt1000_n = sum(flag_distance_gt20_time_lt5 & centroid_snap_distance_m > 1000, na.rm = TRUE),
    primary_facility_snap_gt500_n = sum(flag_distance_gt20_time_lt5 & primary_time_min_facility_snap_distance_m > 500, na.rm = TRUE),
    primary_facility_snap_gt1000_n = sum(flag_distance_gt20_time_lt5 & primary_time_min_facility_snap_distance_m > 1000, na.rm = TRUE),
    either_endpoint_snap_gt500_n = sum(flag_distance_gt20_time_lt5 & primary_endpoint_snap_max_m > 500, na.rm = TRUE),
    either_endpoint_snap_gt1000_n = sum(flag_distance_gt20_time_lt5 & primary_endpoint_snap_max_m > 1000, na.rm = TRUE),
    median_centroid_snap_m_flagged = median(centroid_snap_distance_m[flag_distance_gt20_time_lt5], na.rm = TRUE),
    median_primary_facility_snap_m_flagged = median(primary_time_min_facility_snap_distance_m[flag_distance_gt20_time_lt5], na.rm = TRUE),
    median_max_endpoint_snap_m_flagged = median(primary_endpoint_snap_max_m[flag_distance_gt20_time_lt5], na.rm = TRUE)
  )],
  master2[, .(
    flag = "implied lower-bound speed >150 km/h and distance >=5 km",
    n_flagged = sum(flag_speed_gt150_dist_ge5),
    centroid_snap_gt500_n = sum(flag_speed_gt150_dist_ge5 & centroid_snap_distance_m > 500, na.rm = TRUE),
    centroid_snap_gt1000_n = sum(flag_speed_gt150_dist_ge5 & centroid_snap_distance_m > 1000, na.rm = TRUE),
    primary_facility_snap_gt500_n = sum(flag_speed_gt150_dist_ge5 & primary_time_min_facility_snap_distance_m > 500, na.rm = TRUE),
    primary_facility_snap_gt1000_n = sum(flag_speed_gt150_dist_ge5 & primary_time_min_facility_snap_distance_m > 1000, na.rm = TRUE),
    either_endpoint_snap_gt500_n = sum(flag_speed_gt150_dist_ge5 & primary_endpoint_snap_max_m > 500, na.rm = TRUE),
    either_endpoint_snap_gt1000_n = sum(flag_speed_gt150_dist_ge5 & primary_endpoint_snap_max_m > 1000, na.rm = TRUE),
    median_centroid_snap_m_flagged = median(centroid_snap_distance_m[flag_speed_gt150_dist_ge5], na.rm = TRUE),
    median_primary_facility_snap_m_flagged = median(primary_time_min_facility_snap_distance_m[flag_speed_gt150_dist_ge5], na.rm = TRUE),
    median_max_endpoint_snap_m_flagged = median(primary_endpoint_snap_max_m[flag_speed_gt150_dist_ge5], na.rm = TRUE)
  )]
))
snap_plausibility[, `:=`(
  centroid_snap_gt500_pct = safe_pct(centroid_snap_gt500_n, n_flagged),
  centroid_snap_gt1000_pct = safe_pct(centroid_snap_gt1000_n, n_flagged),
  primary_facility_snap_gt500_pct = safe_pct(primary_facility_snap_gt500_n, n_flagged),
  primary_facility_snap_gt1000_pct = safe_pct(primary_facility_snap_gt1000_n, n_flagged),
  either_endpoint_snap_gt500_pct = safe_pct(either_endpoint_snap_gt500_n, n_flagged),
  either_endpoint_snap_gt1000_pct = safe_pct(either_endpoint_snap_gt1000_n, n_flagged)
)]
fwrite(
  snap_plausibility,
  file.path(out_qc, "qc_snap_distance_vs_physical_plausibility_flags_both_primary_endpoints.csv")
)

# Save all plausibility candidates with BOTH primary endpoint snapping distances.
primary_plausibility_candidates_both_endpoints <- master2[
  flag_distance_gt20_time_lt5 | flag_speed_gt150_dist_ge5,
  .(
    join_key,
    mesh_id,
    centroid_lon,
    centroid_lat,
    nearest_distance_km,
    primary_min_duration_min,
    implied_speed_lower_bound_kmh,
    within_16km,
    nearest_clinic_id,
    nearest_clinic_lon,
    nearest_clinic_lat,
    centroid_snap_distance_m,
    primary_time_min_facility_snap_distance_m,
    primary_endpoint_snap_max_m,
    straight_nearest_facility_lon,
    straight_nearest_facility_lat,
    straight_nearest_facility_snap_distance_m,
    time_min_facility_same_as_straight_nearest,
    pop_total_2025,
    pop_75plus_2025,
    pop_80plus_2025,
    flag_distance_gt20_time_lt5,
    flag_speed_gt150_dist_ge5
  )
]
setorder(
  primary_plausibility_candidates_both_endpoints,
  -implied_speed_lower_bound_kmh
)
fwrite(
  primary_plausibility_candidates_both_endpoints,
  file.path(out_qc, "qc_primary_plausibility_both_endpoint_snaps.csv")
)

# PRIMARY analysis sensitivity under two approaches:
# 1) complete-case exclusion of high-snap centroids
# 2) conservative reclassification of high-snap centroids as unreachable
primary_sensitivity <- list(
  summarise_accessibility(master2, "primary_min_duration_min", "Primary: all centroids")
)

for (cut in snap_sensitivity_m) {
  cc <- master2[is.na(centroid_snap_distance_m) | centroid_snap_distance_m <= cut]
  primary_sensitivity[[length(primary_sensitivity) + 1L]] <-
    summarise_accessibility(cc, "primary_min_duration_min", paste0("Primary complete-case: snap <=", cut, " m"))

  cons <- copy(master2)
  cons[!is.na(centroid_snap_distance_m) & centroid_snap_distance_m > cut,
       primary_min_duration_conservative := NA_real_]
  cons[is.na(primary_min_duration_conservative),
       primary_min_duration_conservative := primary_min_duration_min]
  # The previous assignment would refill the deliberately NA values, so reset them explicitly.
  cons[!is.na(centroid_snap_distance_m) & centroid_snap_distance_m > cut,
       primary_min_duration_conservative := NA_real_]
  primary_sensitivity[[length(primary_sensitivity) + 1L]] <-
    summarise_accessibility(cons, "primary_min_duration_conservative", paste0("Primary conservative: snap >", cut, " m treated unreachable"))
}

primary_sensitivity <- rbindlist(primary_sensitivity, fill = TRUE)
fwrite(primary_sensitivity, file.path(out_tables, "Supplementary_Table_candidate_snap_sensitivity_primary_accessibility.csv"))

# Cross-classification sensitivity for 1-km snapping threshold (complete-case and conservative).
primary_cross <- rbindlist(list(
  summarise_cross(master2, "primary_min_duration_min", "Primary all centroids", NULL),
  summarise_cross(master2, "primary_min_duration_min", "Primary all centroids", "pop_75plus_2025")
))

for (cut in snap_sensitivity_m) {
  cc <- master2[is.na(centroid_snap_distance_m) | centroid_snap_distance_m <= cut]
  primary_cross <- rbindlist(list(
    primary_cross,
    summarise_cross(cc, "primary_min_duration_min", paste0("Complete-case snap <=", cut, " m"), NULL),
    summarise_cross(cc, "primary_min_duration_min", paste0("Complete-case snap <=", cut, " m"), "pop_75plus_2025")
  ), fill = TRUE)

  cons <- copy(master2)
  cons[, primary_duration_cons := primary_min_duration_min]
  cons[!is.na(centroid_snap_distance_m) & centroid_snap_distance_m > cut, primary_duration_cons := NA_real_]
  primary_cross <- rbindlist(list(
    primary_cross,
    summarise_cross(cons, "primary_duration_cons", paste0("Conservative snap >", cut, " m unreachable"), NULL),
    summarise_cross(cons, "primary_duration_cons", paste0("Conservative snap >", cut, " m unreachable"), "pop_75plus_2025")
  ), fill = TRUE)
}
fwrite(primary_cross, file.path(out_tables, "Supplementary_Table_candidate_snap_sensitivity_primary_crossclassification.csv"))

# ----------------------------------------------------------------------------
# C3. FINAL PRIMARY sensitivity using BOTH route endpoints
# ----------------------------------------------------------------------------
# A primary route is flagged when EITHER:
#   - the mesh centroid snap distance exceeds the cut-off, OR
#   - the PRIMARY time-minimising facility-origin snap distance exceeds it.
#
# Complete-case sensitivity excludes such routes from the denominator.
# Conservative sensitivity retains the original denominator but treats such
# routes as unreachable. The primary analysis itself remains unchanged.

primary_endpoint_sensitivity <- list(
  summarise_accessibility(
    master2,
    "primary_min_duration_min",
    "Primary: all centroids"
  )
)

primary_endpoint_cross <- rbindlist(list(
  summarise_cross(
    master2,
    "primary_min_duration_min",
    "Primary all centroids",
    NULL
  ),
  summarise_cross(
    master2,
    "primary_min_duration_min",
    "Primary all centroids",
    "pop_75plus_2025"
  )
))

endpoint_cutoff_summary <- rbindlist(lapply(snap_sensitivity_m, function(cut) {
  bad <- !is.na(master2$primary_endpoint_snap_max_m) &
    master2$primary_endpoint_snap_max_m > cut

  data.table(
    cutoff_m = cut,
    n_primary_routes_above_cutoff = sum(bad),
    pct_primary_routes_above_cutoff = safe_pct(sum(bad), nrow(master2)),
    pop75_above_cutoff = sum(master2$pop_75plus_2025[bad], na.rm = TRUE),
    pop75_pct_above_cutoff = safe_pct(
      sum(master2$pop_75plus_2025[bad], na.rm = TRUE),
      sum(master2$pop_75plus_2025, na.rm = TRUE)
    ),
    n_due_centroid_snap = sum(
      bad & master2$centroid_snap_distance_m > cut,
      na.rm = TRUE
    ),
    n_due_primary_facility_snap = sum(
      bad & master2$primary_time_min_facility_snap_distance_m > cut,
      na.rm = TRUE
    ),
    n_both_endpoints_above = sum(
      master2$centroid_snap_distance_m > cut &
        master2$primary_time_min_facility_snap_distance_m > cut,
      na.rm = TRUE
    )
  )
}))
fwrite(
  endpoint_cutoff_summary,
  file.path(out_qc, "qc_primary_both_endpoint_snap_summary.csv")
)

for (cut in snap_sensitivity_m) {
  bad <- !is.na(master2$primary_endpoint_snap_max_m) &
    master2$primary_endpoint_snap_max_m > cut

  cc <- master2[!bad]
  primary_endpoint_sensitivity[[length(primary_endpoint_sensitivity) + 1L]] <-
    summarise_accessibility(
      cc,
      "primary_min_duration_min",
      paste0("Primary complete-case: either endpoint snap <=", cut, " m")
    )

  cons <- copy(master2)
  cons[, primary_duration_both_endpoint_cons := primary_min_duration_min]
  cons[bad, primary_duration_both_endpoint_cons := NA_real_]

  primary_endpoint_sensitivity[[length(primary_endpoint_sensitivity) + 1L]] <-
    summarise_accessibility(
      cons,
      "primary_duration_both_endpoint_cons",
      paste0(
        "Primary conservative: either endpoint snap >",
        cut,
        " m treated unreachable"
      )
    )

  primary_endpoint_cross <- rbindlist(list(
    primary_endpoint_cross,
    summarise_cross(
      cc,
      "primary_min_duration_min",
      paste0("Complete-case both endpoints <=", cut, " m"),
      NULL
    ),
    summarise_cross(
      cc,
      "primary_min_duration_min",
      paste0("Complete-case both endpoints <=", cut, " m"),
      "pop_75plus_2025"
    ),
    summarise_cross(
      cons,
      "primary_duration_both_endpoint_cons",
      paste0("Conservative either endpoint >", cut, " m unreachable"),
      NULL
    ),
    summarise_cross(
      cons,
      "primary_duration_both_endpoint_cons",
      paste0("Conservative either endpoint >", cut, " m unreachable"),
      "pop_75plus_2025"
    )
  ), fill = TRUE)
}

primary_endpoint_sensitivity <- rbindlist(
  primary_endpoint_sensitivity,
  fill = TRUE
)

fwrite(
  primary_endpoint_sensitivity,
  file.path(
    out_tables,
    "S_primary_both_endpoint_snap_accessibility.csv"
  )
)

fwrite(
  primary_endpoint_cross,
  file.path(
    out_tables,
    "S_primary_both_endpoint_snap_cross.csv"
  )
)

# Figure 2 diagnostic after excluding routes with >1-km snapping at EITHER
# endpoint of the PRIMARY minimum-time route. This is a diagnostic display only;
# the primary manuscript analysis continues to retain all centroids.
fig2_primary_endpoint_clean_data <- master2[
  !is.na(nearest_distance_km) &
    !is.na(primary_min_duration_min) &
    !is.na(primary_endpoint_snap_max_m) &
    primary_endpoint_snap_max_m <= figure2_primary_endpoint_display_cutoff_m &
    nearest_distance_km <= figure_max_distance_km &
    primary_min_duration_min <= figure_max_duration_min
]

fig2_primary_endpoint_clean <- ggplot(
  fig2_primary_endpoint_clean_data,
  aes(nearest_distance_km, primary_min_duration_min)
) +
  stat_binhex(bins = 70) +
  scale_fill_viridis_c(trans = "log10", name = "Mesh count") +
  geom_vline(xintercept = 16, linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = thresholds, linetype = "dotted", linewidth = 0.5) +
  coord_cartesian(
    xlim = c(0, figure_max_distance_km),
    ylim = c(0, figure_max_duration_min)
  ) +
  labs(
    x = "Straight-line distance to nearest qualifying facility (km)",
    y = "OSRM-estimated minimum travel time (min)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(
    out_figures,
    "qc_figure2_both_endpoint_snap_le1000m.png"
  ),
  fig2_primary_endpoint_clean,
  width = 7,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Count whether physically implausible patterns remain after both-endpoint QC.
remaining_plausibility_after_endpoint_qc <- rbindlist(
  lapply(snap_sensitivity_m, function(cut) {
    z <- master2[
      !is.na(primary_endpoint_snap_max_m) &
        primary_endpoint_snap_max_m <= cut
    ]
    data.table(
      cutoff_m = cut,
      n_centroids_retained = nrow(z),
      distance_gt20_time_lt5_remaining = sum(
        z$nearest_distance_km > 20 &
          !is.na(z$primary_min_duration_min) &
          z$primary_min_duration_min < 5,
        na.rm = TRUE
      ),
      speed_gt150_dist_ge5_remaining = sum(
        !is.na(z$implied_speed_lower_bound_kmh) &
          z$nearest_distance_km >= 5 &
          z$implied_speed_lower_bound_kmh > 150,
        na.rm = TRUE
      ),
      speed_gt200_dist_ge5_remaining = sum(
        !is.na(z$implied_speed_lower_bound_kmh) &
          z$nearest_distance_km >= 5 &
          z$implied_speed_lower_bound_kmh > 200,
        na.rm = TRUE
      )
    )
  })
)

fwrite(
  remaining_plausibility_after_endpoint_qc,
  file.path(
    out_qc,
    "qc_plausibility_after_both_endpoint_filter.csv"
  )
)

# ----------------------------------------------------------------------------
# D. Same-facility sensitivity analysis
# ----------------------------------------------------------------------------
# Same-facility duration must be >= primary minimum duration except for small
# differences due to rounding/server reproducibility. Compare particularly in
# rows where the facility coordinates are the same.
master2[, duration_delta_same_minus_primary :=
  same_facility_duration_min - primary_min_duration_min
]

repro_same_coords <- master2[time_min_facility_same_as_straight_nearest == TRUE &
                               !is.na(primary_min_duration_min) &
                               !is.na(same_facility_duration_min)]

repro_qc <- data.table(
  n_same_coordinate_cases = nrow(repro_same_coords),
  median_abs_duration_difference_min = median(abs(repro_same_coords$duration_delta_same_minus_primary), na.rm = TRUE),
  p95_abs_duration_difference_min = as.numeric(quantile(abs(repro_same_coords$duration_delta_same_minus_primary), .95, na.rm = TRUE)),
  p99_abs_duration_difference_min = as.numeric(quantile(abs(repro_same_coords$duration_delta_same_minus_primary), .99, na.rm = TRUE)),
  max_abs_duration_difference_min = max(abs(repro_same_coords$duration_delta_same_minus_primary), na.rm = TRUE)
)
fwrite(repro_qc, file.path(out_qc, "qc_osrm_reproducibility_same_coordinate_facility_cases.csv"))

same_cross <- rbindlist(list(
  summarise_cross(master2, "same_facility_duration_min", "Same straight-line-nearest facility", NULL),
  summarise_cross(master2, "same_facility_duration_min", "Same straight-line-nearest facility", "pop_75plus_2025")
))
fwrite(same_cross, file.path(out_tables, "Supplementary_Table_S8_candidate_same_facility_crossclassification.csv"))

# Same-facility travel-time distribution by straight-line distance band.
master2[, distance_band := cut(
  nearest_distance_km,
  breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, Inf),
  labels = c("0-2 km", "2-4 km", "4-6 km", "6-8 km", "8-10 km", "10-12 km", "12-14 km", "14-16 km", ">16 km"),
  right = TRUE
)]

distance_band_same <- master2[, .(
  n_centroids = .N,
  median_same_facility_time_min = median(same_facility_duration_min, na.rm = TRUE),
  q1_same_facility_time_min = as.numeric(quantile(same_facility_duration_min, .25, na.rm = TRUE)),
  q3_same_facility_time_min = as.numeric(quantile(same_facility_duration_min, .75, na.rm = TRUE)),
  p95_same_facility_time_min = as.numeric(quantile(same_facility_duration_min, .95, na.rm = TRUE)),
  pct_over_15min = safe_pct(sum(is.na(same_facility_duration_min) | same_facility_duration_min > 15), .N),
  pct_over_30min = safe_pct(sum(is.na(same_facility_duration_min) | same_facility_duration_min > 30), .N),
  pct_over_45min = safe_pct(sum(is.na(same_facility_duration_min) | same_facility_duration_min > 45), .N),
  pct_over_60min = safe_pct(sum(is.na(same_facility_duration_min) | same_facility_duration_min > 60), .N)
), by = distance_band]
fwrite(distance_band_same, file.path(out_tables, "Supplementary_Table_candidate_same_facility_time_by_distance_band.csv"))

# Candidate Supplementary Figure S7.
# Save an unfiltered diagnostic PNG, then create the FINAL candidate display
# after restricting BOTH endpoints of the same-facility route to <=1 km snap.
fig_s7_raw_data <- master2[
  !is.na(nearest_distance_km) & !is.na(same_facility_duration_min) &
    nearest_distance_km <= figure_max_distance_km &
    same_facility_duration_min <= figure_max_duration_min
]

fig_s7_raw <- ggplot(
  fig_s7_raw_data,
  aes(nearest_distance_km, same_facility_duration_min)
) +
  stat_binhex(bins = 70) +
  scale_fill_viridis_c(trans = "log10", name = "Mesh count") +
  geom_vline(xintercept = 16, linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = thresholds, linetype = "dotted", linewidth = 0.5) +
  coord_cartesian(
    xlim = c(0, figure_max_distance_km),
    ylim = c(0, figure_max_duration_min)
  ) +
  labs(
    x = "Straight-line distance to nearest qualifying facility (km)",
    y = "OSRM-estimated travel time to the same facility (min)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

ggsave(
  file.path(
    out_figures,
    "qc_S7_raw_unfiltered_same_facility_distance_time_hexbin.png"
  ),
  fig_s7_raw,
  width = 6.83,
  height = 5.4,
  units = "in",
  dpi = 300,
  bg = "white"
)

fig_s7_data <- master2[
  !is.na(nearest_distance_km) &
    !is.na(same_facility_duration_min) &
    !is.na(centroid_snap_distance_m) &
    !is.na(straight_nearest_facility_snap_distance_m) &
    centroid_snap_distance_m <= supplementary_figure_s7_snap_display_cutoff_m &
    straight_nearest_facility_snap_distance_m <=
      supplementary_figure_s7_snap_display_cutoff_m &
    nearest_distance_km <= figure_max_distance_km &
    same_facility_duration_min <= figure_max_duration_min
]

s7_display_filter_summary <- data.table(
  snap_display_cutoff_m = supplementary_figure_s7_snap_display_cutoff_m,
  n_all_unique_centroids = nrow(master2),
  n_same_facility_reachable = sum(!is.na(master2$same_facility_duration_min)),
  n_retained_for_display = nrow(fig_s7_data),
  n_excluded_for_either_endpoint_snap = sum(
    !is.na(master2$same_facility_duration_min) &
      (
        master2$centroid_snap_distance_m >
          supplementary_figure_s7_snap_display_cutoff_m |
        master2$straight_nearest_facility_snap_distance_m >
          supplementary_figure_s7_snap_display_cutoff_m
      ),
    na.rm = TRUE
  )
)
fwrite(
  s7_display_filter_summary,
  file.path(out_qc, "qc_S7_both_endpoint_snap_display_filter_summary.csv")
)

fig_s7 <- ggplot(
  fig_s7_data,
  aes(nearest_distance_km, same_facility_duration_min)
) +
  stat_binhex(bins = 70) +
  scale_fill_viridis_c(trans = "log10", name = "Mesh count") +
  geom_vline(xintercept = 16, linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = thresholds, linetype = "dotted", linewidth = 0.5) +
  annotate(
    "text",
    x = 16.25,
    y = 1.75,
    label = "16 km",
    hjust = 0,
    size = 3.2
  ) +
  coord_cartesian(
    xlim = c(0, figure_max_distance_km),
    ylim = c(0, figure_max_duration_min)
  ) +
  labs(
    x = "Straight-line distance to nearest qualifying facility (km)",
    y = "OSRM-estimated travel time to the same facility (min)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

for (ext in c("pdf", "png", "tiff")) {
  fn <- file.path(
    out_figures,
    paste0(
      "Supplementary_Figure_S7_candidate_same_facility_",
      "distance_time_hexbin_both_endpoint_snap_le1000m.",
      ext
    )
  )

  if (ext == "pdf") {
    ggsave(fn, fig_s7, width = 6.83, height = 5.4, units = "in")
  } else if (ext == "png") {
    ggsave(
      fn,
      fig_s7,
      width = 6.83,
      height = 5.4,
      units = "in",
      dpi = 600,
      bg = "white"
    )
  } else {
    ggsave(
      fn,
      fig_s7,
      width = 6.83,
      height = 5.4,
      units = "in",
      dpi = 600,
      compression = "lzw",
      bg = "white"
    )
  }
}

# Combined comparison summary.
comparison_summary <- data.table(
  metric = c(
    "Unique centroids",
    "Primary unreachable",
    "Same-facility unreachable",
    "Facility coordinates identical: primary time-min vs straight-line nearest",
    "Facility coordinates different",
    "Median primary minimum duration, min",
    "Median same-facility duration, min",
    "Median same-minus-primary duration difference, min",
    "Unique PRIMARY time-minimising facility coordinates",
    "Median PRIMARY time-min facility-origin snap distance, m",
    "PRIMARY routes with either endpoint snap >500 m",
    "PRIMARY routes with either endpoint snap >1000 m"
  ),
  value = c(
    nrow(master2),
    sum(is.na(master2$primary_min_duration_min)),
    sum(is.na(master2$same_facility_duration_min)),
    sum(master2$time_min_facility_same_as_straight_nearest, na.rm = TRUE),
    sum(!master2$time_min_facility_same_as_straight_nearest, na.rm = TRUE),
    median(master2$primary_min_duration_min, na.rm = TRUE),
    median(master2$same_facility_duration_min, na.rm = TRUE),
    median(master2$duration_delta_same_minus_primary, na.rm = TRUE),
    nrow(primary_facilities),
    median(primary_facilities$primary_time_min_facility_snap_distance_m, na.rm = TRUE),
    sum(master2$primary_endpoint_snap_max_m > 500, na.rm = TRUE),
    sum(master2$primary_endpoint_snap_max_m > 1000, na.rm = TRUE)
  )
)
fwrite(comparison_summary, file.path(out_qc, "qc_primary_vs_same_facility_summary.csv"))

# Save session information.
sink(file.path(out_logs, "sessionInfo.txt"))
print(sessionInfo())
sink()

message("All v1.4 additional QC/sensitivity analyses completed successfully.")
message("Output root: ", normalizePath(out_root, winslash = "/", mustWork = FALSE))
# ============================================================================
