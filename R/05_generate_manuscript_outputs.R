# ============================================================================
# Final QC fixes: vector-safe percentages, zero-count categories, zero-unreachable handling, and immutable OSRM image metadata.
# Smoke-test fixes: zero OSRM-unreachable centroids and zero-count cross-classification categories.
# 05_generate_manuscript_outputs.R
# BMJ Health & Care Informatics submission analysis
# Nationwide potential spatial accessibility to home-care support facilities
# Version 7.1: unique-spatial-centroid QC + post-collapse QC fix + BMJ HCI reproducibility metadata
#
# Purpose
#   This script analyses precomputed national OSRM outputs and creates the
#   manuscript tables, figures, supplementary materials, QGIS layers, QC files,
#   and reproducibility metadata for the BMJ HCI submission. It also documents
#   the open-source, locally executable workflow and its implementation on
#   consumer-grade hardware.
#
# Important interpretation
#   - The outcome is potential spatial accessibility, not realised service use.
#   - OSRM-estimated time is a standardised road-network proxy. It is not an
#     observed journey time and does not include traffic, snow, parking,
#     building access, or ferry waiting time.
#   - The 15/30/45/60-minute cut-offs are prespecified operational scenarios,
#     not validated clinical response-time standards.
#   - The facility satisfying the 16-km criterion and the time-minimising
#     facility need not be the same. Cross-classification compares geographic
#     coverage under the two criteria, not facility-pair concordance.
#
# Scope of this file
#   This file starts after the national OSRM table calculations have been
#   completed. The upstream routing script, OSRM/OSM versions, snapshot date,
#   hardware, and routing runtime must be documented in the metadata settings.
#
# Required input files
#   1. policy_crosswalk_16km_osrm_master_v4.csv
#   2. population_mesh_arrival_zone_from_any_clinic_osrm.csv
#   3. japan_population_mesh_centroids_for_osrm_2020_2070.csv
#   4. population_mesh_nearest_clinic_duration_by_osrm.csv
#   5. japan_clinic_address_only_csv_matched.csv
#
# Manuscript outputs
#   Main tables (2):
#     Table 1. National road-network accessibility and cumulative accessibility
#              within the 16-km catchment
#     Table 2. Cross-classification of the 16-km criterion and OSRM scenarios
#   Main figures (3):
#     Figure 1. Reproducible geospatial informatics workflow (R)
#     Figure 2. Straight-line distance versus OSRM-estimated travel time (R)
#     Figure 3. 16-km × 15/30/45/60-minute cross-classification map (QGIS)
#   Supplementary figures (6):
#     S1. Conceptual 16-km radius and time catchments (R)
#     S2. National exclusive travel-time-band map (QGIS)
#     S3. Prefecture heatmap within 16 km (R)
#     S4. Future outside-threshold population trajectories (R)
#     S5. Distance-threshold population coverage (R)
#     S6. Exclusive time-band composition within 16 km (R)
#
# The script also retains detailed analytic audit tables and QC outputs. Only
# files in main_tables_bmj_hci/ and main_figures_bmj_hci/ are intended as the
# two main tables and R-generated main figures for the submission.
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
required_packages <- c(
  "data.table", "stringr", "ggplot2", "scales", "hexbin",
  "sf", "dplyr", "tidyr", "flextable", "officer",
  "openxlsx", "patchwork"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Required packages are not installed: ",
      paste(missing_packages, collapse = ", "),
      ". Install them in a controlled project environment before running."
    )
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(hexbin)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(flextable)
  library(officer)
  library(openxlsx)
  library(patchwork)
})

options(scipen = 999)
script_started_at <- Sys.time()

# ----------------------------------------------------------------------------
# 1. User settings
# ----------------------------------------------------------------------------
# set working directory

file_crosswalk <- cfg$crosswalk_master_file
file_arrival   <- cfg$osrm_arrival_file
file_centroids <- cfg$centroid_file_full
file_nearest   <- cfg$osrm_nearest_file
file_clinics   <- cfg$facility_file

thresholds <- cfg$thresholds
selected_main_years <- c(2025, 2030, 2040, 2050, 2060, 2070)
main_age_groups <- c("75plus", "80plus")

# --------------------------------------------------------------------------
# Reproducibility metadata
# --------------------------------------------------------------------------
# Values known from the analysis workstation are filled below.
# Fields that depend on the upstream OSRM build/download log remain explicit
# placeholders. Do not guess these values; replace them from the original logs
# before submission.
analysis_version <- "BMJ_HCI_OSRM_v7_1_unique_spatial_centroids_qcfix"
routing_script_name <- "R/03_calculate_osrm_minimum_travel_time.R"
osrm_backend_version <- cfg$study_metadata$osrm_backend_version
osrm_docker_image_tag <- cfg$study_metadata$osrm_docker_image_tag
osrm_docker_image_digest <- cfg$study_metadata$osrm_docker_image_digest
osrm_docker_image_id <- cfg$study_metadata$osrm_docker_image_id
osrm_docker_image_created <- cfg$study_metadata$osrm_docker_image_created
osm_extract_provider <- cfg$study_metadata$osm_extract_provider
osm_snapshot_date <- cfg$study_metadata$osm_snapshot_date

# R is detected from the environment in which this post-processing script runs.
r_version_reported <- R.version.string
qgis_version <- cfg$study_metadata$qgis_version

# Workstation used for the national analysis.
analysis_device <- cfg$study_metadata$analysis_device
analysis_os_manual <- cfg$study_metadata$analysis_os_manual
analysis_cpu <- cfg$study_metadata$analysis_cpu
analysis_ram_gb <- cfg$study_metadata$analysis_ram_gb
analysis_storage <- cfg$study_metadata$analysis_storage
analysis_compute_class <- cfg$study_metadata$analysis_compute_class
specialised_hpc_or_cloud_used <- cfg$study_metadata$specialised_hpc_or_cloud_used

# Keep an automatically detected OS string as an audit field as well.
analysis_os_detected <- paste(
  Sys.info()[c("sysname", "release", "machine")],
  collapse = " / "
)

osrm_preprocessing_runtime <- paste0(
  "Exact elapsed preprocessing runtime was not prospectively retained. ",
  "Retained OSRM preprocessing outputs span 2026-05-12 23:57:31 to ",
  "2026-05-13 00:20:10 JST (approximately 22 min 39 s); this file-time ",
  "span is not treated as the exact elapsed runtime."
)
osrm_national_table_runtime <- paste0(
  "The first retained routing checkpoint was created at 05:39:25 JST on ",
  "2026-06-06 after completion of the first facility block, and the final ",
  "checkpoint was saved at 13:40:42 JST on the same day. The observed ",
  "interval was 8 h 1 min 17 s. The exact total runtime was slightly longer ",
  "because the first-block start timestamp was not prospectively retained."
)
repository_url_or_doi <- cfg$repository_url_or_doi

# Data-provenance fields already known from the study record.
jmap_source <- "JMAP Regional Medical Information System"
jmap_acquisition_date <- cfg$study_metadata$jmap_acquisition_date
population_source <- "MLIT 500 m mesh future population estimates (R6 National Spatial Strategy Bureau estimates)"
population_acquisition_date <- cfg$study_metadata$population_acquisition_date

# Main-figure export settings (BMJ full-width target).
main_figure_width_cm <- 17.35
main_figure_dpi <- 600
main_figure_width_in <- main_figure_width_cm / 2.54

# Figure 3 final QGIS layout: 2 x 2 panels. Within each panel, the principal
# islands/mainland frame and remote-island frame are exported at the SAME MAP
# SCALE but with different frame heights. Values reflect the agreed layout.
figure3_total_width_cm <- 17.35
figure3_total_height_cm <- 16.50
figure3_panel_width_cm <- 8.30
figure3_panel_height_cm <- 7.20
figure3_mainland_width_cm <- 7.80
figure3_mainland_height_cm <- 4.60
figure3_islands_width_cm <- 7.80
figure3_islands_height_cm <- 1.50
figure3_export_dpi <- 600

metadata_placeholders <- c(
  routing_script_name,
  osrm_backend_version,
  osrm_docker_image_tag,
  osm_extract_provider,
  osm_snapshot_date,
  osrm_preprocessing_runtime,
  osrm_national_table_runtime
)

if (any(metadata_placeholders == "TO_BE_COMPLETED")) {
  warning(
    "Reproducibility metadata contain TO_BE_COMPLETED fields. ",
    "The analysis can run, but replace them from the upstream routing logs ",
    "before manuscript submission."
  )
}

# Optional manually reviewed island/ferry QC file.
# Expected columns: mesh_id, exclude_from_mainland_sensitivity, review_category,
# review_notes. No mesh is excluded from the main analysis automatically.
file_manual_island_review <- "qc_island_ferry_review_completed.csv"

# 本文Tableの表示桁
table3_pct_digits <- 2L
other_main_pct_digits <- 1L

# Figure 2の表示範囲。解析データ自体は切り捨てず、図示のみを制限する。
figure2_max_distance_km <- 30
figure2_max_duration_min <- 120

# 以前の出力と混在させないため、revisedフォルダに出力する。
out_root <- file.path(cfg$output_dir, "manuscript_outputs")
out_tables <- file.path(out_root, "tables")
out_tables_docx <- file.path(out_root, "tables_docx_analytic_audit")
out_bmj_tables <- file.path(out_root, "main_tables_bmj_hci")
out_bmj_tables_docx <- file.path(out_root, "main_tables_bmj_hci_docx")
out_supp <- file.path(out_root, "supplementary_tables")
out_figures <- file.path(out_root, "main_figures_bmj_hci")
out_supp_figures <- file.path(out_root, "supplementary_figures_bmj_hci")
out_qgis <- file.path(out_root, "qgis_layers")
out_qc <- file.path(out_root, "qc")
out_logs <- file.path(out_root, "logs")

for (d in c(
  out_root, out_tables, out_tables_docx, out_bmj_tables,
  out_bmj_tables_docx, out_supp, out_figures,
  out_supp_figures, out_qgis, out_qc, out_logs
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ----------------------------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------------------------
standardize_mesh_data <- function(dt) {
  dt <- copy(dt)

  rename_map <- c(
    "MESH_ID" = "mesh_id",
    "SHICODE" = "shicode",
    "CENTROID_LON" = "centroid_lon",
    "CENTROID_LAT" = "centroid_lat"
  )

  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(dt) && !new %in% names(dt)) {
      setnames(dt, old, new)
    }
  }

  if ("mesh_id" %in% names(dt)) {
    dt[, mesh_id := as.character(mesh_id)]
  }

  if ("shicode" %in% names(dt)) {
    dt[, shicode := fifelse(
      is.na(shicode),
      NA_character_,
      str_pad(as.character(shicode), width = 5, pad = "0")
    )]
    dt[, pref_code := substr(shicode, 1, 2)]
  }

  if ("centroid_lon" %in% names(dt)) {
    dt[, centroid_lon := as.numeric(centroid_lon)]
  }

  if ("centroid_lat" %in% names(dt)) {
    dt[, centroid_lat := as.numeric(centroid_lat)]
  }

  dt[]
}

make_join_key <- function(dt, digits = 7) {
  stopifnot(all(c("mesh_id", "centroid_lon", "centroid_lat") %in% names(dt)))

  paste0(
    as.character(dt$mesh_id), "_",
    sprintf(paste0("%.", digits, "f"), as.numeric(dt$centroid_lon)), "_",
    sprintf(paste0("%.", digits, "f"), as.numeric(dt$centroid_lat))
  )
}

add_missing_column_by_key <- function(base, source, col, key = "join_key") {
  if (!col %in% names(source)) return(invisible(NULL))
  if (!key %in% names(base) || !key %in% names(source)) return(invisible(NULL))

  # Some source population meshes are split across administrative areas.
  # This yields duplicated spatial keys. When the source and base have the
  # same row count and identical key order, row-order transfer is safe and
  # preserves the administrative population allocation. Otherwise a unique
  # key match is required.
  if (anyDuplicated(source[[key]])) {
    same_row_key_order <-
      nrow(base) == nrow(source) &&
      identical(as.character(base[[key]]), as.character(source[[key]]))

    if (!same_row_key_order) {
      warning(
        "Cannot safely add/fill column from duplicated source join_key: ",
        col,
        ". Row counts/key order are not identical."
      )
      return(invisible(NULL))
    }

    incoming <- source[[col]]
  } else {
    idx <- match(base[[key]], source[[key]])
    incoming <- source[[col]][idx]
  }

  if (!col %in% names(base)) {
    base[, (col) := incoming]
  } else {
    missing_idx <- which(is.na(base[[col]]) & !is.na(incoming))

    if (length(missing_idx) > 0) {
      set(
        base,
        i = missing_idx,
        j = col,
        value = incoming[missing_idx]
      )
    }
  }

  invisible(NULL)
}

as_logical_16km <- function(x) {
  if (is.logical(x)) return(x)

  tolower(as.character(x)) %in% c(
    "true", "t", "1", "yes", "y"
  )
}

fmt_n <- function(x) {
  ifelse(
    is.na(x),
    "",
    format(
      round(x, 0),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}

# 割合表示関数
# 例：
#   digits = 1の場合
#     0       -> 0.0%
#     0.034   -> <0.1%
#     12.345  -> 12.3%
#   digits = 2の場合
#     0.004   -> <0.01%
#     12.345  -> 12.35%
fmt_pct <- function(x, digits = 1) {
  cutoff <- 10^(-digits)

  vapply(x, function(z) {
    if (is.na(z)) return("")

    if (z == 0) {
      return(paste0(
        formatC(0, format = "f", digits = digits),
        "%"
      ))
    }

    if (z > 0 && z < cutoff) {
      return(paste0(
        "<",
        formatC(cutoff, format = "f", digits = digits),
        "%"
      ))
    }

    paste0(
      formatC(z, format = "f", digits = digits, big.mark = ","),
      "%"
    )
  }, character(1))
}

fmt_n_pct <- function(n, pct, digits = 1) {
  paste0(
    fmt_n(n),
    " (",
    fmt_pct(pct, digits),
    ")"
  )
}

safe_ratio_pct <- function(num, den) {
  # Vector-safe percentage helper.
  # Handles scalar or vector numerators/denominators without the length-1
  # behaviour of base::ifelse() when its test argument is scalar.
  num <- as.numeric(num)
  den <- as.numeric(den)

  n <- max(length(num), length(den))
  if (n == 0L) return(numeric(0))

  if (length(num) == 0L) {
    num <- rep(NA_real_, n)
  } else {
    num <- rep_len(num, n)
  }

  if (length(den) == 0L) {
    den <- rep(NA_real_, n)
  } else {
    den <- rep_len(den, n)
  }

  out <- rep(NA_real_, n)
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- 100 * num[ok] / den[ok]
  out
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0

  if (!any(ok)) return(NA_real_)

  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_quantile <- function(x, w, probs = c(0.5), na.rm = TRUE) {
  if (na.rm) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    x <- x[ok]
    w <- w[ok]
  }

  if (length(x) == 0 || sum(w) <= 0) {
    return(rep(NA_real_, length(probs)))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumulative_weight <- cumsum(w) / sum(w)

  sapply(
    probs,
    function(p) x[which(cumulative_weight >= p)[1]]
  )
}

calc_pwa <- function(dt, pop_col, threshold, by_vars = NULL) {
  if (!pop_col %in% names(dt)) {
    stop("Population column not found: ", pop_col)
  }

  tmp <- copy(dt)

  tmp[, pop_value := as.numeric(get(pop_col))]
  tmp[is.na(pop_value), pop_value := 0]

  tmp[, reachable :=
    !is.na(min_duration_min) &
    min_duration_min <= threshold
  ]

  if (is.null(by_vars)) {
    out <- tmp[, .(
      population = sum(pop_value, na.rm = TRUE),
      reachable_population = sum(
        fifelse(reachable, pop_value, 0),
        na.rm = TRUE
      ),
      outside_population = sum(
        fifelse(!reachable, pop_value, 0),
        na.rm = TRUE
      )
    )]
  } else {
    out <- tmp[, .(
      population = sum(pop_value, na.rm = TRUE),
      reachable_population = sum(
        fifelse(reachable, pop_value, 0),
        na.rm = TRUE
      ),
      outside_population = sum(
        fifelse(!reachable, pop_value, 0),
        na.rm = TRUE
      )
    ), by = by_vars]
  }

  out[, pwa_pct :=
    safe_ratio_pct(reachable_population, population)
  ]

  out[, outside_pct :=
    safe_ratio_pct(outside_population, population)
  ]

  out[]
}

make_flextable <- function(
  df,
  caption = NULL,
  font_size = 8.5,
  max_width = 6.5
) {
  ft <- flextable(df)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- valign(ft, valign = "top", part = "all")
  ft <- autofit(ft)
  ft <- fit_to_width(ft, max_width = max_width)

  if (!is.null(caption)) {
    ft <- set_caption(ft, caption = caption)
  }

  ft
}

save_table_docx <- function(ft, filename) {
  doc <- read_docx()
  doc <- body_add_flextable(doc, ft)
  print(doc, target = filename)
}

round_numeric_cols <- function(dt, digits = 2) {
  out <- copy(dt)

  numeric_cols <- names(out)[
    vapply(out, is.numeric, logical(1))
  ]

  out[, (numeric_cols) :=
    lapply(.SD, round, digits = digits),
    .SDcols = numeric_cols
  ]

  out[]
}

# Table 4用：
# 行ごとに明確な分母を保持したlong形式を返す。
#
# 重要：
# 外側のスカラー変数とdata.table内で新規作成する列に同じ名前を使うと、
# data.tableの評価環境で名前が衝突し、割合計算が意図しない値を参照する
# 可能性がある。このため、外側の値はrow_denominator / row_measure_type
# という別名で保持し、列の作成とpct計算を別ステップに分ける。
make_band_long <- function(
  dt,
  label,
  weight_col = NULL,
  time_band_levels
) {
  if (is.null(weight_col)) {
    # メッシュ数を時間帯別に集計する。
    tmp <- dt[, .(
      value = .N
    ), by = time_band]

    # この行の分母は16 km以内にある全メッシュ数。
    row_denominator <- as.numeric(nrow(dt))
    row_measure_type <- "mesh count"
  } else {
    if (!weight_col %in% names(dt)) {
      stop("Weight column not found for Table 4: ", weight_col)
    }

    # 指定された人口を時間帯別に合計する。
    tmp <- dt[, .(
      value = sum(
        as.numeric(get(weight_col)),
        na.rm = TRUE
      )
    ), by = time_band]

    # この行の分母は16 km以内にある全メッシュの対象人口合計。
    row_denominator <- sum(
      as.numeric(dt[[weight_col]]),
      na.rm = TRUE
    )

    row_measure_type <- weight_col
  }

  # 0件の時間帯も必ず表に残すため、全時間帯を先に定義する。
  all_bands <- data.table(
    time_band = factor(
      time_band_levels,
      levels = time_band_levels
    )
  )

  tmp <- merge(
    all_bands,
    tmp,
    by = "time_band",
    all.x = TRUE,
    sort = FALSE
  )

  # 該当するメッシュまたは人口がない時間帯は0とする。
  tmp[is.na(value), value := 0]

  # ラベル、測定種別、分母、表示順を先に作る。
  # pctは同じ:=内で計算せず、次のステップで明示的に計算する。
  tmp[, `:=`(
    population_or_unit = label,
    measure_type = row_measure_type,
    denominator = row_denominator,
    band_order = match(
      as.character(time_band),
      time_band_levels
    )
  )]

  # 各排他的時間帯の割合を、行全体の共通分母で計算する。
  tmp[, pct := fifelse(
    is.na(denominator) | denominator <= 0,
    NA_real_,
    100 * as.numeric(value) / as.numeric(denominator)
  )]

  # 関数内部でも、値の合計が分母と一致することを確認する。
  value_sum <- sum(tmp$value, na.rm = TRUE)

  if (
    is.finite(row_denominator) &&
      abs(value_sum - row_denominator) >
        max(1e-6, abs(row_denominator) * 1e-10)
  ) {
    stop(
      paste0(
        "Table 4 internal QC failed for '", label, "': ",
        "sum(value)=", format(value_sum, scientific = FALSE),
        ", denominator=", format(row_denominator, scientific = FALSE),
        "."
      )
    )
  }

  setorder(tmp, band_order)

  tmp[]
}

# ----------------------------------------------------------------------------
# 3. Read and standardize the five requested files
# ----------------------------------------------------------------------------
for (f in c(
  file_crosswalk,
  file_arrival,
  file_centroids,
  file_nearest,
  file_clinics
)) {
  if (!file.exists(f)) {
    stop("Input file not found: ", f)
  }
}

input_files <- c(
  file_crosswalk,
  file_arrival,
  file_centroids,
  file_nearest,
  file_clinics
)

input_info <- file.info(input_files)
input_manifest <- data.table(
  file = basename(input_files),
  path = normalizePath(input_files, winslash = "/", mustWork = TRUE),
  size_bytes = as.numeric(input_info$size),
  modified_time = format(input_info$mtime, "%Y-%m-%d %H:%M:%S %z"),
  md5 = unname(tools::md5sum(input_files))
)

fwrite(
  input_manifest,
  file.path(out_logs, "input_file_manifest_with_md5.csv")
)

reproducibility_metadata <- data.table(
  field = c(
    "analysis_version", "routing_script_name", "osrm_backend_version",
    "osrm_docker_image_tag", "osrm_docker_image_digest",
    "osrm_docker_image_id", "osrm_docker_image_created",
    "osm_extract_provider", "osm_snapshot_date",
    "R_version", "QGIS_version", "analysis_device", "analysis_OS_manual",
    "analysis_OS_detected", "analysis_CPU", "analysis_RAM_GB",
    "analysis_storage", "analysis_compute_class",
    "specialised_HPC_or_cloud_used", "OSRM_preprocessing_runtime",
    "OSRM_national_table_runtime", "repository_URL_or_DOI",
    "analysis_started_at"
  ),
  value = c(
    analysis_version, routing_script_name, osrm_backend_version,
    osrm_docker_image_tag, osrm_docker_image_digest,
    osrm_docker_image_id, osrm_docker_image_created,
    osm_extract_provider, osm_snapshot_date,
    r_version_reported, qgis_version, analysis_device, analysis_os_manual,
    analysis_os_detected, analysis_cpu, analysis_ram_gb, analysis_storage,
    analysis_compute_class, as.character(specialised_hpc_or_cloud_used),
    osrm_preprocessing_runtime, osrm_national_table_runtime,
    repository_url_or_doi,
    format(script_started_at, "%Y-%m-%d %H:%M:%S %z")
  )
)

fwrite(
  reproducibility_metadata,
  file.path(out_logs, "reproducibility_metadata.csv")
)

cross <- standardize_mesh_data(
  fread(file_crosswalk)
)

arrival <- standardize_mesh_data(
  fread(file_arrival)
)

centroids <- standardize_mesh_data(
  fread(file_centroids)
)

nearest <- standardize_mesh_data(
  fread(file_nearest)
)

clinics <- fread(file_clinics)

# クリニック座標を標準化
if ("fX" %in% names(clinics)) {
  clinics[, clinic_lon := as.numeric(fX)]
}

if ("fY" %in% names(clinics)) {
  clinics[, clinic_lat := as.numeric(fY)]
}

if (!"clinic_id" %in% names(clinics)) {
  clinics[, clinic_id := .I]
}

# join keyを作成
for (obj_name in c(
  "cross",
  "arrival",
  "centroids",
  "nearest"
)) {
  obj <- get(obj_name)

  if (all(
    c(
      "mesh_id",
      "centroid_lon",
      "centroid_lat"
    ) %in% names(obj)
  )) {
    obj[, join_key := make_join_key(obj)]
  }

  assign(obj_name, obj)
}

# policy_crosswalkを行数保持のprimary masterとする
master <- copy(cross)
master[, source_row_id := .I]

expected_n <- nrow(master)

# 他ファイルから人口列を補完
population_cols_all <- unique(c(
  grep(
    "^pop_(total|65plus|75plus|80plus)_[0-9]{4}$",
    names(arrival),
    value = TRUE
  ),
  grep(
    "^pop_(total|65plus|75plus|80plus)_[0-9]{4}$",
    names(centroids),
    value = TRUE
  )
))

for (col in population_cols_all) {
  add_missing_column_by_key(master, arrival, col)
  add_missing_column_by_key(master, centroids, col)
}

# OSRM・最寄り施設情報を補完
for (col in c(
  "min_duration_min",
  "nearest_clinic_id",
  "nearest_clinic_lon",
  "nearest_clinic_lat"
)) {
  add_missing_column_by_key(master, arrival, col)
  add_missing_column_by_key(master, nearest, col)
}

required_master_cols <- c(
  "mesh_id",
  "shicode",
  "centroid_lon",
  "centroid_lat",
  "nearest_distance_km",
  "within_16km",
  "min_duration_min"
)

missing_master_cols <- setdiff(
  required_master_cols,
  names(master)
)

if (length(missing_master_cols) > 0) {
  stop(
    "Master data are missing required columns: ",
    paste(missing_master_cols, collapse = ", ")
  )
}

master[, within_16km :=
  as_logical_16km(within_16km)
]

master[, nearest_distance_km :=
  as.numeric(nearest_distance_km)
]

master[, min_duration_min :=
  as.numeric(min_duration_min)
]

master[, pref_code :=
  substr(shicode, 1, 2)
]

setorder(master, source_row_id)

stopifnot(nrow(master) == expected_n)

# 人口列をnumeric化
pop_cols <- grep(
  "^pop_(total|65plus|75plus|80plus)_[0-9]{4}$",
  names(master),
  value = TRUE
)

for (col in pop_cols) {
  master[, (col) := as.numeric(get(col))]
  master[is.na(get(col)), (col) := 0]
}

# 本コードで必ず使う人口列を確認
required_population_cols <- c(
  "pop_total_2025",
  "pop_75plus_2025",
  "pop_80plus_2025",
  "pop_75plus_2070"
)

missing_population_cols <- setdiff(
  required_population_cols,
  names(master)
)

if (length(missing_population_cols) > 0) {
  stop(
    "Required population columns are missing: ",
    paste(missing_population_cols, collapse = ", ")
  )
}

# Computational-scale audit for the informatics contribution.
# The upstream routing run used all source mesh-administrative-area records.
source_mesh_records <- nrow(master)
unique_spatial_centroids_pre_qc <- uniqueN(master$join_key)
potential_od_pairs <- as.numeric(source_mesh_records) * as.numeric(nrow(clinics))
unique_facility_centroid_pairs <-
  as.numeric(unique_spatial_centroids_pre_qc) * as.numeric(nrow(clinics))

chunk_facilities <- 1000L
chunk_meshes <- 4000L
nominal_facility_blocks <- ceiling(nrow(clinics) / chunk_facilities)
nominal_mesh_blocks <- ceiling(source_mesh_records / chunk_meshes)
nominal_chunk_grid <- nominal_facility_blocks * nominal_mesh_blocks

computational_scale <- data.table(
  metric = c(
    "Number of analysed facilities",
    "Source mesh-administrative-area records routed",
    "Unique spatial mesh centroids represented",
    "Potential facility-record OD pairs actually processed",
    "Unique facility-centroid combinations represented",
    "Facility block size",
    "Mesh-record block size",
    "Nominal facility blocks",
    "Nominal mesh-record blocks",
    "Nominal chunk grid"
  ),
  value = c(
    nrow(clinics),
    source_mesh_records,
    unique_spatial_centroids_pre_qc,
    potential_od_pairs,
    unique_facility_centroid_pairs,
    chunk_facilities,
    chunk_meshes,
    nominal_facility_blocks,
    nominal_mesh_blocks,
    nominal_chunk_grid
  )
)

fwrite(
  computational_scale,
  file.path(out_logs, "computational_scale_audit.csv")
)

# 都道府県コード表
pref_map <- data.table(
  pref_code = sprintf("%02d", 1:47),
  pref_name = c(
    "Hokkaido", "Aomori", "Iwate", "Miyagi",
    "Akita", "Yamagata", "Fukushima",
    "Ibaraki", "Tochigi", "Gunma", "Saitama",
    "Chiba", "Tokyo", "Kanagawa",
    "Niigata", "Toyama", "Ishikawa", "Fukui",
    "Yamanashi", "Nagano", "Gifu", "Shizuoka",
    "Aichi", "Mie", "Shiga", "Kyoto", "Osaka",
    "Hyogo", "Nara", "Wakayama", "Tottori",
    "Shimane", "Okayama", "Hiroshima", "Yamaguchi",
    "Tokushima", "Kagawa", "Ehime", "Kochi",
    "Fukuoka", "Saga", "Nagasaki", "Kumamoto",
    "Oita", "Miyazaki", "Kagoshima", "Okinawa"
  )
)

# ----------------------------------------------------------------------------
# 4. Derived variables: cumulative thresholds and exclusive time bands
# ----------------------------------------------------------------------------
for (th in thresholds) {
  flag_col <- paste0("within_", th, "min")
  cross_col <- paste0("cross_", th)

  master[, (flag_col) :=
    !is.na(min_duration_min) &
    min_duration_min <= th
  ]

  master[, (cross_col) := fcase(
    within_16km == TRUE &
      get(flag_col) == TRUE,
    "Both accessible",

    within_16km == TRUE &
      get(flag_col) == FALSE,
    "16 km only",

    within_16km == FALSE &
      get(flag_col) == TRUE,
    "Time only",

    within_16km == FALSE &
      get(flag_col) == FALSE,
    "Neither",

    default = NA_character_
  )]
}

# 排他的な時間帯
master[, time_band := fcase(
  is.na(min_duration_min),
  ">60 min or unreachable",

  min_duration_min <= 15,
  "<=15 min",

  min_duration_min <= 30,
  ">15-30 min",

  min_duration_min <= 45,
  ">30-45 min",

  min_duration_min <= 60,
  ">45-60 min",

  default = ">60 min or unreachable"
)]

time_band_levels <- c(
  "<=15 min",
  ">15-30 min",
  ">30-45 min",
  ">45-60 min",
  ">60 min or unreachable"
)

master[, time_band := factor(
  time_band,
  levels = time_band_levels
)]

# 距離帯
master[, distance_band := cut(
  nearest_distance_km,
  breaks = c(
    -Inf, 2, 4, 6, 8,
    10, 12, 14, 16, Inf
  ),
  labels = c(
    "0-2 km",
    "2-4 km",
    "4-6 km",
    "6-8 km",
    "8-10 km",
    "10-12 km",
    "12-14 km",
    "14-16 km",
    ">16 km"
  ),
  right = TRUE
)]

# ----------------------------------------------------------------------------
# 4b. Spatial-record QC and national unique-centroid analysis dataset
# ----------------------------------------------------------------------------
# The MLIT source can contain more than one administrative population record
# for the same physical 500 m mesh centroid. These records must not be counted
# twice in national mesh-based analyses. Their population allocations, however,
# must be preserved for population weighting and prefecture-level analyses.
master_admin <- copy(master)

duplicate_key_groups <- master_admin[, .N, by = join_key][N > 1]
duplicate_record_rows <- master_admin[join_key %in% duplicate_key_groups$join_key]

spatial_record_qc <- data.table(
  source_records = nrow(master_admin),
  unique_spatial_centroids = uniqueN(master_admin$join_key),
  duplicated_spatial_key_groups = nrow(duplicate_key_groups),
  rows_in_duplicated_groups = nrow(duplicate_record_rows),
  excess_duplicate_spatial_records =
    nrow(master_admin) - uniqueN(master_admin$join_key)
)

# Dataset-specific expected counts established by the submission QC.
if (isTRUE(cfg$study_mode)) {
  if (
    spatial_record_qc$source_records != cfg$expected_counts$source_records ||
    spatial_record_qc$unique_spatial_centroids != cfg$expected_counts$unique_spatial_centroids ||
    spatial_record_qc$duplicated_spatial_key_groups != cfg$expected_counts$duplicated_spatial_key_groups ||
    spatial_record_qc$excess_duplicate_spatial_records != cfg$expected_counts$excess_duplicate_spatial_records
  ) {
    print(spatial_record_qc)
    stop("Study-mode spatial-record QC did not reproduce the expected submission counts.")
  }
}

# Every duplicated spatial key should occur twice with two administrative codes.
duplicate_structure_qc <- duplicate_record_rows[, .(
  rows = .N,
  n_shicode = uniqueN(shicode)
), by = join_key]

if (any(
  duplicate_structure_qc$rows != 2L |
  duplicate_structure_qc$n_shicode != 2L
)) {
  stop(
    "Unexpected duplicated spatial-record structure. ",
    "Expected each duplicated centroid to have two administrative records."
  )
}

# Verify that routing and 16-km classification are invariant within duplicated
# centroids before collapsing them for national spatial analyses.
spatial_invariant_cols <- intersect(
  c(
    "mesh_id", "centroid_lon", "centroid_lat",
    "nearest_distance_km", "within_16km",
    "min_duration_min", "nearest_clinic_id",
    "nearest_clinic_lon", "nearest_clinic_lat",
    paste0("within_", thresholds, "min"),
    paste0("cross_", thresholds),
    "time_band", "distance_band"
  ),
  names(master_admin)
)

duplicate_spatial_invariance <- duplicate_record_rows[
  ,
  lapply(.SD, uniqueN),
  by = join_key,
  .SDcols = spatial_invariant_cols
]

invariant_failures <- vapply(
  spatial_invariant_cols,
  function(col) sum(duplicate_spatial_invariance[[col]] > 1L, na.rm = TRUE),
  integer(1)
)

if (any(invariant_failures > 0L)) {
  print(invariant_failures[invariant_failures > 0L])
  stop(
    "Routing or spatial classification differs within duplicated centroids; ",
    "national collapse is not safe."
  )
}

# Aggregate population allocations within each physical centroid for national
# population-weighted analyses. Administrative identifiers are intentionally
# not retained in the national unique-centroid dataset.
non_population_cols_unique <- setdiff(
  names(master_admin),
  c(pop_cols, "join_key", "shicode", "pref_code", "source_row_id")
)

master_spatial_unique <- master_admin[
  ,
  c(
    list(first_source_row_id = min(source_row_id)),
    .SD[1]
  ),
  by = join_key,
  .SDcols = non_population_cols_unique
]

master_population_unique <- master_admin[
  ,
  lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
  by = join_key,
  .SDcols = pop_cols
]

master_unique <- merge(
  master_spatial_unique,
  master_population_unique,
  by = "join_key",
  all.x = TRUE,
  sort = FALSE
)

setorder(master_unique, first_source_row_id)

if (anyDuplicated(master_unique$join_key)) {
  stop("Unique-centroid collapse failed: duplicated join_key remains.")
}
if (isTRUE(cfg$study_mode) &&
    nrow(master_unique) != cfg$expected_counts$unique_spatial_centroids) {
  stop("Study-mode unique-centroid count does not match the submission dataset.")
}

# Population conservation must hold to numerical precision after aggregation.
population_conservation_qc <- rbindlist(
  lapply(pop_cols, function(col) {
    original_total <- sum(master_admin[[col]], na.rm = TRUE)
    aggregated_total <- sum(master_unique[[col]], na.rm = TRUE)
    data.table(
      population_column = col,
      source_total = original_total,
      aggregated_total = aggregated_total,
      difference = aggregated_total - original_total
    )
  })
)

if (any(
  abs(population_conservation_qc$difference) >
    pmax(1e-6, abs(population_conservation_qc$source_total) * 1e-12)
)) {
  print(population_conservation_qc)
  stop("Population totals were not conserved after unique-centroid aggregation.")
}

fwrite(
  spatial_record_qc,
  file.path(out_qc, "qc_spatial_record_summary.csv")
)

fwrite(
  duplicate_structure_qc,
  file.path(out_qc, "qc_duplicate_spatial_key_structure.csv")
)

fwrite(
  population_conservation_qc,
  file.path(out_qc, "qc_population_conservation_after_spatial_collapse.csv")
)

fwrite(
  duplicate_record_rows,
  file.path(out_qc, "qc_duplicate_spatial_records_full.csv")
)

# From this point onward, `master` is the national unique-spatial-centroid
# dataset. `master_admin` is retained specifically for prefecture/municipality
# analyses that require administrative population allocations.
master <- copy(master_unique)

# ----------------------------------------------------------------------------
# 5. QC and consistency checks
# ----------------------------------------------------------------------------
qc_file_counts <- data.table(
  file = c(
    file_crosswalk,
    file_arrival,
    file_centroids,
    file_nearest,
    file_clinics
  ),
  n_rows = c(
    nrow(cross),
    nrow(arrival),
    nrow(centroids),
    nrow(nearest),
    nrow(clinics)
  ),
  n_columns = c(
    ncol(cross),
    ncol(arrival),
    ncol(centroids),
    ncol(nearest),
    ncol(clinics)
  )
)

fwrite(
  qc_file_counts,
  file.path(
    out_qc,
    "qc_input_file_counts.csv"
  )
)

qc_master <- master[, .(
  n_mesh = .N,
  n_unique_join_key = uniqueN(join_key),
  n_missing_within16 = sum(is.na(within_16km)),
  n_missing_duration = sum(is.na(min_duration_min)),
  n_missing_distance = sum(is.na(nearest_distance_km)),
  duration_median = median(
    min_duration_min,
    na.rm = TRUE
  ),
  duration_q1 = quantile(
    min_duration_min,
    0.25,
    na.rm = TRUE
  ),
  duration_q3 = quantile(
    min_duration_min,
    0.75,
    na.rm = TRUE
  ),
  duration_p95 = quantile(
    min_duration_min,
    0.95,
    na.rm = TRUE
  ),
  duration_p99 = quantile(
    min_duration_min,
    0.99,
    na.rm = TRUE
  ),
  duration_max = max(
    min_duration_min,
    na.rm = TRUE
  )
)]

fwrite(
  qc_master,
  file.path(
    out_qc,
    "qc_master_summary.csv"
  )
)

qc_extreme <- head(
  master[order(-min_duration_min)],
  200
)[, .(
  mesh_id,
  centroid_lon,
  centroid_lat,
  nearest_distance_km,
  within_16km,
  nearest_clinic_id,
  nearest_clinic_lon,
  nearest_clinic_lat,
  min_duration_min,
  time_band
)]

fwrite(
  qc_extreme,
  file.path(
    out_qc,
    "qc_extreme_travel_time_top200.csv"
  )
)

qc_unreachable <- master[
  is.na(min_duration_min),
  .(
    mesh_id, centroid_lon, centroid_lat,
    nearest_distance_km, within_16km,
    nearest_clinic_id, nearest_clinic_lon, nearest_clinic_lat,
    min_duration_min,
    pop_total_2025, pop_75plus_2025, pop_80plus_2025
  )
]

# QC: quantify the population represented by OSRM-unreachable meshes.
# These meshes remain in the main analysis and are treated as unreachable at
# all travel-time thresholds. This summary is intended for transparent
# reporting of the magnitude of the routing-failure/unreachable population.
# QC: quantify population represented by OSRM-unreachable centroids.
# Construct the summary from a logical index rather than grouping the empty
# subset. This guarantees a one-row summary even when there are zero
# OSRM-unreachable centroids (e.g., the synthetic smoke-test dataset).
unreachable_idx <- is.na(master$min_duration_min)

qc_unreachable_population_2025 <- data.table(
  n_unreachable_meshes = sum(unreachable_idx),
  total_population_2025 = sum(
    master$pop_total_2025[unreachable_idx],
    na.rm = TRUE
  ),
  population_75plus_2025 = sum(
    master$pop_75plus_2025[unreachable_idx],
    na.rm = TRUE
  ),
  population_80plus_2025 = sum(
    master$pop_80plus_2025[unreachable_idx],
    na.rm = TRUE
  )
)

qc_unreachable_population_2025[, `:=`(
  total_population_pct_national_2025 = safe_ratio_pct(
    total_population_2025,
    sum(master$pop_total_2025, na.rm = TRUE)
  ),
  population_75plus_pct_national_2025 = safe_ratio_pct(
    population_75plus_2025,
    sum(master$pop_75plus_2025, na.rm = TRUE)
  ),
  population_80plus_pct_national_2025 = safe_ratio_pct(
    population_80plus_2025,
    sum(master$pop_80plus_2025, na.rm = TRUE)
  )
)]

if (length(qc_master$n_missing_duration) != 1L) {
  stop("QC error: qc_master$n_missing_duration must be a scalar.")
}

if (as.integer(qc_unreachable_population_2025$n_unreachable_meshes[1]) !=
    as.integer(qc_master$n_missing_duration[1])) {
  stop(
    "QC inconsistency: unreachable mesh count does not match ",
    "n_missing_duration in qc_master."
  )
}

fwrite(
  qc_unreachable,
  file.path(out_qc, "qc_unreachable_meshes.csv")
)

fwrite(
  qc_unreachable_population_2025,
  file.path(out_qc, "qc_unreachable_population_2025.csv")
)

# Ready-to-paste Japanese Results sentence. The percentages are included to
# show how much of the national population is represented by the 61 meshes.
unreachable_results_sentence_ja <- sprintf(
  paste0(
    "OSRMで到達不能であった%sメッシュに含まれる2025年予測人口は、",
    "総人口%s人（全国の%.4f%%）、75歳以上%s人（%.4f%%）、",
    "80歳以上%s人（%.4f%%）であった。"
  ),
  fmt_n(qc_unreachable_population_2025$n_unreachable_meshes),
  fmt_n(qc_unreachable_population_2025$total_population_2025),
  qc_unreachable_population_2025$total_population_pct_national_2025,
  fmt_n(qc_unreachable_population_2025$population_75plus_2025),
  qc_unreachable_population_2025$population_75plus_pct_national_2025,
  fmt_n(qc_unreachable_population_2025$population_80plus_2025),
  qc_unreachable_population_2025$population_80plus_pct_national_2025
)

writeLines(
  unreachable_results_sentence_ja,
  file.path(out_qc, "qc_unreachable_population_results_sentence_ja.txt"),
  useBytes = TRUE
)

message("Unreachable-mesh population QC:")
print(qc_unreachable_population_2025)
message(unreachable_results_sentence_ja)

qc_manual_review_template <- unique(
  rbindlist(
    list(
      qc_extreme[, .(
        mesh_id, centroid_lon, centroid_lat,
        nearest_distance_km, within_16km, min_duration_min
      )],
      qc_unreachable[, .(
        mesh_id, centroid_lon, centroid_lat,
        nearest_distance_km, within_16km, min_duration_min
      )]
    ),
    fill = TRUE
  ),
  by = "mesh_id"
)

qc_manual_review_template[, `:=`(
  review_category = NA_character_,
  exclude_from_mainland_sensitivity = NA,
  review_notes = NA_character_
)]

fwrite(
  qc_manual_review_template,
  file.path(out_qc, "qc_island_ferry_review_template.csv")
)

# ----------------------------------------------------------------------------
# 6. Detect population years/groups
# ----------------------------------------------------------------------------
pop_info <- data.table(
  pop_col = pop_cols
)

pop_info[, pop_group :=
  sub(
    "^pop_([^_]+)_[0-9]{4}$",
    "\\1",
    pop_col
  )
]

pop_info[, year :=
  as.integer(
    sub(
      "^pop_[^_]+_([0-9]{4})$",
      "\\1",
      pop_col
    )
  )
]

pop_info <- pop_info[
  pop_group %in% c(
    "total",
    "65plus",
    "75plus",
    "80plus"
  )
]

pop_info[, group_order :=
  match(
    pop_group,
    c(
      "total",
      "65plus",
      "75plus",
      "80plus"
    )
  )
]

setorder(
  pop_info,
  year,
  group_order
)

pop_info[, group_order := NULL]

# ----------------------------------------------------------------------------
# 7. Main Table 1: data sources and roles
# ----------------------------------------------------------------------------
table1 <- data.table(
  Component = c(
    "Population mesh and future population",
    "Home-care support facilities",
    "OSRM minimum travel time",
    "16-km distance crosswalk",
    "Analysis and mapping"
  ),
  `File or tool` = c(
    file_centroids,
    file_clinics,
    paste(
      file_arrival,
      "/",
      file_nearest
    ),
    file_crosswalk,
    "R / QGIS"
  ),
  `Unit / N` = c(
    paste0(
      fmt_n(nrow(centroids)),
      " mesh-administrative-area records; ",
      fmt_n(nrow(master)),
      " unique spatial centroids"
    ),
    paste0(
      fmt_n(nrow(clinics)),
      " facilities"
    ),
    paste0(
      fmt_n(nrow(master)),
      " mesh records"
    ),
    paste0(
      fmt_n(nrow(master)),
      " mesh records"
    ),
    "Mesh, municipality, prefecture"
  ),
  `Role in analysis` = c(
    "Population weights for 2020-2070 and mesh centroid coordinates",
    "Supply-side locations of home-care support clinics and hospitals",
    "Minimum road-network travel time to the nearest facility",
    "Comparison of the 16-km criterion with four travel-time thresholds",
    "Statistical summaries, figures, and spatial visualization"
  )
)

fwrite(
  table1,
  file.path(
    out_tables,
    "table1_data_sources_compact.csv"
  )
)

ft1 <- make_flextable(
  table1,
  "Table 1. Data sources and analytic roles.",
  font_size = 8
)

save_table_docx(
  ft1,
  file.path(
    out_tables_docx,
    "table1_data_sources_compact.docx"
  )
)

# ----------------------------------------------------------------------------
# 8. Main Table 2: overall mesh and travel-time summary
# ----------------------------------------------------------------------------
duration <- master$min_duration_min
n_total <- nrow(master)

table2 <- data.table(
  Metric = c(
    "Number of mesh centroids",
    "Reachable mesh centroids",
    "Unreachable mesh centroids",
    "Median travel time, min",
    "Interquartile range, min",
    "Mean travel time, min",
    "95th percentile, min",
    "99th percentile, min",
    "Maximum travel time, min",
    "Mesh centroids within 15 min",
    "Mesh centroids within 30 min",
    "Mesh centroids within 45 min",
    "Mesh centroids within 60 min"
  ),
  Value = c(
    fmt_n(n_total),

    fmt_n(
      sum(!is.na(duration))
    ),

    fmt_n_pct(
      sum(is.na(duration)),
      100 * sum(is.na(duration)) / n_total,
      digits = other_main_pct_digits
    ),

    formatC(
      median(duration, na.rm = TRUE),
      format = "f",
      digits = 1
    ),

    paste0(
      formatC(
        quantile(
          duration,
          0.25,
          na.rm = TRUE
        ),
        format = "f",
        digits = 1
      ),
      "-",
      formatC(
        quantile(
          duration,
          0.75,
          na.rm = TRUE
        ),
        format = "f",
        digits = 1
      )
    ),

    formatC(
      mean(duration, na.rm = TRUE),
      format = "f",
      digits = 1
    ),

    formatC(
      quantile(
        duration,
        0.95,
        na.rm = TRUE
      ),
      format = "f",
      digits = 1
    ),

    formatC(
      quantile(
        duration,
        0.99,
        na.rm = TRUE
      ),
      format = "f",
      digits = 1
    ),

    formatC(
      max(duration, na.rm = TRUE),
      format = "f",
      digits = 1
    ),

    fmt_n_pct(
      sum(duration <= 15, na.rm = TRUE),
      100 * sum(
        duration <= 15,
        na.rm = TRUE
      ) / n_total,
      digits = other_main_pct_digits
    ),

    fmt_n_pct(
      sum(duration <= 30, na.rm = TRUE),
      100 * sum(
        duration <= 30,
        na.rm = TRUE
      ) / n_total,
      digits = other_main_pct_digits
    ),

    fmt_n_pct(
      sum(duration <= 45, na.rm = TRUE),
      100 * sum(
        duration <= 45,
        na.rm = TRUE
      ) / n_total,
      digits = other_main_pct_digits
    ),

    fmt_n_pct(
      sum(duration <= 60, na.rm = TRUE),
      100 * sum(
        duration <= 60,
        na.rm = TRUE
      ) / n_total,
      digits = other_main_pct_digits
    )
  )
)

fwrite(
  table2,
  file.path(
    out_tables,
    "table2_mesh_travel_time_summary.csv"
  )
)

ft2 <- make_flextable(
  table2,
  paste0(
    "Table 2. Summary of mesh centroids and ",
    "OSRM-estimated travel times."
  )
)

save_table_docx(
  ft2,
  file.path(
    out_tables_docx,
    "table2_mesh_travel_time_summary.docx"
  )
)

# ----------------------------------------------------------------------------
# 9. Supplementary Table S1: national PWA, all groups/years/thresholds
# ----------------------------------------------------------------------------
s1_list <- list()

for (i in seq_len(nrow(pop_info))) {
  for (th in thresholds) {
    tmp <- calc_pwa(
      master,
      pop_info$pop_col[i],
      th
    )

    tmp[, `:=`(
      year = pop_info$year[i],
      pop_group = pop_info$pop_group[i],
      threshold_min = th
    )]

    s1_list[[length(s1_list) + 1]] <- tmp
  }
}

supp_s1_long <- rbindlist(
  s1_list,
  fill = TRUE
)

setcolorder(
  supp_s1_long,
  c(
    "year",
    "pop_group",
    "threshold_min",
    "population",
    "reachable_population",
    "outside_population",
    "pwa_pct",
    "outside_pct"
  )
)

setorder(
  supp_s1_long,
  year,
  pop_group,
  threshold_min
)

supp_s1_long <- round_numeric_cols(
  supp_s1_long,
  3
)

fwrite(
  supp_s1_long,
  file.path(
    out_supp,
    paste0(
      "supp_table_s1_national_pwa_",
      "all_groups_years_thresholds_long.csv"
    )
  )
)

supp_s1_wide <- dcast(
  supp_s1_long,
  year + pop_group ~ threshold_min,
  value.var = c(
    "pwa_pct",
    "outside_population",
    "outside_pct"
  )
)

fwrite(
  supp_s1_wide,
  file.path(
    out_supp,
    paste0(
      "supp_table_s1_national_pwa_",
      "all_groups_years_thresholds_wide.csv"
    )
  )
)

# ----------------------------------------------------------------------------
# 10. Main Table 3: national PWA, selected years, age 75+/80+
# ----------------------------------------------------------------------------
table3_source <- supp_s1_long[
  year %in% selected_main_years &
    pop_group %in% main_age_groups
]

table3_wide <- dcast(
  table3_source,
  year + pop_group ~ threshold_min,
  value.var = "pwa_pct"
)

setnames(
  table3_wide,
  c("15", "30", "45", "60"),
  c(
    "Within 15 min",
    "Within 30 min",
    "Within 45 min",
    "Within 60 min"
  )
)

# PWAは99%台に集中し差が小さいため、小数点第2位まで表示する。
table3 <- table3_wide[, .(
  Year = year,

  `Age group` = fifelse(
    pop_group == "75plus",
    "75 years or older",
    "80 years or older"
  ),

  `Within 15 min` = fmt_pct(
    `Within 15 min`,
    digits = table3_pct_digits
  ),

  `Within 30 min` = fmt_pct(
    `Within 30 min`,
    digits = table3_pct_digits
  ),

  `Within 45 min` = fmt_pct(
    `Within 45 min`,
    digits = table3_pct_digits
  ),

  `Within 60 min` = fmt_pct(
    `Within 60 min`,
    digits = table3_pct_digits
  )
)]

fwrite(
  table3,
  file.path(
    out_tables,
    paste0(
      "table3_national_pwa_older_",
      "residents_four_thresholds.csv"
    )
  )
)

# 未整形値も保存する。
fwrite(
  table3_source,
  file.path(
    out_tables,
    paste0(
      "table3_national_pwa_older_",
      "residents_four_thresholds_raw.csv"
    )
  )
)

ft3 <- make_flextable(
  table3,
  paste0(
    "Table 3. National population-weighted accessibility ",
    "among older residents, 2025-2070. ",
    "Percentages are shown to two decimal places."
  ),
  font_size = 8
)

save_table_docx(
  ft3,
  file.path(
    out_tables_docx,
    paste0(
      "table3_national_pwa_older_",
      "residents_four_thresholds.docx"
    )
  )
)

# ----------------------------------------------------------------------------
# 11. Main Table 4: exclusive travel-time distribution within 16 km
# ----------------------------------------------------------------------------
within16 <- master[
  within_16km == TRUE
]

table4_specs <- list(
  list(
    label = "Mesh centroids",
    weight_col = NULL
  ),
  list(
    label = "Total population, 2025",
    weight_col = "pop_total_2025"
  ),
  list(
    label = "Population aged 75+, 2025",
    weight_col = "pop_75plus_2025"
  ),
  list(
    label = "Population aged 80+, 2025",
    weight_col = "pop_80plus_2025"
  )
)

# まずraw long tableを作る。
table4_long_list <- lapply(
  table4_specs,
  function(spec) {
    make_band_long(
      dt = within16,
      label = spec$label,
      weight_col = spec$weight_col,
      time_band_levels = time_band_levels
    )
  }
)

table4_raw_long <- rbindlist(
  table4_long_list,
  fill = TRUE
)

# --------------------------------------------------------------------------
# 重要な再計算
# --------------------------------------------------------------------------
# 個々の関数内で作られたpctをそのまま信用せず、4行を結合した後に、
# 各population_or_unit内のvalue合計を分母として割合を再計算する。
# これにより、各セルに同じ割合が繰り返される問題や、外側の変数名との
# 評価衝突による誤計算を防ぐ。
table4_raw_long[, denominator :=
  sum(value, na.rm = TRUE),
  by = .(
    population_or_unit,
    measure_type
  )
]

table4_raw_long[, pct := fifelse(
  is.na(denominator) | denominator <= 0,
  NA_real_,
  100 * as.numeric(value) / as.numeric(denominator)
)]

# 表と図の順序を明示的に固定する。
table4_population_levels <- vapply(
  table4_specs,
  function(x) x$label,
  character(1)
)

table4_raw_long[, population_order := match(
  population_or_unit,
  table4_population_levels
)]

setorder(
  table4_raw_long,
  population_order,
  band_order
)

# 各行の排他的5区分の割合合計を検証する。
# pct列の合計だけでなく、sum(value) / denominatorから独立に再計算した
# 割合も確認し、表示用割合の作成ミスと集計値自体の不一致を区別する。
table4_qc <- table4_raw_long[, {
  row_denominators <- unique(denominator)

  if (length(row_denominators) != 1L) {
    stop(
      "Table 4 QC failed: multiple denominators were found within one row: ",
      paste(row_denominators, collapse = ", ")
    )
  }

  row_denominator <- row_denominators[[1L]]
  row_sum_value <- sum(value, na.rm = TRUE)
  row_sum_pct <- sum(pct, na.rm = TRUE)
  independently_recalculated_sum_pct <- safe_ratio_pct(
    row_sum_value,
    row_denominator
  )

  .(
    denominator = row_denominator,
    sum_value = row_sum_value,
    sum_pct = row_sum_pct,
    independently_recalculated_sum_pct =
      independently_recalculated_sum_pct,
    absolute_difference_from_100 = abs(row_sum_pct - 100),
    value_difference_from_denominator =
      row_sum_value - row_denominator
  )
}, by = .(
  population_or_unit,
  measure_type
)]

fwrite(
  round_numeric_cols(table4_raw_long, 6),
  file.path(
    out_tables,
    "table4_travel_time_distribution_within_16km_raw_long.csv"
  )
)

fwrite(
  round_numeric_cols(table4_qc, 6),
  file.path(
    out_qc,
    "qc_table4_row_percent_sums.csv"
  )
)

# 浮動小数点誤差を考慮し、割合合計または値合計に実質的な不一致が
# あれば停止する。
if (any(
  is.na(table4_qc$sum_pct) |
    is.na(table4_qc$independently_recalculated_sum_pct) |
    table4_qc$absolute_difference_from_100 > 0.0001 |
    abs(table4_qc$value_difference_from_denominator) >
      pmax(1e-6, abs(table4_qc$denominator) * 1e-10)
)) {
  print(table4_qc)

  stop(
    paste0(
      "Table 4 QC failed: row percentages do not sum to 100%. ",
      "See qc_table4_row_percent_sums.csv."
    )
  )
}

# 表示用wide tableを作る。
table4_formatted_long <- copy(
  table4_raw_long
)

table4_formatted_long[, display_value :=
  fmt_n_pct(
    value,
    pct,
    digits = other_main_pct_digits
  )
]

table4 <- dcast(
  table4_formatted_long,
  population_or_unit ~ time_band,
  value.var = "display_value"
)

setnames(
  table4,
  "population_or_unit",
  "Population or unit"
)

# 列順を固定する。
setcolorder(
  table4,
  c(
    "Population or unit",
    time_band_levels
  )
)

fwrite(
  table4,
  file.path(
    out_tables,
    "table4_travel_time_distribution_within_16km.csv"
  )
)

ft4 <- make_flextable(
  table4,
  paste0(
    "Table 4. Exclusive travel-time distribution within ",
    "the 16-km catchment. Values are n (row %); ",
    "each row uses its own denominator."
  ),
  font_size = 7.5
)

save_table_docx(
  ft4,
  file.path(
    out_tables_docx,
    "table4_travel_time_distribution_within_16km.docx"
  )
)

# ----------------------------------------------------------------------------
# 12. Main Table 5: four-quadrant cross-classification
# ----------------------------------------------------------------------------
category_order <- c(
  "Both accessible",
  "16 km only",
  "Time only",
  "Neither"
)

table5_list <- list()

for (th in thresholds) {
  cross_col <- paste0(
    "cross_",
    th
  )

  tmp <- master[, .(
    n_mesh = .N,
    pop_total_2025 = sum(
      pop_total_2025,
      na.rm = TRUE
    ),
    pop_75plus_2025 = sum(
      pop_75plus_2025,
      na.rm = TRUE
    )
  ), by = .(
    Category = get(cross_col)
  )]

  # Public/synthetic-workflow robustness:
  # Always retain all four cross-classification categories, even when one or
  # more categories contain zero observations. Without this padding, dcast()
  # later creates only the categories present in the data, and setcolorder()
  # fails for synthetic examples in which every centroid is "Both accessible".
  all_categories <- data.table(
    Category = category_order
  )

  tmp <- merge(
    all_categories,
    tmp,
    by = "Category",
    all.x = TRUE,
    sort = FALSE
  )

  tmp[is.na(n_mesh), n_mesh := 0L]
  tmp[is.na(pop_total_2025), pop_total_2025 := 0]
  tmp[is.na(pop_75plus_2025), pop_75plus_2025 := 0]

  tmp[, `:=`(
    Threshold = paste0(th, " min"),
    mesh_pct = safe_ratio_pct(
      n_mesh,
      sum(n_mesh)
    ),
    total_pct = safe_ratio_pct(
      pop_total_2025,
      sum(pop_total_2025)
    ),
    pop75_pct = safe_ratio_pct(
      pop_75plus_2025,
      sum(pop_75plus_2025)
    ),
    category_order = match(
      Category,
      category_order
    )
  )]

  setorder(
    tmp,
    category_order
  )

  table5_list[[length(table5_list) + 1]] <- tmp
}

table5_raw <- rbindlist(
  table5_list,
  fill = TRUE
)

fwrite(
  round_numeric_cols(table5_raw, 6),
  file.path(
    out_tables,
    paste0(
      "table5_16km_time_crossclassification_",
      "four_thresholds_raw.csv"
    )
  )
)

table5 <- table5_raw[, .(
  Threshold,
  Category,

  `Mesh centroids` = fmt_n_pct(
    n_mesh,
    mesh_pct,
    digits = other_main_pct_digits
  ),

  `Total population, 2025` = fmt_n_pct(
    pop_total_2025,
    total_pct,
    digits = other_main_pct_digits
  ),

  `Population aged 75+, 2025` = fmt_n_pct(
    pop_75plus_2025,
    pop75_pct,
    digits = other_main_pct_digits
  )
)]

fwrite(
  table5,
  file.path(
    out_tables,
    paste0(
      "table5_16km_time_crossclassification_",
      "four_thresholds.csv"
    )
  )
)

ft5 <- make_flextable(
  table5,
  paste0(
    "Table 5. Cross-classification of the 16-km criterion ",
    "and four OSRM travel-time thresholds."
  ),
  font_size = 7.5
)

save_table_docx(
  ft5,
  file.path(
    out_tables_docx,
    paste0(
      "table5_16km_time_crossclassification_",
      "four_thresholds.docx"
    )
  )
)

# ----------------------------------------------------------------------------
# 12A. BMJ HCI Main Table 1
# National road-network accessibility and cumulative accessibility within 16 km
# ----------------------------------------------------------------------------
get_national_pwa <- function(pop_group_value, threshold_value) {
  supp_s1_long[
    year == 2025 &
      pop_group == pop_group_value &
      threshold_min == threshold_value,
    pwa_pct
  ][1]
}

calc_within16_cumulative_pct <- function(threshold_value, weight_col = NULL) {
  reachable_flag <- !is.na(within16$min_duration_min) &
    within16$min_duration_min <= threshold_value

  if (is.null(weight_col)) {
    return(100 * sum(reachable_flag) / nrow(within16))
  }

  denominator <- sum(as.numeric(within16[[weight_col]]), na.rm = TRUE)
  numerator <- sum(
    as.numeric(within16[[weight_col]])[reachable_flag],
    na.rm = TRUE
  )

  safe_ratio_pct(numerator, denominator)
}

bmj_table1_raw <- data.table(
  metric = c(
    "All mesh centroids: n (%)",
    "Population-weighted accessibility, age 75+, 2025",
    "Population-weighted accessibility, age 80+, 2025",
    "Within 16 km: mesh cumulative percentage",
    "Within 16 km: age 75+ cumulative percentage"
  )
)

for (th in thresholds) {
  col_name <- paste0(th, " min")

  bmj_table1_raw[, (col_name) := c(
    100 * sum(
      !is.na(master$min_duration_min) & master$min_duration_min <= th
    ) / nrow(master),
    get_national_pwa("75plus", th),
    get_national_pwa("80plus", th),
    calc_within16_cumulative_pct(th),
    calc_within16_cumulative_pct(th, "pop_75plus_2025")
  )]
}

bmj_table1 <- data.table(
  Metric = bmj_table1_raw$metric
)

for (th in thresholds) {
  raw_col <- paste0(th, " min")
  display_col <- paste0("Within ", th, " min")

  values <- bmj_table1_raw[[raw_col]]
  mesh_n <- sum(
    !is.na(master$min_duration_min) & master$min_duration_min <= th
  )

  bmj_table1[, (display_col) := c(
    fmt_n_pct(mesh_n, values[1], digits = 1),
    fmt_pct(values[2], digits = 2),
    fmt_pct(values[3], digits = 2),
    fmt_pct(values[4], digits = 2),
    fmt_pct(values[5], digits = 2)
  )]
}

fwrite(
  round_numeric_cols(bmj_table1_raw, 6),
  file.path(out_bmj_tables, "table1_national_accessibility_raw.csv")
)

fwrite(
  bmj_table1,
  file.path(out_bmj_tables, "table1_national_accessibility_formatted.csv")
)

bmj_ft1 <- make_flextable(
  bmj_table1,
  paste0(
    "Table 1. National road-network accessibility and cumulative ",
    "accessibility within the 16-km catchment."
  ),
  font_size = 8
)

save_table_docx(
  bmj_ft1,
  file.path(out_bmj_tables_docx, "table1_national_accessibility.docx")
)

# ----------------------------------------------------------------------------
# 12B. BMJ HCI Main Table 2
# Cross-classification of the 16-km criterion and OSRM time scenarios
# ----------------------------------------------------------------------------
bmj_table2_long <- rbindlist(
  list(
    table5_raw[, .(
      threshold_min = as.integer(sub(" min", "", Threshold, fixed = TRUE)),
      Unit = "Mesh centroids",
      Category,
      display_value = fmt_n_pct(
        n_mesh,
        mesh_pct,
        digits = other_main_pct_digits
      )
    )],
    table5_raw[, .(
      threshold_min = as.integer(sub(" min", "", Threshold, fixed = TRUE)),
      Unit = "Population aged 75+, 2025",
      Category,
      display_value = fmt_n_pct(
        pop_75plus_2025,
        pop75_pct,
        digits = other_main_pct_digits
      )
    )]
  )
)

bmj_table2_long[, Category := factor(Category, levels = category_order)]
bmj_table2_long[, Unit := factor(
  Unit,
  levels = c("Mesh centroids", "Population aged 75+, 2025")
)]
setorder(bmj_table2_long, threshold_min, Unit, Category)

bmj_table2 <- dcast(
  bmj_table2_long,
  threshold_min + Unit ~ Category,
  value.var = "display_value"
)

bmj_table2[, Threshold := paste0(threshold_min, " min")]
setcolorder(
  bmj_table2,
  c("Threshold", "Unit", category_order)
)
bmj_table2[, threshold_min := NULL]

fwrite(
  table5_raw,
  file.path(out_bmj_tables, "table2_crossclassification_raw.csv")
)

fwrite(
  bmj_table2,
  file.path(out_bmj_tables, "table2_crossclassification_formatted.csv")
)

bmj_ft2 <- make_flextable(
  bmj_table2,
  paste0(
    "Table 2. Cross-classification of the 16-km criterion and ",
    "OSRM-estimated travel-time scenarios."
  ),
  font_size = 7.5
)

save_table_docx(
  bmj_ft2,
  file.path(out_bmj_tables_docx, "table2_crossclassification.docx")
)

bmj_combined_doc <- read_docx()
bmj_combined_doc <- body_add_par(bmj_combined_doc, "Table 1", style = "heading 1")
bmj_combined_doc <- body_add_flextable(bmj_combined_doc, bmj_ft1)
bmj_combined_doc <- body_add_break(bmj_combined_doc)
bmj_combined_doc <- body_add_par(bmj_combined_doc, "Table 2", style = "heading 1")
bmj_combined_doc <- body_add_flextable(bmj_combined_doc, bmj_ft2)

print(
  bmj_combined_doc,
  target = file.path(out_bmj_tables_docx, "BMJ_HCI_main_tables_1_and_2.docx")
)

# Detailed tables 1-5 above are retained as analytic audit outputs only.

# ----------------------------------------------------------------------------
# 13. Supplementary Table S2: all prefectures, 2025 and 2070 PWA
# ----------------------------------------------------------------------------
s2_info <- pop_info[
  year %in% c(2025, 2070)
]

s2_list <- list()

for (i in seq_len(nrow(s2_info))) {
  for (th in thresholds) {
    tmp <- calc_pwa(
      master_admin,
      s2_info$pop_col[i],
      th,
      by_vars = "pref_code"
    )

    tmp[, `:=`(
      year = s2_info$year[i],
      pop_group = s2_info$pop_group[i],
      threshold_min = th
    )]

    s2_list[[length(s2_list) + 1]] <- tmp
  }
}

supp_s2_long <- rbindlist(
  s2_list,
  fill = TRUE
)

supp_s2_long <- merge(
  supp_s2_long,
  pref_map,
  by = "pref_code",
  all.x = TRUE
)

setcolorder(
  supp_s2_long,
  c(
    "pref_code",
    "pref_name",
    "year",
    "pop_group",
    "threshold_min",
    "population",
    "reachable_population",
    "outside_population",
    "pwa_pct",
    "outside_pct"
  )
)

setorder(
  supp_s2_long,
  pref_code,
  year,
  pop_group,
  threshold_min
)

supp_s2_long <- round_numeric_cols(
  supp_s2_long,
  3
)

fwrite(
  supp_s2_long,
  file.path(
    out_supp,
    paste0(
      "supp_table_s2_all_prefectures_",
      "2025_2070_pwa_long.csv"
    )
  )
)

supp_s2_wide <- dcast(
  supp_s2_long,
  pref_code + pref_name + year + pop_group ~ threshold_min,
  value.var = c(
    "pwa_pct",
    "outside_population",
    "outside_pct"
  )
)

fwrite(
  supp_s2_wide,
  file.path(
    out_supp,
    paste0(
      "supp_table_s2_all_prefectures_",
      "2025_2070_pwa_wide.csv"
    )
  )
)

# ----------------------------------------------------------------------------
# 14. Supplementary Table S3: prefecture × year × age group × threshold
# ----------------------------------------------------------------------------
s3_list <- list()

for (i in seq_len(nrow(pop_info))) {
  for (th in thresholds) {
    tmp <- calc_pwa(
      master_admin,
      pop_info$pop_col[i],
      th,
      by_vars = "pref_code"
    )

    tmp[, `:=`(
      year = pop_info$year[i],
      pop_group = pop_info$pop_group[i],
      threshold_min = th
    )]

    s3_list[[length(s3_list) + 1]] <- tmp
  }
}

supp_s3 <- rbindlist(
  s3_list,
  fill = TRUE
)

supp_s3 <- merge(
  supp_s3,
  pref_map,
  by = "pref_code",
  all.x = TRUE
)

setcolorder(
  supp_s3,
  c(
    "pref_code",
    "pref_name",
    "year",
    "pop_group",
    "threshold_min",
    "population",
    "reachable_population",
    "outside_population",
    "pwa_pct",
    "outside_pct"
  )
)

setorder(
  supp_s3,
  pref_code,
  year,
  pop_group,
  threshold_min
)

supp_s3 <- round_numeric_cols(
  supp_s3,
  3
)

fwrite(
  supp_s3,
  file.path(
    out_supp,
    paste0(
      "supp_table_s3_prefecture_",
      "year_group_threshold_complete.csv"
    )
  )
)

# ----------------------------------------------------------------------------
# 15. Supplementary Table S4: prefecture-level 16-km crosswalk
# ----------------------------------------------------------------------------
s4_list <- list()

for (th in thresholds) {
  cross_col <- paste0(
    "cross_",
    th
  )

  numerator <- master_admin[, .(
    n_admin_records = .N,
    pop_total_2025 = sum(
      pop_total_2025,
      na.rm = TRUE
    ),
    pop_75plus_2025 = sum(
      pop_75plus_2025,
      na.rm = TRUE
    )
  ), by = .(
    pref_code,
    Category = get(cross_col)
  )]

  denominator <- master_admin[, .(
    n_admin_records_pref = .N,
    pop_total_pref_2025 = sum(
      pop_total_2025,
      na.rm = TRUE
    ),
    pop_75plus_pref_2025 = sum(
      pop_75plus_2025,
      na.rm = TRUE
    )
  ), by = pref_code]

  tmp <- merge(
    numerator,
    denominator,
    by = "pref_code",
    all.x = TRUE
  )

  tmp[, `:=`(
    threshold_min = th,
    admin_record_pct_pref = safe_ratio_pct(
      n_admin_records,
      n_admin_records_pref
    ),
    total_pop_pct_pref = safe_ratio_pct(
      pop_total_2025,
      pop_total_pref_2025
    ),
    pop75_pct_pref = safe_ratio_pct(
      pop_75plus_2025,
      pop_75plus_pref_2025
    ),
    category_order = match(
      Category,
      category_order
    )
  )]

  setorder(
    tmp,
    pref_code,
    category_order
  )

  s4_list[[length(s4_list) + 1]] <- tmp
}

supp_s4 <- rbindlist(
  s4_list,
  fill = TRUE
)

supp_s4 <- merge(
  supp_s4,
  pref_map,
  by = "pref_code",
  all.x = TRUE
)

setcolorder(
  supp_s4,
  c(
    "pref_code",
    "pref_name",
    "threshold_min",
    "Category",
    "n_admin_records",
    "admin_record_pct_pref",
    "pop_total_2025",
    "total_pop_pct_pref",
    "pop_75plus_2025",
    "pop75_pct_pref",
    "n_admin_records_pref",
    "pop_total_pref_2025",
    "pop_75plus_pref_2025"
  )
)

setorder(
  supp_s4,
  pref_code,
  threshold_min,
  Category
)

supp_s4 <- round_numeric_cols(
  supp_s4,
  3
)

fwrite(
  supp_s4,
  file.path(
    out_supp,
    paste0(
      "supp_table_s4_prefecture_16km_",
      "crosswalk_four_thresholds.csv"
    )
  )
)

# ----------------------------------------------------------------------------
# 16. Supplementary Table S5: distance threshold population coverage
# ----------------------------------------------------------------------------
distance_thresholds <- seq(
  2,
  16,
  by = 2
)

supp_s5 <- rbindlist(
  lapply(
    distance_thresholds,
    function(d) {
      flag <-
        !is.na(master$nearest_distance_km) &
        master$nearest_distance_km <= d

      data.table(
        distance_threshold_km = d,

        n_mesh_within = sum(flag),

        mesh_pct_within =
          100 * sum(flag) / nrow(master),

        pop_total_2025_within = sum(
          master$pop_total_2025[flag],
          na.rm = TRUE
        ),

        pop_total_2025_pct_within =
          100 * sum(
            master$pop_total_2025[flag],
            na.rm = TRUE
          ) /
          sum(
            master$pop_total_2025,
            na.rm = TRUE
          ),

        pop75_2025_within = sum(
          master$pop_75plus_2025[flag],
          na.rm = TRUE
        ),

        pop75_2025_pct_within =
          100 * sum(
            master$pop_75plus_2025[flag],
            na.rm = TRUE
          ) /
          sum(
            master$pop_75plus_2025,
            na.rm = TRUE
          ),

        pop75_2070_within = sum(
          master$pop_75plus_2070[flag],
          na.rm = TRUE
        ),

        pop75_2070_pct_within =
          100 * sum(
            master$pop_75plus_2070[flag],
            na.rm = TRUE
          ) /
          sum(
            master$pop_75plus_2070,
            na.rm = TRUE
          )
      )
    }
  )
)

supp_s5 <- round_numeric_cols(
  supp_s5,
  3
)

fwrite(
  supp_s5,
  file.path(
    out_supp,
    paste0(
      "supp_table_s5_distance_threshold_",
      "population_coverage.csv"
    )
  )
)

# ----------------------------------------------------------------------------
# 17. Supplementary Table S6: distance-band travel-time heterogeneity
# ----------------------------------------------------------------------------
supp_s6 <- master[
  !is.na(distance_band),
  .(
    n_mesh = .N,

    mesh_pct =
      100 * .N / nrow(master),

    pop_total_2025 = sum(
      pop_total_2025,
      na.rm = TRUE
    ),

    pop_total_2025_pct =
      100 * sum(
        pop_total_2025,
        na.rm = TRUE
      ) /
      sum(
        master$pop_total_2025,
        na.rm = TRUE
      ),

    pop75_2025 = sum(
      pop_75plus_2025,
      na.rm = TRUE
    ),

    pop75_2025_pct =
      100 * sum(
        pop_75plus_2025,
        na.rm = TRUE
      ) /
      sum(
        master$pop_75plus_2025,
        na.rm = TRUE
      ),

    duration_median_min = median(
      min_duration_min,
      na.rm = TRUE
    ),

    duration_q1_min = quantile(
      min_duration_min,
      0.25,
      na.rm = TRUE
    ),

    duration_q3_min = quantile(
      min_duration_min,
      0.75,
      na.rm = TRUE
    ),

    duration_p95_min = quantile(
      min_duration_min,
      0.95,
      na.rm = TRUE
    ),

    mesh_over15_pct =
      100 * sum(
        is.na(min_duration_min) |
          min_duration_min > 15
      ) / .N,

    mesh_over30_pct =
      100 * sum(
        is.na(min_duration_min) |
          min_duration_min > 30
      ) / .N,

    mesh_over45_pct =
      100 * sum(
        is.na(min_duration_min) |
          min_duration_min > 45
      ) / .N,

    mesh_over60_pct =
      100 * sum(
        is.na(min_duration_min) |
          min_duration_min > 60
      ) / .N,

    pop75_over15_pct =
      100 * sum(
        pop_75plus_2025[
          is.na(min_duration_min) |
            min_duration_min > 15
        ],
        na.rm = TRUE
      ) /
      sum(
        pop_75plus_2025,
        na.rm = TRUE
      ),

    pop75_over30_pct =
      100 * sum(
        pop_75plus_2025[
          is.na(min_duration_min) |
            min_duration_min > 30
        ],
        na.rm = TRUE
      ) /
      sum(
        pop_75plus_2025,
        na.rm = TRUE
      ),

    pop75_over45_pct =
      100 * sum(
        pop_75plus_2025[
          is.na(min_duration_min) |
            min_duration_min > 45
        ],
        na.rm = TRUE
      ) /
      sum(
        pop_75plus_2025,
        na.rm = TRUE
      ),

    pop75_over60_pct =
      100 * sum(
        pop_75plus_2025[
          is.na(min_duration_min) |
            min_duration_min > 60
        ],
        na.rm = TRUE
      ) /
      sum(
        pop_75plus_2025,
        na.rm = TRUE
      )
  ),
  by = distance_band
]

supp_s6 <- round_numeric_cols(
  supp_s6,
  3
)

fwrite(
  supp_s6,
  file.path(
    out_supp,
    paste0(
      "supp_table_s6_distance_band_",
      "travel_time_heterogeneity.csv"
    )
  )
)

# ----------------------------------------------------------------------------
# 17b. Supplementary Table S7: data provenance, computational environment,
#      routing parameters, QC, and method transparency
# ----------------------------------------------------------------------------
package_versions_s7 <- data.table(
  package = required_packages,
  version = vapply(
    required_packages,
    function(pkg) as.character(utils::packageVersion(pkg)),
    character(1)
  )
)

package_version_text <- paste0(
  package_versions_s7$package,
  " ",
  package_versions_s7$version,
  collapse = "; "
)

input_row_text <- paste0(
  "crosswalk=", nrow(cross),
  "; arrival=", nrow(arrival),
  "; centroids=", nrow(centroids),
  "; nearest=", nrow(nearest),
  "; facilities=", nrow(clinics),
  "; source mesh-administrative-area records=", nrow(master_admin),
  "; national unique spatial centroids=", nrow(master)
)

critical_missing_text <- paste0(
  "mesh_id missing in national unique master=", sum(is.na(master$mesh_id)),
  "; shicode missing in administrative master=", sum(is.na(master_admin$shicode)),
  "; distance missing=", sum(is.na(master$nearest_distance_km)),
  "; OSRM duration missing/unreachable=", sum(is.na(master$min_duration_min))
)

spatial_record_qc_text <- paste0(
  format(nrow(master_admin), big.mark = ","), " source mesh-administrative-area records represent ",
  format(nrow(master), big.mark = ","), " unique 500 m spatial centroids; ",
  format(nrow(duplicate_key_groups), big.mark = ","), " centroids were represented twice with ",
  "distinct administrative codes/population allocations. Routing time and 16-km classification ",
  "were invariant within duplicated centroids. Population totals were conserved after aggregation."
)

supp_s7 <- data.table(
  category = c(
    "Data provenance", "Data provenance", "Data provenance",
    "Data integration", "Data integration", "Data integration", "Routing", "Routing",
    "Routing", "Routing", "Computational environment",
    "Computational environment", "Computational environment",
    "Computational environment", "Computational environment",
    "Runtime", "Runtime", "Method transparency", "Method transparency",
    "Data sharing"
  ),
  item = c(
    "JMAP facility data", "Population mesh", "Road network",
    "Input/output rows", "QC and missingness", "Spatial-record QC and handling", "OSRM backend",
    "Routing parameters", "Computational scale", "Unreachable meshes",
    "Execution mode", "Workstation", "R", "R packages", "QGIS",
    "OSRM preprocessing", "Nationwide table computation",
    "Analysis records", "Figure 3 layout", "JMAP data restriction"
  ),
  details = c(
    paste0(
      "Source: ", jmap_source,
      "; acquisition date: ", jmap_acquisition_date,
      "; target: home-care support clinics/hospitals; analysed facilities: ",
      format(nrow(clinics), big.mark = ",", scientific = FALSE),
      "; redistribution not permitted"
    ),
    paste0(
      "Source: ", population_source,
      "; acquisition date: ", population_acquisition_date,
      "; source records: ", format(nrow(master_admin), big.mark = ",", scientific = FALSE),
      "; unique spatial centroids: ", format(nrow(master), big.mark = ",", scientific = FALSE)
    ),
    paste0(
      "OpenStreetMap extract provider: ", osm_extract_provider,
      "; snapshot date: ", osm_snapshot_date,
      "; study area: Japan"
    ),
    input_row_text,
    critical_missing_text,
    spatial_record_qc_text,
    paste0(
      "OSRM backend version: ", osrm_backend_version,
      "; Docker image tag: ", osrm_docker_image_tag,
      "; immutable image digest: ", osrm_docker_image_digest,
      "; image ID: ", osrm_docker_image_id,
      "; image creation timestamp: ", osrm_docker_image_created,
      "; profile: car"
    ),
    paste0(
      "algorithm=MLD; max-table-size=5000; facility block=",
      format(chunk_facilities, big.mark = ","),
      "; mesh block=", format(chunk_meshes, big.mark = ",")
    ),
    paste0(
      format(nrow(clinics), big.mark = ","), " facilities x ",
      format(source_mesh_records, big.mark = ","), " routed source records = ",
      format(potential_od_pairs, big.mark = ",", scientific = FALSE),
      " facility-record pairs; these represent ",
      format(unique_facility_centroid_pairs, big.mark = ",", scientific = FALSE),
      " unique facility-centroid combinations; nominal chunk grid=", nominal_chunk_grid
    ),
    paste0(
      sum(is.na(master$min_duration_min)),
      " meshes retained as unreachable for all thresholds"
    ),
    paste0(
      "Local Docker/OSRM execution on ", analysis_compute_class,
      "; specialised HPC or cloud used: ", specialised_hpc_or_cloud_used
    ),
    paste0(
      analysis_device, "; ", analysis_os_manual, "; CPU: ", analysis_cpu,
      "; RAM: ", analysis_ram_gb, " GB; storage: ", analysis_storage
    ),
    r_version_reported,
    package_version_text,
    qgis_version,
    osrm_preprocessing_runtime,
    osrm_national_table_runtime,
    paste0(
      "Input manifest with MD5, reproducibility metadata, computational-scale audit, ",
      "sessionInfo, package versions, QC outputs and execution log retained in study records."
    ),
    paste0(
      "Final composite target ", figure3_total_width_cm, " x ",
      figure3_total_height_cm, " cm at ", figure3_export_dpi,
      " dpi; 2 x 2 panels. Mainland/principal-island and remote-island map frames ",
      "within each panel use the same map scale."
    ),
    paste0(
      "JMAP-derived facility-level data and analysis datasets containing those data are not redistributed. ",
      "Equivalent source data must be obtained directly from JMAP under its applicable terms. ",
      repository_url_or_doi
    )
  )
)

fwrite(
  supp_s7,
  file.path(out_supp, "supp_table_s7_method_transparency.csv")
)

ft_s7 <- make_flextable(
  supp_s7,
  "Supplementary Table S7. Data provenance, computational environment, routing parameters and method transparency.",
  font_size = 8,
  max_width = 6.5
)

save_table_docx(
  ft_s7,
  file.path(out_supp, "supp_table_s7_method_transparency.docx")
)

# Supplementary Tablesを1つのExcel workbookにまとめる。
wb <- createWorkbook()

sheet_data <- list(
  "S1 national long" = supp_s1_long,
  "S1 national wide" = supp_s1_wide,
  "S2 prefecture 2025 2070 long" = supp_s2_long,
  "S2 prefecture 2025 2070 wide" = supp_s2_wide,
  "S3 complete" = supp_s3,
  "S4 pref crosswalk" = supp_s4,
  "S5 distance coverage" = supp_s5,
  "S6 distance bands" = supp_s6,
  "S7 method transparency" = supp_s7,
  "Table4 raw long" = round_numeric_cols(
    table4_raw_long,
    6
  ),
  "Table4 QC" = round_numeric_cols(
    table4_qc,
    6
  )
)

header_style <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  textDecoration = "bold",
  fgFill = "#D9EAF7",
  border = "Bottom",
  halign = "center"
)

body_style <- createStyle(
  fontName = "Arial",
  fontSize = 9,
  valign = "top"
)

for (sheet_name in names(sheet_data)) {
  safe_sheet_name <- substr(
    sheet_name,
    1,
    31
  )

  addWorksheet(
    wb,
    safe_sheet_name
  )

  writeData(
    wb,
    safe_sheet_name,
    sheet_data[[sheet_name]]
  )

  addStyle(
    wb,
    safe_sheet_name,
    header_style,
    rows = 1,
    cols = seq_len(
      ncol(sheet_data[[sheet_name]])
    ),
    gridExpand = TRUE
  )

  if (nrow(sheet_data[[sheet_name]]) > 0) {
    addStyle(
      wb,
      safe_sheet_name,
      body_style,
      rows = 2:(
        nrow(sheet_data[[sheet_name]]) + 1
      ),
      cols = seq_len(
        ncol(sheet_data[[sheet_name]])
      ),
      gridExpand = TRUE
    )
  }

  freezePane(
    wb,
    safe_sheet_name,
    firstRow = TRUE
  )

  setColWidths(
    wb,
    safe_sheet_name,
    cols = seq_len(
      ncol(sheet_data[[sheet_name]])
    ),
    widths = "auto"
  )
}

saveWorkbook(
  wb,
  file.path(
    out_supp,
    "supplementary_tables_s1_to_s7_revised.xlsx"
  ),
  overwrite = TRUE
)

# ----------------------------------------------------------------------------
# 18. Supplementary Figure S1 (R): conceptual 16-km radius and time catchments
# ----------------------------------------------------------------------------
angles <- seq(
  0,
  2 * pi,
  length.out = 400
)

make_irregular_polygon <- function(
  base_radius,
  phase
) {
  radius <-
    base_radius *
    (
      1 +
        0.12 * sin(3 * angles + phase) +
        0.07 * cos(5 * angles - phase)
    )

  data.frame(
    x = radius * cos(angles),
    y = radius * sin(angles)
  )
}

concept_polys <- rbind(
  transform(
    make_irregular_polygon(14.0, 0.2),
    threshold = "60 min"
  ),
  transform(
    make_irregular_polygon(11.5, 0.7),
    threshold = "45 min"
  ),
  transform(
    make_irregular_polygon(8.5, 1.1),
    threshold = "30 min"
  ),
  transform(
    make_irregular_polygon(5.2, 1.6),
    threshold = "15 min"
  )
)

concept_polys$threshold <- factor(
  concept_polys$threshold,
  levels = c(
    "60 min",
    "45 min",
    "30 min",
    "15 min"
  )
)

circle16 <- data.frame(
  x = 16 * cos(angles),
  y = 16 * sin(angles)
)

sfig1_concept <- ggplot() +
  geom_polygon(
    data = concept_polys,
    aes(
      x = x,
      y = y,
      group = threshold,
      fill = threshold
    ),
    alpha = 0.35,
    color = "white",
    linewidth = 0.25
  ) +
  geom_path(
    data = circle16,
    aes(
      x = x,
      y = y
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      x = 0,
      y = 0
    ),
    shape = 23,
    size = 4,
    fill = "white"
  ) +
  annotate(
    "text",
    x = 0,
    y = -2.0,
    label = "Home-care support facility",
    size = 3.3
  ) +
  annotate(
    "text",
    x = 13.8,
    y = 11.5,
    label = "16-km radius",
    size = 3.2
  ) +
  coord_equal(
    xlim = c(-18, 18),
    ylim = c(-18, 18),
    expand = FALSE
  ) +
  scale_fill_brewer(
    palette = "Blues",
    direction = -1
  ) +
  labs(

    fill = "Travel-time threshold"
  ) +
  theme_void(
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s1_concept_16km_and_time_catchments.pdf"
  ),
  sfig1_concept,
  width = 7,
  height = 6
)

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s1_concept_16km_and_time_catchments.png"
  ),
  sfig1_concept,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s1_concept_16km_and_time_catchments.tiff"
  ),
  sfig1_concept,
  width = 7,
  height = 6
)

# ----------------------------------------------------------------------------
# 19. Figure 1 (R): open-source geospatial informatics workflow
# ----------------------------------------------------------------------------
# The figure deliberately foregrounds the informatics contribution: heterogeneous
# data integration, local Dockerised routing, chunked national-scale OD processing,
# reproducibility/QC, and policy-facing outputs. It does not imply that OSRM is a
# novel routing algorithm.
workflow <- data.frame(
  step = 1:8,
  label = c(
    paste0(
      "JMAP\nhome-care\nsupport facilities\n",
      format(nrow(clinics), big.mark = ","), " sites"
    ),
    "Address geocoding\nand coordinate QC",
    paste0(
      "500 m population data\n",
      format(source_mesh_records, big.mark = ","), " records\n",
      format(nrow(master), big.mark = ","), " unique\ncentroids"
    ),
    paste0(
      "Local open-source\nrouting OSRM/Docker\n+\nOpenStreetMap\n",
      analysis_ram_gb, " GB RAM PC"
    ),
    paste0(
      "Chunked OD\ncomputation\n",
      format(chunk_facilities, big.mark = ","), " x ",
      format(chunk_meshes, big.mark = ","), "\n~",
      format(round(potential_od_pairs / 1e9, 2), nsmall = 2),
      " billion pairs"
    ),
    "Minimum\nOSRM-estimated\ntravel time per mesh\n+\nrouting QC",
    "Prespecified\n15/30/45/60-min\nPWA\n+\n16-km\ncross-classification",
    "Re-runnable tables,\nmaps\nand planning outputs"
  ),
  x = c(1, 2, 3, 4, 1, 2, 3, 4),
  y = c(2, 2, 2, 2, 1, 1, 1, 1)
)

arrow_turn <- data.frame(
  x = 4, xend = 1,
  y = 1.68, yend = 1.32
)

fig1 <- ggplot(workflow, aes(x = x, y = y)) +
  geom_rect(
    aes(
      xmin = x - 0.44, xmax = x + 0.44,
      ymin = y - 0.29, ymax = y + 0.29
    ),
    fill = "white",
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(aes(label = label), size = 3.05, lineheight = 0.95) +
  geom_segment(
    data = workflow[c(1:3, 5:7), ],
    aes(x = x + 0.45, xend = x + 0.56, y = y, yend = y),
    arrow = grid::arrow(length = grid::unit(0.09, "inches")),
    linewidth = 0.5
  ) +
  geom_segment(
    data = arrow_turn,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.09, "inches")),
    linewidth = 0.5
  ) +
  coord_cartesian(
    xlim = c(0.45, 4.55),
    ylim = c(0.53, 2.47),
    clip = "off"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(6, 6, 6, 6)
  )

figure1_height_in <- main_figure_width_in / (5 / 3)

ggsave(
  file.path(out_figures, "figure1_geospatial_informatics_workflow.pdf"),
  fig1,
  width = main_figure_width_in,
  height = figure1_height_in,
  units = "in"
)

ggsave(
  file.path(out_figures, "figure1_geospatial_informatics_workflow.tiff"),
  fig1,
  width = main_figure_width_in,
  height = figure1_height_in,
  units = "in",
  dpi = main_figure_dpi,
  compression = "lzw",
  bg = "white"
)

ggsave(
  file.path(out_figures, "figure1_geospatial_informatics_workflow.png"),
  fig1,
  width = main_figure_width_in,
  height = figure1_height_in,
  units = "in",
  dpi = main_figure_dpi,
  bg = "white"
)


# ----------------------------------------------------------------------------
# 20. Figure 2 (R): straight-line distance versus OSRM-estimated travel time
# ----------------------------------------------------------------------------
fig2_data <- master[
  !is.na(nearest_distance_km) &
    !is.na(min_duration_min) &
    nearest_distance_km <= figure2_max_distance_km &
    min_duration_min <= figure2_max_duration_min
]

fig2 <- ggplot(
  fig2_data,
  aes(
    x = nearest_distance_km,
    y = min_duration_min
  )
) +
  stat_binhex(
    bins = 70
  ) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Mesh count"
  ) +
  geom_vline(
    xintercept = 16,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_hline(
    yintercept = thresholds,
    linetype = "dotted",
    linewidth = 0.5
  ) +
  annotate("text", x = 16.25, y = 1.75, label = "16 km", hjust = 0, size = 3.2) +
  scale_x_continuous(
    limits = c(
      0,
      figure2_max_distance_km
    ),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      figure2_max_duration_min
    ),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  labs(
    x = "Straight-line distance to nearest qualifying facility (km)",
    y = "OSRM-estimated travel time (min)"

  ) +
  theme_bw(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "right"
  )

ggsave(
  file.path(
    out_figures,
    "figure2_straight_line_distance_vs_osrm_estimated_time_hexbin.pdf"
  ),
  fig2,
  width = main_figure_width_in,
  height = 5.4,
  units = "in"
)

ggsave(
  file.path(
    out_figures,
    "figure2_straight_line_distance_vs_osrm_estimated_time_hexbin.png"
  ),
  fig2,
  width = main_figure_width_in,
  height = 5.4,
  units = "in",
  dpi = main_figure_dpi,
  bg = "white"
)

ggsave(
  file.path(
    out_figures,
    "figure2_straight_line_distance_vs_osrm_estimated_time_hexbin.tiff"
  ),
  fig2,
  width = main_figure_width_in,
  height = 5.4,
  units = "in",
  dpi = main_figure_dpi,
  compression = "lzw",
  bg = "white"
)


# ----------------------------------------------------------------------------
# 21. Supplementary Figure S3 (R): prefecture heatmap within 16 km
# ----------------------------------------------------------------------------
sfig3_heatmap_list <- list()

for (th in thresholds) {
  tmp <- master_admin[
    within_16km == TRUE,
    .(
      denominator_pop75 = sum(
        pop_75plus_2025,
        na.rm = TRUE
      ),
      beyond_pop75 = sum(
        pop_75plus_2025[
          is.na(min_duration_min) |
            min_duration_min > th
        ],
        na.rm = TRUE
      )
    ),
    by = pref_code
  ]

  tmp[, `:=`(
    threshold_min = th,
    beyond_pct = safe_ratio_pct(
      beyond_pop75,
      denominator_pop75
    )
  )]

  sfig3_heatmap_list[[length(sfig3_heatmap_list) + 1]] <- tmp
}

sfig3_heatmap_data <- rbindlist(
  sfig3_heatmap_list
)

sfig3_heatmap_data <- merge(
  sfig3_heatmap_data,
  pref_map,
  by = "pref_code",
  all.x = TRUE
)

pref_order <- sfig3_heatmap_data[
  threshold_min == 30
][
  order(beyond_pct),
  pref_name
]

sfig3_heatmap_data[, pref_name := factor(
  pref_name,
  levels = pref_order
)]

sfig3_heatmap_data[, threshold_label := factor(
  paste0(
    ">",
    threshold_min,
    " min or unreachable"
  ),
  levels = paste0(
    ">",
    thresholds,
    " min or unreachable"
  )
)]

sfig3_heatmap <- ggplot(
  sfig3_heatmap_data,
  aes(
    x = threshold_label,
    y = pref_name,
    fill = beyond_pct
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    labels = label_number(
      accuracy = 0.1,
      suffix = "%"
    ),
    name = paste0(
      "75+ population\n",
      "outside threshold"
    )
  ) +
  labs(
    x = "Population outside travel-time threshold",
    y = NULL

  ) +
  theme_minimal(
    base_size = 9
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 7
    ),
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    legend.position = "right"
  )

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s3_prefecture_heatmap_within16_beyond_threshold.pdf"
  ),
  sfig3_heatmap,
  width = 8,
  height = 10
)

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s3_prefecture_heatmap_within16_beyond_threshold.png"
  ),
  sfig3_heatmap,
  width = 8,
  height = 10,
  dpi = 300
)

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s3_prefecture_heatmap_within16_beyond_threshold.tiff"
  ),
  sfig3_heatmap,
  width = 8,
  height = 10
)

fwrite(
  sfig3_heatmap_data,
  file.path(
    out_supp_figures,
    "supplementary_figure_s3_prefecture_heatmap_data.csv"
  )
)

# ----------------------------------------------------------------------------
# 22. Supplementary Figure S4: future population outside thresholds
# ----------------------------------------------------------------------------
# PWAそのものは天井効果が強く、15/30/45/60分の線がすべて高く見える。
# そこで、100-PWAに相当するoutside_pctを表示する。
# outside_pctは、各時間閾値を超える、またはOSRMで到達不能な人口割合である。
#
# 例：
#   >15 min or unreachable
#     = 15分以内に到達できない人口割合
#   >60 min or unreachable
#     = 60分以内に到達できない人口割合
#
# 施設配置、道路網、OSRM移動時間は固定し、人口分布のみ年ごとに変化する。
sfig4_data <- supp_s1_long[
  pop_group %in% c(
    "75plus",
    "80plus"
  )
]

sfig4_data[, threshold_label := factor(
  paste0(
    ">",
    threshold_min,
    " min or unreachable"
  ),
  levels = paste0(
    ">",
    thresholds,
    " min or unreachable"
  )
)]

sfig4_data[, group_label := fifelse(
  pop_group == "75plus",
  "Age 75+",
  "Age 80+"
)]

# 0から最大値までを表示する。
# すべて0の場合にもエラーにならないようにする。
sfig4_y_max <- max(
  sfig4_data$outside_pct,
  na.rm = TRUE
)

if (!is.finite(sfig4_y_max) || sfig4_y_max <= 0) {
  sfig4_y_max <- 1
} else {
  sfig4_y_max <- sfig4_y_max * 1.08
}

sfig4 <- ggplot(
  sfig4_data,
  aes(
    x = year,
    y = outside_pct,
    group = threshold_label,
    linetype = threshold_label,
    shape = threshold_label
  )
) +
  geom_line(
    linewidth = 0.75
  ) +
  geom_point(
    size = 1.9
  ) +
  facet_wrap(
    ~group_label,
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = sort(
      unique(sfig4_data$year)
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      sfig4_y_max
    ),
    labels = label_number(
      suffix = "%",
      accuracy = 0.01
    ),
    expand = expansion(
      mult = c(0, 0.03)
    )
  ) +
  labs(
    x = "Year",
    y = "Population outside travel-time threshold",
    linetype = "Outside threshold",
    shape = "Outside threshold"
  ) +
  theme_bw(
    base_size = 10
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 9
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8
    )
  )















# BMJ HCI supplementary file names
# Supplementary Figure S4 outputs
ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s4_future_outside_threshold_trajectories.pdf"
  ),
  sfig4,
  width = 8,
  height = 7.5
)

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s4_future_outside_threshold_trajectories.png"
  ),
  sfig4,
  width = 8,
  height = 7.5,
  dpi = 300
)


ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s4_future_outside_threshold_trajectories.tiff"
  ),
  sfig4,
  width = 8,
  height = 7.5
)
# Supplementary Figure S4に使った値も保存する。
fwrite(
  sfig4_data[, .(
    year,
    pop_group,
    group_label,
    threshold_min,
    threshold_label,
    population,
    outside_population,
    outside_pct,
    pwa_pct
  )],
  file.path(
    out_supp_figures,
    "supplementary_figure_s4_plot_data.csv"
  )
)

# ----------------------------------------------------------------------------
# 23. Supplementary Figure S5: distance-threshold population coverage
# ----------------------------------------------------------------------------
sfig5_long <- melt(
  supp_s5,
  id.vars = "distance_threshold_km",
  measure.vars = c(
    "pop_total_2025_pct_within",
    "pop75_2025_pct_within",
    "pop75_2070_pct_within"
  ),
  variable.name = "population_group",
  value.name = "coverage_pct"
)

sfig5_long[, population_group := factor(
  population_group,
  levels = c(
    "pop_total_2025_pct_within",
    "pop75_2025_pct_within",
    "pop75_2070_pct_within"
  ),
  labels = c(
    "Total population, 2025",
    "Age 75+, 2025",
    "Age 75+, 2070"
  )
)]

# Final title-free plot definition for Supplementary Figure S5
sfig5 <- ggplot(
  sfig5_long,
  aes(
    x = distance_threshold_km,
    y = coverage_pct,
    group = population_group,
    shape = population_group
  )
) +
  geom_line(
    linewidth = 0.75
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = distance_thresholds
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = label_number(
      suffix = "%"
    )
  ) +
  labs(
    x = "Distance threshold (km)",
    y = "Population coverage",
    shape = "Population group"
  ) +
  theme_bw(
    base_size = 10
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s5_distance_threshold_population_coverage.pdf"
  ),
  sfig5,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s5_distance_threshold_population_coverage.png"
  ),
  sfig5,
  width = 7,
  height = 5,
  dpi = 300
)


ggsave(
  file.path(
    out_supp_figures,
    "supplementary_figure_s5_distance_threshold_population_coverage.tiff"
  ),
  sfig5,
  width = 7,
  height = 5
)


# ============================================================
# 24. Supplementary Figure S6: exclusive time-band composition within 16 km
# Exclusive travel-time-band composition within the 16-km catchment
#
# 使い方：
# 既存コード内に重複している Figure S6 セクションをすべて削除し、
# このブロックを1回だけ貼り付けて実行してください。
# 前提：table4_raw_long、out_supp_figures が作成済みであること。
# ============================================================

# ------------------------------------------------------------
# 1. 前提オブジェクトの確認
# ------------------------------------------------------------

if (!exists("table4_raw_long")) {
  stop(
    "table4_raw_long が存在しません。Table 4の作成部分を先に実行してください。"
  )
}

if (!exists("out_supp_figures")) {
  stop(
    "out_supp_figures が存在しません。出力フォルダ設定部分を先に実行してください。"
  )
}

# Figure S6で使う時間帯を、この図の中で明示的に定義する。
sfig6_band_levels <- c(
  "<=15 min",
  ">15-30 min",
  ">30-45 min",
  ">45-60 min",
  ">60 min or unreachable"
)

# ------------------------------------------------------------
# 2. Table 4の未丸めlong形式データをコピー
# ------------------------------------------------------------

sfig6_source <- data.table::copy(
  data.table::as.data.table(table4_raw_long)
)

required_sfig3_cols <- c(
  "population_or_unit",
  "measure_type",
  "time_band",
  "value"
)

missing_sfig3_cols <- setdiff(
  required_sfig3_cols,
  names(sfig6_source)
)

if (length(missing_sfig3_cols) > 0) {
  stop(
    paste0(
      "Figure S6用データに必要な列がありません: ",
      paste(missing_sfig3_cols, collapse = ", ")
    )
  )
}

# 文字列の前後の空白を除く。
sfig6_source[
  ,
  population_or_unit := trimws(
    as.character(population_or_unit)
  )
]

sfig6_source[
  ,
  measure_type := trimws(
    as.character(measure_type)
  )
]

sfig6_source[
  ,
  time_band := trimws(
    as.character(time_band)
  )
]

# valueを数値にそろえる。
# 文字列に桁区切りカンマが含まれていても数値化できるようにする。
sfig6_source[
  ,
  value := suppressWarnings(
    as.numeric(
      gsub(
        ",",
        "",
        as.character(value),
        fixed = TRUE
      )
    )
  )
]

if (all(is.na(sfig6_source$value))) {
  stop(
    "Figure S6用のvalueがすべてNAです。table4_raw_longを確認してください。"
  )
}

# ------------------------------------------------------------
# 3. Figure S6に表示する4対象を定義
# ------------------------------------------------------------

sfig6_group_spec <- data.table::data.table(
  population_or_unit = c(
    "Mesh centroids",
    "Total population, 2025",
    "Population aged 75+, 2025",
    "Population aged 80+, 2025"
  ),
  measure_type = c(
    "mesh count",
    "pop_total_2025",
    "pop_75plus_2025",
    "pop_80plus_2025"
  ),
  group = c(
    "Mesh centroids",
    "Total population, 2025",
    "Age 75+, 2025",
    "Age 80+, 2025"
  )
)

# 対象行だけを残す。
sfig6_source <- sfig6_source[
  population_or_unit %chin%
    sfig6_group_spec$population_or_unit
]

if (nrow(sfig6_source) == 0) {
  print(unique(table4_raw_long$population_or_unit))
  
  stop(
    paste0(
      "Figure S6の対象行を取得できませんでした。 ",
      "population_or_unitの名称を確認してください。"
    )
  )
}

# ------------------------------------------------------------
# 4. 対象×時間帯ごとの実数値を集計
# ------------------------------------------------------------

sfig6_values <- sfig6_source[
  ,
  .(
    value = sum(
      value,
      na.rm = TRUE
    )
  ),
  by = .(
    population_or_unit,
    measure_type,
    time_band
  )
]

# ------------------------------------------------------------
# 5. 4対象×5時間帯の完全な20行を作成
# ------------------------------------------------------------

sfig6_template <- sfig6_group_spec[
  ,
  .(
    time_band = sfig6_band_levels
  ),
  by = .(
    population_or_unit,
    measure_type,
    group
  )
]

sfig6_data <- merge(
  sfig6_template,
  sfig6_values,
  by = c(
    "population_or_unit",
    "measure_type",
    "time_band"
  ),
  all.x = TRUE,
  sort = FALSE
)

# 元データに存在しなかった時間帯は0件・0人とする。
sfig6_data[
  is.na(value),
  value := 0
]

# ------------------------------------------------------------
# 6. x軸と凡例の順序を固定
# ------------------------------------------------------------

sfig6_data[
  ,
  group := factor(
    group,
    levels = c(
      "Mesh centroids",
      "Total population, 2025",
      "Age 75+, 2025",
      "Age 80+, 2025"
    )
  )
]

sfig6_data[
  ,
  time_band := factor(
    time_band,
    levels = sfig6_band_levels
  )
]

if (any(is.na(sfig6_data$group))) {
  print(
    unique(
      sfig6_data[
        is.na(group),
        .(
          population_or_unit,
          measure_type
        )
      ]
    )
  )
  
  stop(
    "Figure S6のgroup作成に失敗しました。"
  )
}

if (any(is.na(sfig6_data$time_band))) {
  print(
    unique(
      sfig6_data[
        is.na(time_band),
        .(
          population_or_unit,
          measure_type
        )
      ]
    )
  )
  
  stop(
    "Figure S6のtime_band作成に失敗しました。"
  )
}

# ------------------------------------------------------------
# 7. 各バー内の構成割合を計算
# ------------------------------------------------------------

# 各対象の実数値合計を分母として、全5時間帯に同じ分母を付与する。
sfig6_data[
  ,
  plot_denominator := sum(
    value,
    na.rm = TRUE
  ),
  by = .(
    population_or_unit,
    measure_type,
    group
  )
]

if (any(
  is.na(sfig6_data$plot_denominator) |
  sfig6_data$plot_denominator <= 0
)) {
  print(
    unique(
      sfig6_data[
        is.na(plot_denominator) |
          plot_denominator <= 0,
        .(
          population_or_unit,
          measure_type,
          group,
          plot_denominator
        )
      ]
    )
  )
  
  stop(
    "Figure S6のいずれかのバーで分母が0またはNAです。"
  )
}

# 百分率（0～100）と作図用比率（0～1）を計算する。
sfig6_data[
  ,
  plot_pct := 100 * value / plot_denominator
]

sfig6_data[
  ,
  plot_proportion := value / plot_denominator
]

# 3%以上の区分だけ棒内に表示する。
sfig6_data[
  ,
  plot_label := ifelse(
    !is.na(plot_pct) &
      plot_pct >= 3,
    paste0(
      format(
        round(plot_pct, 1),
        nsmall = 1,
        trim = TRUE
      ),
      "%"
    ),
    ""
  )
]

data.table::setorder(
  sfig6_data,
  group,
  time_band
)

# ------------------------------------------------------------
# 8. 作図前QC
# ------------------------------------------------------------

sfig6_qc <- sfig6_data[
  ,
  .(
    total_value = sum(
      value,
      na.rm = TRUE
    ),
    sum_plot_pct = sum(
      plot_pct,
      na.rm = TRUE
    ),
    sum_plot_proportion = sum(
      plot_proportion,
      na.rm = TRUE
    ),
    n_time_bands = .N
  ),
  by = .(
    population_or_unit,
    measure_type,
    group
  )
]

print(sfig6_qc)

if (any(
  sfig6_qc$total_value <= 0 |
  abs(
    sfig6_qc$sum_plot_pct - 100
  ) > 0.0001 |
  abs(
    sfig6_qc$sum_plot_proportion - 1
  ) > 0.000001 |
  sfig6_qc$n_time_bands !=
  length(sfig6_band_levels)
)) {
  stop(
    "Figure S6用データのQCに失敗しました。"
  )
}

# QC結果を保存する。
data.table::fwrite(
  sfig6_qc,
  file.path(
    out_supp_figures,
    "supplementary_figure_s6_qc.csv"
  )
)

# 作図に使用する20行も保存する。
data.table::fwrite(
  sfig6_data,
  file.path(
    out_supp_figures,
    "supplementary_figure_s6_plot_data.csv"
  )
)

# ------------------------------------------------------------
# 9. Figure S6を作成
# ------------------------------------------------------------

# 重要：
# yには実数のvalueではなく、0～1のplot_proportionを使う。
# すでに割合を計算済みなので、position = "fill"ではなく
# position = "stack"でそのまま積み上げる。
# Final title-free plot definition for Supplementary Figure S6
sfig6 <- ggplot2::ggplot(
  sfig6_data,
  ggplot2::aes(
    x = group,
    y = plot_proportion,
    fill = time_band
  )
) +
  ggplot2::geom_col(
    width = 0.7,
    position = "stack"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = plot_label
    ),
    position = ggplot2::position_stack(
      vjust = 0.5
    ),
    size = 3,
    check_overlap = TRUE
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.25
    ),
    labels = scales::label_percent(
      accuracy = 1
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0)
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggplot2::scale_fill_discrete(
    drop = FALSE,
    breaks = sfig6_band_levels,
    labels = c(
      "≤15 min",
      ">15–30 min",
      ">30–45 min",
      ">45–60 min",
      ">60 min or unreachable"
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Composition within each population or unit",
    fill = "Exclusive travel-time band"
  ) +
  ggplot2::theme_bw(
    base_size = 10
  ) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 20,
      hjust = 1
    ),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

# RStudioのPlotsペインにも表示する。
print(sfig6)
# ------------------------------------------------------------
# 10. 保存
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = file.path(
    out_supp_figures,
    "supplementary_figure_s6_within16_exclusive_time_band_composition.pdf"
  ),
  plot = sfig6,
  width = 8,
  height = 5.5
)

ggplot2::ggsave(
  filename = file.path(
    out_supp_figures,
    "supplementary_figure_s6_within16_exclusive_time_band_composition.png"
  ),
  plot = sfig6,
  width = 8,
  height = 5.5,
  dpi = 300
)


ggplot2::ggsave(
  filename = file.path(
    out_supp_figures,
    "supplementary_figure_s6_within16_exclusive_time_band_composition.tiff"
  ),
  plot = sfig6,
  width = 8,
  height = 5.5
)

# ----------------------------------------------------------------------------
# 25. QGIS exports for main Figure 3 and Supplementary Figure S2
# ----------------------------------------------------------------------------
# Supplementary Figure S2: exclusive travel-time bands
supp_s2_fields <- c(
  "mesh_id",
  "shicode",
  "pref_code",
  "centroid_lon",
  "centroid_lat",
  "nearest_distance_km",
  "within_16km",
  "min_duration_min",
  "time_band",
  "pop_total_2025",
  "pop_75plus_2025",
  "pop_80plus_2025"
)

supp_s2_fields <- supp_s2_fields[
  supp_s2_fields %in% names(master)
]

supp_s2_dt <- master[, ..supp_s2_fields]

supp_s2_dt[, time_band_order :=
  match(
    as.character(time_band),
    time_band_levels
  )
]

supp_s2_points <- st_as_sf(
  supp_s2_dt,
  coords = c(
    "centroid_lon",
    "centroid_lat"
  ),
  crs = 4326,
  remove = FALSE
)

supp_s2_gpkg <- file.path(
  out_qgis,
  "supplementary_figure_s2_mesh_travel_time_15_30_45_60.gpkg"
)

st_write(
  supp_s2_points,
  supp_s2_gpkg,
  layer = "supp_figure_s2_mesh_travel_time",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Main Figure 3: 16-km × four time-scenario cross-classification
figure3_fields <- c(
  "mesh_id",
  "shicode",
  "pref_code",
  "centroid_lon",
  "centroid_lat",
  "within_16km",
  "nearest_distance_km",
  "min_duration_min",
  "cross_15",
  "cross_30",
  "cross_45",
  "cross_60",
  "pop_total_2025",
  "pop_75plus_2025"
)

figure3_fields <- figure3_fields[
  figure3_fields %in% names(master)
]

figure3_dt <- master[, ..figure3_fields]

for (th in thresholds) {
  col <- paste0(
    "cross_",
    th
  )

  order_col <- paste0(
    col,
    "_order"
  )

  figure3_dt[, (order_col) :=
    match(
      get(col),
      category_order
    )
  ]
}

figure3_points <- st_as_sf(
  figure3_dt,
  coords = c(
    "centroid_lon",
    "centroid_lat"
  ),
  crs = 4326,
  remove = FALSE
)

figure3_gpkg <- file.path(
  out_qgis,
  "figure3_16km_time_crossclassification_15_30_45_60.gpkg"
)

st_write(
  figure3_points,
  figure3_gpkg,
  layer = "figure3_crossclassification",
  delete_dsn = TRUE,
  quiet = TRUE
)

cm_to_px <- function(cm, dpi = figure3_export_dpi) {
  as.integer(round(cm / 2.54 * dpi))
}

figure3_layout_spec <- data.table(
  element = c(
    "Full Figure 3 composite",
    "Each A-D parent panel",
    "Mainland/principal-island map frame within each panel",
    "Remote-island map frame within each panel"
  ),
  width_cm = c(
    figure3_total_width_cm,
    figure3_panel_width_cm,
    figure3_mainland_width_cm,
    figure3_islands_width_cm
  ),
  height_cm = c(
    figure3_total_height_cm,
    figure3_panel_height_cm,
    figure3_mainland_height_cm,
    figure3_islands_height_cm
  )
)

figure3_layout_spec[, `:=`(
  dpi = figure3_export_dpi,
  width_px = cm_to_px(width_cm),
  height_px = cm_to_px(height_cm),
  note = c(
    "2 x 2 composite; shared legend at bottom",
    "Panels A-D correspond to 15, 30, 45 and 60 min",
    "Use the same map scale as the remote-island frame",
    "Use the same map scale as the mainland/principal-island frame"
  )
)]

fwrite(
  figure3_layout_spec,
  file.path(out_qgis, "figure3_qgis_layout_spec_600dpi.csv")
)

writeLines(
  c(
    "Figure 3 QGIS layout specification",
    paste0("Full composite: ", figure3_total_width_cm, " x ", figure3_total_height_cm, " cm at ", figure3_export_dpi, " dpi."),
    paste0("Each A-D panel: ", figure3_panel_width_cm, " x ", figure3_panel_height_cm, " cm."),
    paste0("Mainland/principal-island frame: ", figure3_mainland_width_cm, " x ", figure3_mainland_height_cm, " cm."),
    paste0("Remote-island frame: ", figure3_islands_width_cm, " x ", figure3_islands_height_cm, " cm."),
    "Within each panel, the two map frames must use the same map scale; only the frame extent/height differs.",
    "Use one shared legend for Both accessible / 16 km only / Time only / Neither.",
    "Do not place the full Figure 3 title inside the image; use panel labels A-D and concise threshold headings only."
  ),
  file.path(out_qgis, "figure3_qgis_layout_spec_600dpi.txt"),
  useBytes = TRUE
)

# 施設ポイント
if (all(
  c(
    "clinic_lon",
    "clinic_lat"
  ) %in% names(clinics)
)) {
  clinics_valid <- clinics[
    !is.na(clinic_lon) &
      !is.na(clinic_lat) &
      clinic_lon >= 120 &
      clinic_lon <= 155 &
      clinic_lat >= 20 &
      clinic_lat <= 50
  ]

  clinic_points <- st_as_sf(
    clinics_valid,
    coords = c(
      "clinic_lon",
      "clinic_lat"
    ),
    crs = 4326,
    remove = FALSE
  )

  st_write(
    clinic_points,
    file.path(
      out_qgis,
      "home_care_support_facility_points.gpkg"
    ),
    layer = "facilities",
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

# 極端時間上位200メッシュ
qc_extreme_points <- st_as_sf(
  qc_extreme,
  coords = c(
    "centroid_lon",
    "centroid_lat"
  ),
  crs = 4326,
  remove = FALSE
)

st_write(
  qc_extreme_points,
  file.path(
    out_qgis,
    "qc_extreme_travel_time_top200.gpkg"
  ),
  layer = "extreme_top200",
  delete_dsn = TRUE,
  quiet = TRUE
)

# 市区町村単位の集約CSV
municipality_travel_summary <- master_admin[, {
  pop75_den <- sum(
    pop_75plus_2025,
    na.rm = TRUE
  )

  list(
    n_mesh = .N,

    pop_total_2025 = sum(
      pop_total_2025,
      na.rm = TRUE
    ),

    pop_75plus_2025 = pop75_den,

    duration_median_min = median(
      min_duration_min,
      na.rm = TRUE
    ),

    duration_q1_min = quantile(
      min_duration_min,
      0.25,
      na.rm = TRUE
    ),

    duration_q3_min = quantile(
      min_duration_min,
      0.75,
      na.rm = TRUE
    ),

    within16_mesh_pct =
      100 * sum(
        within_16km == TRUE,
        na.rm = TRUE
      ) / .N,

    pop75_outside15_pct =
      safe_ratio_pct(
        sum(
          pop_75plus_2025[
            is.na(min_duration_min) |
              min_duration_min > 15
          ],
          na.rm = TRUE
        ),
        pop75_den
      ),

    pop75_outside30_pct =
      safe_ratio_pct(
        sum(
          pop_75plus_2025[
            is.na(min_duration_min) |
              min_duration_min > 30
          ],
          na.rm = TRUE
        ),
        pop75_den
      ),

    pop75_outside45_pct =
      safe_ratio_pct(
        sum(
          pop_75plus_2025[
            is.na(min_duration_min) |
              min_duration_min > 45
          ],
          na.rm = TRUE
        ),
        pop75_den
      ),

    pop75_outside60_pct =
      safe_ratio_pct(
        sum(
          pop_75plus_2025[
            is.na(min_duration_min) |
              min_duration_min > 60
          ],
          na.rm = TRUE
        ),
        pop75_den
      )
  )
}, by = .(
  shicode,
  pref_code
)]

municipality_travel_summary <- round_numeric_cols(
  municipality_travel_summary,
  3
)

fwrite(
  municipality_travel_summary,
  file.path(
    out_qgis,
    "municipality_travel_time_summary_for_n03_join.csv"
  )
)

qgis_dictionary <- data.table(
  file = c(
    "supplementary_figure_s2_mesh_travel_time_15_30_45_60.gpkg",
    "figure3_16km_time_crossclassification_15_30_45_60.gpkg",
    "home_care_support_facility_points.gpkg",
    "qc_extreme_travel_time_top200.gpkg",
    "municipality_travel_time_summary_for_n03_join.csv"
  ),
  role = c(
    "Supplementary Figure S2: exclusive travel-time-band map",
    "Main Figure 3: four-scenario 16-km cross-classification",
    "Optional facility overlay",
    "QC overlay for extreme travel-time cases",
    "Join to N03_007/SHICODE for municipality summaries"
  )
)

fwrite(
  qgis_dictionary,
  file.path(
    out_qgis,
    "qgis_field_dictionary.csv"
  )
)

qgis_crossclassification_symbology <- data.table(
  order = 1:4,
  category = category_order,
  label = c(
    "Both accessible",
    "16 km only",
    "Time only",
    "Neither"
  ),
  hex_colour = c("#4D9221", "#D95F02", "#2C7FB8", "#BDBDBD"),
  interpretation = c(
    "Within 16 km and within the selected time scenario",
    "Within 16 km but outside the selected time scenario or unreachable",
    "Outside 16 km but within the selected time scenario",
    "Outside both criteria"
  )
)

qgis_time_band_symbology <- data.table(
  order = 1:5,
  category = time_band_levels,
  label = c(
    "≤15 min", ">15–30 min", ">30–45 min", ">45–60 min",
    ">60 min or unreachable"
  ),
  hex_colour = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#636363")
)

fwrite(
  qgis_crossclassification_symbology,
  file.path(out_qgis, "qgis_symbology_figure3_crossclassification.csv")
)

fwrite(
  qgis_time_band_symbology,
  file.path(out_qgis, "qgis_symbology_supplementary_figure_s2_time_bands.csv")
)

# ----------------------------------------------------------------------------
# 25A. Optional manually reviewed island/ferry sensitivity analysis
# ----------------------------------------------------------------------------
if (file.exists(file_manual_island_review)) {
  manual_review <- fread(file_manual_island_review)

  required_review_cols <- c("mesh_id", "exclude_from_mainland_sensitivity")
  missing_review_cols <- setdiff(required_review_cols, names(manual_review))

  if (length(missing_review_cols) > 0) {
    stop(
      "Manual island/ferry review file is missing columns: ",
      paste(missing_review_cols, collapse = ", ")
    )
  }

  manual_review[, mesh_id := as.character(mesh_id)]
  exclude_ids <- unique(
    manual_review[
      exclude_from_mainland_sensitivity %in% c(TRUE, 1, "TRUE", "true", "1"),
      mesh_id
    ]
  )

  sensitivity_master <- master[!mesh_id %in% exclude_ids]

  sensitivity_summary <- data.table(
    scenario = "Manually reviewed mainland sensitivity",
    n_excluded_mesh = length(exclude_ids),
    n_included_mesh = nrow(sensitivity_master),
    median_duration_min = median(
      sensitivity_master$min_duration_min,
      na.rm = TRUE
    )
  )

  for (th in thresholds) {
    sensitivity_col <- paste0("mesh_within_", th, "min_pct")

    sensitivity_summary[, (sensitivity_col) :=
      100 * sum(
        !is.na(sensitivity_master$min_duration_min) &
          sensitivity_master$min_duration_min <= th
      ) / nrow(sensitivity_master)
    ]
  }

  fwrite(
    round_numeric_cols(sensitivity_summary, 6),
    file.path(out_qc, "sensitivity_manually_reviewed_mainland_summary.csv")
  )
} else {
  message(
    "Optional manual island/ferry sensitivity analysis was not run. ",
    "Complete qc_island_ferry_review_template.csv and save it as ",
    file_manual_island_review,
    " to enable it."
  )
}

# ----------------------------------------------------------------------------
# 26. Key results text for manuscript insertion
# ----------------------------------------------------------------------------
key_2025_75 <- supp_s1_long[
  year == 2025 &
    pop_group == "75plus"
]

key_2070_75 <- supp_s1_long[
  year == 2070 &
    pop_group == "75plus"
]

key_lines <- c(
  paste0(
    "Key results generated by the revised ",
    "15/30/45/60-minute analysis"
  ),

  paste0(
    "Mesh centroids: ",
    fmt_n(nrow(master))
  ),

  paste0(
    "Reachable: ",
    fmt_n(
      sum(
        !is.na(master$min_duration_min)
      )
    )
  ),

  paste0(
    "Unreachable: ",
    fmt_n(
      sum(
        is.na(master$min_duration_min)
      )
    )
  ),

  paste0(
    "Travel time median (IQR): ",
    round(
      median(
        master$min_duration_min,
        na.rm = TRUE
      ),
      1
    ),
    " (",
    round(
      quantile(
        master$min_duration_min,
        0.25,
        na.rm = TRUE
      ),
      1
    ),
    "-",
    round(
      quantile(
        master$min_duration_min,
        0.75,
        na.rm = TRUE
      ),
      1
    ),
    ") min"
  ),

  paste0(
    "Mesh cumulative accessibility: ",
    paste0(
      thresholds,
      " min=",
      round(
        100 *
          sapply(
            thresholds,
            function(th) {
              sum(
                master$min_duration_min <= th,
                na.rm = TRUE
              ) /
                nrow(master)
            }
          ),
        2
      ),
      "%",
      collapse = "; "
    )
  ),

  paste0(
    "Age 75+ PWA in 2025: ",
    paste0(
      key_2025_75$threshold_min,
      " min=",
      round(
        key_2025_75$pwa_pct,
        3
      ),
      "%",
      collapse = "; "
    )
  ),

  paste0(
    "Age 75+ outside-threshold population in 2025: ",
    paste0(
      ">",
      key_2025_75$threshold_min,
      " min=",
      round(
        key_2025_75$outside_pct,
        3
      ),
      "%",
      collapse = "; "
    )
  ),

  paste0(
    "Age 75+ PWA in 2070: ",
    paste0(
      key_2070_75$threshold_min,
      " min=",
      round(
        key_2070_75$pwa_pct,
        3
      ),
      "%",
      collapse = "; "
    )
  ),

  paste0(
    "Age 75+ outside-threshold population in 2070: ",
    paste0(
      ">",
      key_2070_75$threshold_min,
      " min=",
      round(
        key_2070_75$outside_pct,
        3
      ),
      "%",
      collapse = "; "
    )
  )
)

writeLines(
  key_lines,
  file.path(
    out_logs,
    "key_results_for_manuscript.txt"
  ),
  useBytes = TRUE
)

# ----------------------------------------------------------------------------
# 27. Save analysis master and session information
# `master` is the national unique-spatial-centroid dataset.
# `master_admin` retains source administrative population allocations for
# prefecture/municipality analyses.
fwrite(
  master_admin,
  file.path(out_logs, "analysis_master_administrative_records.csv")
)


# ----------------------------------------------------------------------------
master_output_cols <- unique(c(
  "mesh_id",
  "shicode",
  "pref_code",
  "centroid_lon",
  "centroid_lat",
  "nearest_distance_km",
  "within_16km",
  "min_duration_min",
  "time_band",
  paste0(
    "within_",
    thresholds,
    "min"
  ),
  paste0(
    "cross_",
    thresholds
  ),
  pop_cols
))

master_output_cols <- master_output_cols[
  master_output_cols %in% names(master)
]

fwrite(
  master[, ..master_output_cols],
  file.path(
    out_root,
    "analysis_master_unique_spatial_centroids_15_30_45_60.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    out_logs,
    "sessionInfo.txt"
  )
)

package_versions <- data.table(
  package = required_packages,
  version = vapply(
    required_packages,
    function(pkg) as.character(utils::packageVersion(pkg)),
    character(1)
  )
)

fwrite(
  package_versions,
  file.path(out_logs, "package_versions.csv")
)

manuscript_output_dictionary <- data.table(
  item = c(
    "Table 1", "Table 2", "Figure 1", "Figure 2", "Figure 3",
    "Supplementary Figure S1", "Supplementary Figure S2",
    "Supplementary Figure S3", "Supplementary Figure S4",
    "Supplementary Figure S5", "Supplementary Figure S6",
    "Supplementary Table S7"
  ),
  output = c(
    "main_tables_bmj_hci_docx/table1_national_accessibility.docx",
    "main_tables_bmj_hci_docx/table2_crossclassification.docx",
    "main_figures_bmj_hci/figure1_geospatial_informatics_workflow.*",
    "main_figures_bmj_hci/figure2_straight_line_distance_vs_osrm_estimated_time_hexbin.*",
    "qgis_layers/figure3_16km_time_crossclassification_15_30_45_60.gpkg",
    "supplementary_figures_bmj_hci/supplementary_figure_s1_concept_16km_and_time_catchments.*",
    "qgis_layers/supplementary_figure_s2_mesh_travel_time_15_30_45_60.gpkg",
    "supplementary_figures_bmj_hci/supplementary_figure_s3_prefecture_heatmap_within16_beyond_threshold.*",
    "supplementary_figures_bmj_hci/supplementary_figure_s4_future_outside_threshold_trajectories.*",
    "supplementary_figures_bmj_hci/supplementary_figure_s5_distance_threshold_population_coverage.*",
    "supplementary_figures_bmj_hci/supplementary_figure_s6_within16_exclusive_time_band_composition.*",
    "supplementary_tables/supp_table_s7_method_transparency.*"
  )
)

fwrite(
  manuscript_output_dictionary,
  file.path(out_logs, "manuscript_output_dictionary.csv")
)

script_finished_at <- Sys.time()
run_duration_seconds <- as.numeric(
  difftime(script_finished_at, script_started_at, units = "secs")
)

run_summary <- data.table(
  analysis_version = analysis_version,
  started_at = format(script_started_at, "%Y-%m-%d %H:%M:%S %z"),
  finished_at = format(script_finished_at, "%Y-%m-%d %H:%M:%S %z"),
  postprocessing_runtime_seconds = run_duration_seconds,
  n_unique_spatial_centroids = nrow(master),
  n_source_mesh_administrative_records = nrow(master_admin),
  n_facilities = nrow(clinics),
  n_unreachable = sum(is.na(master$min_duration_min)),
  potential_od_pairs = potential_od_pairs,
  compute_class = analysis_compute_class,
  specialised_hpc_or_cloud_used = specialised_hpc_or_cloud_used,
  interpretation = paste0(
    "Potential spatial accessibility using OSRM-estimated time as a ",
    "standardised road-network proxy; not realised access or observed travel time."
  )
)

fwrite(
  run_summary,
  file.path(out_logs, "analysis_run_summary.csv")
)

readme_lines <- c(
  "BMJ HCI analysis output guide",
  "",
  "Main manuscript: use only the two tables in main_tables_bmj_hci_docx/ and Figures 1-3 listed in manuscript_output_dictionary.csv.",
  "Detailed tables 1-5 in tables_docx_analytic_audit/ are retained for audit and supplementary checking; they are not the five main manuscript tables.",
  "Figure 3 and Supplementary Figure S2 require final styling/export in QGIS. Figure 3 layout dimensions and 600-dpi pixel targets are written to qgis_layers/figure3_qgis_layout_spec_600dpi.*.",
  "OSRM-estimated time is a standardised proxy for potential spatial accessibility, not observed travel time.",
  "OSRM/OSM provenance and runtime metadata recovered for submission are written to reproducibility_metadata.csv and Supplementary Table S7.",
  "The national workflow is documented as local execution on a 32-GB consumer-grade computer without specialised HPC or cloud resources.",
  "No island/ferry mesh is excluded automatically. A manually reviewed sensitivity analysis runs only when qc_island_ferry_review_completed.csv is supplied."
)

writeLines(
  readme_lines,
  file.path(out_root, "README_BMJ_HCI_outputs.txt"),
  useBytes = TRUE
)

message(
  "Analysis completed. Outputs are in: ",
  normalizePath(
    out_root,
    winslash = "/",
    mustWork = FALSE
  )
)
