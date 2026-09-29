# ============================================================================
# 02_calculate_16km_accessibility.R
# Calculate the straight-line distance to the nearest qualifying facility and
# classify each population-mesh centroid by the 16-km criterion.
#
# Derived from: centroids_16km_analysis.R
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

required_packages <- c("sf", "dplyr", "data.table")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required R packages before running: ",
       paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
})

mesh_file <- if (file.exists(cfg$centroid_file_full)) {
  cfg$centroid_file_full
} else {
  cfg$centroid_file_2025
}
facility_file <- cfg$facility_file
output_file <- cfg$distance_16km_file

if (!file.exists(mesh_file)) stop("Population centroid file not found: ", mesh_file)
if (!file.exists(facility_file)) stop("Facility file not found: ", facility_file)

mesh_dt <- fread(mesh_file)
clinic_dt <- fread(facility_file)

required_mesh <- c("mesh_id", "shicode", "centroid_lon", "centroid_lat")
required_facility <- c("fX", "fY")
if (length(setdiff(required_mesh, names(mesh_dt))) > 0L) {
  stop("Mesh input is missing: ", paste(setdiff(required_mesh, names(mesh_dt)), collapse = ", "))
}
if (length(setdiff(required_facility, names(clinic_dt))) > 0L) {
  stop("Facility input is missing: ", paste(setdiff(required_facility, names(clinic_dt)), collapse = ", "))
}

mesh_sf <- st_as_sf(
  mesh_dt,
  coords = c("centroid_lon", "centroid_lat"),
  crs = 4326,
  remove = FALSE
)
clinic_sf <- st_as_sf(
  clinic_dt,
  coords = c("fX", "fY"),
  crs = 4326,
  remove = FALSE
)

sf_use_s2(TRUE)
nearest_idx <- st_nearest_feature(mesh_sf, clinic_sf)
nearest_distance_m <- as.numeric(
  st_distance(mesh_sf, clinic_sf[nearest_idx, ], by_element = TRUE)
)

address_col <- if ("col0" %in% names(clinic_dt)) "col0" else if ("LocName" %in% names(clinic_dt)) "LocName" else NULL
nearest_address <- if (is.null(address_col)) rep(NA_character_, nrow(mesh_dt)) else as.character(clinic_dt[[address_col]][nearest_idx])

mesh_access_16km <- copy(mesh_dt)
mesh_access_16km[, `:=`(
  nearest_clinic_row = nearest_idx,
  nearest_clinic_lon = as.numeric(clinic_dt$fX[nearest_idx]),
  nearest_clinic_lat = as.numeric(clinic_dt$fY[nearest_idx]),
  nearest_clinic_address = nearest_address,
  nearest_distance_m = nearest_distance_m,
  nearest_distance_km = nearest_distance_m / 1000,
  within_16km = nearest_distance_m <= 16000
)]

fwrite(mesh_access_16km, output_file)

qc <- mesh_access_16km[, .(
  n_mesh_records = .N,
  n_facilities = nrow(clinic_dt),
  within_16km_n = sum(within_16km, na.rm = TRUE),
  outside_16km_n = sum(!within_16km, na.rm = TRUE),
  median_nearest_distance_km = median(nearest_distance_km, na.rm = TRUE),
  max_nearest_distance_km = max(nearest_distance_km, na.rm = TRUE)
)]
fwrite(qc, "qc_02_16km_accessibility.csv")

message("Saved: ", output_file)
print(qc)
