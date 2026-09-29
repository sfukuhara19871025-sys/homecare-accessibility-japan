# ============================================================================
# 01_prepare_population_mesh.R
# Prepare population-mesh centroids and population weights for OSRM analyses.
#
# Consolidates the study scripts:
#   - creating_centroids.R
#   - 将来人口含むOSRMファイル.R
#
# Required input in cfg$work_dir:
#   japan_population_weighted.gpkg
# Required source fields:
#   MESH_ID, SHICODE, PTN_2020,
#   PTN/PTC/PTD/PTE for 2025, 2030, ..., 2070.
#
# Outputs:
#   japan_population_mesh_centroids_for_osrm_2020_2070.csv
#   japan_population_mesh_centroids_for_osrm.csv  (2020/2025 compatibility file)
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

input_gpkg <- cfg$population_gpkg_file
output_full <- cfg$centroid_file_full
output_2025 <- cfg$centroid_file_2025

if (!file.exists(input_gpkg)) stop("Population GeoPackage not found: ", input_gpkg)

message("Reading population GeoPackage: ", input_gpkg)
japan_pop <- st_read(input_gpkg, quiet = TRUE)
japan_pop_4326 <- st_transform(japan_pop, 4326)

study_years <- seq(2025, 2070, by = 5)
required_source_cols <- c(
  "MESH_ID", "SHICODE", "PTN_2020",
  paste0("PTN_", study_years),
  paste0("PTC_", study_years),
  paste0("PTD_", study_years),
  paste0("PTE_", study_years)
)
missing_cols <- setdiff(required_source_cols, names(japan_pop_4326))
if (length(missing_cols) > 0L) {
  stop("Required population fields are missing: ", paste(missing_cols, collapse = ", "))
}

# The original study calculated centroids in projected coordinates rather than
# directly in longitude/latitude. The same EPSG:3857 -> centroid -> EPSG:4326
# procedure is retained here.
pop_base <- japan_pop_4326 %>%
  select(all_of(required_source_cols))

centroid_sf <- pop_base %>%
  st_transform(3857) %>%
  st_centroid() %>%
  st_transform(4326)
coords <- st_coordinates(centroid_sf)

centroid_dt <- as.data.table(st_drop_geometry(centroid_sf))
centroid_dt[, `:=`(
  mesh_id = as.character(MESH_ID),
  shicode = as.character(SHICODE),
  centroid_lon = coords[, 1],
  centroid_lat = coords[, 2]
)]

out <- centroid_dt[, .(
  mesh_id,
  shicode,
  centroid_lon,
  centroid_lat,
  pop_total_2020 = as.numeric(PTN_2020)
)]

for (yy in study_years) {
  out[, (paste0("pop_total_", yy)) := as.numeric(centroid_dt[[paste0("PTN_", yy)]])]
  out[, (paste0("pop_65plus_", yy)) := as.numeric(centroid_dt[[paste0("PTC_", yy)]])]
  out[, (paste0("pop_75plus_", yy)) := as.numeric(centroid_dt[[paste0("PTD_", yy)]])]
  out[, (paste0("pop_80plus_", yy)) := as.numeric(centroid_dt[[paste0("PTE_", yy)]])]
}

fwrite(out, output_full)
fwrite(
  out[, .(
    mesh_id, shicode, centroid_lon, centroid_lat,
    pop_total_2020,
    pop_total_2025,
    pop_65plus_2025,
    pop_75plus_2025,
    pop_80plus_2025
  )],
  output_2025
)

qc <- data.table(
  source_rows = nrow(japan_pop_4326),
  output_rows = nrow(out),
  missing_lon = sum(is.na(out$centroid_lon)),
  missing_lat = sum(is.na(out$centroid_lat))
)
fwrite(qc, "qc_01_population_mesh_preparation.csv")

message("Saved: ", output_full)
message("Saved: ", output_2025)
message("Rows: ", nrow(out))
