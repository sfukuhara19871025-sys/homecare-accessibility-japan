# Public configuration for the home-care accessibility pipeline.
# Run scripts from the repository root. To use study data stored elsewhere,
# set HOMECARE_WORK_DIR before launching R, for example:
#   Sys.setenv(HOMECARE_WORK_DIR = "D:/homecare_accessibility_data")
#   Sys.setenv(HOMECARE_STUDY_MODE = "true")

repo_root <- normalizePath(".", winslash = "/", mustWork = FALSE)
default_work_dir <- file.path(repo_root, "example_data", "work")

cfg <- list(
  repo_root = repo_root,
  work_dir = normalizePath(
    Sys.getenv("HOMECARE_WORK_DIR", unset = default_work_dir),
    winslash = "/",
    mustWork = FALSE
  ),
  output_dir = NULL,
  study_mode = tolower(Sys.getenv("HOMECARE_STUDY_MODE", unset = "false")) %in%
    c("1", "true", "t", "yes", "y"),

  # Local OSRM server. The original national analysis exposed container port
  # 5000 on host port 5001.
  osrm_server = Sys.getenv("OSRM_SERVER", unset = "http://127.0.0.1:5001/"),
  osrm_profile_r = "car",
  osrm_profile_http = "driving",
  osrm_max_table_size = 5000L,
  osrm_src_chunk_size = 1000L,
  osrm_dst_chunk_size = 4000L,

  thresholds = c(15, 30, 45, 60),
  osrm_thresholds_extended = c(15, 30, 45, 60, 75, 90),
  coordinate_key_digits = 7L,

  # Input/output filenames used in the study pipeline.
  population_gpkg_file = "japan_population_weighted.gpkg",
  centroid_file_full = "japan_population_mesh_centroids_for_osrm_2020_2070.csv",
  centroid_file_2025 = "japan_population_mesh_centroids_for_osrm.csv",
  facility_file = "japan_clinic_address_only_csv_matched.csv",
  distance_16km_file = "japan_population_mesh_nearest_clinic_16km.csv",
  osrm_nearest_file = "population_mesh_nearest_clinic_duration_by_osrm.csv",
  osrm_arrival_file = "population_mesh_arrival_zone_from_any_clinic_osrm.csv",
  crosswalk_master_file = "policy_crosswalk_16km_osrm_master_v4.csv",

  repository_url_or_doi = Sys.getenv(
    "REPOSITORY_URL_OR_DOI",
    unset = "TO_BE_ADDED_AFTER_ZENODO_RELEASE"
  ),

  expected_counts = list(
    source_records = 466792L,
    unique_spatial_centroids = 465895L,
    duplicated_spatial_key_groups = 897L,
    excess_duplicate_spatial_records = 897L,
    facilities = 17931L
  ),

  study_metadata = list(
    osrm_backend_version = "v6.0.0",
    osrm_docker_image_tag = "ghcr.io/project-osrm/osrm-backend:latest",
    osrm_docker_image_digest = "sha256:729461bcc9ae9e6aafa92c0f93db9b060a32e85d5e72092c01ae4a4a9f1eb564",
    osrm_docker_image_id = "sha256:f4e258e56ca9796897e7ab3a364b9d90c2ed6c246866d17759a8bfc9518f6fb1",
    osrm_docker_image_created = "2025-04-21T15:38:14Z",
    osm_extract_provider = "Geofabrik Japan extract",
    osm_snapshot_date = "2026-05-11T20:20:52Z",
    qgis_version = "3.44.8-Solothurn",
    analysis_device = "Dell 14 Plus DB14250",
    analysis_os_manual = "Windows 11 Pro",
    analysis_cpu = "Intel(R) Core(TM) Ultra 7 258V",
    analysis_ram_gb = "32.0",
    analysis_storage = "1 TB SSD",
    analysis_compute_class = "consumer-grade laptop/PC; local execution",
    specialised_hpc_or_cloud_used = FALSE,
    jmap_acquisition_date = "2026-05-15",
    population_acquisition_date = "2026-06-06"
  )
)

cfg$output_dir <- normalizePath(
  file.path(cfg$work_dir, "public_pipeline_outputs"),
  winslash = "/",
  mustWork = FALSE
)
dir.create(cfg$work_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)

# Convert filenames to full paths while preserving the original basenames.
path_fields <- c(
  "population_gpkg_file", "centroid_file_full", "centroid_file_2025",
  "facility_file", "distance_16km_file", "osrm_nearest_file",
  "osrm_arrival_file", "crosswalk_master_file"
)
for (nm in path_fields) {
  cfg[[nm]] <- file.path(cfg$work_dir, cfg[[nm]])
}
