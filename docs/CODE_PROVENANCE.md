# Code provenance

The study was developed iteratively. The authors' private provenance archive contains 12 R scripts, including intermediate QC/correction scripts. The public release consolidates the final computational logic into six ordered production scripts rather than exposing every exploratory intermediate file.

## Mapping from study scripts to public scripts

| Public script | Principal source script(s) | Treatment |
|---|---|---|
| `01_prepare_population_mesh.R` | `creating_centroids.R`; `将来人口含むOSRMファイル.R` | Consolidated into one parameterized preparation script. |
| `02_calculate_16km_accessibility.R` | `centroids_16km_analysis.R` | Local path removed; core nearest-feature/spherical-distance logic retained. |
| `03_calculate_osrm_minimum_travel_time.R` | `osrm_population_mesh_accessibility_analysis.R` | Parameterized OSRM endpoint/chunk settings; sequential minimum-update logic retained. |
| `04_build_16km_osrm_crosswalk.R` | `analyze_16km_vs_osrm_policy_crosswalk_v4_row_preserving.R` | Row-preserving join and duplicate diagnostics retained. |
| `05_generate_manuscript_outputs.R` | `osrm_full_analysis_BMJ_HCI_15_30_45_60_v7_1_unique_spatial_centroids_qcfix.R` | Final submission analysis retained; local paths/metadata moved to configuration; exact-count checks controlled by study mode. |
| `06_technical_qc_same_facility.R` | `BMJ_HCI_additional_QC_same_facility_v1_3_1_shortpaths.R` | Final snapping and same-facility QC retained; private legacy-checkpoint paths removed. Public script 06 v1.4.0 adds diagnostic-only CSIS `iConf`/`iLvl` QC using fields already present in the study facility input; the routing core is unchanged. |

## Intermediate scripts not required for the public production workflow

`osrm_QC.R`, `osrm_QC_no2.R`, `osrm_QC_ver3.R`, `QC後の修正no1.R`, and `QC後の修正no2_mesh-based resultsを再計算.R` were used during investigation of duplicate spatial records and development of the final unique-centroid corrections. Their finalized logic is incorporated in script 05 and/or script 06. The original files remain in the authors’ private provenance archive.

## Cryptographic record

`source_script_sha256.csv` records SHA-256 hashes of the source R scripts supplied when the public repository was assembled. `source_archive_sha256.txt` records the SHA-256 hash of the complete private source-code archive used for this consolidation. These hashes document provenance without redistributing third-party data or development-only files.
