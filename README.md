# Home-care accessibility in Japan: open-source geospatial workflow

This repository contains the consolidated six-script computational workflow accompanying the manuscript **“Open-source geospatial informatics assessment of potential accessibility to home-care support facilities in Japan: a nationwide cross-sectional comparison of the 16-km criterion with road-network travel time.”**

The public workflow preserves the computational logic used in the study while removing local absolute paths and consolidating intermediate correction/QC scripts into the final production scripts. **JMAP-derived facility-level data are not redistributed.** The repository includes synthetic example data so that users can inspect the required schemas and exercise the workflow without access to the restricted facility dataset.

## Workflow

Run from the repository root in this order:

```text
R/01_prepare_population_mesh.R
R/02_calculate_16km_accessibility.R
R/03_calculate_osrm_minimum_travel_time.R
R/04_build_16km_osrm_crosswalk.R
R/05_generate_manuscript_outputs.R
R/06_technical_qc_same_facility.R
```

### What each script does

| Script | Role |
|---|---|
| `01_prepare_population_mesh.R` | Converts the population GeoPackage to WGS84, calculates mesh centroids in projected coordinates, and exports 2020–2070 population weights. |
| `02_calculate_16km_accessibility.R` | Finds the straight-line-nearest qualifying facility and classifies each centroid by the 16-km criterion. |
| `03_calculate_osrm_minimum_travel_time.R` | Performs chunked all-facility × all-centroid OSRM table queries and sequentially retains the minimum travel time and time-minimising facility. |
| `04_build_16km_osrm_crosswalk.R` | Row-preserving integration of the 16-km and OSRM outputs, including population-weighted summaries and policy cross-classification. |
| `05_generate_manuscript_outputs.R` | Collapses duplicated administrative records to unique spatial centroids for national mesh analyses, performs QC, and generates manuscript tables, figures, supplementary outputs, and reproducibility metadata. |
| `06_technical_qc_same_facility.R` | Performs physical-plausibility checks, centroid/facility network-snapping QC, both-endpoint sensitivity analyses, same-facility routing, diagnostic figures, and CSIS `iConf`/`iLvl` geocoding-quality diagnostics when those fields are available. |

## Quick start with synthetic data

The repository ships with a **synthetic** population GeoPackage and four synthetic facilities in `example_data/work/`. These are not study data.

1. Install R packages used by the scripts (`sf`, `dplyr`, `data.table`, `osrm`, `matrixStats`, `jsonlite`, `curl`, `ggplot2`, `hexbin`, `scales`, `stringr`, `tidyr`, `flextable`, `officer`, `openxlsx`, and `patchwork`).
2. Start a local OSRM Japan backend on host port `5001` (see `docs/OSRM_SETUP.md`).
3. From the repository root, run the six scripts in order with `HOMECARE_STUDY_MODE=false` (the default).

The synthetic example is intended to demonstrate data structure and workflow execution. It is **not** expected to reproduce the numerical results in the manuscript.


## Validation status

The v1.0.0 public workflow was runtime-tested on Windows with R 4.5.2. The bundled synthetic workflow completed scripts 01–06, including the script-06 v1.4.0 CSIS `iConf`/`iLvl` diagnostic block. Study-data scripts 05–06 were also rerun with `HOMECARE_STUDY_MODE=true`; key manuscript outputs, snapping/same-facility results and the new geocoding-QC summaries were reproduced. The routing core and primary/same-facility estimands were unchanged. See `docs/VALIDATION.md`.


## Reproducing the study-scale analysis

Place the study input files in a working directory outside the repository and set:

```r
Sys.setenv(HOMECARE_WORK_DIR = "D:/path/to/study/workdir")
Sys.setenv(HOMECARE_STUDY_MODE = "true")
```

Then run the six scripts from the repository root. In study mode, script 05 enforces the recorded submission counts (466,792 source mesh–administrative-area records; 465,895 unique spatial centroids; 897 duplicated spatial keys).

The facility input must follow the schema documented in `docs/DATA_DICTIONARY.md`. The original JMAP-derived facility-level file cannot be redistributed; users must obtain an equivalent facility dataset and provide it in the documented schema. For the study-specific CSIS geocoding QC in script 06, the input also contains `iConf` and `iLvl`. External datasets without those CSIS fields can use the reusable pipeline in non-study mode; the CSIS-specific QC is then skipped with a status file.

## OSRM configuration used in the study

The national run used a local OSRM backend with the car profile, MLD algorithm, `--max-table-size 5000`, facility chunks of 1,000, and centroid chunks of 4,000. The container was exposed at `http://127.0.0.1:5001/`. The standard car profile can use ferry connections represented as routable in OpenStreetMap; ferry timetables, waiting time, service frequency and seasonal operation were not modelled. Full setup and recorded software/network metadata are in `docs/OSRM_SETUP.md` and `docs/REPRODUCIBILITY.md`.

## Facility geocoding QC

The study facility coordinates were produced with the University of Tokyo CSIS CSV Address Matching Service. Script 06 treats the returned `iConf` (matching confidence) and `iLvl` (matched address level) as **diagnostic metadata only**: they do not exclude or reweight facilities. The script reports their distribution across routing-eligible facilities and compares them descriptively with OSRM facility-origin snapping distances for the facilities that minimise primary travel time. This comparison is not interpreted as proof that geocoding quality causes large network snaps. See `docs/GEOCODING_QC.md`.

## Data availability and licensing

The **code in this repository** is released under the MIT License. This license does not grant rights to third-party data. JMAP-derived facility-level data are not included. Public population/geographic data remain subject to their original source terms.

Synthetic files under `example_data/` are generated solely for demonstration and do not represent real facilities, patients, or population estimates.

## Provenance

The study accumulated 12 analysis/QC scripts during development. The public six-script workflow consolidates the final logic while preserving a provenance mapping and SHA-256 hashes of the source scripts in `docs/CODE_PROVENANCE.md` and `docs/source_script_sha256.csv`.

## Citation / Zenodo release

Version `v1.0.0` is prepared for GitHub release and Zenodo archival with a release date of 2026-09-29. `CITATION.cff` and `.zenodo.json` contain the release metadata used for archiving.

The v1.0.0 package intentionally retains `TO_BE_ADDED_AFTER_ZENODO_RELEASE` as the repository/DOI placeholder because a DOI does not exist until Zenodo mints it. After DOI assignment, add the DOI to the manuscript Data Availability statement and the GitHub default-branch README. Do not rewrite or retag the archived `v1.0.0` release solely to insert its own DOI; use the DOI in subsequent repository metadata/releases as appropriate.

## Important interpretation

This workflow estimates **potential spatial accessibility**. OSRM-estimated travel time is a standardized road-network proxy, not observed journey time. The 15/30/45/60-minute thresholds are analytical scenarios rather than validated clinical response-time standards. The primary geographic-coverage comparison allows the facility satisfying the 16-km criterion and the time-minimising facility to differ; script 06 provides a same-facility sensitivity analysis.
