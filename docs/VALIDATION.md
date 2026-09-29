# v1.0.0 runtime validation

This document records runtime checks completed for the v1.0.0 public workflow.

## Synthetic workflow

On 2026-09-06, the public workflow was run on the bundled synthetic example data on Windows with R 4.5.2.

Scripts completed successfully in order:

1. `R/01_prepare_population_mesh.R`
2. `R/02_calculate_16km_accessibility.R`
3. `R/03_calculate_osrm_minimum_travel_time.R`
4. `R/04_build_16km_osrm_crosswalk.R`
5. `R/05_generate_manuscript_outputs.R`
6. `R/06_technical_qc_same_facility.R`

The test exercised the local OSRM connection, routing, row-preserving crosswalk, manuscript-output generation, zero-unreachable handling, zero-count cross-classification categories, and same-facility/network-snapping QC.

## Study-data post-processing and technical QC

The v1.0.0 release-code versions of scripts 05 and 06 were also run with `HOMECARE_STUDY_MODE=true` using the archived study inputs.

Key checks reproduced:

- 466,792 source mesh–administrative-area records.
- 465,895 unique spatial centroids.
- 17,931 qualifying facilities.
- 61 OSRM-unreachable centroids.
- Median minimum travel time 6.3 min (IQR 3.4–11.0).
- Main Table 2 30-min mesh cross-classification: 95.6% Both accessible, 0.5% 16 km only, 2.4% Time only, and 1.6% Neither.

Script 06 completed the same-facility and both-endpoint snapping workflow against the same local OSRM backend.

## Windows path-length note

Deep Windows working-directory paths can prevent graphics devices from opening long PDF output filenames. For testing or study reruns on Windows, use a short `HOMECARE_WORK_DIR` path when practical (for example `C:/hc_study`).

## Scope of validation

The repository does not include restricted JMAP-derived study data or the large study outputs. Therefore an independent user cannot reproduce the manuscript numbers from the public repository alone without an authorized equivalent facility dataset and the corresponding population/road-network inputs.

No `renv.lock` is included because an environment lockfile was not generated from the controlled study environment. Package requirements are documented in `docs/PACKAGES.md`.

## CSIS geocoding-QC validation

On 2026-09-07, script 06 version 1.4.0 was rerun on both the bundled synthetic inputs and the completed study inputs. The new `iConf`/`iLvl` diagnostic block completed successfully and did not change the prior snapping or same-facility estimands.

Study-data checks:

- 17,931/17,931 facility rows met the routing-coordinate eligibility criteria used by script 03.
- `iConf=5`: 17,835/17,931 (99.46%).
- `iLvl>=7`: 15,954/17,931 (88.97%).
- `iLvl>=6`: 16,306/17,931 (90.94%).
- `iLvl<=5` among nonmissing codes: 1,585/17,931 (8.84%); missing/unknown: 40/17,931 (0.22%).
- Unique primary time-minimising facilities: 15,481.
- Primary facility-origin snap >500 m: 50 facilities; all 50 had `iConf=5`, but only 1/50 (2.0%) had `iLvl>=6`.
- Primary facility-origin snap >1,000 m: 19 facilities; all 19 had `iConf=5`, but only 1/19 (5.3%) had `iLvl>=6`.

Interpretation: large facility-origin snaps were not concentrated among low `iConf` scores; they were strongly concentrated among coarser address-hierarchy matches (`iLvl<6`). Because address-match resolution and OSRM network snapping describe different processes, this is a descriptive association and not evidence that geocoding alone caused the snaps.

Expected geocoding-QC output files were generated: `qc_geocoding_status.csv`, `qc_geocoding_iConf_distribution.csv`, `qc_geocoding_iLvl_distribution.csv`, `qc_geocoding_quality_summary.csv`, `qc_primary_facility_snap_geocoding_detail.csv`, and `qc_primary_facility_snap_vs_geocoding_quality.csv`.

The v1.0.0 code is therefore runtime-validated for the bundled synthetic workflow and for the study-data post-processing/technical-QC stages used to support the manuscript.
