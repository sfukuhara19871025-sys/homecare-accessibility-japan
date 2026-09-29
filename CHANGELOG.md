# Changelog

## 1.0.0 — 2026-09-29

- Consolidated 12 study/development R scripts into six ordered public production scripts.
- Added configuration-based paths and study-mode count checks.
- Added synthetic demonstration inputs.
- Added OSRM setup, reproducibility, data dictionary, code provenance, GitHub/Zenodo release metadata, and licensing files.
- Included the final both-endpoint network-snapping and same-facility sensitivity workflow.
- Hardened script 05 so zero OSRM-unreachable centroids are handled correctly in reusable/synthetic datasets.
- Hardened 16-km × travel-time cross-classification output so zero-count categories are retained explicitly.
- Fixed vector percentage formatting in script 05 so all cross-classification categories retain their own percentages.
- Added immutable OSRM container digest, image ID, and image creation timestamp to generated reproducibility metadata.
- Runtime-validated the full synthetic workflow (scripts 01–06) on Windows with R 4.5.2.
- Runtime-validated study-data post-processing and technical QC (scripts 05–06) with `HOMECARE_STUDY_MODE=true`; key submission counts and Table 2 cross-classification values matched the archived manuscript outputs.
- Added CSIS address-matching QC to script 06: `iConf` and `iLvl` distributions, routing-eligible facility summary, and descriptive comparison with PRIMARY facility-origin OSRM snapping distances.
- Kept geocoding fields diagnostic-only: no facility exclusion, weighting, or change to the primary routing/same-facility estimands was introduced.
- Added explicit ferry-routing documentation: the standard OSRM car profile may use routable OSM ferry connections, while timetables, waiting time, frequency, and seasonal operation are not modelled.
- Added `docs/GEOCODING_QC.md` and expanded the data dictionary, reproducibility notes, provenance, and release checklist.
- Updated the synthetic facility CSV to use fictional but valid CSIS-style `iConf`/`iLvl` codes so the new QC branches are represented in the example schema.
- Runtime-validated script 06 v1.4.0 on both bundled synthetic data and the study inputs; all expected geocoding-QC files were generated.
- Study-data geocoding QC showed high overall CSIS match confidence (`iConf=5` for 99.46%) and fine address-level matching for most facilities (`iLvl>=6` for 90.94%); large facility-origin snaps were concentrated among coarser address-level matches rather than low `iConf` scores.
- Synchronized the manuscript/Supplementary Table S7 reporting plan with facility-identity, geocoding-QC and ferry-routing results before the public v1.0.0 freeze.
