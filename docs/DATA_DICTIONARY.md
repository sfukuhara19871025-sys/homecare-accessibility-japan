# Data dictionary

## 1. Population GeoPackage (`japan_population_weighted.gpkg`)

Required fields used by script 01:

- `MESH_ID`: mesh identifier.
- `SHICODE`: administrative-area code associated with the source record.
- `PTN_2020`: total population in 2020.
- `PTN_YYYY`: total population for 2025–2070 at 5-year intervals.
- `PTC_YYYY`: population aged 65+.
- `PTD_YYYY`: population aged 75+.
- `PTE_YYYY`: population aged 80+.
- polygon geometry.

The source can contain more than one administrative population record for the same physical mesh centroid. The final national analysis therefore distinguishes source records from unique spatial centroids and aggregates population allocations only after routing invariance has been verified.

## 2. Facility input (`japan_clinic_address_only_csv_matched.csv`)

Minimum required fields for routing:

- `fX`: longitude (WGS84 decimal degrees).
- `fY`: latitude (WGS84 decimal degrees).

Study-specific CSIS address-matching fields used by script 06 when present:

- `iConf`: CSIS conversion-confidence code. The CSIS FAQ defines 3 as a unique match at one address-hierarchy level, 4 as multiple matches at two or more levels, and 5 as a unique match at two or more levels. The study QC treats these codes as source-reported diagnostic metadata, not as exclusion criteria.
- `iLvl`: CSIS matched-address level. Codes are `-1` coordinates unknown, `0` level unknown, `1` prefecture, `2` county/subprefecture, `3` municipality/special ward, `4` designated-city ward, `5` oaza, `6` chome/koaza, `7` block/lot, and `8` house/building branch number.

CSIS code reference: <https://geocode.csis.u-tokyo.ac.jp/home/csv-admatch/faq/>

Other optional fields used for local traceability when present:

- `col0`: address or source label.
- `LocName`: facility name/label.
- other source-specific columns may be retained locally.

Script 03 recreates `clinic_id` from the original facility-file row order before coordinate filtering. Script 06 reproduces the same row-order identifier so source geocoding metadata can be linked to the primary time-minimising facility without relying on facility names.

**The original JMAP-derived facility file is not included in this repository.** Do not commit third-party facility-level data or generated facility-level QC tables unless redistribution is explicitly permitted.

## 3. Key derived files

- `japan_population_mesh_centroids_for_osrm_2020_2070.csv`: centroid coordinates plus population weights.
- `japan_population_mesh_nearest_clinic_16km.csv`: nearest straight-line facility, distance, and `within_16km`.
- `population_mesh_nearest_clinic_duration_by_osrm.csv`: time-minimising facility and minimum OSRM duration.
- `population_mesh_arrival_zone_from_any_clinic_osrm.csv`: OSRM result joined to population attributes and threshold flags.
- `policy_crosswalk_16km_osrm_master_v4.csv`: row-preserving integration of distance and time results.

## 4. Spatial join key

The production scripts use `mesh_id + centroid_lon + centroid_lat`, with longitude/latitude formatted to seven decimal places, to distinguish physical spatial records. This avoids dropping records solely because `mesh_id` is duplicated across administrative allocations.

## 5. Technical-QC geocoding outputs (script 06)

When `iConf` and `iLvl` are available, script 06 writes the following under `public_pipeline_outputs/technical_qc/qc/` in the local work directory:

- `qc_geocoding_status.csv`: whether CSIS-specific QC was available/run.
- `qc_geocoding_iConf_distribution.csv`: facility distribution by `iConf`.
- `qc_geocoding_iLvl_distribution.csv`: facility distribution by `iLvl`.
- `qc_geocoding_quality_summary.csv`: compact counts/percentages for confidence and address-level groupings.
- `qc_primary_facility_snap_geocoding_detail.csv`: local facility-row-level diagnostic linking primary facility use, `iConf`/`iLvl`, and facility-origin snap distance.
- `qc_primary_facility_snap_vs_geocoding_quality.csv`: aggregated comparison for all primary time-minimising facilities and for facility-origin snap >500 m and >1000 m.

These are diagnostic outputs. Large OSRM snapping should not automatically be attributed to geocoding quality; road-network representation, access geometry, islands and ferry connections may also contribute. Generated study-data QC outputs are intentionally excluded from the public repository.
