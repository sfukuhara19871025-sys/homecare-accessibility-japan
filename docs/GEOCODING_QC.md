# Facility geocoding quality control

## Purpose

The study facility coordinates were generated with the University of Tokyo Center for Spatial Information Science (CSIS) CSV Address Matching Service. Script `R/06_technical_qc_same_facility.R` version 1.4.0 adds a diagnostic link between the source address-matching metadata and OSRM facility-origin network snapping.

This QC asks a narrow question: **are unusually large facility-origin OSRM snaps concentrated among facilities with weaker or coarser address-matching results?** It does not assume that geocoding quality is the cause of a large snap.

## CSIS fields

The CSIS FAQ defines:

- `iConf`: conversion confidence. Code 3 indicates a unique match at one address-hierarchy level; 4 indicates multiple matches at two or more hierarchy levels; 5 indicates a unique match at two or more hierarchy levels.
- `iLvl`: matched address level: -1 coordinates unknown, 0 unknown, 1 prefecture, 2 county/subprefecture, 3 municipality/special ward, 4 designated-city ward, 5 oaza, 6 chome/koaza, 7 block/lot, 8 house/building branch number.

Official reference: <https://geocode.csis.u-tokyo.ac.jp/home/csv-admatch/faq/>

## What script 06 does

When both fields are present, script 06:

1. reproduces the `clinic_id` row-order identifier used by script 03;
2. restricts the global summary to facilities meeting the same Japan-coordinate validity criteria used by script 03;
3. reports `iConf` and `iLvl` distributions without excluding or weighting any facility;
4. identifies the unique facilities that minimise primary OSRM travel time for at least one reachable centroid;
5. links those facilities to their OSRM network-snapping distance; and
6. reports descriptive geocoding-quality summaries for all primary time-minimising facilities, facility-origin snap >500 m, and facility-origin snap >1000 m.

The facility-row-level diagnostic contains only the internal row-order `clinic_id`, quality fields, use counts and snapping metadata; it is still a study-data-derived output and should remain local unless redistribution is permitted.

## Interpretation

`iConf` and `iLvl` describe the address-matching result, whereas the OSRM snap distance describes the distance from an input coordinate to the routable network used by OSRM. A large OSRM snap can therefore arise even after a detailed/high-confidence address match, for example because of road-network representation, facility access geometry, islands or ferry-connected locations. The QC is descriptive and should not be phrased as proof of causation.

## Study-data validation results

The v1.4.0 diagnostic block was rerun against the completed study inputs. Among all 17,931 facilities, 17,835 (99.46%) had `iConf=5`, 15,954 (88.97%) had `iLvl>=7`, and 16,306 (90.94%) had `iLvl>=6`.

Among 15,481 unique primary time-minimising facilities, 50 had facility-origin snap >500 m and 19 had snap >1,000 m. All 50 and all 19 had `iConf=5`; only 1/50 (2.0%) and 1/19 (5.3%), respectively, had `iLvl>=6`. Thus, the large-snap groups did not show low match-confidence scores but were strongly concentrated among coarser address-hierarchy matches.

This pattern should not be interpreted as proof that address matching caused the network snaps. `iConf` measures matching confidence, `iLvl` records the matched address hierarchy, and OSRM snap distance depends on the routable network and local access geometry.

## Reusable datasets

External facility datasets may not contain CSIS `iConf`/`iLvl`. In non-study mode, script 06 then writes `qc_geocoding_status.csv` and skips only the CSIS-specific block; the remaining snapping and same-facility workflow is unchanged. In study mode, the fields are expected so the submission-specific QC cannot be silently omitted.
