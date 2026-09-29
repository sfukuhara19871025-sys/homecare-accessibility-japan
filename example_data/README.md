# Synthetic example data

`work/` contains a small synthetic population GeoPackage and a synthetic facility CSV using the same field names expected by the public scripts.

These files are fictional and are provided only to demonstrate input schemas. They do not contain JMAP data, real facilities, patients, or official population estimates.

To run OSRM-based scripts 03 and 06 on the synthetic coordinates, a Japan OSRM backend must be running as described in `docs/OSRM_SETUP.md`.

The synthetic facility CSV also contains fictional CSIS-style `iConf` and `iLvl` values solely to exercise the geocoding-QC schema in script 06. They are not measurements from real addresses.
