# GitHub / Zenodo release checklist

Status as of 2026-09-29 for the final pre-publication `v1.0.0` package.

## Package preparation completed

- [x] Creator names and order in `CITATION.cff` and `.zenodo.json` match the manuscript author order: Satoshi Fukuhara; Yohei Ono.
- [x] Release date set to 2026-09-29 in `CITATION.cff`, `.zenodo.json`, and `CHANGELOG.md`.
- [x] Current manuscript title reflected in `README.md`.
- [x] No JMAP-derived study CSV, address list, real facility-coordinate file, or other restricted study data are included in the repository.
- [x] macOS packaging artefacts (`__MACOSX`, `.DS_Store`, `._*`) removed from the release package.
- [x] Repository checked for accidental personal absolute paths or local cloud-storage paths; only generic documentation examples remain.
- [x] Bundled synthetic workflow previously completed scripts 01–06; see `docs/VALIDATION.md`.
- [x] Study-data release-code post-processing/QC with `HOMECARE_STUDY_MODE=true` previously completed for scripts 05–06 and reproduced key manuscript outputs; see `docs/VALIDATION.md`.
- [x] Script 06 v1.4.0 geocoding-QC branches previously validated on bundled synthetic data and study inputs.
- [x] Manuscript/Supplementary reporting synchronized with facility identity, CSIS geocoding QC, ferry handling, Supplementary Table S8, and Supplementary Figure S7.
- [x] `MANIFEST_SHA256.txt` recomputed after final package edits.

## Author sign-off / publication actions

- [ ] Final author/legal sign-off: confirm the copyright holder in `LICENSE` (currently `Satoshi Fukuhara`).
- [ ] Create the public GitHub repository/release using tag `v1.0.0`; do not commit restricted study inputs or generated study outputs.
- [ ] Enable Zenodo for the GitHub repository and archive the `v1.0.0` release, or upload this exact release ZIP to Zenodo if using manual deposition.
- [ ] Record the assigned Zenodo DOI in the manuscript Data Availability statement and in the GitHub default-branch README.
- [ ] Do not rewrite the archived `v1.0.0` tag solely to insert its own DOI. If code changes after peer review, issue a new tagged release/Zenodo version rather than silently modifying the archived release.
