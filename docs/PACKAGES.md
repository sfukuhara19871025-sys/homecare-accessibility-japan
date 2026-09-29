# R package requirements

The six public scripts use the following R packages:

- `data.table`
- `sf`
- `dplyr`
- `osrm`
- `matrixStats`
- `jsonlite`
- `curl`
- `ggplot2`
- `hexbin`
- `scales`
- `stringr`
- `tidyr`
- `flextable`
- `officer`
- `openxlsx`
- `patchwork`

The repository does not fabricate an `renv.lock` file because an environment lockfile was not generated from the controlled study environment. The v1.0.0 public workflow was runtime-tested as documented in `docs/VALIDATION.md`. Scripts 05 and 06 write reproducibility/session metadata when executed; users who require an isolated package environment may initialize `renv` in their own validated environment.
