# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

HaloXi is a small R package for processing Halo object CSV exports (cell
segmentation + marker classification from multiplex imaging). It turns raw
Halo files into tidy per-cell and per-marker tables, summaries, plots, and
collaborator-facing Excel/HTML reports.

## Package commands

There is no build system beyond standard R CMD / roxygen2. Run from the
package root.

```r
# regenerate NAMESPACE and man/*.Rd from roxygen comments (do this after
# editing any #' docs or @export tags)
roxygen2::roxygenise()      # or devtools::document()

# load the package from source without installing (what scripts do)
pkgload::load_all(".")      # or devtools::load_all()

# install
R CMD INSTALL .             # or devtools::install()

# check
R CMD build . && R CMD check HaloXi_*.tar.gz   # or devtools::check()
```

There is no test suite (no `tests/` directory) and no linter configured.

## Running the scanner end to end

The user-facing entry point is the CLI driver, not a package function:

```sh
scripts/scan_data.R MANIFEST.csv [OUTDIR] [--refresh] [--rows=N | --full]
```

- `MANIFEST.csv` must have columns `Sample` and `HaloFile` (paths absolute or
  relative to the manifest's own directory).
- Default reads only the first **100 rows per file** for a fast QC pass; use
  `--full` (or `--rows=N`) for a complete load.
- `--refresh` ignores the RDS cache at `OUTDIR/cache/scan_obj.rds`.
- The script auto-loads HaloXi: installed package if available, else
  `pkgload::load_all()` from the package root (it locates the root via its own
  `--file=` path).

## Architecture

Data flows in one direction; each stage is its own file in `R/`:

1. **`read_halo.R`** — `read_halo()` reads one Halo CSV with `readr` and
   normalises column names via `fixColNames()` (UTF-8 sanitise, spaces→`_`,
   strip trailing `(...)` units). The same normalisation logic is duplicated
   in `scan_halo_markers()`; **if you change column-name handling, change both.**

2. **`load_halo.R`** — `load_halo()` is the core per-file loader. It splits a
   Halo file into three things returned as a list: `cell.data` (one row per
   cell, with a SHA1 `UUID` from `uuidCols`), `marker.data` (long: one row per
   cell × marker, from `*_Positive_Classification` columns), and `VERSION`.
   It also computes the `MarkerPos` phenotype string per cell (sorted,
   `;`-joined positive non-control markers). `controlMarkers` default `DAPI`.

3. **`scan_manifest.R`** — the orchestration layer (most of the package). Key
   pieces:
   - `read_manifest()` validates/normalises the manifest and resolves paths.
   - `scan_halo_markers()` reads **headers only** to get each sample's marker
     panel — fast and independent of the row cap, so panel-presence tables are
     always complete even under `--rows=N`.
   - `load_manifest()` calls `load_halo()` per sample, row-binds into one
     combined object, and caches to RDS (cache keyed on `n_max`).
   - `summarize_*` / `marker_*_matrix()` build the summary tibbles.
   - `plot_*` builders each return a ggplot object (no I/O).
   - `scan_manifest()` is the single top-level function that runs all of the
     above and returns `list(tables, plots, obj, meta)`.

4. **`write_scan_workbooks.R`** — `write_scan_workbooks()` renders the summary
   tables into two styled `openxlsx` workbooks (`Halo_scan_overview.xlsx`,
   `Halo_scan_markers.xlsx`).

5. **`scripts/scan_data.R`** + **`scripts/scan_report.Rmd`** — the CLI driver
   wires `scan_manifest()` → workbooks → PNGs → HTML report. `scripts/` and
   `attic/` are excluded from the package build via `.Rbuildignore`.

`R/VERSION.R` defines the `VERSION` string stamped into every returned object;
bump it together with the `Version:` field in `DESCRIPTION`.

### Conventions specific to this code

- Sample identity flows through a `Sample` column everywhere; cells are keyed
  by `UUID` (SHA1 of `Image_Location` + bounding box, see `.HALO_UUID_COLS`).
- Marker names appear both raw (`Marker`) and upper-cased (`MarkerNorm`);
  matrices and control checks use `MarkerNorm`.
- In presence/positivity matrices, a blank/`NA` cell means "marker absent from
  that sample's panel" — distinct from "present but 0% positive".
- `attic/` holds the older v20 code this package replaces; do not edit or
  import from it.

## Tidyverse style

Follow the global R style rules in `~/.Rprofile` / global CLAUDE.md: tidyverse
first (`dplyr`/`purrr`/`tidyr`, base pipe `|>`), `stringr` + `glue` for all
string work, `fs` for filesystem, `readr` with `show_col_types = FALSE`. Note
the existing `load_halo.R` still uses some base-R string ops (`gsub`) and
`%>%`; prefer the tidyverse equivalents in new code. No emoji anywhere
(commits, docs, comments).
