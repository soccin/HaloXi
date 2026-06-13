# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`HaloXi` is a small, standalone R package for processing HALO object files
(per-cell tables exported from Indica Labs HALO multiplex-immunofluorescence
analysis). It is the reusable analysis kernel: it knows how to read one HALO
CSV, assign stable per-cell identities, and reduce per-marker positivity into
a compact cell phenotype. It knows nothing about any particular study,
marker panel, or patient metadata — all of that is supplied by the calling
project.

This is its **own git repository** (`git@github.com:soccin/HaloXi`), with its
own history and branches (`main`, `devs`, ...). Host projects do **not**
vendor it as a git submodule. Instead they copy the working tree into the
project and pin it to a specific commit via a `KERNEL.lock` file, restoring
it with a `bin/kernel-restore.sh` helper (`git -C HaloXi checkout <sha>`).
Consequences when working in here:

- Commits made in this directory belong to the `HaloXi` repo, not to whatever
  outer project happens to contain it. Treat it as a separate repo.
- This file is the **only** documentation that travels with the kernel when
  it is cloned into a new project. Keep it self-contained: do not add
  study-specific context (specific marker panels, patient metadata schemas,
  driver-script pipelines) here. That belongs in the host project's own
  `CLAUDE.md`.

## Public API

Three functions are user-facing; only `load_halo` is exported in `NAMESPACE`,
but `read_halo` and `schema_summary` are also `@export`-tagged and intended
for use. See `R/` for source and `man/` for generated docs.

### `load_halo(hfile, uuid_cols, sample_name, colRenameMap, cols_extra, marker_map, control_markers)`

The core entry point. Reads one HALO object CSV and returns a list:

- `cell.data` — tibble of `UUID`, `Sample`, the `XMin/XMax/YMin/YMax`
  bounding box, and `MarkerPos`. With `cols_extra`, the named extra columns
  are joined on as well.
- `marker.data` — long tibble `UUID` x `Marker` x `Positive` (plus a
  `MarkerNorm` uppercased helper column), one row per cell per marker.
- `VERSION` — the package version string (from `R/VERSION.R`).

Behavior that is easy to get wrong:

- **UUID.** Each cell gets a SHA1 `UUID` hashed from `Sample` + the columns
  named in `uuid_cols` (joined with `;`). `uuid_cols` is required — omitting
  it is a fatal error. `Sample` is always part of the hash, so identical
  coordinates in two samples do not collide. The marker join back onto cells
  is by `UUID`, so the chosen `uuid_cols` must be unique per cell within a
  file.
- **`sample_name`.** May be a literal string, a `function(hfile) -> string`,
  or omitted (then derived from the filename: basename with the `.csv*`
  suffix stripped).
- **Markers come from `*_Positive_Classification` columns.** `marker.data`
  is built by gathering every column matching `_Positive_Classification$`
  and stripping that suffix to get the marker name.
- **`marker_map` is identity-filled.** It is a named vector `old = new` used
  to rename markers. You only need to list markers you want to **rename** or
  **drop** — any marker not named in `marker_map` is kept under its own name
  automatically. Map a marker to `NA` (or filter it out upstream) to drop it.
- **`MarkerPos` is the cell phenotype.** For each cell, the markers scored
  positive (`Positive == 1`), excluding `control_markers`, are uppercased,
  sorted, and `;`-joined into one string. Sorting means column order in the
  source file does not matter — a CD8/PD1 double-positive is always
  `CD8;PD1`. `control_markers` defaults to `c("DAPI")`.

### `read_halo(file, colRenameMap, ...)`

Thin `readr::read_csv` wrapper (always `show_col_types = FALSE`,
`progress = FALSE`) that cleans column names via `fix_col_names`:

- spaces -> `_`
- trailing parenthetical units `_(...)` at end of name -> removed
- `%_X` -> `X_PCT`

With an optional `colRenameMap` it then normalizes names: the map is a named
list `canonical = c(variant, variant, ...)`; for each column, the first
variant found as a literal (fixed) substring is rewritten to the canonical
name. Variants are matched longest-first to avoid partial-substring
collisions. Pass extra `read_csv` args (e.g. `n_max = 1` to peek at the
header) through `...`.

### `schema_summary(df)`

Returns a tibble describing each column: `column`, `class`, `n_unique`,
`n_na`, `all_na`. Used during exploration to discover which
`*_Positive_Classification` marker columns a file actually carries.

## Internal helpers (not exported)

- `generate_cell_uuid(dat, cols_uuid)` — the SHA1 hashing behind `load_halo`.
- `fix_col_names(col_names)` — the column-name cleaner used by `read_halo`.
- `normalize_name(name, mappings)` — the `colRenameMap` substring rewriter.

## Layout

- `R/` — package source. `load_halo.R`, `read_halo.R`, `schema_summary.R`,
  and `R/VERSION.R` (single `VERSION` string; bump here on release).
- `man/` — roxygen2-generated `.Rd` docs. **Generated — do not hand-edit.**
- `NAMESPACE` — roxygen2-generated. **Do not hand-edit.**
- `DESCRIPTION` — package metadata; depends on `dplyr`, `tidyr`, `purrr`
  (also uses `digest`, `readr`).
- `attic/` — the previous-generation (`haloV20`) implementation. Legacy
  reference only; do not extend it or import from it.

## Development

Standard `devtools` package workflow:

```r
devtools::load_all(".")   # load for interactive use / from a host project: load_all("HaloXi")
devtools::document()      # regenerate NAMESPACE + man/ from #' roxygen blocks
devtools::check()         # R CMD check
```

`NAMESPACE` and `man/` are generated by roxygen2 — edit the `#'` blocks in
`R/`, then `document()`. Do not edit the generated files directly. Host
projects typically consume the package with `devtools::load_all("HaloXi")`
(dev mode, no install), so edits to `R/` take effect on the next `load_all`.

## Conventions

- Follow the user's global R style rules (tidyverse-first; `dplyr`/`purrr`
  verbs over base subsetting and the `apply` family; `stringr`/`glue` over
  base string ops; base pipe `|>` preferred). Some existing code still mixes
  `%>%` and base idioms — match tidyverse style in new code.
- No trailing whitespace.
- No emoji anywhere (code, comments, commit messages, docs).
- This package is study-agnostic by design. Resist adding hardcoded marker
  names, panels, or metadata assumptions — push that configuration out to the
  caller via the existing arguments (`marker_map`, `colRenameMap`,
  `uuid_cols`, `control_markers`, `cols_extra`).
