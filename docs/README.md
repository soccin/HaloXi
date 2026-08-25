# HaloXi — Overview

High-level, "what does this do and how do I run it" documentation. For
function-level reference see the package help (`?function_name`) and `man/`.

## What HaloXi is

HaloXi turns **Halo object CSV exports** (one row per segmented cell, with each
marker scored positive/negative by Halo) into tidy tables, summaries, plots, and
collaborator-facing reports. It is a small, reusable R package: it knows how to
read and summarize Halo multiplex data, but it carries **no study-specific
biology** of its own (see "Agnostic vs. project-specific" below).

## The two stages

The work happens in two sequential stages. Each is a separate command that
produces an Excel workbook or two, a folder of plots, and a self-contained HTML
report you can open in a browser and send to collaborators.

```
   Halo CSVs  ──►  [ STAGE 1: SCAN ]  ──►  QC: how much data, which markers,
   + manifest        is my data sane?        marker positivity, tissue footprint
                          │
                          ▼  (reuses the loaded-data cache)
                     [ STAGE 2: ANNOTATE ]  ──►  cell types & states:
                       what cell is each one?     composition, tumor states, etc.
```

1. **Scan** — *initial QC*. Before any analysis: how many cells per sample,
   which markers are in each sample's panel, what fraction of cells are positive
   for each marker, and where the tissue sits. Answers "is my data sane and what
   do I actually have?" See **[stage-scan.md](stage-scan.md)**.

2. **Annotate** — *cell typing*. Assigns every cell a **type** (tumor, T cell,
   B cell, NK, macrophage, endothelial, myofibroblast) and **states** (tumor
   proliferation/signaling, T-helper subsets, M1/M2, exhaustion) from the marker
   calls, using a human-readable rules file you (and your biologist) control.
   Answers "what is each cell?" See **[stage-annotate.md](stage-annotate.md)**.

Run them in order: **scan first** (always), then **annotate**. Both cache the
data they load, and pointing them at the same cache with `--cache=` means it is
read once and the second run is fast. The cache is never written inside the
output directory: that folder is what you send to a collaborator, and the cache
is a build artifact that can run to hundreds of megabytes.

## The one input you provide: a manifest

Both stages read a **manifest** — a tiny CSV listing your samples and where
their Halo files are:

```csv
Sample,HaloFile
S1,data/raw/S22-53201 (MP).csv
S2,data/raw/S22-7844 (MP).csv
S3,data/raw/S23-13342 (ZP).csv
```

- `Sample` is the short name used everywhere in the outputs.
- `HaloFile` is the path to that sample's Halo CSV (absolute, or relative to the
  manifest's own folder).

## Fast-QC by default (important)

By default both stages read only the **first 100 rows of each Halo file** — a
fast pass so you can iterate. Marker-panel coverage is always read from the full
file headers (so "which markers exist" is always complete), but **cell counts
and percentages reflect only those 100 rows** until you do a full run.

- Add `--full` for a complete, whole-file run (slower; one sample here is ~1.2M
  rows).
- Or `--rows=N` for a specific cap.

Every report banners which mode produced it, so you can't mistake a fast-QC
preview for final numbers.

## Quickstart

```sh
# from the project folder, with a manifest.csv present

# Stage 1 — QC scan (fast preview)
Rscript HaloXi/scripts/scan_data.R     manifest.csv results/scan

# Stage 2 — cell annotation (fast preview)
Rscript HaloXi/scripts/annotate_data.R manifest.csv results/annot

# when the data and the rules look right, run both for real:
Rscript HaloXi/scripts/scan_data.R     manifest.csv results/scan  --full
Rscript HaloXi/scripts/annotate_data.R manifest.csv results/annot --full
```

Open the `*.html` report in each output folder to see the results.

## Agnostic vs. project-specific (what lives where)

HaloXi is a **shared package** — improvements to it benefit every study that
uses it. So anything tied to *one* study's biology (its marker panel, antibody
clones, cell-type definitions) lives **with the project**, not in the package.
For cell annotation, the dividing line falls between the *code* and the *rules*:

- **The code is agnostic and ships in the package.** Both stage drivers
  (`HaloXi/scripts/scan_data.R`, `HaloXi/scripts/annotate_data.R`) and the
  annotation engine know nothing about any particular study — `annotate_data.R`
  reads every cell-type and marker definition at run time from the rules file
  you point it at. Hand it a different study's rules and it just works.
- **The rules are project-specific and live with the project.** Your cell-type
  definitions are `annotation/cell_rules.yaml`, kept next to your data. The
  annotate driver defaults to that path and you can override it with `--rules=`.
  This is the one file that encodes *your* biology; it is never bundled in the
  package.

In short: the package supplies the engine and the commands; your project
supplies the rules they run on (and the manifest).

You do not need to care about this to run things — just note that the two
commands have different paths (`HaloXi/scripts/...` vs `annotation/...`). The
[stage-annotate.md](stage-annotate.md) doc explains the rules file.

## Requirements

- R (>= 4.3) with the package's dependencies installed (`dplyr`, `tidyr`,
  `purrr`, `fs`, `glue`, `readr`, `yaml`, plus `ggplot2`, `openxlsx`,
  `rmarkdown`/`knitr` for plots and reports).
- The driver scripts auto-load HaloXi: an installed copy if present, otherwise
  from source — no install step strictly required to run them.
- `pandoc` (bundled with RStudio, or installed separately) for the HTML reports.
