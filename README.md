# Halo Xi

## v1.1.pre

Tools for processing Halo object CSV exports (cell segmentation + marker
classification from multiplex imaging) into tidy tables, summaries, plots, and
collaborator-facing Excel/HTML reports.

The work runs in two stages:

1. **Scan** — initial QC: how many cells, which markers per sample, marker
   positivity, tissue footprint.
2. **Annotate** — cell typing: assign each cell a type and states from the
   marker calls, driven by a human-readable rules file.

## Documentation

High-level, "what does this do and how do I run it" docs live in
[`docs/`](docs/):

- [docs/README.md](docs/README.md) — overview, the two stages, quickstart.
- [docs/stage-scan.md](docs/stage-scan.md) — Stage 1: the QC scan.
- [docs/stage-annotate.md](docs/stage-annotate.md) — Stage 2: cell annotation.

For function-level reference, see the package help (`?function_name`) and `man/`.

## Quickstart

```sh
# from the project folder, with a manifest.csv present
Rscript HaloXi/scripts/scan_data.R     manifest.csv results/scan
Rscript HaloXi/scripts/annotate_data.R manifest.csv results/annot
```

The annotate stage reads its cell-type definitions from a rules file
(`annotation/cell_rules.yaml` by default; override with `--rules=`). See
[docs/README.md](docs/README.md) for details (fast-QC vs. `--full`, the manifest
format, and the rules file).
