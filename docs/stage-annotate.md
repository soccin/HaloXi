# Stage 2 — Annotate (cell types & states)

**Purpose:** give every cell a **type** (tumor, T cell, B cell, NK, macrophage,
endothelial, myofibroblast) and **states** (e.g. tumor Ki-67+/pSTAT+, T-helper
subsets, M1/M2 macrophage, exhaustion) based on which markers it is positive for.

Run **[Stage 1 (scan)](stage-scan.md) first** — annotate reuses the data scan
loaded and cached, and you want to confirm the data is sane before typing cells.

## The rules file — the heart of this stage

Cell typing is driven by a single human-readable **rules file**:
`annotation/cell_rules.yaml` (in the project, not in the package — the rules are
*your* biology). It lists, in plain terms:

- each **marker**'s friendly name and the actual Halo channel it maps to
  (Halo decorates markers with antibody clones, so `CD30` is really
  `CD30-E4L4I-CST` in the data — the rules file holds that mapping so you never
  have to);
- each **cell type** as a marker pattern, e.g. *T cell = CD3 positive AND CD20
  negative*;
- each **state** and which parent type it applies to.

**You don't read code to check the logic.** Every run also writes
`cell_rules.md` — the same rules rendered as plain-English pseudocode — into the
output folder. That is the document to review and to send to a biologist for
sign-off. Because both the code and that document come from the one rules file,
what you read is exactly what was computed; they cannot drift apart.

To change how cells are called, edit `annotation/cell_rules.yaml` and re-run. Do
not edit the code.

## How to run it

```sh
Rscript HaloXi/scripts/annotate_data.R MANIFEST.csv [OUTDIR] [--rules=FILE] [--cache=FILE] [--refresh] [--rows=N | --full]
```

| Argument | Meaning |
|---|---|
| `MANIFEST.csv` | Same manifest as the scan stage. |
| `OUTDIR` | Where outputs go. Default: `results/annot`. |
| `--rules=FILE` | Rules YAML to use. Default: `annotation/cell_rules.yaml` (relative to where you run the command). |
| `--full` / `--rows=N` | Whole-file vs. first-N-rows, same as scan. |
| `--cache=FILE` | Loaded-data cache to build or reuse. Default: `cache/<name of OUTDIR>/scan_obj.rds`. Point it at the scan stage's cache to load the data once. |
| `--refresh` | Re-read the CSVs instead of using the cache. |

**Default is fast QC (first 100 rows per file)**, same as scan.

> The driver lives in the package (`HaloXi/scripts/`, alongside `scan_data.R`) —
> it is study-agnostic. The one project-specific piece is the **rules file**,
> which lives with your data (`annotation/cell_rules.yaml`) and is passed in via
> `--rules` (the default points there). See the overview's "Agnostic vs.
> project-specific".

### Typical use

```sh
# fast preview (uses annotation/cell_rules.yaml by default)
Rscript HaloXi/scripts/annotate_data.R manifest.csv results/annot

# real run once the rules look right
Rscript HaloXi/scripts/annotate_data.R manifest.csv results/annot --full
```

## What you get (in `OUTDIR`)

- **`cell_rules.md`** — the plain-English rules the calls were made from. Review
  this / circulate it to collaborators.
- **`Cell_annotation_report.html`** — open this for the results: composition,
  states, and the rules inlined.
- **`Cell_annotation.xlsx`** — the composition table (counts per type per sample)
  and the state breakdown.
- **`plots/`** — `composition_counts.png` and `composition_pct.png` (cell-type
  mix per sample), `state_heatmap.png` (state positivity), and
  `tumor_states.png` (the tumor Ki-67/pSTAT1/pSTAT3/GZMB breakdown, when there
  are tumor cells).

## How to read the results — three labels that are not cell types

This is the one concept to understand. When the markers disagree, a cell can
come out as one of three non-type labels, kept deliberately distinct:

- **UNKNOWN** — the cell matches the marker pattern of **more than one** type at
  once (e.g. positive for both a T-cell and a macrophage marker). A genuine
  conflict. By default nothing forces a winner, so a large UNKNOWN fraction
  tells you how much marker co-expression / ambiguity is in the data before
  anyone imposes a priority order. A rules file may add a
  `conflict_resolution:` block to settle some of these; cells its priority
  order does not settle stay UNKNOWN, and the `TypeCall` / `TypeConflict`
  columns record which is which.
- **UNCLASSIFIED** — the cell matches **no** type (negative for every
  type-defining marker).
- **NA / un-callable** — a marker the rule needed was **not measured in that
  sample**, so the call can't be made. We never treat "not measured" as
  "negative". For example, if the Th1/Th2/Th17 markers are missing from a sample,
  those subsets show as blank/`-` for that sample, not as zero.

So in the composition plot, the coloured bars are real cell types; the grey
UNKNOWN / UNCLASSIFIED segments are cells set aside on this pass.

### States

States (tumor Ki-67/pSTAT/GZMB; T-helper subsets; M1/M2; exhaustion) are only
assigned **once a cell's type is settled** — e.g. tumor states only on cells
already called Tumor. In the state heatmap, a blank/`-` tile means un-callable in
that sample (marker absent, or no cells of that parent type) — again, never a
silent zero. Each tumor state is an **independent** flag: a tumor cell can be
Ki-67+ and pSTAT3+ at the same time.

## Editing the rules

1. Open `annotation/cell_rules.yaml`. It is commented and grouped by markers /
   cell types / states.
2. Change a marker pattern, add a type, etc.
3. Re-run the command above.
4. Re-read the freshly written `cell_rules.md` to confirm the change reads the
   way you intended, and circulate it.

If the file references a marker that isn't declared in its `markers:` section, or
points at a channel not in the data, the run stops with a clear message — so a
typo can't silently produce wrong calls.

## Things to watch for

- **Fast-QC vs. full.** As with scan, default counts are from 100 rows/file. A
  fast-QC preview can have very few or zero cells of a given type (e.g. no tumor
  cells in the first 100 rows, so the tumor-state plot is skipped). Use `--full`
  for real composition.
- **"Please confirm" items.** The top of `cell_rules.md` may list choices that
  were not fully specified by the original marker list and need a biologist's
  confirmation. Resolve those, update the YAML, re-run.
- **Sanity check.** Per sample, the typed cells plus UNKNOWN plus UNCLASSIFIED
  plus un-callable add up to the total cell count — nothing is dropped.
