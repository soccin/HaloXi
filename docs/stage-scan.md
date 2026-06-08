# Stage 1 — Scan (initial QC)

**Purpose:** before any analysis, find out *what data you actually have* and
*whether it is sane*. This is the first thing to run on a new set of Halo files.

It answers, per sample:
- How many cells are there?
- Which markers are in the panel? (panels often differ between samples)
- What fraction of cells are positive for each marker?
- Where does the tissue sit, and how much of it is "phenotyped" (positive for at
  least one real marker) vs. blank?

It does **not** assign cell types — that is Stage 2 (annotate).

## How to run it

```sh
Rscript HaloXi/scripts/scan_data.R MANIFEST.csv [OUTDIR] [--refresh] [--rows=N | --full]
```

| Argument | Meaning |
|---|---|
| `MANIFEST.csv` | CSV with `Sample` and `HaloFile` columns (see the overview). |
| `OUTDIR` | Where outputs go. Default: `results/scan`. |
| `--full` | Read **all** rows of every file (accurate, slower). |
| `--rows=N` | Read only the first N rows per file. |
| `--refresh` | Ignore the cached loaded data and re-read the CSVs from scratch. |

**Default (no `--full`/`--rows`): fast QC — first 100 rows per file.** Good for a
first look; not final numbers.

### Typical use

```sh
# quick first look
Rscript HaloXi/scripts/scan_data.R manifest.csv results/scan

# the real, whole-file scan once the quick look is sensible
Rscript HaloXi/scripts/scan_data.R manifest.csv results/scan --full
```

## What you get (in `OUTDIR`)

- **`Halo_scan_report.html`** — open this first. A self-contained report tying
  everything below into one readable page.
- **`Halo_scan_overview.xlsx`** — the manifest, the per-sample cell counts, and
  the **marker-panel coverage** matrix (which markers each sample has).
- **`Halo_scan_markers.xlsx`** — per-marker **positivity** (the % and number of
  cells positive for each marker, per sample), long and wide.
- **`plots/`** — `cell_counts.png`, `marker_presence.png` (panel coverage),
  `marker_heatmap.png` (positivity), and one `spatial_<Sample>.png` tissue
  footprint per sample.
- **`cache/scan_obj.rds`** — the loaded data, cached. **Stage 2 reuses this**, so
  run scan before annotate and the annotate step is fast.

## How to read the results

- **Cell counts** — total cells per sample, split into *phenotyped* (positive for
  ≥1 non-control marker) vs. *unphenotyped* (negative for everything). A high
  unphenotyped fraction can indicate staining or threshold issues.
- **Marker-panel coverage** — a sample × marker grid. **Panels are often not
  identical across samples.** A marker that is *absent* from a sample's panel is
  shown blank/grey — this is different from "present but never positive". This
  matters: a marker can only be compared across samples where it exists in all of
  them.
- **Marker positivity** — % of scored cells positive for each marker. In the
  matrix, a **blank cell means the marker is absent from that sample** (not 0%);
  control markers (e.g. DAPI) are flagged.
- **Spatial footprint** — cell centroids per sample, coloured by phenotyped
  status. Useful for spotting empty regions, tissue extent, or artefacts before
  any spatial analysis.

## Things to watch for

- **Fast-QC vs. full.** If the report banner says "Fast QC mode", the counts and
  percentages are from 100 rows per file only. Re-run with `--full` for real
  numbers. (Panel coverage is always complete regardless.)
- **Differing panels.** Don't compare a marker across samples that don't all have
  it. The coverage matrix tells you which are safe.
- **Re-running.** Normal re-runs reuse the cache (fast). Use `--refresh` only if
  the underlying CSVs changed. The cache also remembers whether it was a fast or
  full load, and won't reuse a 100-row cache for a `--full` request.

## Next step

Once the data looks sane, move to **[stage-annotate.md](stage-annotate.md)** to
assign cell types and states.
