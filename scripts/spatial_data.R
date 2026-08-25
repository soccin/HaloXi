#!/usr/bin/env Rscript

## ===========================================================================
## spatial_data.R - co-location and neighbourhood analysis of annotated cells
##
## Answers two spatial questions over an annotated Halo manifest:
##   * which cell types sit closer together than a random arrangement of the
##     same labels would put them (co-location, at one or more radii);
##   * what the tissue in a ring around every cell of one chosen type is made
##     of (neighbourhood composition).
##
## Study-AGNOSTIC, like scan_data.R and annotate_data.R: cell types come from
## the rules YAML, and the scan resolution, radii and anchor type are all
## command-line arguments with no study values baked in.
##
##   usage: spatial_data.R MANIFEST.csv [OUTDIR] --um-per-px=VALUE
##                         [--rules=FILE] [--cache=FILE] [--samples=A,B]
##                         [--radii=20,50] [--ring=20-50] [--anchor=TYPE]
##                         [--perms=N] [--seed=N] [--refresh]
##                         [--rows=N | --full]
##
##     MANIFEST.csv     CSV with columns Sample, HaloFile
##     OUTDIR           output directory (default: results/spatial)
##     --um-per-px=V    microns per pixel. REQUIRED, no default: Halo
##                      coordinates are pixels and the file carries no scan
##                      resolution, so guessing it would rescale every distance
##     --rules=FILE     cell-annotation rules YAML
##                      (default: annotation/cell_rules.yaml)
##     --cache=FILE     loaded-object cache to build or reuse
##                      (default: cache/<name of OUTDIR>/scan_obj.rds)
##     --samples=A,B    analyse only these samples (default: all in the
##                      manifest). Applied after loading, so the shared cache
##                      stays valid
##     --radii=20,50    co-location radii in microns, measured from 0
##     --ring=20-50     neighbourhood ring in microns, inner-outer
##     --anchor=TYPE    cell type to measure outward from (default: Tumor)
##     --perms=N        label permutations for the null (default: 99)
##     --seed=N         RNG seed (default: 1)
##     --refresh        reload from source, ignoring cache
##     --rows=N/--full  row cap per Halo file (default 100, fast QC)
##
## Shares the loaded-object cache with scan_data.R and annotate_data.R -- point
## them at one file with --cache= and the 600 MB object is built once. The cache
## is keyed on the manifest and the row cap together, so a run against an edited
## manifest reloads rather than silently describing the old sample set. It is NOT
## written under OUTDIR: OUTDIR is a deliverable and the cache is a build
## artifact of several hundred MB.
##
## Produces, under OUTDIR:
##   Spatial_analysis.xlsx          co-location, neighbourhood, ring spread
##   neighborhood_per_anchor.csv.gz one row per anchor cell
##   plots/*.png                    heatmap, ring composition, ring enrichment
##   Spatial_analysis_report.html   self-contained report
## ===========================================================================

suppressPackageStartupMessages({
    library(fs)
})

this_file <- local({
    cargs <- commandArgs(FALSE)
    m <- grep("^--file=", cargs, value = TRUE)
    if (length(m)) sub("^--file=", "", m[1]) else NA_character_
})
script_dir <- if (!is.na(this_file)) fs::path_dir(fs::path_abs(this_file)) else getwd()

args <- commandArgs(trailingOnly = TRUE)

usage <- function(msg = NULL) {
    if (!is.null(msg)) cat("\n   ERROR: ", msg, "\n", sep = "")
    cat("\n")
    cat("   usage: spatial_data.R MANIFEST.csv [OUTDIR] --um-per-px=VALUE\n")
    cat("                         [--rules=FILE] [--cache=FILE] [--samples=A,B]\n")
    cat("                         [--radii=20,50] [--ring=20-50] [--anchor=TYPE]\n")
    cat("                         [--perms=N] [--seed=N] [--refresh]\n")
    cat("                         [--rows=N | --full]\n\n")
    cat("      --um-per-px=V  microns per pixel. REQUIRED -- Halo coordinates are\n")
    cat("                     pixels and carry no scan resolution\n")
    cat("      --samples=A,B  analyse only these samples (default: all)\n")
    cat("      --radii=20,50  co-location radii in um, measured from 0\n")
    cat("      --ring=20-50   neighbourhood ring in um, inner-outer\n")
    cat("      --anchor=TYPE  cell type to measure outward from (default: Tumor)\n")
    cat("      --perms=N      label permutations (default: 99)\n\n")
    quit(status = 1)
}

opt <- function(flag, default = NULL) {
    hit <- grep(paste0("^--", flag, "="), args, value = TRUE)
    if (length(hit) == 0) return(default)
    sub(paste0("^--", flag, "="), "", hit[1])
}

## --- the one argument with no default -------------------------------------
## A wrong scan resolution rescales every distance in the study without
## producing a single visible error, so it is refused rather than assumed.
um_per_px <- opt("um-per-px")
if (is.null(um_per_px)) {
    usage("--um-per-px= is required. Halo coordinates are in pixels and the file carries no scan resolution; pass the study's um/pixel.")
}
um_per_px <- suppressWarnings(as.numeric(um_per_px))
if (is.na(um_per_px) || um_per_px <= 0 || !is.finite(um_per_px)) {
    usage("--um-per-px= must be a positive number")
}

refresh <- "--refresh" %in% args

n_max <- 100
if ("--full" %in% args) n_max <- Inf
rows_arg <- opt("rows")
if (!is.null(rows_arg)) {
    n_max <- suppressWarnings(as.numeric(rows_arg))
    if (is.na(n_max) || n_max < 1) usage("--rows= must be a positive number")
}

rules_path <- opt("rules", fs::path("annotation", "cell_rules.yaml"))
anchor <- opt("anchor", "Tumor")
n_perm <- as.integer(opt("perms", "99"))
seed <- as.integer(opt("seed", "1"))

radii <- opt("radii", "20,50") |> strsplit(",") |> unlist() |> as.numeric()
if (any(is.na(radii)) || any(radii <= 0)) usage("--radii= must be positive numbers")

ring <- opt("ring", "20-50") |> strsplit("-") |> unlist() |> as.numeric()
if (length(ring) != 2 || any(is.na(ring)) || ring[1] >= ring[2] || ring[1] < 0) {
    usage("--ring= must be INNER-OUTER in microns, with INNER < OUTER")
}

keep_samples <- opt("samples")
if (!is.null(keep_samples)) {
    keep_samples <- trimws(unlist(strsplit(keep_samples, ",")))
    keep_samples <- keep_samples[nzchar(keep_samples)]
    if (length(keep_samples) == 0) usage("--samples= listed no sample")
}

pos <- args[!grepl("^--", args)]
if (length(pos) < 1) usage()
manifest_csv <- pos[1]
outdir <- if (length(pos) >= 2) pos[2] else "results/spatial"

## ---- locate and load the HaloXi package (same pattern as annotate_data.R) --
pkg_root <- if (!is.na(this_file)) {
    fs::path_norm(fs::path(script_dir, ".."))
} else {
    "HaloXi"
}

if (requireNamespace("HaloXi", quietly = TRUE)) {
    suppressPackageStartupMessages(library(HaloXi))
    message("spatial_data: using installed HaloXi")
} else {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("spatial_data: HaloXi not installed and pkgload unavailable to load from source")
    }
    message(glue::glue("spatial_data: loading HaloXi from source at {pkg_root}"))
    pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE)
}

## ---- load, annotate, subset ----------------------------------------------
fs::dir_create(outdir)
plots_dir <- fs::path(outdir, "plots")
fs::dir_create(plots_dir)
## Default to this stage's own cache, like the other drivers, but outside
## OUTDIR. --cache= points it at an existing one instead -- the loaded object is
## the same for every stage and is ~650 MB on a whole-section study, so a third
## copy of it is waste, and rebuilding one means re-reading a multi-gigabyte CSV.
cache_rds <- resolve_cache_path(outdir, opt("cache"))

rules <- read_cell_rules(rules_path)
message(glue::glue("spatial_data: rules <- {rules$rules_path}"))

manifest <- read_manifest(manifest_csv)
message("spatial_data: loading cells",
        if (is.finite(n_max)) sprintf(" (first %d rows/file)", n_max) else " (all rows)")
obj <- load_manifest(manifest, cache_rds = cache_rds, refresh = refresh, n_max = n_max)
obj <- annotate_cells(obj, rules)

## Sample selection is an instruction from the analyst, not something this code
## can derive, so it is applied here rather than guessed. Filtering after the
## load keeps one manifest as the single source of truth and leaves the shared
## cache -- which is keyed on the row count it was built from -- valid.
if (!is.null(keep_samples)) {
    have <- unique(as.character(obj$cell.data$Sample))
    missing <- setdiff(keep_samples, have)
    if (length(missing)) {
        stop("spatial_data: --samples= names sample(s) not in the data: ",
             paste(missing, collapse = ", "),
             ". Present: ", paste(have, collapse = ", "))
    }
    obj$cell.data <- obj$cell.data |> dplyr::filter(Sample %in% keep_samples)
    message(glue::glue(
        "spatial_data: restricted to {paste(keep_samples, collapse = ', ')} ",
        "({format(nrow(obj$cell.data), big.mark = ',')} cells)"))
}

## Two distinct cells can share a centroid after segmentation. The half-open
## distance band excludes them from every pair count, so say how many there are
## rather than letting them disappear quietly.
dup_centroids <- obj$cell.data |>
    dplyr::mutate(X = (XMin + XMax) / 2, Y = (YMin + YMax) / 2) |>
    dplyr::count(Sample, X, Y) |>
    dplyr::filter(n > 1) |>
    dplyr::summarize(cells = sum(n), .groups = "drop") |>
    dplyr::pull(cells)
dup_centroids <- if (length(dup_centroids)) dup_centroids else 0
if (dup_centroids > 0) {
    message(glue::glue(
        "spatial_data: {format(dup_centroids, big.mark = ',')} cells share a ",
        "centroid with another cell; pairs at distance 0 are outside the band ",
        "and are not counted"))
}

## ---- the two analyses -----------------------------------------------------
message(glue::glue("spatial_data: co-location at {paste(radii, collapse = ', ')} um, ",
                   "{n_perm} permutations"))
coloc <- purrr::map(radii, function(r) {
    colocation_test(obj, um_per_px = um_per_px, r_outer = r, r_inner = 0,
                    n_perm = n_perm, seed = seed)
}) |> purrr::list_rbind()

message(glue::glue("spatial_data: neighbourhood of '{anchor}', ",
                   "ring {ring[1]}-{ring[2]} um"))
neigh <- neighborhood_composition(obj, um_per_px = um_per_px, anchor_type = anchor,
                                  r_inner = ring[1], r_outer = ring[2],
                                  n_perm = n_perm, seed = seed)

## ---- outputs --------------------------------------------------------------
message("spatial_data: writing Excel workbook")
xlsx_path <- write_spatial_workbook(coloc, neigh, outdir)

per_anchor_path <- fs::path(outdir, "neighborhood_per_anchor.csv.gz")
readr::write_csv(neigh$per_anchor, per_anchor_path)
message(glue::glue("spatial_data: wrote {per_anchor_path}"))

message("spatial_data: writing plots")
ggsave2 <- function(name, plot, width, height) {
    if (is.null(plot)) return(NA_character_)
    p <- fs::path(plots_dir, name)
    ggplot2::ggsave(p, plot, width = width, height = height, dpi = 150, bg = "white")
    p
}
n_types <- dplyr::n_distinct(c(coloc$TypeA, coloc$TypeB))
n_samples <- dplyr::n_distinct(coloc$Sample)
n_bands <- length(radii)
plot_paths <- c(
    ggsave2("colocation_heatmap.png", plot_colocation_heatmap(coloc),
            max(7, 3 + 0.55 * n_types * n_samples),
            max(5, 2.5 + 0.55 * n_types * n_bands)),
    ggsave2("neighborhood_composition.png", plot_neighborhood_composition(neigh),
            max(7, 3 + 3 * n_samples), max(4.5, 0.45 * n_types + 2.5)),
    ggsave2("neighborhood_enrichment.png", plot_neighborhood_enrichment(neigh),
            max(7, 3 + 3 * n_samples), max(4.5, 0.45 * n_types + 2.5))
)
plot_paths <- plot_paths[!is.na(plot_paths)]

## ---- HTML report ----------------------------------------------------------
rmd_src <- fs::path(script_dir, "spatial_report.Rmd")

if (requireNamespace("rmarkdown", quietly = TRUE) && fs::file_exists(rmd_src)) {
    message("spatial_data: rendering HTML report")
    out_abs <- as.character(fs::path_abs(outdir))
    renv <- new.env(parent = globalenv())
    renv$sp_coloc <- coloc
    renv$sp_neigh <- neigh
    renv$sp_rules <- rules
    renv$sp_plots_dir <- as.character(fs::path_abs(plots_dir))
    renv$sp_n_max <- n_max
    renv$sp_um_per_px <- um_per_px
    renv$sp_radii <- radii
    renv$sp_samples <- sort(unique(as.character(obj$cell.data$Sample)))
    renv$sp_dup_centroids <- dup_centroids
    rmarkdown::render(
        input = rmd_src,
        output_file = "Spatial_analysis_report.html",
        output_dir = out_abs,
        envir = renv,
        quiet = TRUE
    )
    report_out <- fs::path(out_abs, "Spatial_analysis_report.html")
} else {
    report_out <- NA
    message("spatial_data: rmarkdown unavailable or template missing; skipping HTML report")
}

## ---- summary --------------------------------------------------------------
cat("\n========================================================\n")
cat("Spatial analysis complete\n")
cat(sprintf("  row cap (n_max)  : %s\n", if (is.finite(n_max)) format(n_max, big.mark = ",") else "all rows"))
cat(sprintf("  um per pixel     : %s\n", format(um_per_px)))
cat(sprintf("  samples          : %s\n", paste(sort(unique(as.character(obj$cell.data$Sample))), collapse = ", ")))
cat(sprintf("  cells analysed   : %s\n", format(nrow(obj$cell.data), big.mark = ",")))
cat(sprintf("  co-location radii: %s um\n", paste(radii, collapse = ", ")))
cat(sprintf("  ring             : %s-%s um around %s\n", ring[1], ring[2], anchor))
cat(sprintf("  permutations     : %d (smallest reportable p = %s)\n",
            n_perm, format(signif(2 / (n_perm + 1), 2))))
cat("  outputs:\n")
for (p in c(xlsx_path, per_anchor_path, plot_paths, report_out)) {
    if (!is.na(p)) cat(sprintf("    %s\n", p))
}
cat("========================================================\n\n")
