#!/usr/bin/env Rscript

## ===========================================================================
## scan_data.R - initial data scanner for a Halo sample manifest
##
##   usage: scan_data.R MANIFEST.csv [OUTDIR] [--cache=FILE] [--refresh]
##                      [--rows=N | --full]
##
##     MANIFEST.csv  CSV with columns Sample, HaloFile
##     OUTDIR        output directory (default: results/scan)
##     --cache=FILE  loaded-object cache to build or reuse
##                   (default: cache/<name of OUTDIR>/scan_obj.rds)
##     --refresh     ignore any cached loaded object and reload from source
##     --rows=N      read only the first N data rows per file (default 100,
##                   for a fast initial QC scan)
##     --full        read all rows (equivalent to --rows=Inf; slow on big files)
##
## Marker-panel coverage is always read from the full file headers, so the
## sample x marker presence table is complete regardless of the row cap; only
## cell counts and positivity reflect the sampled rows.
##
## Produces, under OUTDIR:
##   Halo_scan_overview.xlsx   manifest, cell counts, marker panel presence
##   Halo_scan_markers.xlsx    per-marker positivity (% and counts)
##   plots/*.png               cell counts, panel coverage, positivity heatmap,
##                             and one spatial footprint per sample
##                             (spatial_<Sample>.png)
##   Halo_scan_report.html     self-contained report tying it together
##
## The loaded-object cache is NOT written under OUTDIR -- OUTDIR is a
## deliverable and the cache is a build artifact of several hundred MB. It goes
## to cache/<name of OUTDIR>/scan_obj.rds instead; --cache= overrides that. A
## cache left where older versions put it, under OUTDIR, is named and ignored.
##
## The scanner uses the HaloXi package functions (load_halo, scan_manifest,
## ...). If HaloXi is not installed it is loaded from source with pkgload.
## ===========================================================================

suppressPackageStartupMessages({
    library(fs)
})

args <- commandArgs(trailingOnly = TRUE)

usage <- function() {
    cat("\n")
    cat("   usage: scan_data.R MANIFEST.csv [OUTDIR] [--cache=FILE] [--refresh] [--rows=N | --full]\n\n")
    cat("      MANIFEST.csv  CSV with columns Sample, HaloFile\n")
    cat("      OUTDIR        output directory (default: results/scan)\n")
    cat("      --cache=FILE  loaded-object cache (default: cache/<OUTDIR name>/scan_obj.rds)\n")
    cat("      --refresh     reload from source, ignoring cache\n")
    cat("      --rows=N      first N data rows per file (default 100, fast QC)\n")
    cat("      --full        read all rows (slow on big files)\n\n")
    quit(status = 1)
}

refresh <- "--refresh" %in% args

cache_arg <- grep("^--cache=", args, value = TRUE)
cache_arg <- if (length(cache_arg)) sub("^--cache=", "", cache_arg[1]) else NULL

## row cap: --full -> Inf, --rows=N -> N, otherwise default 100 (fast QC)
n_max <- 100
if ("--full" %in% args) {
    n_max <- Inf
}
rows_arg <- grep("^--rows=", args, value = TRUE)
if (length(rows_arg)) {
    n_max <- as.numeric(sub("^--rows=", "", rows_arg[1]))
    if (is.na(n_max) || n_max < 1) stop("scan_data: --rows= must be a positive number")
}

args <- args[!grepl("^--", args)]
if (length(args) < 1) usage()

manifest_csv <- args[1]
outdir <- if (length(args) >= 2) args[2] else "results/scan"

## ---- locate and load the HaloXi package ----------------------------------
## This script lives at <pkg>/scripts/scan_data.R, so the package root is one
## level up. Prefer an installed HaloXi, fall back to source via pkgload.
this_file <- local({
    cargs <- commandArgs(FALSE)
    m <- grep("^--file=", cargs, value = TRUE)
    if (length(m)) sub("^--file=", "", m[1]) else NA_character_
})
pkg_root <- if (!is.na(this_file)) {
    fs::path_norm(fs::path(fs::path_dir(this_file), ".."))
} else {
    "HaloXi"
}

if (requireNamespace("HaloXi", quietly = TRUE)) {
    suppressPackageStartupMessages(library(HaloXi))
    message("scan_data: using installed HaloXi")
} else {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("scan_data: HaloXi not installed and pkgload unavailable to load from source")
    }
    message(glue::glue("scan_data: loading HaloXi from source at {pkg_root}"))
    pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE)
}

## ---- run the scan ---------------------------------------------------------
fs::dir_create(outdir)
plots_dir <- fs::path(outdir, "plots")
fs::dir_create(plots_dir)
cache_rds <- resolve_cache_path(outdir, cache_arg)

message("scan_data: scanning manifest ", manifest_csv,
        if (is.finite(n_max)) sprintf(" (first %d rows/file)", n_max) else " (all rows)")
res <- scan_manifest(manifest_csv, cache_rds = cache_rds, refresh = refresh, n_max = n_max)

## ---- Excel workbooks ------------------------------------------------------
message("scan_data: writing Excel workbooks")
xlsx_paths <- write_scan_workbooks(res$tables, outdir)

## ---- PNG plots ------------------------------------------------------------
message("scan_data: writing plots")
ggsave2 <- function(name, plot, width, height) {
    p <- fs::path(plots_dir, name)
    ggplot2::ggsave(p, plot, width = width, height = height, dpi = 150, bg = "white")
    p
}
n_samp <- res$meta$n_loaded
n_mark <- res$meta$n_markers
plot_paths <- c(
    ggsave2("cell_counts.png",       res$plots$cell_counts,       7, max(3, 0.6 * n_samp + 1.5)),
    ggsave2("marker_presence.png",   res$plots$marker_presence,   max(5, 1.2 * n_samp + 2), max(5, 0.28 * n_mark + 1.5)),
    ggsave2("marker_heatmap.png",    res$plots$marker_heatmap,    max(6, 1.4 * n_samp + 2.5), max(5, 0.30 * n_mark + 1.5))
)

## one standalone spatial footprint PNG per sample. Sanitise the sample id for
## a safe filename; coord_fixed makes these square, so use a square canvas.
spatial_paths <- purrr::imap_chr(res$plots$spatial_footprints, function(p, sid) {
    safe <- gsub("[^A-Za-z0-9._-]+", "_", sid)
    ggsave2(sprintf("spatial_%s.png", safe), p, 7, 7)
})
plot_paths <- c(plot_paths, spatial_paths)

## ---- HTML report ----------------------------------------------------------
rmd_src <- if (!is.na(this_file)) {
    fs::path(fs::path_dir(this_file), "scan_report.Rmd")
} else {
    fs::path(pkg_root, "scripts", "scan_report.Rmd")
}

if (requireNamespace("rmarkdown", quietly = TRUE) && fs::file_exists(rmd_src)) {
    message("scan_data: rendering HTML report")
    ## Pass data via a render environment rather than YAML params: render
    ## rewrites relative-looking path params against the knit working dir,
    ## which mangles plots_dir. Absolute paths placed in `renv` are immune.
    out_abs <- as.character(fs::path_abs(outdir))
    renv <- new.env(parent = globalenv())
    renv$scan_res <- res
    renv$scan_plots_dir <- as.character(fs::path_abs(plots_dir))
    rmarkdown::render(
        input = rmd_src,
        output_file = "Halo_scan_report.html",
        output_dir = out_abs,
        envir = renv,
        quiet = TRUE
    )
    report_out <- fs::path(out_abs, "Halo_scan_report.html")
} else {
    report_out <- NA
    message("scan_data: rmarkdown unavailable or template missing; skipping HTML report")
}

## ---- summary --------------------------------------------------------------
cat("\n========================================================\n")
cat("Halo scan complete\n")
cat(sprintf("  row cap (n_max)     : %s\n", if (is.finite(res$meta$n_max)) format(res$meta$n_max, big.mark = ",") else "all rows"))
cat(sprintf("  samples in manifest : %d\n", res$meta$n_samples))
cat(sprintf("  samples loaded      : %d\n", res$meta$n_loaded))
cat(sprintf("  distinct markers    : %d\n", res$meta$n_markers))
cat(sprintf("  cells scanned       : %s\n", format(res$meta$total_cells, big.mark = ",")))
cat("  outputs:\n")
for (p in c(xlsx_paths, plot_paths, report_out)) {
    if (!is.na(p)) cat(sprintf("    %s\n", p))
}
cat("========================================================\n\n")
