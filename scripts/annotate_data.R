#!/usr/bin/env Rscript

## ===========================================================================
## annotate_data.R - assign cell types and states to a Halo sample manifest
##
## This driver is study-AGNOSTIC: all cell-type and marker definitions are read
## at run time from the rules YAML passed via --rules. The rules file itself is
## the project artifact (it encodes one study's biology); this script, like
## scan_data.R, just orchestrates the HaloXi engine over whatever rules and
## manifest it is given.
##
##   usage: annotate_data.R MANIFEST.csv [OUTDIR] [--rules=FILE]
##                          [--refresh] [--rows=N | --full]
##
##     MANIFEST.csv  CSV with columns Sample, HaloFile
##     OUTDIR        output directory (default: results/annot)
##     --rules=FILE  cell-annotation rules YAML. Default: annotation/cell_rules.yaml
##                   relative to the current directory (the project convention).
##     --refresh     ignore any cached loaded object and reload from source
##     --rows=N      read only the first N data rows per file (default 100,
##                   fast QC); --full reads all rows
##
## Reuses the same loaded-object cache as scan_data.R (cache keyed on n_max), so
## an existing scan cache is reused. Produces, under OUTDIR:
##   cell_rules.md            human-readable rules the calls were made from
##   Cell_annotation.xlsx     composition + state breakdown tables
##   plots/*.png              composition bars, state heatmap, per-parent states
##   Cell_annotation_report.html  self-contained report
##   cache/scan_obj.rds       cached combined loaded object (shared with scanner)
##
## The script uses the HaloXi package functions. If HaloXi is not installed it is
## loaded from source with pkgload (package root is one level up from scripts/).
## ===========================================================================

suppressPackageStartupMessages({
    library(fs)
})

## this script lives at <pkg>/scripts/annotate_data.R; this_file locates it so
## we can find the package root (one level up) and the sibling report template.
this_file <- local({
    cargs <- commandArgs(FALSE)
    m <- grep("^--file=", cargs, value = TRUE)
    if (length(m)) sub("^--file=", "", m[1]) else NA_character_
})
script_dir <- if (!is.na(this_file)) fs::path_dir(fs::path_abs(this_file)) else getwd()

args <- commandArgs(trailingOnly = TRUE)

usage <- function() {
    cat("\n")
    cat("   usage: annotate_data.R MANIFEST.csv [OUTDIR] [--rules=FILE] [--refresh] [--rows=N | --full]\n\n")
    cat("      MANIFEST.csv  CSV with columns Sample, HaloFile\n")
    cat("      OUTDIR        output directory (default: results/annot)\n")
    cat("      --rules=FILE  rules YAML (default: annotation/cell_rules.yaml)\n")
    cat("      --refresh     reload from source, ignoring cache\n")
    cat("      --rows=N      first N data rows per file (default 100, fast QC)\n")
    cat("      --full        read all rows (slow on big files)\n\n")
    quit(status = 1)
}

refresh <- "--refresh" %in% args

n_max <- 100
if ("--full" %in% args) n_max <- Inf
rows_arg <- grep("^--rows=", args, value = TRUE)
if (length(rows_arg)) {
    n_max <- as.numeric(sub("^--rows=", "", rows_arg[1]))
    if (is.na(n_max) || n_max < 1) stop("annotate_data: --rows= must be a positive number")
}

## --rules default is the project convention annotation/cell_rules.yaml,
## relative to the current directory (NOT to this script: the package ships no
## rules of its own). Override with --rules=PATH; read_cell_rules() errors
## clearly if the file is absent.
rules_arg <- grep("^--rules=", args, value = TRUE)
rules_path <- if (length(rules_arg)) {
    sub("^--rules=", "", rules_arg[1])
} else {
    fs::path("annotation", "cell_rules.yaml")
}

args <- args[!grepl("^--", args)]
if (length(args) < 1) usage()

manifest_csv <- args[1]
outdir <- if (length(args) >= 2) args[2] else "results/annot"

## ---- locate and load the HaloXi package (same pattern as scan_data.R) ------
## This script lives at <pkg>/scripts/annotate_data.R, so the package root is
## one level up. Prefer an installed HaloXi, fall back to source via pkgload.
pkg_root <- if (!is.na(this_file)) {
    fs::path_norm(fs::path(script_dir, ".."))
} else {
    "HaloXi"
}

if (requireNamespace("HaloXi", quietly = TRUE)) {
    suppressPackageStartupMessages(library(HaloXi))
    message("annotate_data: using installed HaloXi")
} else {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("annotate_data: HaloXi not installed and pkgload unavailable to load from source")
    }
    message(glue::glue("annotate_data: loading HaloXi from source at {pkg_root}"))
    pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE)
}

## ---- load rules, load cells (shared cache), annotate ----------------------
fs::dir_create(outdir)
plots_dir <- fs::path(outdir, "plots")
fs::dir_create(plots_dir)
cache_rds <- fs::path(outdir, "cache", "scan_obj.rds")

rules <- read_cell_rules(rules_path)
message(glue::glue("annotate_data: rules <- {rules$rules_path}"))

manifest <- read_manifest(manifest_csv)
message("annotate_data: loading cells",
        if (is.finite(n_max)) sprintf(" (first %d rows/file)", n_max) else " (all rows)")
obj <- load_manifest(manifest, cache_rds = cache_rds, refresh = refresh, n_max = n_max)
obj <- annotate_cells(obj, rules)

ct_summary <- summarize_celltypes(obj)
state_summary <- summarize_states(obj)

## ---- human-readable rules document (always written) -----------------------
rules_md <- fs::path(outdir, "cell_rules.md")
render_rules_md(rules, rules_md)
message(glue::glue("annotate_data: wrote rules spec {rules_md}"))

## ---- Excel workbook -------------------------------------------------------
message("annotate_data: writing Excel workbook")
xlsx_path <- write_annotation_workbook(ct_summary, state_summary, outdir)

## ---- PNG plots ------------------------------------------------------------
message("annotate_data: writing plots")
ggsave2 <- function(name, plot, width, height) {
    if (is.null(plot)) return(NA_character_)
    p <- fs::path(plots_dir, name)
    ggplot2::ggsave(p, plot, width = width, height = height, dpi = 150, bg = "white")
    p
}
n_types <- dplyr::n_distinct(ct_summary$long$CellType)
n_states <- dplyr::n_distinct(state_summary$State)
plot_paths <- c(
    ggsave2("composition_counts.png",  plot_celltype_composition(ct_summary, percent = FALSE, rules = rules), 8, max(3, 0.5 * n_types + 2)),
    ggsave2("composition_pct.png",     plot_celltype_composition(ct_summary, percent = TRUE,  rules = rules), 8, max(3, 0.5 * n_types + 2)),
    ggsave2("state_heatmap.png",       plot_state_heatmap(state_summary),    max(5, 1.2 * dplyr::n_distinct(state_summary$Sample) + 2), max(5, 0.32 * n_states + 1.5))
)

## one state barplot per parent lineage the rules define states for, named by
## the same tag that names the state columns
tags <- state_tags(rules)
for (parent in names(tags)) {
    plot_paths <- c(plot_paths, ggsave2(
        glue::glue("states_{tags[[parent]]}.png"),
        plot_parent_states(state_summary, tags[[parent]], parent_label = parent),
        7, 4
    ))
}
plot_paths <- plot_paths[!is.na(plot_paths)]

## ---- HTML report ----------------------------------------------------------
## the report template is a sibling of this script in the project annotation/
rmd_src <- fs::path(script_dir, "annotate_report.Rmd")

if (requireNamespace("rmarkdown", quietly = TRUE) && fs::file_exists(rmd_src)) {
    message("annotate_data: rendering HTML report")
    ## data via render envir, not YAML params (same path-mangling fix as scanner)
    out_abs <- as.character(fs::path_abs(outdir))
    renv <- new.env(parent = globalenv())
    renv$ann_obj <- obj
    renv$ann_rules <- rules
    renv$ann_ct <- ct_summary
    renv$ann_states <- state_summary
    renv$ann_plots_dir <- as.character(fs::path_abs(plots_dir))
    renv$ann_n_max <- n_max
    rmarkdown::render(
        input = rmd_src,
        output_file = "Cell_annotation_report.html",
        output_dir = out_abs,
        envir = renv,
        quiet = TRUE
    )
    report_out <- fs::path(out_abs, "Cell_annotation_report.html")
} else {
    report_out <- NA
    message("annotate_data: rmarkdown unavailable or template missing; skipping HTML report")
}

## ---- summary --------------------------------------------------------------
cat("\n========================================================\n")
cat("Cell annotation complete\n")
cat(sprintf("  row cap (n_max)  : %s\n", if (is.finite(n_max)) format(n_max, big.mark = ",") else "all rows"))
cat(sprintf("  samples          : %d\n", dplyr::n_distinct(obj$cell.data$Sample)))
cat(sprintf("  cells annotated  : %s\n", format(nrow(obj$cell.data), big.mark = ",")))
cat("  outputs:\n")
for (p in c(rules_md, xlsx_path, plot_paths, report_out)) {
    if (!is.na(p)) cat(sprintf("    %s\n", p))
}
cat("========================================================\n\n")
