#' @import dplyr
#' @import tidyr
#' @import purrr

suppressPackageStartupMessages({
    require(dplyr)
    require(tidyr)
    require(purrr)
})

## ---------------------------------------------------------------------------
## Initial data scanner for a Halo sample manifest.
##
## A manifest is a CSV with (at least) two columns:
##   Sample    - short sample id used throughout the analysis
##   HaloFile  - path to the Halo object CSV for that sample
##
## HaloFile paths may be absolute or relative to the manifest's own
## directory. The scanner produces a cell-count-per-sample table, a
## sample x marker presence matrix, and per-marker positivity tables,
## together with the supporting plots. See `scan_manifest()` for the
## one-call entry point used by scripts/scan_data.R.
## ---------------------------------------------------------------------------

## Columns used to build a per-cell UUID. `load_halo()` requires these and
## they must exist (post fixColNames) in every Halo file. Image_Location plus
## the bounding box uniquely identifies a cell within and across samples.
.HALO_UUID_COLS <- c("Image_Location", "XMin", "XMax", "YMin", "YMax")

#' Resolve manifest HaloFile paths relative to the manifest directory
#'
#' @param paths Character vector of HaloFile entries from the manifest.
#' @param manifest_dir Directory the manifest CSV lives in.
#'
#' @return Character vector of resolved paths. Absolute paths are returned
#'   unchanged; relative paths are anchored to `manifest_dir`.
#'
#' @keywords internal
resolve_halo_paths <- function(paths, manifest_dir) {
    is_abs <- fs::is_absolute_path(paths)
    out <- ifelse(is_abs, paths, fs::path(manifest_dir, paths))
    fs::path_norm(out) |> as.character()
}

#' Read and validate a sample manifest
#'
#' @param manifest_csv Path to the manifest CSV. Must contain `Sample` and
#'   `HaloFile` columns.
#'
#' @return A tibble with columns `Sample`, `HaloFile` (resolved absolute
#'   path), and `Exists` (logical). Stops if required columns are missing or
#'   sample ids are duplicated.
#'
#' @export
read_manifest <- function(manifest_csv) {

    if (!fs::file_exists(manifest_csv)) {
        stop(glue::glue("read_manifest: manifest not found: {manifest_csv}"))
    }

    manifest <- readr::read_csv(manifest_csv, show_col_types = FALSE, progress = FALSE)

    needed <- c("Sample", "HaloFile")
    missing_cols <- setdiff(needed, names(manifest))
    if (length(missing_cols) > 0) {
        stop(glue::glue(
            "read_manifest: manifest missing column(s): {paste(missing_cols, collapse=', ')}\n",
            "  found: {paste(names(manifest), collapse=', ')}"
        ))
    }

    dup <- manifest$Sample[duplicated(manifest$Sample)]
    if (length(dup) > 0) {
        stop(glue::glue("read_manifest: duplicated Sample id(s): {paste(unique(dup), collapse=', ')}"))
    }

    manifest_dir <- fs::path_dir(fs::path_abs(manifest_csv))

    manifest |>
        mutate(
            Sample = as.character(Sample),
            HaloFile = resolve_halo_paths(HaloFile, manifest_dir),
            Exists = fs::file_exists(HaloFile)
        )
}

#' Scan Halo file headers for marker presence (no cell loading)
#'
#' Reads only the header line of each Halo CSV and extracts the marker panel
#' by applying the same column-name normalisation `load_halo()` uses, then
#' selecting `*_Positive_Classification` columns. Fast even for very large
#' files because no cell rows are read.
#'
#' @param manifest A validated manifest tibble from [read_manifest()].
#'
#' @return A long tibble with columns `Sample`, `Marker`, `MarkerNorm`,
#'   `Present` (always TRUE for rows that appear). Markers absent from a given
#'   sample simply do not appear; use [marker_presence_matrix()] to get the
#'   full rectangular matrix.
#'
#' @export
scan_halo_markers <- function(manifest) {

    fix_col_names <- function(ss) {
        ## Halo files vary in encoding (UTF-8 vs Latin-1 for the um/squared
        ## glyphs in area columns); sanitise to valid UTF-8 before regex work
        ## so gsub does not error on non-marker columns.
        ss <- enc2utf8(iconv(ss, from = "", to = "UTF-8", sub = "byte"))
        gsub(" ", "_", ss) |> gsub("_\\(.*\\)$", "", x = _)
    }

    scan_one <- function(Sample, HaloFile, Exists, ...) {
        if (!Exists) {
            return(tibble(Sample = Sample, Marker = character(), MarkerNorm = character()))
        }
        ## header only: read a single row, take its column names
        hdr <- readr::read_csv(
            HaloFile, n_max = 0, show_col_types = FALSE, progress = FALSE
        )
        cols <- fix_col_names(names(hdr))
        markers <- cols |>
            (\(x) x[grepl("_Positive_Classification$", x)])() |>
            (\(x) sub("_Positive_Classification$", "", x))()
        tibble(
            Sample = Sample,
            Marker = markers,
            MarkerNorm = toupper(markers)
        )
    }

    manifest |>
        purrr::pmap(scan_one) |>
        purrr::list_rbind() |>
        mutate(Present = TRUE)
}

#' Build a rectangular sample x marker presence matrix
#'
#' @param marker_scan Long tibble from [scan_halo_markers()], or the
#'   `marker.data` of a loaded object summarised to one row per
#'   Sample/MarkerNorm.
#'
#' @return A wide tibble: one row per `MarkerNorm`, one logical column per
#'   sample, `TRUE` where that marker's panel includes the marker. A trailing
#'   `nSamples` column counts how many samples carry each marker.
#'
#' @export
marker_presence_matrix <- function(marker_scan) {
    wide <- marker_scan |>
        distinct(Sample, MarkerNorm) |>
        mutate(Present = TRUE) |>
        pivot_wider(
            names_from = Sample,
            values_from = Present,
            values_fill = FALSE
        ) |>
        arrange(MarkerNorm)

    sample_cols <- setdiff(names(wide), "MarkerNorm")
    wide |>
        mutate(nSamples = rowSums(across(all_of(sample_cols)))) |>
        arrange(desc(nSamples), MarkerNorm)
}

#' Load every sample in a manifest into a single combined Halo object
#'
#' Calls [load_halo()] on each existing HaloFile and row-binds the per-sample
#' `cell.data` and `marker.data` tables. The combined object is cached to an
#' RDS file so repeated scans of the same manifest are fast.
#'
#' @param manifest A validated manifest tibble from [read_manifest()].
#' @param cache_rds Optional path to cache the combined object. If the file
#'   exists and `refresh` is FALSE it is reloaded instead of re-parsing the
#'   Halo CSVs.
#' @param refresh If TRUE, ignore any existing cache and reload from source.
#' @param controlMarkers Markers treated as controls (excluded from the
#'   `MarkerPos` cell phenotype). Passed through to [load_halo()].
#' @param n_max Max data rows to read per Halo file. The default of 100 gives a
#'   fast initial QC scan; set to `Inf` for a full load. Passed to [load_halo()].
#'
#' @return A list with `cell.data`, `marker.data` (both with a `Sample`
#'   column), `manifest`, `n_max`, and `VERSION`.
#'
#' @export
load_manifest <- function(manifest, cache_rds = NULL, refresh = FALSE,
                          controlMarkers = c("DAPI"), n_max = 100) {

    if (!is.null(cache_rds) && fs::file_exists(cache_rds) && !refresh) {
        cached <- readRDS(cache_rds)
        ## only reuse a cache built with the same row cap
        if (identical(cached$n_max, n_max)) {
            message(glue::glue("load_manifest: using cache {cache_rds} (n_max={n_max})"))
            return(cached)
        }
        message(glue::glue(
            "load_manifest: cache n_max={cached$n_max} != requested {n_max}; reloading"
        ))
    }

    usable <- manifest |> filter(Exists)
    if (nrow(usable) == 0) {
        stop("load_manifest: no existing HaloFiles in manifest")
    }
    if (any(!manifest$Exists)) {
        miss <- manifest |> filter(!Exists) |> pull(Sample)
        warning(glue::glue("load_manifest: skipping missing sample(s): {paste(miss, collapse=', ')}"))
    }

    load_one <- function(Sample, HaloFile, ...) {
        message(glue::glue("load_manifest: loading {Sample}  <-  {fs::path_file(HaloFile)} (n_max={n_max})"))
        load_halo(
            HaloFile,
            uuidCols = .HALO_UUID_COLS,
            sampleName = Sample,
            controlMarkers = controlMarkers,
            n_max = n_max
        )
    }

    objs <- usable |> purrr::pmap(load_one)
    names(objs) <- usable$Sample

    obj <- list(
        cell.data   = purrr::map(objs, "cell.data") |> purrr::list_rbind(),
        marker.data = purrr::map(objs, "marker.data") |> purrr::list_rbind(),
        manifest    = manifest,
        n_max       = n_max,
        VERSION     = VERSION
    )

    if (!is.null(cache_rds)) {
        fs::dir_create(fs::path_dir(cache_rds))
        saveRDS(obj, cache_rds)
        message(glue::glue("load_manifest: cached to {cache_rds}"))
    }

    obj
}

#' Per-sample cell-count summary
#'
#' @param obj A combined object from [load_manifest()].
#'
#' @return A tibble: `Sample`, `nCells`, `nPhenotyped` (cells positive for at
#'   least one non-control marker), `pctPhenotyped`, and `nUnphenotyped`
#'   (cells negative for every non-control marker). Ordered by `nCells`.
#'
#' @export
summarize_cells <- function(obj) {
    obj$cell.data |>
        group_by(Sample) |>
        summarize(
            nCells = n(),
            nPhenotyped = sum(MarkerPos != "" & !is.na(MarkerPos)),
            .groups = "drop"
        ) |>
        mutate(
            nUnphenotyped = nCells - nPhenotyped,
            pctPhenotyped = round(100 * nPhenotyped / nCells, 1)
        ) |>
        select(Sample, nCells, nPhenotyped, pctPhenotyped, nUnphenotyped) |>
        arrange(desc(nCells))
}

#' Per-sample, per-marker positivity summary
#'
#' @param obj A combined object from [load_manifest()].
#' @param controlMarkers Marker names (case-insensitive) to flag as controls.
#'
#' @return A long tibble: `Sample`, `Marker`, `MarkerNorm`, `IsControl`,
#'   `nCells` (cells scored for that marker in that sample), `nPos`
#'   (positive cells), `pctPos`. Markers not in a sample's panel are absent.
#'
#' @export
summarize_markers <- function(obj, controlMarkers = c("DAPI")) {
    control_norm <- toupper(controlMarkers)
    obj$marker.data |>
        ## marker.data has one row per cell x marker; attach Sample via cell.data
        left_join(obj$cell.data |> select(UUID, Sample), by = "UUID") |>
        group_by(Sample, Marker, MarkerNorm) |>
        summarize(
            nCells = n(),
            nPos = sum(Positive == 1, na.rm = TRUE),
            .groups = "drop"
        ) |>
        mutate(
            pctPos = round(100 * nPos / nCells, 2),
            IsControl = MarkerNorm %in% control_norm
        ) |>
        arrange(Sample, desc(pctPos))
}

#' Wide sample x marker matrix of positive-cell percentages
#'
#' @param marker_summary Long tibble from [summarize_markers()].
#' @param value One of "pctPos" or "nPos" - the cell value to spread.
#'
#' @return A wide tibble: one row per marker, one column per sample. Cells are
#'   `NA` where a marker is absent from that sample's panel, distinguishing it
#'   from a marker present but never positive (0).
#'
#' @export
marker_positivity_matrix <- function(marker_summary, value = c("pctPos", "nPos")) {
    value <- match.arg(value)
    marker_summary |>
        select(MarkerNorm, IsControl, Sample, all_of(value)) |>
        pivot_wider(names_from = Sample, values_from = all_of(value)) |>
        arrange(IsControl, MarkerNorm)
}

## ---------------------------------------------------------------------------
## Plot builders. Each returns a ggplot object so the driver can size/save.
## ---------------------------------------------------------------------------

#' Barplot of cell counts per sample
#' @param cell_summary Tibble from [summarize_cells()].
#' @return A ggplot object.
#' @export
plot_cell_counts <- function(cell_summary) {
    pd <- cell_summary |>
        select(Sample, Phenotyped = nPhenotyped, Unphenotyped = nUnphenotyped) |>
        pivot_longer(-Sample, names_to = "Class", values_to = "nCells") |>
        mutate(
            Sample = forcats::fct_reorder(Sample, nCells, .fun = sum),
            Class = factor(Class, levels = c("Unphenotyped", "Phenotyped"))
        )

    ggplot2::ggplot(pd, ggplot2::aes(Sample, nCells, fill = Class)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(
            data = cell_summary |> mutate(Sample = factor(Sample, levels = levels(pd$Sample))),
            ggplot2::aes(Sample, nCells, label = scales::comma(nCells)),
            inherit.aes = FALSE, hjust = -0.1, size = 3
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::scale_fill_manual(values = c(Unphenotyped = "grey75", Phenotyped = "#2c7fb8")) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = "Cells per sample",
            subtitle = "Phenotyped = positive for >=1 non-control marker",
            x = NULL, y = "Number of cells", fill = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "top")
}

#' Heatmap of per-marker positive-cell percentage across samples
#' @param marker_summary Long tibble from [summarize_markers()].
#' @return A ggplot object.
#' @export
plot_marker_heatmap <- function(marker_summary) {
    ## build the (character) display label first, then order the factor by mean
    ## positivity; ifelse() on an already-factor column would return level codes.
    pd <- marker_summary |>
        mutate(
            Label = ifelse(IsControl,
                           paste0(as.character(MarkerNorm), " *"),
                           as.character(MarkerNorm)),
            Label = forcats::fct_reorder(Label, pctPos, .fun = mean)
        )

    ggplot2::ggplot(pd, ggplot2::aes(Sample, Label, fill = pctPos)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.4) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", pctPos)), size = 2.8) +
        ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08519c", limits = c(0, NA)) +
        ggplot2::labs(
            title = "Marker positivity (% of scored cells positive)",
            subtitle = "Blank tile = marker absent from that sample's panel; * = control",
            x = NULL, y = NULL, fill = "% positive"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
            panel.grid = ggplot2::element_blank()
        )
}

#' Tile plot of which markers each sample's panel contains
#' @param presence_matrix Wide tibble from [marker_presence_matrix()].
#' @return A ggplot object.
#' @export
plot_marker_presence <- function(presence_matrix) {
    sample_cols <- setdiff(names(presence_matrix), c("MarkerNorm", "nSamples"))
    pd <- presence_matrix |>
        select(MarkerNorm, all_of(sample_cols)) |>
        pivot_longer(-MarkerNorm, names_to = "Sample", values_to = "Present") |>
        mutate(MarkerNorm = forcats::fct_rev(forcats::fct_inorder(MarkerNorm)))

    ggplot2::ggplot(pd, ggplot2::aes(Sample, MarkerNorm, fill = Present)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.4) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = "#31a354", `FALSE` = "grey90"),
                                   labels = c(`TRUE` = "present", `FALSE` = "absent")) +
        ggplot2::labs(
            title = "Marker panel coverage (sample x marker)",
            x = NULL, y = NULL, fill = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Spatial footprint of cell centroids, faceted by sample
#'
#' @param obj A combined object from [load_manifest()].
#' @param max_points_per_sample Down-sample each sample to at most this many
#'   points so the plot stays light for very large samples.
#' @param seed RNG seed for reproducible down-sampling.
#'
#' @return A ggplot object.
#' @export
plot_spatial_footprint <- function(obj, max_points_per_sample = 30000, seed = 101) {
    set.seed(seed)
    pd <- obj$cell.data |>
        mutate(X = (XMin + XMax) / 2, Y = (YMin + YMax) / 2) |>
        group_by(Sample) |>
        slice_sample(n = max_points_per_sample) |>
        ungroup() |>
        mutate(Phenotyped = MarkerPos != "" & !is.na(MarkerPos))

    n_shown <- nrow(pd)
    subtitle <- if (n_shown < max_points_per_sample) {
        "all scanned cells shown"
    } else {
        glue::glue("down-sampled to <= {scales::comma(max_points_per_sample)} cells/sample")
    }

    ## aspect.ratio (not coord_fixed) keeps tiles square while allowing the
    ## free per-sample scales facet_wrap needs in this ggplot2 version.
    ggplot2::ggplot(pd, ggplot2::aes(X, Y, color = Phenotyped)) +
        ggplot2::geom_point(size = 0.3, alpha = 0.5) +
        ggplot2::scale_color_manual(values = c(`TRUE` = "#2c7fb8", `FALSE` = "grey70")) +
        ggplot2::scale_y_reverse() +
        ggplot2::facet_wrap(~Sample, scales = "free") +
        ggplot2::labs(
            title = "Cell centroid spatial footprint",
            subtitle = subtitle,
            x = "X (px)", y = "Y (px)", color = "Phenotyped"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            legend.position = "top",
            aspect.ratio = 1,
            axis.text = ggplot2::element_text(size = 6)
        ) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2, alpha = 1)))
}

## ---------------------------------------------------------------------------
## Top-level orchestrator
## ---------------------------------------------------------------------------

#' Run the initial QC scan of a Halo sample manifest
#'
#' Convenience wrapper that reads the manifest, scans marker panels from
#' headers, loads samples via [load_halo()] (cached), and assembles every
#' summary table and plot. The CLI driver (`scripts/scan_data.R`) calls
#' this and then writes the Excel workbooks, PNGs and HTML report.
#'
#' By default only the first `n_max` rows of each Halo file are read, giving a
#' fast initial QC pass. Marker-panel coverage (presence) is always derived
#' from the full headers and so is unaffected by `n_max`; only the cell counts
#' and positivity percentages reflect the sampled rows. Set `n_max = Inf` for a
#' complete load.
#'
#' @param manifest_csv Path to the manifest CSV.
#' @param cache_rds Optional RDS cache path for the combined loaded object.
#' @param refresh If TRUE, ignore an existing cache.
#' @param controlMarkers Markers treated as controls.
#' @param n_max Max data rows to read per Halo file (default 100 for fast QC;
#'   `Inf` for a full load).
#' @param max_points_per_sample Down-sample cap for the spatial footprint plot.
#'
#' @return A list with `tables` (named list of summary tibbles), `plots`
#'   (named list of ggplot objects), `obj` (the combined loaded object), and
#'   `meta` (scan metadata: timestamp, version, counts, n_max).
#'
#' @export
scan_manifest <- function(manifest_csv, cache_rds = NULL, refresh = FALSE,
                          controlMarkers = c("DAPI"),
                          n_max = 100,
                          max_points_per_sample = 30000) {

    manifest <- read_manifest(manifest_csv)

    ## fast header-only panel scan (always full panel, independent of n_max)
    marker_scan <- scan_halo_markers(manifest)
    presence_matrix <- marker_presence_matrix(marker_scan)

    ## load cells (capped at n_max rows per file for QC; cached)
    obj <- load_manifest(manifest, cache_rds = cache_rds, refresh = refresh,
                         controlMarkers = controlMarkers, n_max = n_max)

    cell_summary <- summarize_cells(obj)
    marker_summary <- summarize_markers(obj, controlMarkers = controlMarkers)
    positivity_pct <- marker_positivity_matrix(marker_summary, "pctPos")
    positivity_n <- marker_positivity_matrix(marker_summary, "nPos")

    tables <- list(
        manifest        = manifest,
        marker_scan     = marker_scan,
        presence_matrix = presence_matrix,
        cell_summary    = cell_summary,
        marker_summary  = marker_summary,
        positivity_pct  = positivity_pct,
        positivity_n    = positivity_n
    )

    plots <- list(
        cell_counts      = plot_cell_counts(cell_summary),
        marker_presence  = plot_marker_presence(presence_matrix),
        marker_heatmap   = plot_marker_heatmap(marker_summary),
        spatial_footprint = plot_spatial_footprint(obj, max_points_per_sample)
    )

    meta <- list(
        manifest_csv = fs::path_abs(manifest_csv),
        scanned_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        version      = VERSION,
        n_max        = n_max,
        n_samples    = nrow(manifest),
        n_loaded     = dplyr::n_distinct(obj$cell.data$Sample),
        n_markers    = dplyr::n_distinct(marker_scan$MarkerNorm),
        total_cells  = nrow(obj$cell.data)
    )

    list(tables = tables, plots = plots, obj = obj, meta = meta)
}
