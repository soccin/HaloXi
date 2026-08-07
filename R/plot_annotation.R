## ---------------------------------------------------------------------------
## Plot builders for cell-annotation results. Each returns a ggplot object (no
## I/O), styled to match the QC scanner plots in scan_manifest.R
## (theme_minimal, coord_flip bars, the #2c7fb8 / #08519c blues).
## ---------------------------------------------------------------------------

## a stable-ish palette for cell types: real lineages get colour, the
## not-a-clean-call buckets get neutral greys so they read as "set aside".
.celltype_palette <- function(levels) {
    lineage_cols <- c(
        "Tumor"         = "#e6550d",
        "T cell"        = "#2c7fb8",
        "B cell"        = "#41ab5d",
        "NK cell"       = "#807dba",
        "Macrophage"    = "#d6616b",
        "Endothelial"   = "#fec44f",
        "Myofibroblast" = "#8c6d31"
    )
    neutral <- c(
        "UNKNOWN"            = "grey55",
        "UNCLASSIFIED"       = "grey80",
        "(NA / un-callable)" = "grey90"
    )
    pal <- c(lineage_cols, neutral)
    ## any level not covered (e.g. renamed) falls back to a mid grey
    missing <- setdiff(levels, names(pal))
    if (length(missing)) pal <- c(pal, stats::setNames(rep("grey65", length(missing)), missing))
    pal[levels]
}

#' Stacked bar of cell-type composition per sample
#'
#' @param ct_summary The list returned by [summarize_celltypes()] (uses its
#'   `long` element), or that long tibble directly.
#' @param percent If TRUE, bars are scaled to 100% within each sample; if FALSE,
#'   absolute cell counts.
#'
#' @return A ggplot object.
#' @export
plot_celltype_composition <- function(ct_summary, percent = FALSE) {

    long <- if (is.list(ct_summary) && !is.data.frame(ct_summary)) ct_summary$long else ct_summary

    ## order cell types: lineages first (by total), then the set-aside buckets
    buckets <- c("UNKNOWN", "UNCLASSIFIED", "(NA / un-callable)")
    totals <- long |>
        group_by(CellType) |>
        summarize(tot = sum(nCells), .groups = "drop")
    lineage_order <- totals |>
        filter(!CellType %in% buckets) |>
        arrange(tot) |>
        pull(CellType) |>
        as.character()
    level_order <- c(intersect(buckets, as.character(totals$CellType)), lineage_order)

    pd <- long |>
        mutate(
            CellType = factor(as.character(CellType), levels = level_order),
            value = if (percent) pct else nCells
        )

    pal <- .celltype_palette(level_order)

    y_lab <- if (percent) "% of cells" else "Number of cells"
    ttl <- "Cell-type composition per sample"
    sub <- "UNKNOWN = multi-lineage conflict; UNCLASSIFIED = no lineage marker; first-pass (no priority)"

    ggplot2::ggplot(pd, ggplot2::aes(Sample, value, fill = CellType)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(values = pal, breaks = rev(level_order)) +
        { if (percent)
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)))
          else
            ggplot2::scale_y_continuous(labels = scales::comma,
                                        expand = ggplot2::expansion(mult = c(0, 0.05))) } +
        ggplot2::coord_flip() +
        ggplot2::labs(title = ttl, subtitle = sub, x = NULL, y = y_lab, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "right")
}

#' Heatmap of sub-state positivity (% of parent-type cells) across samples
#'
#' Shows, per state and sample, the percentage of the relevant parent-type cells
#' that carry the state flag. Blank tiles mean the state was un-callable in that
#' sample (e.g. the marker was absent, or the sample had none of that parent
#' type) -- distinct from a measured 0%.
#'
#' @param state_summary Long tibble from [summarize_states()].
#'
#' @return A ggplot object.
#' @export
plot_state_heatmap <- function(state_summary) {

    pd <- state_summary |>
        mutate(State = forcats::fct_rev(forcats::fct_inorder(State)))

    ggplot2::ggplot(pd, ggplot2::aes(Sample, State, fill = pctPos)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.4) +
        ggplot2::geom_text(
            ggplot2::aes(label = ifelse(is.na(pctPos), "-", sprintf("%.0f", pctPos))),
            size = 2.8
        ) +
        ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08519c",
                                     limits = c(0, NA), na.value = "grey92") +
        ggplot2::labs(
            title = "State positivity (% of the parent-type cells)",
            subtitle = "Blank/\"-\" = un-callable (marker absent, or the sample had no cells of that parent type)",
            x = NULL, y = NULL, fill = "% positive"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Barplot of one parent lineage's state flags
#'
#' Focused view of a single parent type: of that type's cells in each sample,
#' how many carry each of its state flags (independent flags, not a partition).
#'
#' @param state_summary Long tibble from [summarize_states()].
#' @param tag The parent's state-column tag, as returned by [state_tags()].
#'   Rows whose `State` begins `<tag>_` are shown, matched as a fixed prefix so
#'   a tag can never pick up a longer tag's rows.
#' @param parent_label Display name for the parent in the title and axis
#'   (defaults to `tag`).
#'
#' @return A ggplot object, or `NULL` if no state rows carry that tag.
#' @export
plot_parent_states <- function(state_summary, tag, parent_label = tag) {

    prefix <- paste0(tag, "_")
    pd <- state_summary |> filter(startsWith(State, prefix))
    if (nrow(pd) == 0 || all(is.na(pd$pctPos))) return(NULL)

    pd <- pd |>
        mutate(State = substring(State, nchar(prefix) + 1L),
               State = sub("_pos$", "", State))

    ggplot2::ggplot(pd, ggplot2::aes(State, pctPos, fill = Sample)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
        ggplot2::scale_y_continuous(limits = c(0, NA),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(
            title = glue::glue("{parent_label} states (% of {parent_label} cells positive)"),
            subtitle = glue::glue(
                "Independent flags; blank where no {parent_label} cells were called"),
            x = NULL, y = glue::glue("% of {parent_label} cells"), fill = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "top")
}
