## ---------------------------------------------------------------------------
## Plot builders for the spatial stage. Each returns a ggplot and does no I/O,
## matching the scan and annotation builders.
##
## Every one of these leads with a ratio -- log2FC, log2Enrich, a percentage --
## rather than with a raw count. Counts near the tissue border are biased low by
## truncated neighbourhoods; the ratios are not, because the permutation null
## carries the same truncation. Plotting counts here would put the least
## trustworthy number in front of the reader.
## ---------------------------------------------------------------------------

## diverging fill for a log2 ratio: depleted blue, enriched red, nothing grey.
.enrichment_fill <- function(limits = NULL) {
    ggplot2::scale_fill_gradient2(
        low = "#2166ac", mid = "grey95", high = "#b2182b",
        midpoint = 0, limits = limits, na.value = "grey85"
    )
}

## a band as it should read in a title: "0-50 um", "20-50 um"
.band_label <- function(r_inner, r_outer) {
    paste0(format(r_inner, trim = TRUE), "-", format(r_outer, trim = TRUE), " um")
}

#' Heatmap of which cell-type pairs sit closer together than chance
#'
#' Cell type against cell type, filled by `log2FC` -- how many more (or fewer)
#' pairs of those two types fall in the distance band than a random arrangement
#' of the same labels over the same cells would give. Pairs that do not clear
#' `sig_q` are drawn muted, so the eye lands on the ones the data supports.
#'
#' The matrix is mirrored for reading: [colocation_test()] returns each
#' unordered pair once, and both triangles are drawn from that single row.
#'
#' @param coloc The tibble from [colocation_test()].
#' @param sig_q Significance cut on `qBH`. Cells above it are muted, not hidden
#'   -- an absent tile and a non-significant one are different statements.
#'
#' @return A ggplot object.
#' @export
plot_colocation_heatmap <- function(coloc, sig_q = 0.05) {

    ## mirror the single row for each unordered pair into both triangles
    mirrored <- bind_rows(
        coloc,
        coloc |> filter(TypeA != TypeB) |> rename(TypeA = TypeB, TypeB = TypeA)
    )

    lv <- sort(unique(c(coloc$TypeA, coloc$TypeB)))
    pd <- mirrored |>
        mutate(
            TypeA = factor(TypeA, levels = lv),
            TypeB = factor(TypeB, levels = rev(lv)),
            Band = .band_label(RadiusInner, RadiusOuter),
            Significant = !is.na(qBH) & qBH < sig_q
        )

    facets <- if (dplyr::n_distinct(pd$Band) > 1) {
        ggplot2::facet_grid(Band ~ Sample)
    } else {
        ggplot2::facet_wrap(~Sample)
    }

    ggplot2::ggplot(pd, ggplot2::aes(TypeA, TypeB, fill = log2FC)) +
        ggplot2::geom_tile(ggplot2::aes(alpha = Significant),
                           color = "white", linewidth = 0.4) +
        ggplot2::scale_alpha_manual(values = c(`FALSE` = 0.25, `TRUE` = 1),
                                    guide = "none") +
        .enrichment_fill() +
        facets +
        ggplot2::labs(
            title = "Which cell types sit closer together than chance",
            subtitle = .wrap_subtitle(glue::glue(
                "log2 ratio of observed to expected pairs in the band; expected is a ",
                "random rearrangement of the same cell-type labels over the same cells. ",
                "Red = together more often than chance, blue = less often. ",
                "Muted tiles do not clear q < {sig_q}.")),
            x = NULL, y = NULL, fill = "log2 obs/exp"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
}

#' Ring composition against the composition of the whole specimen
#'
#' What the ring around each anchor cell is made of, next to what the specimen
#' as a whole is made of. A neighbour type standing above its background line is
#' over-represented next to the anchor type; level with it means the anchors sit
#' in ordinary tissue.
#'
#' @param neigh The list from [neighborhood_composition()].
#'
#' @return A ggplot object.
#' @export
plot_neighborhood_composition <- function(neigh) {

    p <- neigh$params
    pd <- neigh$summary |>
        select(Sample, NeighborType, pctOfRing, pctBackground) |>
        tidyr::pivot_longer(c(pctOfRing, pctBackground),
                            names_to = "Where", values_to = "pct") |>
        mutate(Where = factor(
            Where,
            levels = c("pctOfRing", "pctBackground"),
            labels = c(glue::glue("in the {.band_label(p$r_inner, p$r_outer)} ring"),
                       "in the whole specimen")
        ))

    ggplot2::ggplot(pd, ggplot2::aes(NeighborType, pct, fill = Where)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                          width = 0.7) +
        ggplot2::scale_fill_manual(values = c("#08519c", "grey70")) +
        ggplot2::scale_y_continuous(limits = c(0, NA),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::facet_wrap(~Sample) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = glue::glue(
                "What the tissue around each {p$anchor_type} cell is made of"),
            subtitle = .wrap_subtitle(glue::glue(
                "Cell types in the {.band_label(p$r_inner, p$r_outer)} ring around every ",
                "{p$anchor_type} cell, against the same types' share of the whole specimen. ",
                "Counts near the tissue edge are biased low, so read the percentages.")),
            x = NULL, y = "% of cells", fill = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "top")
}

#' Enrichment of each neighbour type in the ring
#'
#' `log2Enrich` per neighbour type: how much more (or less) of that type sits in
#' the ring than a random rearrangement of the labels would put there. Unlike
#' the raw counts this is unaffected by anchors whose rings are cut off at the
#' tissue border, because the null is computed over the same truncated rings.
#'
#' @param neigh The list from [neighborhood_composition()].
#' @param sig_q Significance cut on `qBH`; bars above it are muted.
#'
#' @return A ggplot object.
#' @export
plot_neighborhood_enrichment <- function(neigh, sig_q = 0.05) {

    p <- neigh$params
    pd <- neigh$summary |>
        mutate(Significant = !is.na(qBH) & qBH < sig_q)

    ggplot2::ggplot(pd, ggplot2::aes(stats::reorder(NeighborType, log2Enrich),
                                     log2Enrich, fill = log2Enrich)) +
        ggplot2::geom_col(ggplot2::aes(alpha = Significant), width = 0.7) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
        ggplot2::scale_alpha_manual(values = c(`FALSE` = 0.3, `TRUE` = 1),
                                    guide = "none") +
        .enrichment_fill() +
        ggplot2::facet_wrap(~Sample) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = glue::glue(
                "Which cell types are over-represented around {p$anchor_type} cells"),
            subtitle = .wrap_subtitle(glue::glue(
                "log2 ratio of observed to expected cells in the ",
                "{.band_label(p$r_inner, p$r_outer)} ring; expected is a random ",
                "rearrangement of the same labels over the same cells. ",
                "Muted bars do not clear q < {sig_q}.")),
            x = NULL, y = "log2 observed / expected", fill = "log2 obs/exp"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "none")
}
