## ---------------------------------------------------------------------------
## Collaborator-facing Excel output for the spatial stage. Reuses the scanner's
## add_scan_sheet() styling so every workbook in the project reads the same way.
## ---------------------------------------------------------------------------

## Quantiles of a count column, as one row. Used to put the shape of the
## per-anchor distribution in the workbook without shipping a sheet with one
## row per anchor cell -- a large specimen has hundreds of thousands of them,
## and the full table goes out as a CSV instead.
.count_quantiles <- function(x) {
    q <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 0.9, 1), names = FALSE)
    tibble::tibble(
        mean = mean(x), min = q[1], q25 = q[2], median = q[3],
        q75 = q[4], q90 = q[5], max = q[6]
    )
}

#' Write the spatial-analysis Excel workbook
#'
#' One collaborator workbook (`Spatial_analysis.xlsx`) with the co-location
#' test, the neighbourhood composition, and the spread of ring sizes behind the
#' neighbourhood means.
#'
#' The per-anchor table itself is deliberately not a sheet: it carries one row
#' per anchor cell, which on a whole-section specimen is hundreds of thousands
#' of rows that no one reads in Excel. Its distribution is summarised here and
#' the full table is written as a CSV by the driver.
#'
#' @param coloc The tibble from [colocation_test()], for one or several bands.
#' @param neigh The list from [neighborhood_composition()].
#' @param outdir Directory to write the workbook into (created if needed).
#'
#' @return The workbook path written (invisibly).
#'
#' @export
write_spatial_workbook <- function(coloc, neigh, outdir) {

    fs::dir_create(outdir)
    wb <- openxlsx::createWorkbook()

    p <- neigh$params

    add_scan_sheet(
        wb, "Co-location", coloc,
        title = paste0(
            "Cell-type pairs within a distance band, against a random ",
            "rearrangement of the same labels. log2FC > 0 = together more ",
            "often than chance. pEmp cannot fall below ",
            signif(2 / (p$n_perm + 1), 2), " with ", p$n_perm, " permutations."),
        numfmt = list(
            RadiusInner = "0", RadiusOuter = "0",
            nA = "#,##0", nB = "#,##0", nObs = "#,##0", nExp = "#,##0.0",
            sdExp = "#,##0.0", log2FC = "0.00", z = "0.0",
            pEmp = "0.0000", qBH = "0.0000"
        )
    )

    add_scan_sheet(
        wb, "Neighbourhood", neigh$summary,
        title = paste0(
            "Cell types in the ", p$r_inner, "-", p$r_outer, " um ring around every ",
            p$anchor_type, " cell. pctBackground is the same type's share of the ",
            "whole specimen; log2Enrich compares the ring against a random ",
            "rearrangement of the labels and is unaffected by rings cut off at ",
            "the tissue edge."),
        numfmt = list(
            RadiusInner = "0", RadiusOuter = "0",
            nAnchors = "#,##0", nNeighbors = "#,##0", meanPerAnchor = "0.00",
            pctOfRing = "0.00", pctBackground = "0.00", nExp = "#,##0.0",
            log2Enrich = "0.00", z = "0.0", pEmp = "0.0000", qBH = "0.0000"
        )
    )

    types <- as.character(unique(neigh$summary$NeighborType))
    spread <- neigh$per_anchor |>
        tidyr::pivot_longer(all_of(c(types, "nRing")),
                            names_to = "NeighborType", values_to = "n") |>
        group_by(Sample, NeighborType) |>
        reframe(.count_quantiles(n)) |>
        mutate(NeighborType = ifelse(NeighborType == "nRing",
                                     "ALL (ring total)", NeighborType)) |>
        arrange(Sample, desc(mean))

    add_scan_sheet(
        wb, "Ring size per anchor", spread,
        title = paste0(
            "Spread of the per-anchor counts behind the neighbourhood means: ",
            "one distribution per neighbour type over the ", p$anchor_type,
            " cells of each specimen."),
        numfmt = stats::setNames(
            as.list(rep("#,##0.0", 7)),
            c("mean", "min", "q25", "median", "q75", "q90", "max"))
    )

    path <- fs::path(outdir, "Spatial_analysis.xlsx")
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    invisible(path)
}
