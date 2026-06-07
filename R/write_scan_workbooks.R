## ---------------------------------------------------------------------------
## Collaborator-facing Excel output for the Halo manifest scanner.
##
## Related tables are grouped into a small number of workbooks, each with
## clearly named sheets, styled headers, frozen header rows and auto-sized
## columns so collaborators can open and read them without further work.
## ---------------------------------------------------------------------------

#' Add one styled sheet to an openxlsx workbook
#'
#' @param wb An openxlsx workbook.
#' @param sheet Sheet name (truncated to Excel's 31-char limit).
#' @param data A data frame to write.
#' @param title Optional title written above the table.
#' @param numfmt Optional named list mapping column name -> openxlsx numFmt
#'   string (e.g. list(pctPos = "0.0")). Applied to the data rows.
#'
#' @keywords internal
add_scan_sheet <- function(wb, sheet, data, title = NULL, numfmt = NULL) {

    sheet <- substr(sheet, 1, 31)
    openxlsx::addWorksheet(wb, sheet)

    start_row <- 1L
    if (!is.null(title)) {
        openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1)
        openxlsx::addStyle(
            wb, sheet,
            openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
            rows = 1, cols = 1, gridExpand = TRUE
        )
        start_row <- 3L
    }

    openxlsx::writeData(
        wb, sheet, data,
        startRow = start_row, startCol = 1,
        headerStyle = openxlsx::createStyle(
            textDecoration = "bold", fgFill = "#1f4e79", fontColour = "white",
            halign = "center", border = "bottom"
        )
    )

    ## numeric formatting on requested columns
    if (!is.null(numfmt)) {
        for (col_name in intersect(names(numfmt), names(data))) {
            ci <- match(col_name, names(data))
            openxlsx::addStyle(
                wb, sheet,
                openxlsx::createStyle(numFmt = numfmt[[col_name]]),
                rows = (start_row + 1L):(start_row + nrow(data)),
                cols = ci, gridExpand = TRUE, stack = TRUE
            )
        }
    }

    openxlsx::freezePane(wb, sheet, firstActiveRow = start_row + 1L)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(data)), widths = "auto")

    invisible(wb)
}

#' Write the scanner's collaborator Excel workbooks
#'
#' Produces two workbooks under `outdir`:
#'  * `Halo_scan_overview.xlsx` - manifest, cell-count summary, marker panel
#'    presence matrix (the "is my data sane" workbook).
#'  * `Halo_scan_markers.xlsx` - per-marker positivity (long + wide %positive
#'    and #positive matrices) for downstream analysts.
#'
#' @param tables A named list of the scanner's summary tables, as assembled by
#'   the driver: `manifest`, `cell_summary`, `presence_matrix`,
#'   `marker_summary`, `positivity_pct`, `positivity_n`.
#' @param outdir Directory to write the workbooks into (created if needed).
#'
#' @return Character vector of the workbook paths written (invisibly).
#'
#' @export
write_scan_workbooks <- function(tables, outdir) {

    fs::dir_create(outdir)
    paths <- character()

    ## ---- Overview workbook -------------------------------------------------
    wb1 <- openxlsx::createWorkbook()

    add_scan_sheet(
        wb1, "Manifest",
        tables$manifest |> dplyr::mutate(HaloFile = fs::path_file(HaloFile)),
        title = "Sample manifest (HaloFile shown as basename)"
    )
    add_scan_sheet(
        wb1, "Cell counts", tables$cell_summary,
        title = "Cells per sample",
        numfmt = list(nCells = "#,##0", nPhenotyped = "#,##0",
                      nUnphenotyped = "#,##0", pctPhenotyped = "0.0")
    )
    add_scan_sheet(
        wb1, "Marker presence", tables$presence_matrix,
        title = "Sample x marker panel coverage (TRUE = marker in panel)"
    )

    p1 <- fs::path(outdir, "Halo_scan_overview.xlsx")
    openxlsx::saveWorkbook(wb1, p1, overwrite = TRUE)
    paths <- c(paths, p1)

    ## ---- Marker positivity workbook ---------------------------------------
    wb2 <- openxlsx::createWorkbook()

    add_scan_sheet(
        wb2, "Positivity pct", tables$positivity_pct,
        title = "% of scored cells positive per marker (blank = marker absent)",
        numfmt = stats::setNames(
            as.list(rep("0.00", ncol(tables$positivity_pct) - 2)),
            setdiff(names(tables$positivity_pct), c("MarkerNorm", "IsControl"))
        )
    )
    add_scan_sheet(
        wb2, "Positivity counts", tables$positivity_n,
        title = "Number of positive cells per marker (blank = marker absent)",
        numfmt = stats::setNames(
            as.list(rep("#,##0", ncol(tables$positivity_n) - 2)),
            setdiff(names(tables$positivity_n), c("MarkerNorm", "IsControl"))
        )
    )
    add_scan_sheet(
        wb2, "Positivity long", tables$marker_summary,
        title = "Per-sample per-marker positivity (long form)",
        numfmt = list(nCells = "#,##0", nPos = "#,##0", pctPos = "0.00")
    )

    p2 <- fs::path(outdir, "Halo_scan_markers.xlsx")
    openxlsx::saveWorkbook(wb2, p2, overwrite = TRUE)
    paths <- c(paths, p2)

    invisible(paths)
}
