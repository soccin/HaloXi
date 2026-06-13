## ---------------------------------------------------------------------------
## Collaborator-facing Excel output for the Halo manifest scanner.
##
## Related tables are grouped into a small number of workbooks, each with
## clearly named sheets, styled headers, frozen header rows and panes, banded
## rows, cell borders, number formatting, and format-aware column widths so
## collaborators can open and read them without further work.
## ---------------------------------------------------------------------------

#' Display width of a column as it will appear in Excel
#'
#' Returns the widest rendered value in a column, accounting for the openxlsx
#' number format that will be applied (thousands separators and fixed decimal
#' places both add characters the raw values do not have). NA/blank cells
#' contribute nothing.
#'
#' @param col A column vector.
#' @param fmt Optional openxlsx numFmt string (e.g. "#,##0", "0.00").
#'
#' @return Numeric scalar: the maximum displayed character width (0 if empty).
#'
#' @keywords internal
displayed_value_width <- function(col, fmt = NULL) {
    if (length(col) == 0) return(0)

    if (is.numeric(col) && !is.null(fmt)) {
        decimals <- if (grepl("\\.", fmt)) nchar(sub("^[^.]*\\.", "", fmt)) else 0
        big_mark <- if (grepl(",", fmt)) "," else ""
        shown <- formatC(col, format = "f", digits = decimals,
                         big.mark = big_mark, drop0trailing = FALSE)
    } else {
        shown <- format(col, trim = TRUE)
    }
    ## NA renders as a blank cell, not the literal "NA"
    shown[is.na(col)] <- ""

    w <- nchar(shown, type = "width")
    if (length(w) == 0) 0 else max(w, na.rm = TRUE)
}

#' Compute explicit column widths for a data frame
#'
#' Sizes each column to the wider of its header name and its displayed cell
#' values (number formatting included), adds padding, and clamps to a sensible
#' range. Used instead of openxlsx's `widths = "auto"`, which undercounts,
#' ignores the bold header style, and ignores applied number formats, so
#' columns clip.
#'
#' @param data A data frame.
#' @param numfmt Optional named list mapping column name -> openxlsx numFmt
#'   string, matching what [add_scan_sheet()] will apply.
#' @param pad Characters of padding added to every column.
#' @param min_width,max_width Lower/upper bounds on the returned widths.
#'
#' @return A numeric vector of column widths, one per column of `data`.
#'
#' @keywords internal
compute_col_widths <- function(data, numfmt = NULL, pad = 3,
                               min_width = 9, max_width = 60) {
    purrr::map2_dbl(names(data), data, function(nm, col) {
        body_w <- displayed_value_width(col, numfmt[[nm]])
        ## +3 covers the bold, centered header glyphs being a touch wider
        header_w <- nchar(nm, type = "width") + 3
        w <- max(body_w, header_w) + pad
        min(max(w, min_width), max_width)
    })
}

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
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)

    ncol_data <- ncol(data)
    all_cols <- seq_len(ncol_data)

    ## A header row, then a blank spacer row, so the table starts at row 3.
    start_row <- 1L
    if (!is.null(title)) {
        openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1)
        openxlsx::addStyle(
            wb, sheet,
            openxlsx::createStyle(
                textDecoration = "bold", fontSize = 13, fontColour = "#1f4e79",
                valign = "center"
            ),
            rows = 1, cols = 1, gridExpand = TRUE
        )
        openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 20)
        start_row <- 3L
    }

    header_row <- start_row
    first_body <- start_row + 1L
    last_body <- start_row + nrow(data)
    has_rows <- nrow(data) > 0

    openxlsx::writeData(
        wb, sheet, data,
        startRow = start_row, startCol = 1,
        headerStyle = openxlsx::createStyle(
            textDecoration = "bold", fgFill = "#1f4e79", fontColour = "white",
            halign = "center", valign = "center", wrapText = TRUE,
            border = "TopBottomLeftRight", borderColour = "#1f4e79"
        )
    )
    ## a little extra height so wrapped/centered headers breathe
    openxlsx::setRowHeights(wb, sheet, rows = header_row, heights = 28)

    if (has_rows) {
        body_rows <- first_body:last_body

        ## thin light borders on every data cell
        openxlsx::addStyle(
            wb, sheet,
            openxlsx::createStyle(
                border = "TopBottomLeftRight", borderColour = "#d9d9d9",
                valign = "center"
            ),
            rows = body_rows, cols = all_cols, gridExpand = TRUE, stack = TRUE
        )

        ## banded rows (zebra striping) for readability on wide tables
        shaded <- body_rows[seq_along(body_rows) %% 2 == 0]
        if (length(shaded)) {
            openxlsx::addStyle(
                wb, sheet,
                openxlsx::createStyle(fgFill = "#eef3f8"),
                rows = shaded, cols = all_cols, gridExpand = TRUE, stack = TRUE
            )
        }

        ## right-align numeric columns; left-align everything else
        is_num <- purrr::map_lgl(data, is.numeric)
        if (any(is_num)) {
            openxlsx::addStyle(
                wb, sheet, openxlsx::createStyle(halign = "right"),
                rows = body_rows, cols = which(is_num), gridExpand = TRUE, stack = TRUE
            )
        }
        if (any(!is_num)) {
            openxlsx::addStyle(
                wb, sheet, openxlsx::createStyle(halign = "left"),
                rows = body_rows, cols = which(!is_num), gridExpand = TRUE, stack = TRUE
            )
        }

        ## numeric formatting on requested columns
        if (!is.null(numfmt)) {
            for (col_name in intersect(names(numfmt), names(data))) {
                ci <- match(col_name, names(data))
                openxlsx::addStyle(
                    wb, sheet,
                    openxlsx::createStyle(numFmt = numfmt[[col_name]]),
                    rows = body_rows, cols = ci, gridExpand = TRUE, stack = TRUE
                )
            }
        }
    }

    openxlsx::freezePane(wb, sheet, firstActiveRow = first_body, firstActiveCol = 2)

    ## openxlsx's widths="auto" undercounts and ignores the bold header style and
    ## applied number formats, so headers and longer/formatted values get
    ## clipped. Size each column explicitly instead.
    col_widths <- compute_col_widths(data, numfmt = numfmt)
    openxlsx::setColWidths(wb, sheet, cols = all_cols, widths = col_widths)

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

#' Write the cell-annotation Excel workbook
#'
#' One collaborator workbook (`Cell_annotation.xlsx`) with the cell-type
#' composition (long + wide count matrix) and the sub-state breakdown. Reuses
#' the scanner's [add_scan_sheet()] styling.
#'
#' @param ct_summary The list from [summarize_celltypes()] (`long` + `wide`).
#' @param state_summary Long tibble from [summarize_states()].
#' @param outdir Directory to write the workbook into (created if needed).
#'
#' @return The workbook path written (invisibly).
#'
#' @export
write_annotation_workbook <- function(ct_summary, state_summary, outdir) {

    fs::dir_create(outdir)
    wb <- openxlsx::createWorkbook()

    add_scan_sheet(
        wb, "Composition (counts)", ct_summary$wide,
        title = "Cell-type counts per sample (UNKNOWN = conflict, UNCLASSIFIED = no lineage marker)",
        numfmt = stats::setNames(
            as.list(rep("#,##0", ncol(ct_summary$wide) - 1)),
            setdiff(names(ct_summary$wide), "CellType")
        )
    )
    add_scan_sheet(
        wb, "Composition (long)", ct_summary$long,
        title = "Cell-type composition per sample (long, with %)",
        numfmt = list(nCells = "#,##0", pct = "0.00")
    )
    add_scan_sheet(
        wb, "States", state_summary,
        title = "Sub-state positivity (nScored = parent cells with a callable flag; blank pctPos = un-callable)",
        numfmt = list(nScored = "#,##0", nPos = "#,##0", pctPos = "0.00")
    )

    p <- fs::path(outdir, "Cell_annotation.xlsx")
    openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
    invisible(p)
}
