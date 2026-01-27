#' Read a Halo CSV file
#'
#' Reads a Halo-exported CSV and cleans column names.
#'
#' @param file Path to CSV file.
#' @param colRenameMap Optional named list for normalizing column names.
#' @param ... Additional arguments passed to readr::read_csv().
#' @return A tibble with cleaned column names.
#' @export
read_halo <- function(file, colRenameMap = NULL, ...) {
  dd <- readr::read_csv(file, show_col_types = FALSE, progress = FALSE, ...) |>
    dplyr::rename_with(fix_col_names)
  if (!is.null(colRenameMap)) {
    dd <- dplyr::rename_with(
      dd,
      ~ purrr::map_chr(.x, normalize_name, mappings = colRenameMap)
    )
  }
  dd
}

#' Clean column names for Halo data
#'
#' Replaces spaces with underscores and removes trailing parenthetical units.
#'
#' @param col_names Character vector of column names.
#' @return Cleaned column names.
fix_col_names <- function(col_names) {
  col_names |>
    gsub(" ", "_", x = _) |>
    gsub("_\\(.*\\)$", "", x = _) |>
    sub("%_(.+)", "\\1_PCT", x = _)
}

normalize_name <- function(name, mappings) {
  for (canonical in names(mappings)) {
    variants <- mappings[[canonical]]
    # Sort longest first to avoid partial matches
    variants <- variants[order(nchar(variants), decreasing = TRUE)]
    for (v in variants) {
      if (v != canonical && grepl(v, name, fixed = TRUE)) {
        name <- sub(v, canonical, name, fixed = TRUE)
        break
      }
    }
  }
  name
}
