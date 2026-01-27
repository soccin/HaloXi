#' Clean column names for Halo data
#'
#' Replaces spaces with underscores and removes trailing parenthetical units.
#'
#' @param col_names Character vector of column names.
#' @return Cleaned column names.
fix_col_names <- function(col_names) {
  col_names |>
    gsub(" ", "_", x = _) |>
    gsub("_\\(.*\\)$", "", x = _)
}

#' Read a Halo CSV file
#'
#' Reads a Halo-exported CSV and cleans column names.
#'
#' @param file Path to CSV file.
#' @param ... Additional arguments passed to readr::read_csv().
#' @return A tibble with cleaned column names.
#' @export
read_halo <- function(file, ...) {
  readr::read_csv(file, show_col_types = FALSE, progress = FALSE, ...) |>
    dplyr::rename_all(fix_col_names)
}
