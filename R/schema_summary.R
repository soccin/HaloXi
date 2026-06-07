#' Summarize dataframe schema
#'
#' Returns a tibble describing each column's class, uniqueness, and missing values.
#'
#' @param df A dataframe or tibble to summarize.
#' @return A tibble with columns: column, class, n_unique, n_na, all_na.
#' @export
schema_summary <- function(df) {
  tibble(
    column   = names(df),
    class    = map_chr(df, ~ class(.x)[1]),
    n_unique = map_int(df, n_distinct),
    n_na     = map_int(df, ~ sum(is.na(.x))),
    all_na   = map_lgl(df, ~ all(is.na(.x)))
  )
}
