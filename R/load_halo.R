#' @import dplyr
#' @import tidyr
#' @import purrr

suppressPackageStartupMessages({
  require(dplyr)
  require(tidyr)
  require(purrr)
})

#' Load a Halo Object CSV file
#'
#' Reads a Halo CSV and returns structured cell and marker data.
#'
#' @param hfile Path to Halo CSV file.
#' @param uuid_cols Column names used to compute cell UUID.
#' @param sample_name Sample identifier: string or function(hfile) -> string.
#'   If missing, derived from filename.
#' @param cols_extra Additional columns to include in cell.data.
#' @param marker_map Named vector to rename markers (old = new).
#' @param control_markers Markers to exclude from MarkerPos (default: "DAPI").
#'
#' @return A list with elements:
#'   \item{cell.data}{Tibble with UUID, Sample, coordinates, and MarkerPos.}
#'   \item{marker.data}{Tibble with UUID, Marker, and Positive status.}
#'   \item{VERSION}{Package version string.}
#'
#' @export
load_halo <- function(hfile, uuid_cols, sample_name, cols_extra, marker_map,
                      control_markers) {

  if (missing(uuid_cols)) {
    stop("\n\nFATAL ERROR::load_halo\nuuid_cols Missing\n")
  }

  # Determine sample ID
  if (missing(sample_name)) {
    sid <- basename(hfile) |> gsub("\\.csv.*", "", x = _)
  } else if (is.function(sample_name)) {
    sid <- sample_name(hfile)
  } else {
    sid <- sample_name
  }

  dd <- read_halo(hfile) |> mutate(Sample = sid)
  dd$UUID <- generate_cell_uuid(dd, uuid_cols)

  cell.data <- dd |> select(UUID, Sample, XMin, XMax, YMin, YMax)

  marker.data <- dd |>
    select(UUID, matches("_Positive_Classification$")) |>
    gather(Marker, Positive, -UUID) |>
    mutate(Marker = gsub("_Positive_Classification$", "", Marker))

  if (!missing(marker_map)) {
    marker.data$Marker <- marker_map[marker.data$Marker]
    marker.data <- marker.data %>% filter(!is.na(Marker))
  }

  if (missing(control_markers)) {
    control_markers <- c("DAPI")
  }

  marker.data <- marker.data |>
    mutate(MarkerNorm = toupper(Marker))

  marker_pos <- marker.data |>
    filter(!(Marker %in% control_markers)) |>
    group_by(UUID) |>
    summarize(MarkerPos = paste0(sort(MarkerNorm[Positive == 1]), collapse = ";")) |>
    ungroup()

  cell.data <- left_join(cell.data, marker_pos)

  if (!missing(cols_extra)) {
    extra.data <- dd %>% select(UUID, all_of(cols_extra))
    cell.data <- left_join(cell.data, extra.data)
  }

  obj <- list(cell.data = cell.data, marker.data = marker.data, VERSION = VERSION)

  obj
}

#' Generate unique cell UUIDs
#'
#' Creates SHA1 hash from specified columns to uniquely identify cells.
#'
#' @param dat Dataframe containing the columns.
#' @param cols_uuid Column names to use for UUID generation.
#' @return Character vector of UUIDs.
generate_cell_uuid <- function(dat, cols_uuid) {
  lapply(
    transpose(dat[, cols_uuid]),
    function(x) { digest::digest(paste(x, collapse = ";"), algo = "sha1") }
  ) |>
    unlist()
}
