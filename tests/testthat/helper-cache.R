## Fixtures for the loaded-object cache tests. Everything is synthetic and
## written in-test: no project data, no large files.

## The columns load_halo() needs: the UUID columns it is pointed at, plus at
## least one *_Positive_Classification marker column.
make_halo_csv <- function(path, n_cells = 3, marker = "CD3") {
    tibble::tibble(
        Image_Location = sprintf("%s_img_%02d.tif", fs::path_ext_remove(fs::path_file(path)), seq_len(n_cells)),
        XMin = seq_len(n_cells) * 10L,
        XMax = seq_len(n_cells) * 10L + 5L,
        YMin = seq_len(n_cells) * 20L,
        YMax = seq_len(n_cells) * 20L + 5L,
        DAPI_Positive_Classification = rep(1L, n_cells)
    ) |>
        dplyr::mutate("{marker}_Positive_Classification" := rep(c(0L, 1L), length.out = n_cells)) |>
        readr::write_csv(path, progress = FALSE)
    as.character(path)
}

make_manifest_csv <- function(path, samples, halo_files) {
    tibble::tibble(Sample = samples, HaloFile = halo_files) |>
        readr::write_csv(path, progress = FALSE)
    read_manifest(path)
}

## TRUE when the call reused the cache rather than re-reading the CSVs.
cache_was_used <- function(expr) {
    any(grepl("using cache", testthat::capture_messages(expr), fixed = TRUE))
}
