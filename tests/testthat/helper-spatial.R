## Synthetic fixtures for the spatial tests. Study-agnostic and built in-test:
## coordinates are made up here, cell types are A/B/C, and nothing reads project
## data.

## An object in the shape annotate_cells() returns, from explicit coordinates.
## Coordinates are given in MICRONS and divided back to pixels, so a test can
## state the geometry it means and pass the matching um_per_px.
spatial_obj <- function(x_um, y_um, types, sample = "S1", um_per_px = 1,
                        levels = NULL) {
    n <- length(x_um)
    px_x <- x_um / um_per_px
    px_y <- y_um / um_per_px
    tibble::tibble(
        UUID   = paste0("c", seq_len(n)),
        Sample = rep(sample, length.out = n),
        XMin = px_x, XMax = px_x, YMin = px_y, YMax = px_y,
        CellType = factor(types, levels = levels %||% sort(unique(types)))
    ) |> (\(cd) list(cell.data = cd))()
}

## every pair within the band, by brute force over a full distance matrix
brute_pairs <- function(x, y, r_outer, r_inner = 0) {
    d <- as.matrix(stats::dist(cbind(x, y)))
    idx <- which(d > r_inner & d <= r_outer, arr.ind = TRUE)
    idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
    out <- paste(idx[, 1], idx[, 2], sep = "-")
    sort(out)
}

pair_keys <- function(pr) {
    if (is.null(pr)) return(character())
    sort(paste(pr$i, pr$j, sep = "-"))
}
