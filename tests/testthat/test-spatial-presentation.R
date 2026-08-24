## The plots and the workbook are what the collaborator actually sees, so they
## are exercised end to end on a small fixture rather than trusted to be
## thin wrappers.

spatial_fixture <- function(n = 400, seed = 51) {
    set.seed(seed)
    obj <- spatial_obj(runif(n, 0, 300), runif(n, 0, 300),
                       sample(c("A", "B", "C"), n, replace = TRUE))
    list(
        coloc = bind_rows(
            colocation_test(obj, 1, r_outer = 20, n_perm = 19, verbose = FALSE),
            colocation_test(obj, 1, r_outer = 50, n_perm = 19, verbose = FALSE)
        ),
        neigh = neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 20,
                                         r_outer = 50, n_perm = 19, verbose = FALSE)
    )
}

test_that("the co-location heatmap draws both triangles from one row each", {

    skip_if_not_installed("ggplot2")

    fx <- spatial_fixture()
    p <- plot_colocation_heatmap(fx$coloc)

    expect_s3_class(p, "ggplot")
    ## every ordered combination of the types appears once per sample per band
    n_types <- dplyr::n_distinct(c(fx$coloc$TypeA, fx$coloc$TypeB))
    expect_equal(nrow(p$data), n_types^2 * dplyr::n_distinct(fx$coloc$RadiusOuter))
})

test_that("the neighbourhood plots build and name the anchor type", {

    skip_if_not_installed("ggplot2")

    fx <- spatial_fixture()

    comp <- plot_neighborhood_composition(fx$neigh)
    enr <- plot_neighborhood_enrichment(fx$neigh)

    expect_s3_class(comp, "ggplot")
    expect_s3_class(enr, "ggplot")
    expect_match(enr$labels$title, "A cells")
    expect_match(comp$labels$subtitle, "20-50 um ring")
})

test_that("plot subtitles wrap instead of running off the canvas", {

    skip_if_not_installed("ggplot2")

    fx <- spatial_fixture()
    subs <- c(
        plot_colocation_heatmap(fx$coloc)$labels$subtitle,
        plot_neighborhood_composition(fx$neigh)$labels$subtitle,
        plot_neighborhood_enrichment(fx$neigh)$labels$subtitle
    )

    for (s in subs) {
        expect_true(all(nchar(strsplit(s, "\n", fixed = TRUE)[[1]]) <= 90))
    }
})

test_that("the workbook writes every sheet", {

    skip_if_not_installed("openxlsx")

    fx <- spatial_fixture()
    out <- withr::local_tempdir()

    path <- write_spatial_workbook(fx$coloc, fx$neigh, out)

    expect_true(fs::file_exists(path))
    expect_setequal(openxlsx::getSheetNames(path),
                    c("Co-location", "Neighbourhood", "Ring size per anchor"))
})

test_that("the ring-size sheet reconciles with the neighbourhood means", {

    skip_if_not_installed("openxlsx")

    fx <- spatial_fixture()
    out <- withr::local_tempdir()
    path <- write_spatial_workbook(fx$coloc, fx$neigh, out)

    spread <- openxlsx::read.xlsx(path, sheet = "Ring size per anchor",
                                  startRow = 2)
    got <- spread |>
        filter(NeighborType != "ALL (ring total)") |>
        arrange(NeighborType)
    want <- fx$neigh$summary |> arrange(NeighborType)

    expect_equal(got$mean, want$meanPerAnchor, tolerance = 1e-6)
})
