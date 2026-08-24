## Contracts that keep a spatial result honest: a scan resolution can never be
## guessed, un-callable cells never acquire a label, and samples never lean on
## one another.

test_that("um_per_px is required and must be a single positive number", {

    obj <- spatial_obj(c(0, 10), c(0, 0), c("A", "B"))

    expect_error(cell_centroids(obj), "um_per_px is required")
    expect_error(cell_centroids(obj, NULL), "um_per_px is required")
    expect_error(cell_centroids(obj, 0), "single positive finite")
    expect_error(cell_centroids(obj, -1), "single positive finite")
    expect_error(cell_centroids(obj, c(1, 2)), "single positive finite")
    expect_error(cell_centroids(obj, Inf), "single positive finite")
    expect_error(cell_centroids(obj, "0.325"), "single positive finite")

    expect_error(colocation_test(obj), "um_per_px is required")
    expect_error(neighborhood_composition(obj, anchor_type = "A"),
                 "um_per_px is required")
})

test_that("pixels are converted to microns using the given scale", {

    obj <- spatial_obj(c(0, 100), c(0, 0), c("A", "B"), um_per_px = 1)
    ## the fixture stored microns/1 as pixels, so at 0.5 um/px the same cells
    ## are half as far apart
    cen <- cell_centroids(obj, 0.5)

    expect_equal(cen$X, c(0, 50))
})

test_that("un-callable cells are dropped, set-aside buckets are kept", {

    obj <- spatial_obj(c(0, 10, 20, 30), rep(0, 4),
                       c("A", "UNKNOWN", "UNCLASSIFIED", "B"))
    obj$cell.data$CellType[1] <- NA

    cen <- cell_centroids(obj, 1)

    expect_equal(nrow(cen), 3)
    expect_setequal(as.character(cen$CellType), c("UNKNOWN", "UNCLASSIFIED", "B"))
    expect_false("A" %in% levels(cen$CellType))
})

test_that("samples are analysed independently", {

    set.seed(31)
    n <- 120
    one <- spatial_obj(runif(n, 0, 200), runif(n, 0, 200),
                       sample(c("A", "B"), n, replace = TRUE), sample = "S1")
    two <- spatial_obj(runif(n, 0, 200), runif(n, 0, 200),
                       sample(c("A", "B"), n, replace = TRUE), sample = "S2")
    both <- list(cell.data = bind_rows(one$cell.data, two$cell.data))

    together <- colocation_test(both, um_per_px = 1, r_outer = 30, n_perm = 19,
                                verbose = FALSE) |> filter(Sample == "S1")
    alone <- colocation_test(one, um_per_px = 1, r_outer = 30, n_perm = 19,
                             verbose = FALSE)

    expect_equal(together$nObs, alone$nObs)
    expect_equal(together$nA, alone$nA)
})

test_that("an unknown anchor type is refused, and a sample without one is skipped", {

    set.seed(32)
    n <- 80
    s1 <- spatial_obj(runif(n, 0, 150), runif(n, 0, 150),
                      sample(c("A", "B"), n, replace = TRUE), sample = "S1")
    s2 <- spatial_obj(runif(n, 0, 150), runif(n, 0, 150),
                      rep("B", n), sample = "S2")
    both <- list(cell.data = bind_rows(s1$cell.data, s2$cell.data))

    expect_error(neighborhood_composition(both, 1, anchor_type = "Z"),
                 "no cell is called")

    res <- neighborhood_composition(both, 1, anchor_type = "A", r_inner = 0,
                                    r_outer = 40, n_perm = 9, verbose = FALSE)

    expect_setequal(res$summary$Sample, "S1")
})

test_that("the neighbour counts reconcile between summary and per_anchor", {

    set.seed(33)
    n <- 400
    obj <- spatial_obj(runif(n, 0, 300), runif(n, 0, 300),
                       sample(c("A", "B", "C"), n, replace = TRUE))

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 10,
                                    r_outer = 40, n_perm = 9, verbose = FALSE)
    types <- as.character(res$summary$NeighborType)

    expect_equal(rowSums(res$per_anchor[types]), res$per_anchor$nRing)
    expect_equal(sum(res$summary$nNeighbors), sum(res$per_anchor$nRing))
    expect_equal(colSums(res$per_anchor[types]),
                 setNames(res$summary$nNeighbors, types))
    expect_equal(nrow(res$per_anchor), unique(res$summary$nAnchors))
})

test_that("the result carries its own parameters", {

    set.seed(34)
    n <- 120
    obj <- spatial_obj(runif(n, 0, 200), runif(n, 0, 200),
                       sample(c("A", "B"), n, replace = TRUE))

    res <- neighborhood_composition(obj, 0.5, anchor_type = "A", r_inner = 5,
                                    r_outer = 25, n_perm = 9, seed = 7,
                                    verbose = FALSE)

    expect_equal(res$params, list(um_per_px = 0.5, anchor_type = "A",
                                  r_inner = 5, r_outer = 25,
                                  n_perm = 9, seed = 7))
    expect_true(all(res$summary$RadiusInner == 5))
    expect_true(all(res$summary$RadiusOuter == 25))
})
