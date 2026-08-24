## The statistics have to answer three questions correctly: are these two types
## drawn together, pushed apart, or arranged independently. Each fixture builds
## a pattern where the answer is known by construction.

## A and B sit 5 um apart in tight couples, the couples 300 um apart, and a
## large C population sits far away in isolation. Every close pair is therefore
## an A-B pair, while A and B are a small share of the cells -- so a random
## arrangement of the labels would almost never produce an A-B pair.
attraction_obj <- function(n_pair = 40, n_filler = 400) {
    couples <- seq(0, by = 300, length.out = n_pair)
    filler <- 2e5 + seq(0, by = 300, length.out = n_filler)
    spatial_obj(
        x_um  = c(couples, couples + 5, filler),
        y_um  = rep(0, 2 * n_pair + n_filler),
        types = c(rep("A", n_pair), rep("B", n_pair), rep("C", n_filler))
    )
}

test_that("interleaved types read as attraction", {

    res <- colocation_test(attraction_obj(), um_per_px = 1, r_outer = 20,
                           n_perm = 99, verbose = FALSE)
    ab <- res |> filter(TypeA == "A", TypeB == "B")

    expect_equal(ab$nObs, 40)
    expect_gt(ab$log2FC, 3)
    expect_equal(ab$pEmp, 2 / 100)
})

test_that("types held apart read as segregation", {

    ## A fills the left block, B the right, with a gap far wider than the band
    set.seed(21)
    n <- 200
    obj <- spatial_obj(
        x_um  = c(runif(n, 0, 300), runif(n, 5000, 5300)),
        y_um  = c(runif(n, 0, 300), runif(n, 0, 300)),
        types = rep(c("A", "B"), each = n)
    )

    res <- colocation_test(obj, um_per_px = 1, r_outer = 40,
                           n_perm = 99, verbose = FALSE)
    ab <- res |> filter(TypeA == "A", TypeB == "B")

    expect_lt(ab$log2FC, -1)
    expect_equal(ab$nObs, 0)
    expect_equal(ab$pEmp, 2 / 100)
})

test_that("labels scattered at random read as no structure", {

    set.seed(22)
    n <- 600
    obj <- spatial_obj(
        x_um  = runif(n, 0, 600),
        y_um  = runif(n, 0, 600),
        types = sample(c("A", "B", "C"), n, replace = TRUE)
    )

    res <- colocation_test(obj, um_per_px = 1, r_outer = 40,
                           n_perm = 99, verbose = FALSE)

    expect_true(all(abs(res$log2FC) < 0.5))
    expect_true(mean(res$qBH > 0.1) > 0.5)
})

test_that("the empirical p has a floor and never reaches zero", {

    res <- colocation_test(attraction_obj(), um_per_px = 1, r_outer = 20,
                           n_perm = 99, verbose = FALSE)

    expect_true(all(res$pEmp >= 2 / 100))
    expect_true(all(res$pEmp > 0))
})

test_that("the same seed reproduces, a different one moves only the null", {

    set.seed(23)
    n <- 300
    obj <- spatial_obj(
        x_um  = runif(n, 0, 400),
        y_um  = runif(n, 0, 400),
        types = sample(c("A", "B"), n, replace = TRUE)
    )

    a <- colocation_test(obj, um_per_px = 1, r_outer = 40, n_perm = 49,
                         seed = 1, verbose = FALSE)
    b <- colocation_test(obj, um_per_px = 1, r_outer = 40, n_perm = 49,
                         seed = 1, verbose = FALSE)
    c2 <- colocation_test(obj, um_per_px = 1, r_outer = 40, n_perm = 49,
                          seed = 2, verbose = FALSE)

    expect_equal(a, b)
    expect_equal(a$nObs, c2$nObs)
    expect_false(isTRUE(all.equal(a$nExp, c2$nExp)))
})

test_that("the observed pair counts are the brute-force counts", {

    ## the statistics are only as good as nObs, so it is checked against a
    ## count taken straight off a distance matrix
    set.seed(24)
    n <- 150
    x <- runif(n, 0, 200); y <- runif(n, 0, 200)
    types <- sample(c("A", "B"), n, replace = TRUE)
    obj <- spatial_obj(x, y, types)

    res <- colocation_test(obj, um_per_px = 1, r_outer = 30, n_perm = 9,
                           verbose = FALSE)

    d <- as.matrix(stats::dist(cbind(x, y)))
    idx <- which(d > 0 & d <= 30, arr.ind = TRUE)
    idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
    tab <- table(paste(pmin(types[idx[, 1]], types[idx[, 2]]),
                       pmax(types[idx[, 1]], types[idx[, 2]])))

    got <- res |>
        mutate(key = paste(TypeA, TypeB)) |>
        arrange(key)

    expect_equal(got$nObs, as.numeric(tab[got$key]))
})
