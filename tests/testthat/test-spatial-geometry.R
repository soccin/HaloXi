## The pair search is what every spatial number rests on: if it returns the
## wrong set of pairs, every count, enrichment and p-value downstream is wrong
## in a way no later test would catch. So it is checked against brute force
## rather than against itself.

test_that("the pair search matches a brute-force distance matrix", {

    set.seed(11)
    xy <- tibble::tibble(X = runif(200, 0, 300), Y = runif(200, 0, 300))

    expect_equal(pair_keys(.sample_pairs(xy, r_outer = 40)),
                 brute_pairs(xy$X, xy$Y, 40))
})

test_that("the pair search matches brute force for a ring as well", {

    set.seed(12)
    xy <- tibble::tibble(X = runif(200, 0, 300), Y = runif(200, 0, 300))

    expect_equal(pair_keys(.sample_pairs(xy, r_outer = 50, r_inner = 20)),
                 brute_pairs(xy$X, xy$Y, 50, 20))
})

test_that("the band is half-open: r_inner is out, r_outer is in", {

    ## distances from point 1: 20 (exactly r_inner), 35, 50 (exactly r_outer)
    xy <- tibble::tibble(X = c(0, 20, 35, 50), Y = 0)

    pr <- .sample_pairs(xy, r_outer = 50, r_inner = 20)

    expect_false("1-2" %in% pair_keys(pr))   # d == r_inner, excluded
    expect_true("1-3" %in% pair_keys(pr))
    expect_true("1-4" %in% pair_keys(pr))    # d == r_outer, included
})

test_that("indices come back as i < j, each unordered pair once", {

    set.seed(13)
    xy <- tibble::tibble(X = runif(120, 0, 100), Y = runif(120, 0, 100))

    pr <- .sample_pairs(xy, r_outer = 30)

    expect_true(all(pr$i < pr$j))
    expect_equal(anyDuplicated(paste(pr$i, pr$j)), 0L)
})

test_that("doubling um_per_px and the radii gives the same pairs", {

    set.seed(14)
    n <- 150
    px <- tibble::tibble(X = runif(n, 0, 400), Y = runif(n, 0, 400))

    at_1 <- .sample_pairs(px, r_outer = 40, r_inner = 10)
    at_2 <- .sample_pairs(px * 2, r_outer = 80, r_inner = 20)

    expect_equal(pair_keys(at_1), pair_keys(at_2))
})

test_that("fewer than two points, or no pairs in the band, gives NULL", {

    expect_null(.sample_pairs(tibble::tibble(X = 1, Y = 1), r_outer = 10))
    expect_null(.sample_pairs(tibble::tibble(X = c(0, 500), Y = c(0, 0)),
                              r_outer = 10))
})

test_that("a degenerate point pattern does not break the window", {

    ## every cell at the same coordinate: a zero-width search window would
    ## error, so the range is padded. The pairs themselves are all at d == 0,
    ## which the half-open band excludes -- see the next test.
    xy <- tibble::tibble(X = rep(5, 4), Y = rep(5, 4))

    expect_no_error(.sample_pairs(xy, r_outer = 10))
})

test_that("cells sharing a centroid are excluded at every band", {

    ## d == 0 fails `r_inner < d` for r_inner = 0, so two distinct cells whose
    ## centroids coincide are never paired. That is the half-open band doing
    ## what a ring needs it to do, and it means a segmentation artefact cannot
    ## register as the closest possible association. Asserted so the behaviour
    ## is a decision rather than a side effect.
    xy <- tibble::tibble(X = c(5, 5, 30), Y = c(5, 5, 5))

    expect_equal(pair_keys(.sample_pairs(xy, r_outer = 50)), c("1-3", "2-3"))
})

test_that("an impossible band is refused rather than quietly returning nothing", {

    xy <- tibble::tibble(X = c(0, 10), Y = c(0, 0))

    expect_error(.sample_pairs(xy, r_outer = 0), "positive finite")
    expect_error(.sample_pairs(xy, r_outer = 10, r_inner = 10), "r_inner < r_outer")
})
