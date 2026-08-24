## The neighbourhood analysis answers "what is the tissue around this cell type
## made of". These fixtures build rings whose composition is known by hand.

test_that("the ring excludes what is inside it", {

    ## around the single anchor: B at 10 um (inside r_inner), C at 30 um (in
    ## the ring), A at 300 um (outside)
    obj <- spatial_obj(
        x_um  = c(0, 10, 30, 300),
        y_um  = rep(0, 4),
        types = c("A", "B", "C", "A")
    )

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 20,
                                    r_outer = 50, n_perm = 9, verbose = FALSE)
    got <- res$summary |> select(NeighborType, nNeighbors)

    expect_equal(got$nNeighbors[got$NeighborType == "C"], 1)
    expect_equal(got$nNeighbors[got$NeighborType == "B"], 0)
    expect_equal(sum(got$nNeighbors), 1)
})

test_that("an anchor in another anchor's ring is counted", {

    ## two anchors 30 um apart, each in the other's ring
    obj <- spatial_obj(c(0, 30), c(0, 0), c("A", "A"))

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 20,
                                    r_outer = 50, n_perm = 9, verbose = FALSE)

    expect_equal(res$summary$nNeighbors, 2)       # once from each direction
    expect_equal(res$summary$nAnchors, 2)
    expect_equal(res$summary$meanPerAnchor, 1)
    expect_equal(res$per_anchor$nRing, c(1, 1))
})

test_that("an anchor is found at either end of an undirected pair", {

    ## the pair list holds (i < j) only, so a neighbour with a lower index than
    ## its anchor is only seen by looking from both ends
    obj <- spatial_obj(c(0, 30), c(0, 0), c("B", "A"))

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 20,
                                    r_outer = 50, n_perm = 9, verbose = FALSE)

    expect_equal(res$summary$nNeighbors[res$summary$NeighborType == "B"], 1)
})

test_that("a type packed around the anchors is enriched over background", {

    ## every anchor carries a shell of B at 30 um; C is scattered far away, so
    ## B is common in the rings and rare in the section
    set.seed(41)
    n_anchor <- 30
    ax <- seq(0, by = 400, length.out = n_anchor)
    shell <- c(-30, 30)
    obj <- spatial_obj(
        x_um  = c(ax, rep(ax, each = 2) + rep(shell, n_anchor), 5e4 + seq_len(600) * 50),
        y_um  = c(rep(0, n_anchor + 2 * n_anchor), rep(0, 600)),
        types = c(rep("A", n_anchor), rep("B", 2 * n_anchor), rep("C", 600))
    )

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 20,
                                    r_outer = 50, n_perm = 99, verbose = FALSE)
    b <- res$summary |> filter(NeighborType == "B")

    expect_equal(b$pctOfRing, 100)
    expect_lt(b$pctBackground, 10)
    expect_gt(b$log2Enrich, 3)
    expect_equal(b$pEmp, 2 / 100)
})

test_that("randomly arranged labels give a ring that matches the background", {

    set.seed(42)
    n <- 900
    obj <- spatial_obj(runif(n, 0, 700), runif(n, 0, 700),
                       sample(c("A", "B", "C"), n, replace = TRUE))

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 20,
                                    r_outer = 50, n_perm = 99, verbose = FALSE)

    expect_true(all(abs(res$summary$log2Enrich) < 0.3))
    expect_true(all(abs(res$summary$pctOfRing - res$summary$pctBackground) < 6))
})

test_that("the null keeps the anchors where they are", {

    ## nExp is what the same rings would hold if the labels were shuffled, so
    ## it must track the ring total and the background share, not the anchor
    ## geometry. Sum of nExp equals the observed ring total by construction.
    set.seed(43)
    n <- 500
    obj <- spatial_obj(runif(n, 0, 400), runif(n, 0, 400),
                       sample(c("A", "B", "C"), n, replace = TRUE))

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 10,
                                    r_outer = 40, n_perm = 99, verbose = FALSE)

    expect_equal(sum(res$summary$nExp), sum(res$summary$nNeighbors))
    expect_equal(res$summary$nExp / sum(res$summary$nExp),
                 res$summary$pctBackground / 100, tolerance = 0.02)
})

test_that("per_anchor keeps the distribution behind the mean", {

    set.seed(44)
    n <- 300
    obj <- spatial_obj(runif(n, 0, 250), runif(n, 0, 250),
                       sample(c("A", "B"), n, replace = TRUE))

    res <- neighborhood_composition(obj, 1, anchor_type = "A", r_inner = 10,
                                    r_outer = 40, n_perm = 9, verbose = FALSE)

    expect_true(all(c("UUID", "Sample", "nRing") %in% names(res$per_anchor)))
    expect_gt(stats::sd(res$per_anchor$nRing), 0)
    expect_equal(mean(res$per_anchor$nRing),
                 sum(res$summary$meanPerAnchor))
})
