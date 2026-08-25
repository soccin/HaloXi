## load_manifest(): what the loaded-object cache is keyed on.
##
## Keying on n_max alone let a run against an edited manifest silently reuse the
## object built from the old one, so every downstream number described the old
## sample set while every message said otherwise.

## One tempdir per test, so a reload writing back to the cache cannot leak into
## the next one.
fixture <- function(n_cells = 3) {
    tmp <- withr::local_tempdir(.local_envir = parent.frame())
    s1 <- make_halo_csv(fs::path(tmp, "s1.csv"), n_cells)
    s2 <- make_halo_csv(fs::path(tmp, "s2.csv"), n_cells)
    s2b <- make_halo_csv(fs::path(tmp, "s2_other_copy.csv"), n_cells)
    list(
        dir = tmp,
        cache = as.character(fs::path(tmp, "scan_obj.rds")),
        m_one = make_manifest_csv(fs::path(tmp, "m_one.csv"), "S1", s1),
        m_two = make_manifest_csv(fs::path(tmp, "m_two.csv"), c("S1", "S2"), c(s1, s2)),
        m_two_rev = make_manifest_csv(fs::path(tmp, "m_two_rev.csv"), c("S2", "S1"), c(s2, s1)),
        m_two_alt = make_manifest_csv(fs::path(tmp, "m_two_alt.csv"), c("S1", "S2"), c(s1, s2b))
    )
}

build_cache <- function(fx, manifest, n_max = Inf) {
    suppressMessages(load_manifest(manifest, cache_rds = fx$cache, n_max = n_max))
}

## --- the test that would have caught the defect ---------------------------

test_that("a one-sample manifest against a two-sample cache returns one sample", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    obj <- suppressMessages(
        load_manifest(fx$m_one, cache_rds = fx$cache, n_max = Inf)
    )

    ## assert on the data, not the message: this is what was silently wrong
    expect_equal(sort(unique(obj$cell.data$Sample)), "S1")
    expect_equal(nrow(obj$manifest), 1L)
})

## --- the truth table -------------------------------------------------------

test_that("same manifest and same n_max reuses the cache", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    expect_true(cache_was_used(
        load_manifest(fx$m_two, cache_rds = fx$cache, n_max = Inf)
    ))
})

test_that("same manifest, different n_max reloads and says so", {
    fx <- fixture()
    build_cache(fx, fx$m_two, n_max = Inf)

    expect_message(
        load_manifest(fx$m_two, cache_rds = fx$cache, n_max = 2),
        "n_max"
    )
    expect_false(cache_was_used(
        load_manifest(fx$m_two, cache_rds = fx$cache, n_max = 1)
    ))
})

test_that("a different sample set reloads and says the manifest differs", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    expect_message(
        load_manifest(fx$m_one, cache_rds = fx$cache, n_max = Inf),
        "cache manifest differs"
    )
})

test_that("the same samples pointing at different files reloads", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    expect_message(
        load_manifest(fx$m_two_alt, cache_rds = fx$cache, n_max = Inf),
        "cache manifest differs"
    )
})

test_that("the same samples in a different order reloads", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    expect_message(
        load_manifest(fx$m_two_rev, cache_rds = fx$cache, n_max = Inf),
        "cache manifest differs"
    )
})

test_that("a cached object with no usable manifest reloads rather than hitting", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    ## what a cache written by an older version looks like. identical(NULL, NULL)
    ## is TRUE, so this is the case that has to be tested for explicitly -- and
    ## these are the caches most likely to be stale.
    stale <- readRDS(fx$cache)
    stale$manifest <- NULL
    saveRDS(stale, fx$cache)

    expect_message(
        obj <- load_manifest(fx$m_two, cache_rds = fx$cache, n_max = Inf),
        "cache manifest differs"
    )
    expect_false(is.null(obj$manifest))
})

## --- a hit is still a faithful hit -----------------------------------------

test_that("a cache hit returns what a fresh load returns", {
    fx <- fixture()
    fresh <- build_cache(fx, fx$m_two)

    hit <- suppressMessages(
        load_manifest(fx$m_two, cache_rds = fx$cache, n_max = Inf)
    )

    expect_equal(hit, fresh)
})

test_that("--refresh ignores a cache that would otherwise hit", {
    fx <- fixture()
    build_cache(fx, fx$m_two)

    expect_false(cache_was_used(
        load_manifest(fx$m_two, cache_rds = fx$cache, refresh = TRUE, n_max = Inf)
    ))
})
