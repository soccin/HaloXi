## resolve_cache_path(): where a driver's loaded-object cache goes.
##
## The rule under test: never inside the output directory. OUTDIR is a
## deliverable and the cache is a build artifact of several hundred MB.

test_that("the default is cache/<name of OUTDIR>/scan_obj.rds, outside OUTDIR", {
    expect_equal(resolve_cache_path("results/scan"),
                 "cache/scan/scan_obj.rds")
    expect_equal(resolve_cache_path("results/run04/annot"),
                 "cache/annot/scan_obj.rds")
    expect_equal(resolve_cache_path("results/run04/spatial"),
                 "cache/spatial/scan_obj.rds")
})

test_that("a trailing slash on OUTDIR makes no difference", {
    expect_equal(resolve_cache_path("results/scan/"),
                 resolve_cache_path("results/scan"))
})

test_that("--cache= is returned unchanged, relative or absolute", {
    expect_equal(resolve_cache_path("results/run04/annot", "cache/run04/scan_obj.rds"),
                 "cache/run04/scan_obj.rds")
    expect_equal(resolve_cache_path("results/run04/annot", "/abs/path/x.rds"),
                 "/abs/path/x.rds")
})

test_that("an empty --cache= falls through to the default", {
    expect_equal(resolve_cache_path("results/scan", ""),
                 "cache/scan/scan_obj.rds")
})

test_that("a degenerate OUTDIR still resolves to a usable path", {
    for (outdir in c(".", "..", "/", "")) {
        expect_equal(resolve_cache_path(outdir), "cache/halo/scan_obj.rds",
                     info = paste0("outdir = [", outdir, "]"))
    }
})

test_that("a cache left where older versions put it is named, not used", {
    tmp <- withr::local_tempdir()
    withr::local_dir(tmp)
    fs::dir_create(fs::path("results", "annot", "cache"))
    legacy <- fs::path("results", "annot", "cache", "scan_obj.rds")
    saveRDS(list(), legacy)

    expect_message(
        got <- resolve_cache_path("results/annot"),
        "ignoring the cache at"
    )
    ## named, but NOT used: the returned path is still the new default
    expect_equal(got, "cache/annot/scan_obj.rds")
})

test_that("no migration message when --cache= was given", {
    tmp <- withr::local_tempdir()
    withr::local_dir(tmp)
    fs::dir_create(fs::path("results", "annot", "cache"))
    saveRDS(list(), fs::path("results", "annot", "cache", "scan_obj.rds"))

    expect_no_message(resolve_cache_path("results/annot", "cache/run04/scan_obj.rds"))
})

test_that("resolve_cache_path creates nothing", {
    tmp <- withr::local_tempdir()
    withr::local_dir(tmp)

    resolve_cache_path("results/annot")

    expect_false(fs::dir_exists("cache"))
    expect_false(fs::dir_exists("results"))
})
