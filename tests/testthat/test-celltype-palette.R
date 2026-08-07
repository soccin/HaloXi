## Colours and bar ordering come from the rules, never from literal label
## strings. A study that renames its conflict bucket must still get that
## bucket's grey and its position at the end of the bar.

comp_fixture <- function(types) {
    tibble::tibble(
        Sample   = "S1",
        CellType = types,
        nCells   = seq_along(types) * 10L,
        pct      = round(100 * seq_along(types) / sum(seq_along(types)), 2)
    )
}

test_that("a lineage with no palette: entry still gets a stable colour", {

    rules <- fixture_rules(conflict = FALSE)
    levels <- c("Tumor", "T", "Mac")

    pal <- .celltype_palette(levels, rules)

    expect_equal(pal, .celltype_palette(levels, rules))
    expect_false(any(is.na(pal)))
    expect_false(any(pal == "grey65"))
})

test_that("a declared palette: colour wins over the generated one", {

    rules <- fixture_rules(conflict = FALSE)
    rules$palette <- list(Tumor = "#123456")

    expect_equal(.celltype_palette("Tumor", rules)[["Tumor"]], "#123456")
})

test_that("a renamed unknown label still gets a grey", {

    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unknown <- "AMBIGUOUS"

    pal <- .celltype_palette(c("Tumor", "AMBIGUOUS"), rules)

    expect_equal(pal[["AMBIGUOUS"]], "grey55")
})

test_that("a renamed unknown label still sorts into the set-aside group", {

    skip_if_not_installed("ggplot2")

    ## AMBIGUOUS carries the largest count, so were it treated as a lineage it
    ## would sort last, not first
    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unknown <- "AMBIGUOUS"

    p <- plot_celltype_composition(
        comp_fixture(c("Tumor", "T", "AMBIGUOUS")), rules = rules)

    expect_equal(levels(p$data$CellType)[1], "AMBIGUOUS")
})

test_that("the un-callable bucket is set aside with or without rules", {

    expect_equal(.celltype_palette(.NA_CELLTYPE)[[.NA_CELLTYPE]], "grey90")
    expect_true(.NA_CELLTYPE %in% .setaside_levels())
})

test_that("the palette and the plot still work with no rules at all", {

    skip_if_not_installed("ggplot2")

    pal <- .celltype_palette(c("A", "B"))
    expect_false(any(pal == "grey65"))

    p <- plot_celltype_composition(comp_fixture(c("A", "B")))
    expect_s3_class(p, "ggplot")
})
