## The conflict audit has to reach the collaborator-facing outputs. A priority
## order reassigns whole populations between cell types; if the workbook and the
## plot subtitle say nothing about it, nothing in the deliverable does.

wb_fixture <- function() {
    list(
        ct = list(
            wide = tibble::tibble(CellType = c("A", "B"), S1 = c(10L, 5L)),
            long = tibble::tibble(Sample = "S1", CellType = c("A", "B"),
                                  nCells = c(10L, 5L), pct = c(66.67, 33.33))
        ),
        states = tibble::tibble(Sample = "S1", State = "A_Cycling",
                                nScored = 10L, nPos = 4L, pctPos = 40)
    )
}

conflict_fixture <- function(n = 1L) {
    tibble::tibble(
        Sample   = rep("S1", n),
        Conflict = rep("A+B", n),
        Outcome  = rep("A", n),
        Resolved = rep(TRUE, n),
        nCells   = rep(7L, n)
    )
}

test_that("no conflict summary writes the three original sheets", {

    skip_if_not_installed("openxlsx")

    f <- wb_fixture()
    p <- write_annotation_workbook(f$ct, f$states, tempfile())

    expect_equal(openxlsx::getSheetNames(p),
                 c("Composition (counts)", "Composition (long)", "States"))
})

test_that("a non-empty conflict summary adds a fourth Conflicts sheet", {

    skip_if_not_installed("openxlsx")

    f <- wb_fixture()
    p <- write_annotation_workbook(f$ct, f$states, tempfile(), conflict_fixture())

    sheets <- openxlsx::getSheetNames(p)
    expect_length(sheets, 4)
    expect_equal(sheets[4], "Conflicts")
})

test_that("a zero-row conflict summary writes no Conflicts sheet", {

    skip_if_not_installed("openxlsx")

    ## a blank tab would read as "we looked and there were conflicts", so the
    ## sheet is omitted rather than written empty
    f <- wb_fixture()
    p <- write_annotation_workbook(f$ct, f$states, tempfile(), conflict_fixture(0L))

    expect_length(openxlsx::getSheetNames(p), 3)
})

test_that("the composition subtitle reports which conflict policy ran", {

    skip_if_not_installed("ggplot2")

    long <- tibble::tibble(Sample = "S1", CellType = c("A", "B"),
                           nCells = c(10L, 5L), pct = c(66.67, 33.33))

    first_pass <- plot_celltype_composition(long)
    with_prio  <- plot_celltype_composition(long, priority = TRUE)

    expect_match(as.character(first_pass$labels$subtitle), "first-pass \\(no priority\\)")
    expect_match(as.character(with_prio$labels$subtitle), "priority order")
    expect_no_match(as.character(with_prio$labels$subtitle), "first-pass")
})

test_that("the subtitle names the rules' own bucket labels", {

    skip_if_not_installed("ggplot2")

    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unknown <- "AMBIGUOUS"
    long <- tibble::tibble(Sample = "S1", CellType = "Tumor",
                           nCells = 10L, pct = 100)

    p <- plot_celltype_composition(long, rules = rules)

    expect_match(as.character(p$labels$subtitle), "AMBIGUOUS = multi-lineage conflict")
})
