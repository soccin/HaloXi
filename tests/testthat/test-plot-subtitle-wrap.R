## The composition subtitle carries the disclosure that a priority order
## reassigned cells. At the width the driver saves these plots it used to run
## off the canvas and get cut mid-word, so the disclosure was the part the
## collaborator never saw. These tests hold the layout fix in place and, more
## importantly, stop a future "fix" that shortens the string instead.

comp_fixture <- function(types) {
    tibble::tibble(
        Sample   = "S1",
        CellType = types,
        nCells   = seq_along(types) * 10L,
        pct      = round(100 * seq_along(types) / sum(seq_along(types)), 2)
    )
}

subtitle_of <- function(...) {
    plot_celltype_composition(comp_fixture(c("Tumor", "T", "Mac")), ...)$labels$subtitle
}

## the joined, unwrapped string the builder is meant to be rendering
joined <- function(rules, priority) {
    paste(
        c(
            glue::glue("{rules$labels$unknown} = multi-lineage conflict"),
            glue::glue("{rules$labels$unclassified} = no lineage marker"),
            if (priority) "multi-type cells settled by the rules' priority order"
            else "first-pass (no priority)"
        ),
        collapse = "; "
    )
}

test_that("the priority subtitle wraps rather than overflowing", {

    skip_if_not_installed("ggplot2")

    sub <- subtitle_of(rules = fixture_rules(), priority = TRUE)

    expect_match(sub, "\n", fixed = TRUE)
    expect_true(all(nchar(strsplit(sub, "\n", fixed = TRUE)[[1]]) <= 90))
})

## SPEC_HaloXi_plotlabels_260806 section 4 predicts one line here and three in
## the long-label case. Both counts are out: with the default labels the
## no-priority string is 93 characters, and the 40-character-label string is
## 153, so at width 90 they take two lines each rather than one and three. The
## requirement the spec is actually making -- nothing clipped, nothing lost --
## is what is asserted; the line counts were arithmetic, not a decision.
test_that("the no-priority subtitle also stays inside the width", {

    skip_if_not_installed("ggplot2")

    sub <- subtitle_of(rules = fixture_rules(), priority = FALSE)

    expect_true(all(nchar(strsplit(sub, "\n", fixed = TRUE)[[1]]) <= 90))
})

test_that("a long renamed label wraps further, still within the width", {

    skip_if_not_installed("ggplot2")

    rules <- fixture_rules()
    rules$labels$unknown <- strrep("A", 40)

    sub <- subtitle_of(rules = rules, priority = TRUE)
    lines <- strsplit(sub, "\n", fixed = TRUE)[[1]]

    expect_gte(length(lines), 2)
    expect_true(all(nchar(lines) <= 90))
})

test_that("wrapping changes layout only -- no character is lost", {

    skip_if_not_installed("ggplot2")

    rules <- fixture_rules()
    long  <- fixture_rules()
    long$labels$unknown <- strrep("A", 40)

    cases <- list(
        list(rules = rules, priority = TRUE),
        list(rules = rules, priority = FALSE),
        list(rules = long,  priority = TRUE)
    )

    for (case in cases) {
        sub <- subtitle_of(rules = case$rules, priority = case$priority)
        expect_equal(gsub("\n", " ", sub, fixed = TRUE),
                     joined(case$rules, case$priority))
    }
})
