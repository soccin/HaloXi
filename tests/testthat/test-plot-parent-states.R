## The parent-state plot selects its rows by a FIXED prefix. A regex built by
## pasting the tag in would let the tag "T" swallow every "Tc_*" row.

state_fixture <- function() {
    tibble::tibble(
        Sample  = "S1",
        State   = c("T_CD4", "T_CD8", "Tc_Exhausted"),
        nScored = 10L,
        nPos    = c(4L, 3L, 2L),
        pctPos  = c(40, 30, 20)
    )
}

test_that("NULL when no state carries the tag", {

    skip_if_not_installed("ggplot2")

    expect_null(plot_parent_states(state_fixture(), "Mac"))
})

test_that("a tag never picks up a longer tag's rows", {

    skip_if_not_installed("ggplot2")

    p <- plot_parent_states(state_fixture(), "T")

    expect_setequal(as.character(p$data$State), c("CD4", "CD8"))
})

test_that("the tag prefix is stripped and parent_label titles the plot", {

    skip_if_not_installed("ggplot2")

    p <- plot_parent_states(state_fixture(), "Tc", parent_label = "T cell")

    expect_equal(as.character(p$data$State), "Exhausted")
    expect_match(as.character(p$labels$title), "T cell states")
    expect_match(as.character(p$labels$y), "% of T cell cells")
})

test_that("parent_label defaults to the tag", {

    skip_if_not_installed("ggplot2")

    p <- plot_parent_states(state_fixture(), "Tc")

    expect_match(as.character(p$labels$title), "^Tc states")
})
