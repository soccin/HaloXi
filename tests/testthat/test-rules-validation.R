## Every way a rules file can be wrong must stop the run and name the offending
## key. The exhaustion applies_to case is the one that matters most: before
## HaloXi 1.3 it was completely silent -- the Exhausted column came back all NA,
## indistinguishable from "that marker was not in the panel".

test_that("a states: parent that is not a declared lineage errors", {

    rules <- fixture_rules(conflict = FALSE)
    rules$states <- list(Fibroblast = list(Activated = list(pos = "M2")))

    expect_error(read_cell_rules(write_rules_yaml(rules)), "Fibroblast")
})

test_that("a states: parent that is a declared lineage passes", {

    rules <- fixture_rules(conflict = FALSE)
    rules$states <- list(Tumor = list(Cycling = list(pos = "M2")))

    expect_no_error(read_cell_rules(write_rules_yaml(rules)))
})

test_that("an exhaustion applies_to naming an undeclared lineage errors", {

    rules <- fixture_rules(conflict = FALSE)
    rules$exhaustion <- list(applies_to = c("T", "Ghost"), any_pos = "M4")

    expect_error(read_cell_rules(write_rules_yaml(rules)), "Ghost")
})

test_that("an exhaustion applies_to naming declared lineages passes", {

    rules <- fixture_rules(conflict = FALSE)
    rules$exhaustion <- list(applies_to = c("T", "Mac"), any_pos = "M4")

    expect_no_error(read_cell_rules(write_rules_yaml(rules)))
})

test_that("labels: missing unknown errors and names it", {

    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unknown <- NULL

    expect_error(read_cell_rules(write_rules_yaml(rules)), "unknown")
})

test_that("labels: missing unclassified errors and names it", {

    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unclassified <- NULL

    expect_error(read_cell_rules(write_rules_yaml(rules)), "unclassified")
})

test_that("labels: an empty value errors", {

    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unknown <- ""

    expect_error(read_cell_rules(write_rules_yaml(rules)), "unknown")
})

test_that("a typo'd labels: key errors instead of being dropped", {

    ## the typo would otherwise leave unclassified at its (absent) default while
    ## the file reads as though it had been renamed
    rules <- fixture_rules(conflict = FALSE)
    rules$labels$unclassifed <- "NO MATCH"

    expect_error(read_cell_rules(write_rules_yaml(rules)), "unclassifed")
})
