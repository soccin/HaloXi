## State column names come from the rules file, not from a map of cell-type
## names baked into the engine. Before HaloXi 1.3 that map covered seven names
## and crashed with "subscript out of bounds" on anything else.

## rules giving each named parent one state, so state_tags() has work to do
tag_rules <- function(parents, declared = NULL) {
    one_state <- rep(list(list(Cycling = list(pos = "M2"))), length(parents))
    rules <- list(
        markers  = list(M1 = "M1", M2 = "M2"),
        lineages = stats::setNames(
            rep(list(list(any_pos = "M1")), length(parents)), parents),
        states   = stats::setNames(one_state, parents),
        labels   = list(unknown = "UNKNOWN", unclassified = "UNCLASSIFIED")
    )
    if (!is.null(declared)) rules$state_tags <- declared
    rules
}

test_that("a tag is derived from the parent name when none is declared", {

    expect_equal(state_tags(tag_rules("Tumor"))[["Tumor"]], "Tumor")
    expect_equal(state_tags(tag_rules("T cell"))[["T cell"]], "Tcell")
    expect_equal(state_tags(tag_rules("NK cell"))[["NK cell"]], "NKcell")
    expect_equal(state_tags(tag_rules("B-cell"))[["B-cell"]], "Bcell")
})

test_that("a declared tag wins over the derived one", {

    expect_equal(
        state_tags(tag_rules("T cell", list(`T cell` = "T")))[["T cell"]], "T")
    expect_equal(
        state_tags(tag_rules("Macrophage", list(Macrophage = "Mac")))[["Macrophage"]],
        "Mac")
})

test_that("rules defining no states yield no tags", {

    expect_length(state_tags(fixture_rules(conflict = FALSE)), 0)
})

test_that("a derived tag that is not a legal column name errors", {

    ## "7 cell" derives "7cell", which cannot start a column name
    expect_error(read_cell_rules(write_rules_yaml(tag_rules("7 cell"))), "7 cell")
})

test_that("a declared tag that is not a legal column name errors", {

    rules <- tag_rules("Tumor", list(Tumor = "Tumor cell"))

    expect_error(read_cell_rules(write_rules_yaml(rules)), "Tumor")
})

test_that("two parents deriving the same tag error, naming both and the tag", {

    err <- expect_error(
        read_cell_rules(write_rules_yaml(tag_rules(c("T cell", "T-cell")))))

    expect_match(conditionMessage(err), "T cell")
    expect_match(conditionMessage(err), "T-cell")
    expect_match(conditionMessage(err), "Tcell")
})

test_that("a state_tags: key that is not a declared lineage errors, naming it", {

    rules <- tag_rules("Tumor", list(Fibroblast = "Fib"))

    expect_error(read_cell_rules(write_rules_yaml(rules)), "Fibroblast")
})

test_that("annotate_states works for a parent the engine never knew about", {

    ## the regression test for the hardcoded-map crash
    rules <- tag_rules("Fibroblast")
    wide <- tibble::tibble(UUID = c("c1", "c2"), Sample = "S1",
                           M1 = c(TRUE, TRUE), M2 = c(TRUE, FALSE))

    st <- annotate_states(wide, c("Fibroblast", "Fibroblast"), rules)

    expect_named(st, "Fibroblast_Cycling")
    expect_equal(st$Fibroblast_Cycling, c(TRUE, FALSE))
})

test_that("declared tags reproduce the column names the old hardcoded map gave", {

    ## backward compatibility: this is how a study keeps its existing outputs
    parents <- c("Tumor", "T cell", "Macrophage")
    rules <- tag_rules(
        parents, list(Tumor = "Tumor", `T cell` = "T", Macrophage = "Mac"))
    wide <- tibble::tibble(UUID = "c1", Sample = "S1", M1 = TRUE, M2 = TRUE)

    st <- annotate_states(wide, "Tumor", rules)

    expect_named(st, c("Tumor_Cycling", "T_Cycling", "Mac_Cycling"))
})
