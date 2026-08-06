test_that("a valid rules file with conflict_resolution reads back intact", {

    path <- write_rules_yaml(fixture_rules())
    rules <- read_cell_rules(path)

    expect_equal(rules$conflict_resolution$policy, "priority")
    expect_equal(rules$conflict_resolution$default_rank, 50)
    expect_equal(rules$marker_levels, c("M1", "M2", "M3", "M4"))
})

test_that("a when: clause naming an undeclared marker errors", {

    rules <- fixture_rules()
    rules$conflict_resolution$ranks[[1]]$when$all_pos <- c("M1", "M99")

    expect_error(read_cell_rules(write_rules_yaml(rules)), "M99")
})

test_that("a ranks: entry naming an undeclared lineage errors", {

    rules <- fixture_rules()
    rules$conflict_resolution$ranks[[1]]$lineage <- "Fibroblast"

    expect_error(read_cell_rules(write_rules_yaml(rules)), "Fibroblast")
})

test_that("an unknown top-level key errors instead of being ignored", {

    ## the whole point: an engine that shrugs at a key it does not know produces
    ## old-policy numbers from a file that reads as though the new policy ran
    rules <- fixture_rules()
    rules$spatial_analysis <- list(radius = 30)

    expect_error(read_cell_rules(write_rules_yaml(rules)), "spatial_analysis")
})

test_that("an unknown conflict_resolution sub-key errors", {

    rules <- fixture_rules()
    rules$conflict_resolution$tie_break <- "first"

    expect_error(read_cell_rules(write_rules_yaml(rules)), "tie_break")
})

test_that("an unknown when: sub-key errors", {

    ## a typo'd any_neg: must fail loudly, not silently drop the condition
    rules <- fixture_rules()
    rules$conflict_resolution$ranks[[1]]$when$any_neg <- "M3"

    expect_error(read_cell_rules(write_rules_yaml(rules)), "any_neg")
})

test_that("an unknown ranks: sub-key errors", {

    rules <- fixture_rules()
    rules$conflict_resolution$ranks[[1]]$unless <- list(all_pos = "M3")

    expect_error(read_cell_rules(write_rules_yaml(rules)), "unless")
})

test_that("rules without conflict_resolution still validate", {

    expect_no_error(read_cell_rules(write_rules_yaml(fixture_rules(conflict = FALSE))))
})
