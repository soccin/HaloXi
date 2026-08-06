test_that("the priority order is rendered as a table for sign-off", {

    rules <- fixture_rules()
    rules$conflict_resolution$notes <- "Priority decides only what a mixed cell is reported as."
    md <- rules_md_lines(rules)

    expect_true(any(grepl("Which type wins when a cell matches several", md)))
    expect_true(any(grepl("| Cell type | Priority | Applies when", md, fixed = TRUE)))
    ## the conditional entry, in plain marker syntax
    expect_true(any(grepl("| Tumor | 100 | M1+ and M2+ |", md, fixed = TRUE)))
    ## the fallback entry and the default_rank catch-all
    expect_true(any(grepl("| Tumor | 10 | any other cell of this type |", md, fixed = TRUE)))
    expect_true(any(grepl("| every other type | 50 | (always) |", md, fixed = TRUE)))
    ## notes verbatim
    expect_true(any(grepl("reported as.", md, fixed = TRUE)))
})

test_that("the stale no-priority wording is gone when priority is active", {

    md <- rules_md_lines(fixture_rules())

    expect_false(any(grepl("not yet resolving by priority", md, fixed = TRUE)))
    expect_false(any(grepl("without a priority order", md, fixed = TRUE)))
})

test_that("rules without conflict_resolution render the first-pass wording", {

    md <- rules_md_lines(fixture_rules(conflict = FALSE))

    expect_false(any(grepl("Which type wins", md)))
    expect_true(any(grepl("not yet resolving by priority", md, fixed = TRUE)))
    expect_true(any(grepl("without a priority order", md, fixed = TRUE)))
})

test_that("the rendered document has no bare horizontal rule", {

    ## a bare --- is parsed as a YAML block when this text is inlined into the
    ## report Rmd, which breaks the render
    for (rules in list(fixture_rules(), fixture_rules(conflict = FALSE))) {
        expect_false(any(trimws(rules_md_lines(rules)) == "---"))
    }
})
