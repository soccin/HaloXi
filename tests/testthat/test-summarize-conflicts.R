## An annotated object stub: summarize_conflicts() only reads cell.data.
conflict_obj <- function() {
    wide <- fixture_wide(
        M1 = c(TRUE,  TRUE,  FALSE, FALSE, TRUE,  NA),
        M2 = c(TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE),
        M3 = c(TRUE,  TRUE,  TRUE,  FALSE, FALSE, TRUE),
        M4 = c(FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE)
    )
    lineage <- annotate_lineage(wide, fixture_rules())

    list(cell.data = dplyr::bind_cols(
        dplyr::select(wide, UUID, Sample),
        lineage
    ))
}

test_that("summarize_conflicts reports one row per sample x conflict x outcome", {

    got <- summarize_conflicts(conflict_obj())

    expect_named(got, c("Sample", "Conflict", "Outcome", "Resolved", "nCells"))
    expect_equal(sum(got$nCells), 4L)          # 4 of the 6 cells matched >1 lineage

    resolved <- dplyr::filter(got, Resolved)
    expect_equal(sort(resolved$Outcome), c("T", "Tumor"))
    expect_equal(sum(resolved$nCells), 2L)

    unresolved <- dplyr::filter(got, !Resolved)
    expect_true(all(unresolved$Outcome == "UNKNOWN"))
    expect_equal(sort(unresolved$Conflict), c("T+Mac", "Tumor+T"))
})

test_that("summarize_conflicts counts reconcile with TypeCall", {

    obj <- conflict_obj()
    got <- summarize_conflicts(obj)

    from_calls <- table(obj$cell.data$TypeCall)
    expect_equal(sum(got$nCells[got$Resolved]), unname(from_calls[["priority"]]))
    expect_equal(sum(got$nCells[!got$Resolved]), unname(from_calls[["conflict"]]))
})

test_that("summarize_conflicts errors on an object predating the provenance columns", {

    obj <- conflict_obj()
    obj$cell.data <- dplyr::select(obj$cell.data, -TypeConflict)

    expect_error(summarize_conflicts(obj), "TypeConflict")
})
