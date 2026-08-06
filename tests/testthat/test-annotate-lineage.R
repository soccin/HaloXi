## SPEC_HaloXi_260805 section 8.1, in spec order. Each row becomes its own
## test, named by its marker pattern, so a failure identifies the case directly.
truth_table <- tibble::tribble(
    ~M1,   ~M2,   ~M3,   ~M4,   ~CellType,      ~TypeCall,
    TRUE,  TRUE,  TRUE,  FALSE, "Tumor",        "priority",
    TRUE,  FALSE, TRUE,  FALSE, "T",            "priority",
    FALSE, TRUE,  TRUE,  FALSE, "T",            "priority",
    TRUE,  TRUE,  FALSE, FALSE, "Tumor",        "single",
    TRUE,  FALSE, FALSE, FALSE, "Tumor",        "single",
    FALSE, FALSE, TRUE,  TRUE,  "UNKNOWN",      "conflict",
    FALSE, FALSE, TRUE,  FALSE, "T",            "single",
    FALSE, FALSE, FALSE, FALSE, "UNCLASSIFIED", "unclassified",
    TRUE,  TRUE,  TRUE,  TRUE,  "Tumor",        "priority",
    NA,    TRUE,  TRUE,  FALSE, "UNKNOWN",      "conflict"
)

truth_got <- annotate_lineage(
    fixture_wide(truth_table$M1, truth_table$M2, truth_table$M3, truth_table$M4),
    fixture_rules()
)

for (i in seq_len(nrow(truth_table))) {
    pattern <- paste0(
        "M1=", truth_table$M1[i], " M2=", truth_table$M2[i],
        " M3=", truth_table$M3[i], " M4=", truth_table$M4[i]
    )
    test_that(paste0("truth table row ", i, " (", pattern, ")"), {
        expect_equal(truth_got$CellType[i], truth_table$CellType[i])
        expect_equal(truth_got$TypeCall[i], truth_table$TypeCall[i])
    })
}

test_that("an un-callable priority condition does not fall through to the fallback", {

    ## The truth table's last row, stated on its own because it is the rule the
    ## whole design turns on: Tumor matches (M2+), but its rank-100 test
    ## all_pos: [M1, M2] is un-callable, so Tumor's rank is NA and LOCKED. The
    ## rank-10 fallback must not fire and hand the cell to T.
    wide <- fixture_wide(M1 = NA, M2 = TRUE, M3 = TRUE, M4 = FALSE)
    got <- annotate_lineage(wide, fixture_rules())

    expect_equal(got$CellType, "UNKNOWN")
    expect_equal(got$TypeCall, "conflict")
    expect_equal(got$TypeConflict, "Tumor+T")
})

test_that("without conflict_resolution every multi-lineage cell is UNKNOWN", {

    wide <- fixture_wide(
        M1 = c(TRUE,  TRUE,  FALSE, FALSE, TRUE,  FALSE),
        M2 = c(TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE),
        M3 = c(TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE),
        M4 = c(FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE)
    )

    got <- annotate_lineage(wide, fixture_rules(conflict = FALSE))

    ## identical to what the pre-change code produced for the same input
    expect_equal(
        got$CellType,
        c("UNKNOWN", "UNKNOWN", "UNKNOWN", "T", "Tumor", "UNCLASSIFIED")
    )
    expect_equal(
        got$TypeCall,
        c("conflict", "conflict", "conflict", "single", "single", "unclassified")
    )

    multi <- got$TypeCall == "conflict"
    expect_true(all(got$CellType[multi] == "UNKNOWN"))
})

test_that("a policy other than priority leaves resolution unchanged", {

    rules <- fixture_rules()
    rules$conflict_resolution$policy <- "none"

    wide <- fixture_wide(M1 = TRUE, M2 = TRUE, M3 = TRUE, M4 = FALSE)
    got <- annotate_lineage(wide, rules)

    ## the same cell resolves to Tumor under policy: priority (truth table row 1)
    expect_equal(got$CellType, "UNKNOWN")
    expect_equal(got$TypeCall, "conflict")
})

test_that("lineages tied on default_rank stay UNKNOWN", {

    ## T and Mac both sit on default_rank 50 and neither has a ranks entry
    wide <- fixture_wide(M1 = FALSE, M2 = FALSE, M3 = TRUE, M4 = TRUE)
    got <- annotate_lineage(wide, fixture_rules())

    expect_equal(got$CellType, "UNKNOWN")
    expect_equal(got$TypeCall, "conflict")
    expect_equal(got$TypeConflict, "T+Mac")
})

test_that("an unmatched lineage cannot win however high its rank", {

    ## Mac is promoted above everything, but this cell is M4-negative: it never
    ## matched Mac, so the conflict is still Tumor vs T and Tumor's rank-10
    ## fallback loses to T's default 50.
    rules <- fixture_rules()
    rules$conflict_resolution$ranks <- c(
        rules$conflict_resolution$ranks,
        list(list(lineage = "Mac", rank = 999))
    )

    wide <- fixture_wide(M1 = TRUE, M2 = FALSE, M3 = TRUE, M4 = FALSE)
    got <- annotate_lineage(wide, rules)

    expect_equal(got$CellType, "T")
    expect_equal(got$TypeCall, "priority")
})

test_that("TypeConflict records the full matched set even when priority resolves it", {

    ## truth table rows 1, 2 and 9: all resolved, all still reporting what they
    ## matched. Without this a priority rule could absorb a population silently.
    wide <- fixture_wide(
        M1 = c(TRUE, TRUE,  TRUE),
        M2 = c(TRUE, FALSE, TRUE),
        M3 = c(TRUE, TRUE,  TRUE),
        M4 = c(FALSE, FALSE, TRUE)
    )
    got <- annotate_lineage(wide, fixture_rules())

    expect_equal(got$TypeCall, rep("priority", 3))
    expect_equal(got$CellType, c("Tumor", "T", "Tumor"))
    expect_equal(got$TypeConflict, c("Tumor+T", "Tumor+T", "Tumor+T+Mac"))
})

test_that("TypeConflict is NA for cells that matched at most one lineage", {

    wide <- fixture_wide(
        M1 = c(TRUE,  FALSE, FALSE),
        M2 = c(FALSE, FALSE, FALSE),
        M3 = c(FALSE, TRUE,  FALSE),
        M4 = c(FALSE, FALSE, FALSE)
    )
    got <- annotate_lineage(wide, fixture_rules())

    expect_equal(got$CellType, c("Tumor", "T", "UNCLASSIFIED"))
    expect_true(all(is.na(got$TypeConflict)))
})

test_that("TypeConflict names lineages in rules order, not match order", {

    ## rules order is Tumor, T, Mac; the cell below matches T and Mac only
    wide <- fixture_wide(M1 = FALSE, M2 = FALSE, M3 = TRUE, M4 = TRUE)
    expect_equal(annotate_lineage(wide, fixture_rules())$TypeConflict, "T+Mac")
})

test_that("a cell with no callable lineage is uncallable, not unclassified", {

    ## every lineage marker missing from this sample's panel
    wide <- fixture_wide(M1 = NA, M2 = NA, M3 = NA, M4 = NA)
    got <- annotate_lineage(wide, fixture_rules())

    expect_true(is.na(got$CellType))
    expect_equal(got$TypeCall, "uncallable")
})

test_that("annotate_lineage returns one row per input cell", {

    wide <- fixture_wide(
        M1 = c(TRUE, FALSE, NA), M2 = c(FALSE, FALSE, TRUE),
        M3 = c(TRUE, FALSE, TRUE), M4 = c(FALSE, TRUE, FALSE)
    )
    got <- annotate_lineage(wide, fixture_rules())

    expect_s3_class(got, "tbl_df")
    expect_equal(nrow(got), nrow(wide))
    expect_named(got, c("CellType", "TypeCall", "TypeConflict"))
})
