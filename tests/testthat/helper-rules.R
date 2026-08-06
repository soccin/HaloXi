## Synthetic fixtures for the annotation tests.
##
## Deliberately study-agnostic and built in-test: markers M1..M4, lineages
## Tumor/T/Mac. Nothing here reads project data.

## Lineages: Tumor = M1 or M2; T = M3; Mac = M4.
## Conflict order: Tumor outranks everything when BOTH its markers are positive,
## and is outranked by everything when only one is (rank 10 fallback). Every
## other lineage sits on default_rank 50.
fixture_rules <- function(conflict = TRUE) {

    rules <- list(
        markers  = list(M1 = "M1", M2 = "M2", M3 = "M3", M4 = "M4"),
        lineages = list(
            Tumor = list(any_pos = c("M1", "M2")),
            T     = list(any_pos = "M3"),
            Mac   = list(any_pos = "M4")
        ),
        labels = list(unknown = "UNKNOWN", unclassified = "UNCLASSIFIED")
    )

    if (conflict) {
        rules$conflict_resolution <- list(
            policy       = "priority",
            default_rank = 50,
            ranks = list(
                list(lineage = "Tumor", rank = 100,
                     when = list(all_pos = c("M1", "M2"))),
                list(lineage = "Tumor", rank = 10)
            )
        )
    }

    rules
}

## A wide marker table in the shape marker_pos_wide() returns.
fixture_wide <- function(M1, M2, M3, M4) {
    tibble::tibble(
        UUID   = paste0("c", seq_along(M1)),
        Sample = "S1",
        M1 = M1, M2 = M2, M3 = M3, M4 = M4
    )
}

## Write a rules list out as YAML so read_cell_rules() can validate it.
## The file lands in the session tempdir, which testthat cleans up.
write_rules_yaml <- function(rules) {
    path <- tempfile(fileext = ".yaml")
    yaml::write_yaml(rules, path)
    path
}
