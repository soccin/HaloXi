#' @import dplyr
#' @import tidyr
#' @import purrr

suppressPackageStartupMessages({
    require(dplyr)
    require(tidyr)
    require(purrr)
})

## ---------------------------------------------------------------------------
## Cell annotation: markers -> cell types and states.
##
## Turns the combined object from load_manifest() (cell.data + long
## marker.data) into per-cell type/state calls, driven entirely by a declarative
## rules file the CALLER supplies (read_cell_rules(path)). This package is
## project-agnostic: it interprets the grammar of a rules file (lineages,
## states, NA propagation) but ships no rules of its own -- the rules are a
## project artifact, passed in exactly like the sample manifest is. The same
## rules object is rendered to a human-readable spec by render_cell_rules.R, so
## the logic the biologist signs off on and the logic that runs here are one
## artifact.
##
## First-pass policy (NOT a priority tree):
##   * a cell matching >1 lineage          -> UNKNOWN      (true conflict)
##   * a cell matching 0 lineages          -> UNCLASSIFIED
##   * a required marker absent from a
##     sample's panel                      -> NA           (un-callable)
##
## Sub-states (tumor flags, T subsets, M1/M2, exhaustion) are evaluated ONLY on
## a cleanly-resolved parent lineage; everything else gets NA for them.
## ---------------------------------------------------------------------------

#' Read and validate the cell-annotation rules
#'
#' The rules file is a *project* artifact (it encodes one study's marker panel
#' and cell-type definitions), so the caller must supply its path -- this
#' package bundles no rules of its own. This mirrors [read_manifest()], which
#' likewise takes a project file path.
#'
#' @param path Path to the rules YAML (required).
#'
#' @return The parsed rules as a list, with an added `marker_levels` element
#'   (the friendly marker names) for convenience. Stops if `path` is missing,
#'   the file does not exist, or a rule references a marker not declared in
#'   `markers:`.
#'
#' @export
read_cell_rules <- function(path) {

    if (missing(path) || is.null(path) || !nzchar(path)) {
        stop("read_cell_rules: 'path' to the rules YAML is required (this package ships no default rules).")
    }
    if (!fs::file_exists(path)) {
        stop(glue::glue("read_cell_rules: rules file not found: {path}"))
    }

    rules <- yaml::read_yaml(path)

    needed <- c("markers", "lineages", "labels")
    miss <- setdiff(needed, names(rules))
    if (length(miss) > 0) {
        stop(glue::glue("read_cell_rules: rules missing section(s): {paste(miss, collapse=', ')}"))
    }

    declared <- names(rules$markers)

    ## every marker referenced anywhere must be declared in markers:
    referenced <- c(
        purrr::map(rules$lineages, ~ c(.x$require_pos, .x$require_neg)),
        purrr::map(rules$states, function(parent) {
            purrr::map(parent, ~ .x$pos)
        }),
        list(rules$exhaustion$any_pos),
        list(rules$controls)
    ) |> unlist() |> unique()
    referenced <- referenced[!is.na(referenced)]

    undeclared <- setdiff(referenced, declared)
    if (length(undeclared) > 0) {
        stop(glue::glue(
            "read_cell_rules: rule(s) reference undeclared marker(s): ",
            "{paste(undeclared, collapse=', ')}"
        ))
    }

    rules$marker_levels <- declared
    rules$rules_path <- as.character(fs::path_abs(path))
    rules
}

#' Pivot marker.data to one logical column per marker, per cell
#'
#' Builds a wide cell x marker table of positivity from the combined object.
#' Crucially, a marker that is **absent from a sample's panel** is `NA` for that
#' sample's cells (not `FALSE`): un-callable, never silently negative. This is
#' what makes downstream calls collapse to `NA` rather than a wrong answer on
#' partial panels.
#'
#' Columns are named by the friendly marker name from the rules (e.g. `CD30`),
#' resolved from the data's `MarkerNorm` via the rules' `markers:` map. Data
#' markers not present in the rules are dropped (with the exception that nothing
#' breaks if they are missing).
#'
#' @param obj A combined object from [load_manifest()].
#' @param rules Parsed rules from [read_cell_rules()].
#'
#' @return A tibble with `UUID`, `Sample`, and one logical column per friendly
#'   marker name. `TRUE`/`FALSE` where scored, `NA` where the marker is absent
#'   from that cell's sample panel.
#'
#' @export
marker_pos_wide <- function(obj, rules) {

    ## friendly name <-> data MarkerNorm
    map_tbl <- tibble::tibble(
        Friendly   = names(rules$markers),
        MarkerNorm = toupper(unlist(rules$markers, use.names = FALSE))
    )

    ## which (sample, marker) pairs were actually measured -> presence
    measured <- obj$marker.data |>
        left_join(obj$cell.data |> select(UUID, Sample), by = "UUID") |>
        distinct(Sample, MarkerNorm)

    ## long positivity, friendly-named, restricted to rule markers
    long_pos <- obj$marker.data |>
        left_join(obj$cell.data |> select(UUID, Sample), by = "UUID") |>
        mutate(MarkerNorm = toupper(MarkerNorm)) |>
        inner_join(map_tbl, by = "MarkerNorm") |>
        mutate(Pos = Positive == 1) |>
        select(UUID, Sample, Friendly, Pos)

    wide <- long_pos |>
        pivot_wider(names_from = Friendly, values_from = Pos)

    ## ensure every friendly marker is a column even if never measured anywhere
    for (fm in map_tbl$Friendly) {
        if (!fm %in% names(wide)) wide[[fm]] <- NA
    }

    ## guarantee logical type (pivot can yield list/logical depending on data)
    marker_cols <- intersect(map_tbl$Friendly, names(wide))
    wide <- wide |>
        mutate(across(all_of(marker_cols), ~ as.logical(.x)))

    wide |> arrange(Sample, UUID)
}

## three-valued AND across a set of logical columns: TRUE only if all TRUE,
## FALSE if any FALSE, NA if no FALSE but some NA. Empty set -> all TRUE.
.all_true <- function(df, cols) {
    if (length(cols) == 0) return(rep(TRUE, nrow(df)))
    m <- as.matrix(df[, cols, drop = FALSE])
    any_false <- rowSums(m == FALSE, na.rm = TRUE) > 0
    any_na    <- rowSums(is.na(m)) > 0
    out <- rep(TRUE, nrow(df))
    out[any_na]    <- NA
    out[any_false] <- FALSE       # a definite FALSE beats NA (still not a match)
    out
}

## three-valued "all negative": TRUE if all FALSE, FALSE if any TRUE, NA if no
## TRUE but some NA. Empty set -> all TRUE.
.all_false <- function(df, cols) {
    if (length(cols) == 0) return(rep(TRUE, nrow(df)))
    m <- as.matrix(df[, cols, drop = FALSE])
    any_true <- rowSums(m == TRUE, na.rm = TRUE) > 0
    any_na   <- rowSums(is.na(m)) > 0
    out <- rep(TRUE, nrow(df))
    out[any_na]   <- NA
    out[any_true] <- FALSE
    out
}

## three-valued "any positive": TRUE if any TRUE, FALSE if all FALSE, NA if no
## TRUE but some NA. Empty set -> all FALSE.
.any_true <- function(df, cols) {
    cols <- intersect(cols, names(df))
    if (length(cols) == 0) return(rep(FALSE, nrow(df)))
    m <- as.matrix(df[, cols, drop = FALSE])
    any_true <- rowSums(m == TRUE, na.rm = TRUE) > 0
    any_na   <- rowSums(is.na(m)) > 0
    out <- rep(FALSE, nrow(df))
    out[any_na]   <- NA
    out[any_true] <- TRUE          # a definite TRUE beats NA
    out
}

#' Assign a top-level cell type (lineage) to each cell
#'
#' Evaluates every lineage's `require_pos` / `require_neg` rule with three-valued
#' logic, then resolves per cell:
#' \itemize{
#'   \item >1 lineage definitely matched -> `UNKNOWN` (conflict)
#'   \item exactly 1 matched             -> that lineage
#'   \item 0 matched, but >=1 lineage un-callable (a required marker absent from
#'         the sample) -> `NA`
#'   \item 0 matched, all lineages callable -> `UNCLASSIFIED`
#' }
#'
#' @param wide Wide cell x marker logical table from [marker_pos_wide()].
#' @param rules Parsed rules from [read_cell_rules()].
#'
#' @return A character vector of cell types, one per row of `wide`.
#'
#' @export
annotate_lineage <- function(wide, rules) {

    lineage_names <- names(rules$lineages)

    ## per-lineage three-valued match for every cell
    match_mat <- purrr::map(rules$lineages, function(rule) {
        pos_ok <- .all_true(wide, rule$require_pos)
        neg_ok <- .all_false(wide, rule$require_neg)
        ## both must hold; combine with three-valued AND
        res <- pos_ok & neg_ok
        res
    })
    match_df <- as.data.frame(match_mat, optional = TRUE)
    names(match_df) <- lineage_names

    mm <- as.matrix(match_df)
    n_true <- rowSums(mm == TRUE, na.rm = TRUE)
    n_na   <- rowSums(is.na(mm))

    unknown_lab <- rules$labels$unknown
    unclass_lab <- rules$labels$unclassified

    ## index of the single matching lineage (only meaningful where n_true == 1)
    first_true <- apply(mm, 1, function(r) {
        w <- which(r == TRUE)
        if (length(w) == 1) lineage_names[w] else NA_character_
    })

    out <- dplyr::case_when(
        n_true > 1               ~ unknown_lab,
        n_true == 1              ~ first_true,
        n_true == 0 & n_na > 0   ~ NA_character_,
        TRUE                     ~ unclass_lab
    )
    out
}

#' Compute parent-gated sub-state flags for every cell
#'
#' For each parent lineage in `rules$states`, sets each state's boolean flag
#' only on cells whose `CellType` equals that parent; all other cells get `NA`.
#' Adds the `Exhausted` flag (a state on T/NK cells, positive for TIM3 or LAG3).
#' Where a state's marker is absent from a cell's sample panel the flag is `NA`,
#' even for cells of the right parent type.
#'
#' @param wide Wide cell x marker logical table from [marker_pos_wide()].
#' @param cell_type Character vector of resolved cell types (from
#'   [annotate_lineage()]), aligned row-wise to `wide`.
#' @param rules Parsed rules from [read_cell_rules()].
#'
#' @return A tibble of state columns (one per state across all parents, plus
#'   `Exhausted`), aligned row-wise to `wide`. Column names are prefixed by a
#'   short parent tag, e.g. `Tumor_Ki67_pos`, `T_CD4`, `Mac_M1`.
#'
#' @export
annotate_states <- function(wide, cell_type, rules) {

    ## short, file/column-safe parent tags for state column names
    parent_tag <- c(
        "Tumor" = "Tumor", "T cell" = "T", "B cell" = "B", "NK cell" = "NK",
        "Macrophage" = "Mac", "Endothelial" = "Endo", "Myofibroblast" = "Myo"
    )

    out <- tibble::tibble(.rows = nrow(wide))

    for (parent in names(rules$states)) {
        tag <- parent_tag[[parent]]
        is_parent <- cell_type == parent & !is.na(cell_type)
        for (state in names(rules$states[[parent]])) {
            pos_markers <- rules$states[[parent]][[state]]$pos
            flag <- .any_true(wide, pos_markers)   # three-valued positivity
            ## gate on parent: non-parent cells get NA for this state
            flag[!is_parent] <- NA
            col <- glue::glue("{tag}_{state}")
            out[[col]] <- flag
        }
    }

    ## exhaustion: a state on the parent lineages in applies_to
    if (!is.null(rules$exhaustion)) {
        applies <- rules$exhaustion$applies_to
        is_applicable <- cell_type %in% applies & !is.na(cell_type)
        ex <- .any_true(wide, rules$exhaustion$any_pos)
        ex[!is_applicable] <- NA
        out[["Exhausted"]] <- ex
    }

    out
}

#' Annotate a loaded Halo object with cell types and states
#'
#' One-call entry point: builds the wide positivity table, assigns each cell a
#' `CellType`, computes parent-gated state flags and `Exhausted`, and a compact
#' `CellState` summary string, then returns the object with these columns added
#' to `cell.data`.
#'
#' @param obj A combined object from [load_manifest()].
#' @param rules Parsed rules from [read_cell_rules()] (required).
#'
#' @return The input `obj` with `cell.data` extended by `CellType` (factor,
#'   levels = lineages + UNKNOWN + UNCLASSIFIED), the per-state flag columns,
#'   `Exhausted`, and `CellState` (a `;`-joined string of the cell's TRUE state
#'   flags). Adds `rules_path` and `VERSION` markers to the object.
#'
#' @export
annotate_cells <- function(obj, rules) {

    if (missing(rules)) {
        stop("annotate_cells: 'rules' is required; load it with read_cell_rules(path).")
    }

    wide <- marker_pos_wide(obj, rules)

    cell_type <- annotate_lineage(wide, rules)
    states <- annotate_states(wide, cell_type, rules)

    type_levels <- c(names(rules$lineages),
                     rules$labels$unknown, rules$labels$unclassified)

    state_cols <- names(states)
    ## compact human summary: the names of the TRUE flags for each cell
    cell_state <- purrr::pmap_chr(states, function(...) {
        vals <- c(...)
        on <- names(vals)[!is.na(vals) & vals]
        if (length(on) == 0) "" else paste(on, collapse = ";")
    })

    annot <- wide |>
        select(UUID) |>
        mutate(CellType = factor(cell_type, levels = type_levels)) |>
        bind_cols(states) |>
        mutate(CellState = cell_state)

    obj$cell.data <- obj$cell.data |>
        left_join(annot, by = "UUID")

    obj$rules_path <- rules$rules_path
    obj$annot_state_cols <- state_cols
    obj$VERSION <- VERSION
    obj
}

## ---------------------------------------------------------------------------
## Composition summaries
## ---------------------------------------------------------------------------

#' Per-sample cell-type composition
#'
#' @param obj An annotated object from [annotate_cells()].
#'
#' @return A list with `long` (Sample, CellType, nCells, pct) and `wide`
#'   (Sample x CellType count matrix). `NA` cell types (un-callable) are counted
#'   under the label `"(NA / un-callable)"` so they are visible, not dropped.
#'
#' @export
summarize_celltypes <- function(obj) {

    cd <- obj$cell.data |>
        mutate(CellType = forcats::fct_na_value_to_level(CellType, "(NA / un-callable)"))

    long <- cd |>
        count(Sample, CellType, name = "nCells") |>
        group_by(Sample) |>
        mutate(pct = round(100 * nCells / sum(nCells), 2)) |>
        ungroup() |>
        arrange(Sample, desc(nCells))

    wide <- long |>
        select(Sample, CellType, nCells) |>
        pivot_wider(names_from = Sample, values_from = nCells, values_fill = 0) |>
        arrange(desc(rowSums(across(where(is.numeric)))))

    list(long = long, wide = wide)
}

#' Sub-state breakdowns (tumor flags, T subsets, M1/M2, exhaustion)
#'
#' Counts, per sample, how many cells of the relevant parent type carry each
#' state flag. A flag value of `NA` (marker absent from the sample's panel, or
#' the cell is not of the parent type) is reported separately so a small
#' denominator from a missing marker is never mistaken for a real zero.
#'
#' @param obj An annotated object from [annotate_cells()].
#'
#' @return A long tibble: `Sample`, `State`, `nScored` (parent cells with a
#'   callable flag = the denominator), `nPos` (flag TRUE), `pctPos` (of
#'   `nScored`). A state with `nScored == 0` for a sample (e.g. Th1/Th2/Th17 in
#'   S3, where the marker is absent) reports `pctPos = NA`, never a false 0.
#'
#' @export
summarize_states <- function(obj) {

    state_cols <- obj$annot_state_cols
    cd <- obj$cell.data

    purrr::map(state_cols, function(col) {
        cd |>
            group_by(Sample) |>
            summarize(
                ## a non-NA flag means: cell IS of the parent type AND the
                ## marker was measured -> the honest denominator.
                nScored = sum(!is.na(.data[[col]])),
                nPos = sum(.data[[col]] == TRUE, na.rm = TRUE),
                .groups = "drop"
            ) |>
            mutate(
                State = col,
                pctPos = ifelse(nScored > 0, round(100 * nPos / nScored, 2), NA_real_)
            ) |>
            select(Sample, State, nScored, nPos, pctPos)
    }) |>
        purrr::list_rbind() |>
        arrange(State, Sample)
}
