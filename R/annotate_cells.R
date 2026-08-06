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
##
## A rules file MAY override the multi-lineage default with a
## `conflict_resolution:` block (policy: priority), which assigns each lineage a
## conditional rank and hands a conflicted cell to the single highest-ranked
## lineage it matched. The mechanism lives here; the ranks and their conditions
## are project content and live in the rules file.
## ---------------------------------------------------------------------------

## top-level rules keys this version of the grammar understands. Anything else
## is a hard error: an engine that silently ignores a key it has never heard of
## will happily produce old-policy numbers from a rules file that reads as
## though the new policy were active, and nothing in the output says so.
.RULES_KEYS <- c("markers", "controls", "lineages", "conflict_resolution",
                 "states", "exhaustion", "labels", "to_confirm", "panel_notes")

.CONFLICT_KEYS <- c("policy", "default_rank", "ranks", "notes")
.RANK_KEYS     <- c("lineage", "rank", "when")
.WHEN_KEYS     <- c("all_pos", "any_pos", "all_neg")

## stop() naming the offending keys, used by every strict-key check below
.check_keys <- function(x, allowed, what) {
    unknown <- setdiff(names(x), allowed)
    if (length(unknown) > 0) {
        stop(glue::glue(
            "read_cell_rules: unknown {what} key(s): ",
            "{paste(unknown, collapse=', ')}. Known keys: ",
            "{paste(allowed, collapse=', ')}. ",
            "(A rules file written for a newer HaloXi must fail here, not be ",
            "silently ignored.)"
        ))
    }
    invisible(NULL)
}

#' Read and validate the cell-annotation rules
#'
#' The rules file is a *project* artifact (it encodes one study's marker panel
#' and cell-type definitions), so the caller must supply its path -- this
#' package bundles no rules of its own. This mirrors [read_manifest()], which
#' likewise takes a project file path.
#'
#' Unknown top-level keys are a **hard error**, as are unknown keys inside
#' `conflict_resolution:` and its `when:` clauses. This is deliberate: an engine
#' that predates a block ignores it silently, so a rules file written for a
#' newer HaloXi would otherwise run to completion and report old-policy numbers.
#'
#' @param path Path to the rules YAML (required).
#'
#' @return The parsed rules as a list, with an added `marker_levels` element
#'   (the friendly marker names) for convenience. Stops if `path` is missing,
#'   the file does not exist, a rule references a marker not declared in
#'   `markers:`, a `conflict_resolution:` rank names a lineage not declared in
#'   `lineages:`, or any unrecognised key is present.
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

    .check_keys(rules, .RULES_KEYS, "top-level")

    cr <- rules$conflict_resolution
    if (!is.null(cr)) {
        .check_keys(cr, .CONFLICT_KEYS, "conflict_resolution")
        for (entry in cr$ranks) {
            .check_keys(entry, .RANK_KEYS, "conflict_resolution ranks")
            .check_keys(entry$when, .WHEN_KEYS, "conflict_resolution when")
        }
        bad_lin <- setdiff(purrr::map_chr(cr$ranks, "lineage"), names(rules$lineages))
        if (length(bad_lin) > 0) {
            stop(glue::glue(
                "read_cell_rules: conflict_resolution ranks name undeclared ",
                "lineage(s): {paste(bad_lin, collapse=', ')}"
            ))
        }
    }

    declared <- names(rules$markers)

    ## every marker referenced anywhere must be declared in markers:
    referenced <- c(
        purrr::map(rules$lineages, ~ c(.x$require_pos, .x$require_neg, .x$any_pos)),
        purrr::map(rules$states, function(parent) {
            purrr::map(parent, ~ .x$pos)
        }),
        purrr::map(cr$ranks, ~ unlist(.x$when[.WHEN_KEYS], use.names = FALSE)),
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

## three-valued evaluation of one `when:` clause from conflict_resolution.
## The three optional marker lists are ANDed; an absent/empty clause is
## unconditionally TRUE. Uses the same helpers as lineage matching, so "marker
## not measured in this sample" stays NA and never becomes FALSE.
.eval_when <- function(df, when) {
    parts <- list(
        if (length(when$all_pos)) .all_true(df, when$all_pos),
        if (length(when$any_pos)) .any_true(df, when$any_pos),
        if (length(when$all_neg)) .all_false(df, when$all_neg)
    ) |> purrr::compact()
    if (length(parts) == 0) return(rep(TRUE, nrow(df)))
    purrr::reduce(parts, `&`)     # `&` is already three-valued
}

## per-cell x per-lineage rank matrix implied by conflict_resolution$ranks.
##
## Entries are scanned in file order. The first entry for a lineage whose `when`
## is TRUE sets that lineage's rank and settles it. An entry whose `when` is NA
## settles the lineage at rank NA -- deliberately: once a rank decision depends
## on a measurement the sample does not have, no later fallback entry may stand
## in for it. Lineages no entry settles take `default_rank`.
.lineage_ranks <- function(wide, rules) {

    cr <- rules$conflict_resolution
    lineage_names <- names(rules$lineages)

    ranks <- matrix(
        as.numeric(cr$default_rank %||% 0),
        nrow = nrow(wide), ncol = length(lineage_names),
        dimnames = list(NULL, lineage_names)
    )
    settled <- matrix(FALSE, nrow = nrow(wide), ncol = length(lineage_names),
                      dimnames = list(NULL, lineage_names))

    for (entry in cr$ranks) {
        j <- match(entry$lineage, lineage_names)
        cond <- .eval_when(wide, entry$when)
        open <- !settled[, j]
        fires  <- open & !is.na(cond) & cond
        blocks <- open & is.na(cond)
        ranks[fires, j]  <- as.numeric(entry$rank)
        ranks[blocks, j] <- NA_real_
        settled[fires | blocks, j] <- TRUE
    }

    ranks
}

## "A+B" labels for the matched-lineage set of every multi-lineage cell.
##
## Encodes each cell's matched set as a bitmask and labels only the distinct
## masks, then maps back -- the study's largest sample is 1.19M cells, so a
## per-row paste() is not an option. Each distinct mask is decoded from any one
## row that carries it, which keeps this correct for any lineage count rather
## than capping at integer bit width.
.conflict_labels <- function(matched, lineage_names) {

    mask <- as.vector(matched %*% 2^(seq_along(lineage_names) - 1))
    mask[rowSums(matched) < 2] <- 0

    keys <- unique(mask[mask != 0])
    if (length(keys) == 0) return(rep(NA_character_, length(mask)))

    labels <- purrr::map_chr(match(keys, mask), function(i) {
        paste(lineage_names[matched[i, ]], collapse = "+")
    })

    ifelse(mask == 0, NA_character_, labels[match(mask, keys)])
}

#' Assign a top-level cell type (lineage) to each cell
#'
#' Evaluates every lineage's `require_pos` / `require_neg` (and optional
#' `any_pos`) rule with three-valued logic, then resolves per cell.
#' `require_pos` markers must ALL be positive; `require_neg` markers must ALL be
#' negative; if `any_pos` is given, at least ONE of those markers must also be
#' positive. The per-cell resolution is then:
#' \itemize{
#'   \item >1 lineage definitely matched -> `UNKNOWN` (conflict), unless the
#'         rules set `conflict_resolution: {policy: priority}` and it names a
#'         single winner (see below)
#'   \item exactly 1 matched             -> that lineage
#'   \item 0 matched, but >=1 lineage un-callable (a required marker absent from
#'         the sample) -> `NA`
#'   \item 0 matched, all lineages callable -> `UNCLASSIFIED`
#' }
#'
#' With `policy: priority`, each lineage gets a rank from the first
#' `conflict_resolution$ranks` entry naming it whose `when:` clause is TRUE
#' (`default_rank` if none fires). A conflicted cell goes to the highest-ranked
#' lineage it **matched**; ties, and any matched lineage whose rank is un-callable
#' because a `when:` marker was not measured, stay `UNKNOWN`. A missing
#' measurement never decides a call, so a fallback entry can never stand in for
#' one. Without the block, or with any other `policy`, behaviour is unchanged.
#'
#' @param wide Wide cell x marker logical table from [marker_pos_wide()].
#' @param rules Parsed rules from [read_cell_rules()].
#'
#' @return A tibble with one row per row of `wide`:
#'   \describe{
#'     \item{`CellType`}{chr, the resolved call.}
#'     \item{`TypeCall`}{chr, how it was reached: `single`, `priority`,
#'       `conflict`, `unclassified` or `uncallable`.}
#'     \item{`TypeConflict`}{chr, the matched lineage set as `"A+B"` for every
#'       cell that matched more than one lineage -- including ones priority
#'       resolved, so a priority rule can never absorb a population without
#'       leaving a trace of its size -- and `NA` otherwise. Lineages are joined
#'       in `rules$lineages` order.}
#'   }
#'
#' @section Breaking change:
#' Before HaloXi 1.2 this function returned a bare character vector of cell
#' types; that is now the `CellType` column of the returned tibble.
#'
#' @export
annotate_lineage <- function(wide, rules) {

    lineage_names <- names(rules$lineages)

    ## per-lineage three-valued match for every cell
    match_mat <- purrr::map(rules$lineages, function(rule) {
        pos_ok <- .all_true(wide, rule$require_pos)   # ALL of require_pos (AND)
        neg_ok <- .all_false(wide, rule$require_neg)  # ALL of require_neg negative
        res <- pos_ok & neg_ok
        ## optional ANY-of clause: lineage also needs >=1 of any_pos positive.
        ## Only applied when the rule supplies it (absent -> no constraint), so
        ## existing require_pos-only lineages are unaffected.
        if (length(rule$any_pos) > 0) {
            res <- res & .any_true(wide, rule$any_pos)
        }
        res
    })
    match_df <- as.data.frame(match_mat, optional = TRUE)
    names(match_df) <- lineage_names

    mm <- as.matrix(match_df)
    matched <- mm == TRUE & !is.na(mm)
    n_true <- rowSums(matched)
    n_na   <- rowSums(is.na(mm))

    unknown_lab <- rules$labels$unknown
    unclass_lab <- rules$labels$unclassified

    ## the single matching lineage (only meaningful where n_true == 1)
    first_true <- lineage_names[max.col(matched, ties.method = "first")]

    ## priority resolution of multi-lineage cells, if the rules ask for it.
    ## Unmatched lineages are pushed to -Inf so they can never win however high
    ## their rank; a matched lineage with an un-callable rank forces UNKNOWN.
    conflicted <- n_true > 1
    winner  <- rep(NA_character_, nrow(wide))
    settled <- rep(FALSE, nrow(wide))

    if (identical(rules$conflict_resolution$policy, "priority")) {
        ranks <- .lineage_ranks(wide, rules)
        ranks[!matched] <- -Inf
        rank_na <- rowSums(is.na(ranks)) > 0
        ranks[is.na(ranks)] <- -Inf

        ## row-wise max, column by column: pmax() is vectorised where a
        ## row-wise apply() over ~1.2M rows is not
        best <- purrr::map(seq_len(ncol(ranks)), ~ ranks[, .x]) |>
            purrr::reduce(pmax)
        sole_best <- rowSums(ranks == best) == 1

        settled <- conflicted & !rank_na & sole_best
        winner[settled] <- lineage_names[max.col(ranks, ties.method = "first")][settled]
    }

    cell_type <- dplyr::case_when(
        settled                  ~ winner,
        conflicted               ~ unknown_lab,
        n_true == 1              ~ first_true,
        n_na > 0                 ~ NA_character_,
        TRUE                     ~ unclass_lab
    )

    type_call <- dplyr::case_when(
        settled                  ~ "priority",
        conflicted               ~ "conflict",
        n_true == 1              ~ "single",
        n_na > 0                 ~ "uncallable",
        TRUE                     ~ "unclassified"
    )

    tibble::tibble(
        CellType     = cell_type,
        TypeCall     = type_call,
        TypeConflict = .conflict_labels(matched, lineage_names)
    )
}

#' Compute parent-gated sub-state flags for every cell
#'
#' For each parent lineage in `rules$states`, sets each state's boolean flag
#' only on cells whose `CellType` equals that parent; all other cells get `NA`.
#' Adds the `Exhausted` flag (a state on T/NK cells, positive for any
#' exhaustion marker defined in the rules).
#' Where a state's marker is absent from a cell's sample panel the flag is `NA`,
#' even for cells of the right parent type.
#'
#' @param wide Wide cell x marker logical table from [marker_pos_wide()].
#' @param cell_type Character vector of resolved cell types (the `CellType`
#'   column of [annotate_lineage()]), aligned row-wise to `wide`.
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
#'   levels = lineages + UNKNOWN + UNCLASSIFIED), `TypeCall` and `TypeConflict`
#'   (the call provenance from [annotate_lineage()]: how each type was reached,
#'   and the full matched lineage set of every multi-lineage cell), the
#'   per-state flag columns, `Exhausted`, and `CellState` (a `;`-joined string
#'   of the cell's TRUE state flags). Adds `rules_path` and `VERSION` markers to
#'   the object.
#'
#' @export
annotate_cells <- function(obj, rules) {

    if (missing(rules)) {
        stop("annotate_cells: 'rules' is required; load it with read_cell_rules(path).")
    }

    wide <- marker_pos_wide(obj, rules)

    lineage <- annotate_lineage(wide, rules)
    cell_type <- lineage$CellType
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
        mutate(
            CellType     = factor(cell_type, levels = type_levels),
            TypeCall     = lineage$TypeCall,
            TypeConflict = lineage$TypeConflict
        ) |>
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
#'   `nScored`). A state whose marker is absent from a sample's panel has
#'   `nScored == 0` there and reports `pctPos = NA`, never a false 0.
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
