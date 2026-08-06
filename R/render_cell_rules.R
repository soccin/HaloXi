## ---------------------------------------------------------------------------
## Human-readable renderer for the cell-annotation rules.
##
## Turns the SAME rules object the engine runs (read_cell_rules()) into a
## plain-language pseudocode document for biology collaborators to verify and
## sign off on. Because both the code and this document come from one YAML file,
## the rules people approve and the rules that run cannot drift.
##
## Output is Markdown (rules_md_lines() returns the lines; render_rules_md()
## writes them). No R/analysis code appears in the output -- only the logic.
## ---------------------------------------------------------------------------

## small null-coalescing helper (avoids importing rlang's %||% at package load)
`%||%` <- function(x, y) if (is.null(x)) y else x

## format a +/- marker requirement list as plain text, e.g. "CD3+ and CD20-".
## `any` is an OR group: rendered "(A+ or B+)" and ANDed with the rest, e.g.
## "(CD30+ or TYK2+)".
.fmt_requirement <- function(pos = character(), neg = character(),
                             any = character()) {
    any_grp <- if (length(any)) {
        grp <- paste(paste0(any, "+"), collapse = " or ")
        if (length(any) > 1) paste0("(", grp, ")") else grp
    }
    parts <- c(
        if (length(pos)) paste0(pos, "+"),
        any_grp,
        if (length(neg)) paste0(neg, "-")
    )
    if (length(parts) == 0) return("(no marker requirement)")
    paste(parts, collapse = " and ")
}

## format a conflict_resolution `when:` clause in the same plain marker syntax
## as the cell-type rules above, e.g. "A+ and B-". No clause means the entry is
## that type's catch-all.
.fmt_when <- function(when) {
    if (length(when) == 0) return("any other cell of this type")
    .fmt_requirement(pos = when$all_pos, neg = when$all_neg, any = when$any_pos)
}

## TRUE when the rules ask for priority resolution of multi-type cells
.has_priority <- function(rules) {
    identical(rules$conflict_resolution$policy, "priority")
}

#' Build the human-readable rules document as Markdown lines
#'
#' @param rules Parsed rules from [read_cell_rules()].
#'
#' @return A character vector of Markdown lines (no trailing newline handling;
#'   join with a newline). Intended for [render_rules_md()] and for inlining
#'   into the HTML report.
#'
#' @export
rules_md_lines <- function(rules) {

    L <- character()
    add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
    blank <- function() add("")

    add("# Cell annotation rules (for review)")
    blank()
    add("This document is generated directly from the machine-readable rules ",
        "file the analysis code uses, so what you read here is exactly what the ",
        "code does. To change the logic, edit the rules file and this document ",
        "regenerates. Please review and tell us what to correct.")
    blank()

    ## ---- TO CONFIRM callout (front and center) ----------------------------
    if (length(rules$to_confirm)) {
        add("## Please confirm")
        blank()
        add("These choices were not fully specified; we picked a default and ",
            "need your confirmation:")
        blank()
        for (q in rules$to_confirm) add("- ", q)
        blank()
    }

    ## ---- how to read it ---------------------------------------------------
    add("## How to read these rules")
    blank()
    add("Each cell in the image has every marker scored independently as ",
        "positive or negative. We use those calls as follows:")
    blank()
    add("- A cell is given **one** cell type when it matches the marker ",
        "pattern of exactly one type below.")
    if (.has_priority(rules)) {
        add("- **", rules$labels$unknown, "** = the cell matches the pattern of ",
            "*more than one* type at once and the priority order below does not ",
            "settle which one wins.")
    } else {
        add("- **", rules$labels$unknown, "** = the cell matches the pattern of ",
            "*more than one* type at once (a conflict we are not yet resolving by ",
            "priority).")
    }
    add("- **", rules$labels$unclassified, "** = the cell matches *no* type ",
        "(negative for every type-defining marker).")
    add("- **NA / un-callable** = a marker the rule needs was not measured in ",
        "that sample, so the call cannot be made (we never treat \"not ",
        "measured\" as \"negative\").")
    add("- States (e.g. proliferating, exhausted, T-helper subset) are only ",
        "assigned to a cell once its main type is settled.")
    blank()

    ## ---- marker map -------------------------------------------------------
    add("## Markers")
    blank()
    add("Friendly names used below map to these measured marker channels:")
    blank()
    add("| Marker | Measured channel | Role |")
    add("|---|---|---|")
    ## roles for annotation in the table
    role_of <- .marker_roles(rules)
    ctrl <- rules$controls %||% character()
    for (m in names(rules$markers)) {
        role <- role_of[[m]] %||% ""
        if (m %in% ctrl) role <- "control (ignored)"
        add("| ", m, " | `", rules$markers[[m]], "` | ", role, " |")
    }
    blank()

    ## ---- cell types -------------------------------------------------------
    add("## Cell types")
    blank()
    for (lin in names(rules$lineages)) {
        r <- rules$lineages[[lin]]
        add("- **", lin, "**: ",
            .fmt_requirement(r$require_pos, r$require_neg, r$any_pos))
    }
    blank()
    if (.has_priority(rules)) {
        add("A cell positive for the defining markers of two or more of these ",
            "types is settled by the priority order below, and labeled ",
            rules$labels$unknown, " where that order does not pick a single ",
            "winner.")
    } else {
        add("A cell positive for the defining markers of two or more of these ",
            "types is labeled ", rules$labels$unknown, ".")
    }
    blank()

    ## ---- conflict resolution ----------------------------------------------
    if (.has_priority(rules)) {
        cr <- rules$conflict_resolution
        add("## Which type wins when a cell matches several")
        blank()
        add("Some cells are positive for the defining markers of more than one ",
            "type. Each type is given a priority for those cells; the type with ",
            "the highest priority wins. This decides only which type a mixed ",
            "cell is *reported* as -- it does not change what makes a cell that ",
            "type in the first place.")
        blank()
        add("| Cell type | Priority | Applies when the cell is |")
        add("|---|---|---|")
        for (entry in cr$ranks) {
            add("| ", entry$lineage, " | ", entry$rank, " | ",
                .fmt_when(entry$when), " |")
        }
        add("| every other type | ", cr$default_rank %||% 0,
            " | (always) |")
        blank()
        add("Two rules apply to the table:")
        blank()
        add("- Only types the cell actually matches can win, however high their ",
            "priority.")
        add("- If two matching types tie on priority, or if a marker needed to ",
            "decide a priority was not measured in that sample, the cell stays ",
            rules$labels$unknown, " -- a missing measurement never decides a ",
            "call.")
        blank()
        if (length(cr$notes)) {
            add("Notes on this order:")
            blank()
            for (n in cr$notes) add("- ", n)
            blank()
        }
    }

    ## ---- states -----------------------------------------------------------
    add("## States (only assigned within the matching cell type)")
    blank()
    for (parent in names(rules$states)) {
        add("### ", parent)
        blank()
        states <- rules$states[[parent]]
        ## note independence for tumor's four flags explicitly
        add("Assigned only to cells already typed as **", parent,
            "**. Each is an independent yes/no flag (a cell can have several):")
        blank()
        for (st in names(states)) {
            add("- **", st, "**: ", .fmt_requirement(pos = states[[st]]$pos))
        }
        blank()
    }

    ## ---- exhaustion -------------------------------------------------------
    if (!is.null(rules$exhaustion)) {
        ex <- rules$exhaustion
        add("### Exhaustion")
        blank()
        add("A cell is **exhausted** if it is one of {",
            paste(ex$applies_to, collapse = ", "),
            "} and positive for any of: ",
            paste(paste0(ex$any_pos, "+"), collapse = " or "), ".")
        blank()
    }

    ## ---- edge cases / panel coverage --------------------------------------
    add("## Edge cases and sample coverage")
    blank()
    if (length(rules$panel_notes)) {
        for (n in rules$panel_notes) add("- ", n)
    }
    add("- Where a state's marker is missing in a sample, that state is ",
        "reported as NA for that sample (not 0), so a missing marker is never ",
        "mistaken for a true absence.")
    blank()
    ## NB: avoid a bare "---" horizontal rule here. This text is also inlined
    ## verbatim into the HTML report's Rmd, and a "---" line there is parsed by
    ## pandoc as a YAML metadata block (breaking the render). A blank line and
    ## italic footer separate the section just as well.
    if (.has_priority(rules)) {
        add("*Generated from the rules file by HaloXi. Cells matching more than ",
            "one type are settled by the priority order above; where it picks no ",
            "single winner they remain ", rules$labels$unknown, ".*")
    } else {
        add("*Generated from the rules file by HaloXi. Cell types are assigned on a ",
            "first pass without a priority order; multi-type cells are ",
            rules$labels$unknown, " and will be revisited once you confirm the ",
            "rules above.*")
    }

    unlist(L, use.names = FALSE)
}

## derive a short role string per marker from where it appears in the rules,
## purely for the documentation table.
.marker_roles <- function(rules) {
    role <- list()
    note <- function(m, txt) {
        for (x in m) {
            role[[x]] <<- if (is.null(role[[x]])) txt else paste(role[[x]], txt, sep = "; ")
        }
    }
    for (lin in names(rules$lineages)) {
        r <- rules$lineages[[lin]]
        if (length(r$require_pos)) note(r$require_pos, glue::glue("{lin} (+)"))
        if (length(r$require_neg)) note(r$require_neg, glue::glue("{lin} (-)"))
        if (length(r$any_pos)) note(r$any_pos, glue::glue("{lin} (+, any)"))
    }
    for (parent in names(rules$states)) {
        for (st in names(rules$states[[parent]])) {
            note(rules$states[[parent]][[st]]$pos, glue::glue("{parent}:{st}"))
        }
    }
    if (!is.null(rules$exhaustion)) note(rules$exhaustion$any_pos, "exhaustion")
    role
}

#' Render the cell-annotation rules to a Markdown file
#'
#' Writes the plain-language pseudocode document collaborators sign off on.
#'
#' @param rules Parsed rules from [read_cell_rules()].
#' @param path Output `.md` path (parent directory created if needed).
#'
#' @return `path`, invisibly.
#'
#' @export
render_rules_md <- function(rules, path) {
    fs::dir_create(fs::path_dir(path))
    lines <- rules_md_lines(rules)
    writeLines(lines, path)
    invisible(path)
}
