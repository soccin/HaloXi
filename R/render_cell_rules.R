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

## format a +/- marker requirement list as plain text, e.g. "CD3+ and CD20-"
.fmt_requirement <- function(pos = character(), neg = character()) {
    parts <- c(
        if (length(pos)) paste0(pos, "+"),
        if (length(neg)) paste0(neg, "-")
    )
    if (length(parts) == 0) return("(no marker requirement)")
    paste(parts, collapse = " and ")
}

#' Build the human-readable rules document as Markdown lines
#'
#' @param rules Parsed rules from [read_cell_rules()].
#'
#' @return A character vector of Markdown lines (no trailing newline handling;
#'   join with "\n"). Intended for [render_rules_md()] and for inlining into the
#'   HTML report.
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
    add("- **", rules$labels$unknown, "** = the cell matches the pattern of ",
        "*more than one* type at once (a conflict we are not yet resolving by ",
        "priority).")
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
        add("- **", lin, "**: ", .fmt_requirement(r$require_pos, r$require_neg))
    }
    blank()
    add("A cell positive for the defining markers of two or more of these ",
        "types is labeled ", rules$labels$unknown, ".")
    blank()

    ## ---- subtypes (mutually exclusive within a cell type) ------------------
    if (length(rules$subtypes)) {
        add("## Subtypes (exactly one within a cell type)")
        blank()
        add("Once a cell's main type is settled, it is given **one** subtype ",
            "from the list for that type. The rules are checked in order and the ",
            "first that fits wins; if a needed marker was not measured the ",
            "subtype is left blank (un-callable), but the main type still stands.")
        blank()
        for (parent in names(rules$subtypes)) {
            add("### Within ", parent)
            blank()
            add("| Subtype | Requires |")
            add("|---|---|")
            for (rule in rules$subtypes[[parent]]) {
                add("| ", rule$name, " | ",
                    .fmt_requirement(rule$require_pos, rule$require_neg), " |")
            }
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
    add("*Generated from the rules file by HaloXi. Cell types are assigned on a ",
        "first pass without a priority order; multi-type cells are ",
        rules$labels$unknown, " and will be revisited once you confirm the ",
        "rules above.*")

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
    }
    for (parent in names(rules$subtypes)) {
        for (rule in rules$subtypes[[parent]]) {
            if (length(rule$require_pos)) note(rule$require_pos, glue::glue("{parent}:{rule$name} (+)"))
            if (length(rule$require_neg)) note(rule$require_neg, glue::glue("{parent}:{rule$name} (-)"))
        }
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
