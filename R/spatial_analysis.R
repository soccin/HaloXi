## ---------------------------------------------------------------------------
## Spatial analysis of annotated cells.
##
## Two questions, one machine underneath both: find every pair of cells whose
## centres fall in a distance band, then ask whether the cell types at the ends
## of those pairs are arranged differently from chance.
##
##   colocation_test()          which type-pairs sit closer together than chance
##   neighborhood_composition() what the tissue in a ring around one type is
##                              made of
##
## The null everywhere is a LABEL permutation, never a point process. Tissue is
## not a uniform random field; against a Poisson/CSR null almost every pair in a
## real section reads as significant, because the section is not uniform. A
## label permutation holds the geometry, the local density and the overall
## composition of the sample fixed, and destroys only the association between
## where a cell is and what it is -- which is the thing being tested.
##
## Study-agnostic: no marker, cell type, radius or scan resolution appears here.
## `anchor_type = "Tumor"` is a default argument value and nothing else; the
## logic never asks what a type is called.
## ---------------------------------------------------------------------------

## a scan resolution guessed wrong silently rescales every distance in a study,
## so it is required everywhere and never defaulted
.check_um_per_px <- function(um_per_px) {
    if (missing(um_per_px) || is.null(um_per_px)) {
        stop("um_per_px is required: Halo coordinates are in pixels and the ",
             "file carries no scan resolution. Pass the study's um/pixel.")
    }
    if (!is.numeric(um_per_px) || length(um_per_px) != 1 ||
        !is.finite(um_per_px) || um_per_px <= 0) {
        stop("um_per_px must be a single positive finite number; got: ",
             paste(format(um_per_px), collapse = ", "))
    }
    invisible(as.numeric(um_per_px))
}

#' Cell centroids in microns
#'
#' Halo exports a bounding box per cell, in pixels, and carries no scan
#' metadata. The centroid is the midpoint of that box, converted to microns.
#'
#' Cells whose `CellType` is `NA` -- un-callable, because a marker the call
#' needed was not run in that sample -- are dropped: they cannot take part in a
#' type-based test, and carrying them into the permutation would invent a label
#' for them. Cells labelled as the rules' conflict (`UNKNOWN`) or no-lineage
#' (`UNCLASSIFIED`) buckets are **kept**. They are real cells occupying real
#' space, and dropping them would distort both the local density and the
#' marginal type frequencies the null is built from. They appear in results as
#' categories of their own.
#'
#' @param obj An annotated object from [annotate_cells()].
#' @param um_per_px Microns per pixel. **Required, with no default.**
#'
#' @return A tibble of `UUID`, `Sample`, `CellType` (factor, unused levels
#'   dropped), `X`, `Y`, with `X`/`Y` in microns.
#'
#' @export
cell_centroids <- function(obj, um_per_px) {

    .check_um_per_px(um_per_px)

    cd <- obj$cell.data
    need <- c("UUID", "Sample", "CellType", "XMin", "XMax", "YMin", "YMax")
    miss <- setdiff(need, names(cd))
    if (length(miss)) {
        stop("cell_centroids: cell.data is missing column(s): ",
             paste(miss, collapse = ", "),
             ". Annotate the object with annotate_cells() first.")
    }

    cd |>
        filter(!is.na(CellType)) |>
        mutate(
            CellType = droplevels(as.factor(CellType)),
            X = (XMin + XMax) / 2 * um_per_px,
            Y = (YMin + YMax) / 2 * um_per_px
        ) |>
        select(UUID, Sample, CellType, X, Y)
}

## All pairs of points in ONE sample whose separation falls in the half-open
## band r_inner < d <= r_outer. Returns 1-based index vectors i, j with i < j
## (each unordered pair exactly once), or NULL when there are none.
##
## closepairs() is a grid-based fixed-radius search, so this is O(pairs) rather
## than O(n^2) -- the target study's largest sample is 1.19M cells, where an
## n^2 distance matrix is 10 TB and not an option.
##
## rmax is nudged up by a relative epsilon and the band applied explicitly
## afterwards, so which side of `d == r_outer` a pair lands on is decided here
## and does not depend on closepairs' own boundary convention.
##
## The band is half-open on the left, so with the default r_inner = 0 two
## distinct cells whose centroids coincide (d == 0) are NOT paired. That is
## what makes a ring a ring, and it also keeps a segmentation artefact from
## registering as the closest association in the tissue.
.sample_pairs <- function(xy, r_outer, r_inner = 0) {

    if (!is.finite(r_outer) || r_outer <= 0) {
        stop(".sample_pairs: r_outer must be a positive finite number")
    }
    if (!is.finite(r_inner) || r_inner < 0 || r_inner >= r_outer) {
        stop(".sample_pairs: need 0 <= r_inner < r_outer")
    }
    if (!requireNamespace("spatstat.geom", quietly = TRUE)) {
        stop(".sample_pairs: package 'spatstat.geom' is required")
    }

    x <- as.numeric(xy$X)
    y <- as.numeric(xy$Y)
    if (length(x) < 2) return(NULL)

    ## owin() needs a non-degenerate window; a sample can be a single column of
    ## cells, or every cell at one coordinate, so pad any flat range
    pad_range <- function(v) {
        r <- range(v)
        if (r[1] == r[2]) r + c(-1, 1) else r + c(-1, 1) * (diff(r) * 1e-6 + 1e-9)
    }
    win <- spatstat.geom::owin(xrange = pad_range(x), yrange = pad_range(y))
    pp <- spatstat.geom::ppp(x, y, window = win, check = FALSE)

    cp <- spatstat.geom::closepairs(pp, rmax = r_outer * (1 + 1e-9),
                                    what = "ijd", twice = FALSE)
    if (length(cp$i) == 0) return(NULL)

    keep <- cp$d > r_inner & cp$d <= r_outer
    if (!any(keep)) return(NULL)

    i <- as.integer(cp$i)[keep]
    j <- as.integer(cp$j)[keep]
    list(i = pmin(i, j), j = pmax(i, j))
}

## Unordered pair counts as a symmetric K x K matrix, from the integer type
## codes at each end of the pair list.
##
## Tabulating an integer key is the only approach that scales: the target
## study's largest band holds ~30M pairs, where paste()/table() on character
## keys would need tens of gigabytes.
.pair_type_counts <- function(a, b, K) {
    m <- matrix(tabulate(a + K * (b - 1L), nbins = K * K), nrow = K, ncol = K)
    ## m[A, B] holds the pairs whose first end is A; folding it onto its
    ## transpose gives each unordered pair once, except the diagonal, which was
    ## already counted once and must not be doubled
    d <- diag(m)
    m <- m + t(m)
    diag(m) <- d
    m
}

## Two-sided empirical p from permutation tail counts.
##
## The +1 counts the observed statistic as one of its own null draws, which is
## what stops a p of exactly zero: the smallest value this can return is
## 2 / (n_perm + 1). That floor is the resolution of the test, not evidence.
.emp_p <- function(n_ge, n_le, n_perm) {
    pmin(1, 2 * (pmin(n_ge, n_le) + 1) / (n_perm + 1))
}

## running mean / sd / tail accumulator over permutations, so the null
## distribution never has to be held in memory all at once
.null_accumulator <- function(dim) {
    e <- new.env(parent = emptyenv())
    e$n <- 0L
    e$sum <- numeric(dim)
    e$sumsq <- numeric(dim)
    e$ge <- numeric(dim)
    e$le <- numeric(dim)
    e
}

.null_update <- function(acc, values, observed) {
    acc$n <- acc$n + 1L
    acc$sum <- acc$sum + values
    acc$sumsq <- acc$sumsq + values^2
    acc$ge <- acc$ge + (values >= observed)
    acc$le <- acc$le + (values <= observed)
    invisible(acc)
}

.null_stats <- function(acc) {
    n <- acc$n
    mean_v <- acc$sum / n
    var_v <- (acc$sumsq - n * mean_v^2) / max(1, n - 1)
    var_v[var_v < 0] <- 0          # floating-point noise around an exact zero
    list(nExp = mean_v, sdExp = sqrt(var_v), ge = acc$ge, le = acc$le, n = n)
}

#' Which cell types sit closer together than chance
#'
#' For every unordered pair of cell types, counts the cell pairs of those types
#' whose centres fall in the band `r_inner < d <= r_outer`, and compares that
#' count against a null built by permuting the cell-type labels across the same
#' point pattern, within sample. Self-pairs (a type with itself) are included
#' and measure how clustered that type is.
#'
#' Samples are analysed separately throughout. They are different tissues with
#' different composition and geometry, and pooling them would let the largest
#' specimen decide every call.
#'
#' @section The p-value floor:
#' `pEmp` can never fall below `2 / (n_perm + 1)` -- with the default 99
#' permutations, 0.02. A row sitting at that floor means "smaller than this
#' test can resolve", not "p = 0". Raise `n_perm` to resolve further; never
#' report such a row as `p = 0`.
#'
#' @section Edge effects:
#' Cells near the tissue border have truncated neighbourhoods, so raw pair
#' **counts** are biased low there. `log2FC` is robust to it: the null is
#' computed over the same point pattern with the same truncation, so the bias
#' hits `nObs` and `nExp` identically and cancels in the ratio. No border
#' correction or guard band is applied, deliberately -- lead with `log2FC`
#' rather than with `nObs`.
#'
#' @param obj An annotated object from [annotate_cells()].
#' @param um_per_px Microns per pixel. **Required, with no default.**
#' @param r_outer,r_inner The distance band, in microns: `r_inner < d <= r_outer`.
#' @param n_perm Label permutations used to build the null.
#' @param seed RNG seed, so a result is reproducible.
#' @param verbose Progress messages.
#'
#' @return A tibble, one row per sample x type-pair, with `RadiusInner`,
#'   `RadiusOuter`, `Sample`, `TypeA`, `TypeB` (`TypeA` <= `TypeB` in level
#'   order), `nA`, `nB`, `nObs`, `nExp`, `sdExp`, `log2FC`, `z` (`NA` where
#'   `sdExp` is 0), `pEmp` and `qBH` (Benjamini-Hochberg within sample).
#'
#' @export
colocation_test <- function(obj, um_per_px, r_outer = 50, r_inner = 0,
                            n_perm = 99, seed = 1, verbose = TRUE) {

    .check_um_per_px(um_per_px)
    cen <- cell_centroids(obj, um_per_px)
    lv <- levels(cen$CellType)
    K <- length(lv)
    set.seed(seed)

    out <- purrr::map(sort(unique(as.character(cen$Sample))), function(smp) {

        sub <- cen |> filter(Sample == smp)
        code <- as.integer(sub$CellType)
        n_type <- tabulate(code, nbins = K)

        if (verbose) {
            message(glue::glue(
                "colocation_test: {smp} -- {format(nrow(sub), big.mark = ',')} cells, ",
                "band {r_inner}-{r_outer} um"))
        }

        pr <- .sample_pairs(sub, r_outer = r_outer, r_inner = r_inner)
        if (is.null(pr)) {
            if (verbose) message(glue::glue("colocation_test: {smp} -- no pairs in band; skipped"))
            return(NULL)
        }
        if (verbose) {
            message(glue::glue("colocation_test: {smp} -- ",
                               "{format(length(pr$i), big.mark = ',')} pairs, ",
                               "{n_perm} permutations"))
        }

        obs <- .pair_type_counts(code[pr$i], code[pr$j], K)
        obs_v <- as.numeric(obs)
        acc <- .null_accumulator(K * K)
        for (p in seq_len(n_perm)) {
            perm <- sample(code)
            .null_update(acc,
                         as.numeric(.pair_type_counts(perm[pr$i], perm[pr$j], K)),
                         obs_v)
        }
        st <- .null_stats(acc)

        ## one row per unordered type-pair: the upper triangle including the
        ## diagonal, over the types this sample actually has
        grid <- expand.grid(a = seq_len(K), b = seq_len(K)) |>
            filter(a <= b, n_type[a] > 0, n_type[b] > 0)
        idx <- grid$a + K * (grid$b - 1L)

        tibble::tibble(
            RadiusInner = r_inner,
            RadiusOuter = r_outer,
            Sample = smp,
            TypeA = lv[grid$a],
            TypeB = lv[grid$b],
            nA = n_type[grid$a],
            nB = n_type[grid$b],
            nObs = obs_v[idx],
            nExp = st$nExp[idx],
            sdExp = st$sdExp[idx],
            log2FC = log2((nObs + 0.5) / (nExp + 0.5)),
            z = ifelse(sdExp == 0, NA_real_, (nObs - nExp) / sdExp),
            pEmp = .emp_p(st$ge[idx], st$le[idx], n_perm)
        ) |>
            mutate(qBH = stats::p.adjust(pEmp, method = "BH"))
    })

    out |>
        purrr::compact() |>
        purrr::list_rbind() |>
        arrange(Sample, desc(log2FC))
}

#' What the tissue in a ring around one cell type is made of
#'
#' For every cell of `anchor_type`, tabulates the types of all cells whose
#' centres fall in the ring `r_inner < d <= r_outer` around it. A ring, not a
#' disc: it describes the surrounding tissue rather than the cells in immediate
#' contact.
#'
#' The anchor's own type is included among the neighbour types. Anchor-anchor
#' neighbours measure how clustered the anchor population is, which is a result
#' in its own right, not a self-count to be removed.
#'
#' @section The null:
#' The anchor set is fixed from the observed labels. Each permutation reassigns
#' the cell-type labels across the sample's points and recounts the neighbour
#' types over the **same** anchors and the **same** rings. Anchors are not
#' recomputed from the permuted labels: that would move the anchor population
#' around the tissue, which measures something else and destroys the edge-effect
#' cancellation below.
#'
#' @section Edge effects:
#' Anchors near the tissue border have truncated rings, so `nNeighbors` and
#' `meanPerAnchor` are biased low. `log2Enrich` is robust to it -- the null is
#' computed on the same anchors with the same truncated rings, so the truncation
#' hits observed and expected identically and cancels in the ratio. No border
#' correction or guard band is applied, deliberately; lead with `pctOfRing` and
#' `log2Enrich` rather than with counts.
#'
#' @section The p-value floor:
#' As for [colocation_test()], `pEmp` cannot fall below `2 / (n_perm + 1)`.
#'
#' @param obj An annotated object from [annotate_cells()].
#' @param um_per_px Microns per pixel. **Required, with no default.**
#' @param anchor_type The cell type to measure outward from.
#' @param r_inner,r_outer The ring, in microns: `r_inner < d <= r_outer`.
#' @param n_perm Label permutations used to build the null.
#' @param seed RNG seed, so a result is reproducible.
#' @param verbose Progress messages.
#'
#' @return A list of `summary` (one row per sample x neighbour type, with
#'   `RadiusInner`, `RadiusOuter`, `nAnchors`, `nNeighbors`, `meanPerAnchor`,
#'   `pctOfRing`, `pctBackground`, `nExp`, `log2Enrich`, `z`, `pEmp`, `qBH`),
#'   `per_anchor` (one row per anchor cell: `UUID`, `Sample`, a count column per
#'   neighbour type, and `nRing`), and `params` (every argument, echoed back).
#'
#' @export
neighborhood_composition <- function(obj, um_per_px, anchor_type = "Tumor",
                                     r_inner = 20, r_outer = 50,
                                     n_perm = 99, seed = 1, verbose = TRUE) {

    .check_um_per_px(um_per_px)
    cen <- cell_centroids(obj, um_per_px)
    lv <- levels(cen$CellType)
    K <- length(lv)

    if (!anchor_type %in% lv) {
        stop("neighborhood_composition: no cell is called '", anchor_type,
             "'. Types present: ", paste(lv, collapse = ", "))
    }
    anchor_code <- match(anchor_type, lv)
    set.seed(seed)

    per_sample <- purrr::map(sort(unique(as.character(cen$Sample))), function(smp) {

        sub <- cen |> filter(Sample == smp)
        code <- as.integer(sub$CellType)
        n_cells <- length(code)
        anchors <- which(code == anchor_code)

        if (length(anchors) == 0) {
            if (verbose) message(glue::glue(
                "neighborhood_composition: {smp} -- no '{anchor_type}' cells; skipped"))
            return(NULL)
        }
        if (verbose) message(glue::glue(
            "neighborhood_composition: {smp} -- {format(length(anchors), big.mark = ',')} ",
            "anchors, ring {r_inner}-{r_outer} um"))

        pr <- .sample_pairs(sub, r_outer = r_outer, r_inner = r_inner)
        if (is.null(pr)) {
            if (verbose) message(glue::glue(
                "neighborhood_composition: {smp} -- no pairs in the ring; skipped"))
            return(NULL)
        }

        ## the pair list is undirected, so an anchor can be at either end: take
        ## the pairs anchored at i, then those anchored at j, and concatenate
        is_anchor <- code == anchor_code
        from_i <- is_anchor[pr$i]
        from_j <- is_anchor[pr$j]
        anch_of <- c(pr$i[from_i], pr$j[from_j])
        neigh_of <- c(pr$j[from_i], pr$i[from_j])

        if (length(neigh_of) == 0) {
            if (verbose) message(glue::glue(
                "neighborhood_composition: {smp} -- anchors have empty rings; skipped"))
            return(NULL)
        }
        if (verbose) message(glue::glue(
            "neighborhood_composition: {smp} -- ",
            "{format(length(neigh_of), big.mark = ',')} anchor-neighbour pairs, ",
            "{n_perm} permutations"))

        obs <- tabulate(code[neigh_of], nbins = K)
        acc <- .null_accumulator(K)
        for (p in seq_len(n_perm)) {
            perm <- sample(code)
            .null_update(acc, tabulate(perm[neigh_of], nbins = K), obs)
        }
        st <- .null_stats(acc)

        n_type <- tabulate(code, nbins = K)
        n_anchors <- length(anchors)
        keep <- n_type > 0

        summary <- tibble::tibble(
            RadiusInner = r_inner,
            RadiusOuter = r_outer,
            Sample = smp,
            NeighborType = lv,
            nAnchors = n_anchors,
            nNeighbors = obs,
            meanPerAnchor = obs / n_anchors,
            pctOfRing = 100 * obs / sum(obs),
            pctBackground = 100 * n_type / n_cells,
            nExp = st$nExp,
            log2Enrich = log2((obs + 0.5) / (st$nExp + 0.5)),
            z = ifelse(st$sdExp == 0, NA_real_, (obs - st$nExp) / st$sdExp),
            pEmp = .emp_p(st$ge, st$le, n_perm)
        )[keep, ] |>
            mutate(qBH = stats::p.adjust(pEmp, method = "BH"))

        ## per-anchor counts: tabulate a (anchor rank, type) integer key, for
        ## the same reason the pair counts do -- an anchor x type table built by
        ## paste() would not fit
        rank_of <- match(anch_of, anchors)
        counts <- matrix(
            tabulate(rank_of + as.numeric(n_anchors) * (code[neigh_of] - 1L),
                     nbins = as.numeric(n_anchors) * K),
            nrow = n_anchors, ncol = K,
            dimnames = list(NULL, lv)
        )
        per_anchor <- tibble::tibble(UUID = sub$UUID[anchors], Sample = smp) |>
            bind_cols(tibble::as_tibble(counts)) |>
            mutate(nRing = rowSums(counts))

        list(summary = summary, per_anchor = per_anchor)
    }) |> purrr::compact()

    if (length(per_sample) == 0) {
        stop("neighborhood_composition: no sample yielded an anchor with a ",
             "non-empty ring at ", r_inner, "-", r_outer, " um")
    }

    list(
        summary = purrr::map(per_sample, "summary") |> purrr::list_rbind(),
        per_anchor = purrr::map(per_sample, "per_anchor") |> purrr::list_rbind(),
        params = list(
            um_per_px = um_per_px, anchor_type = anchor_type,
            r_inner = r_inner, r_outer = r_outer,
            n_perm = n_perm, seed = seed
        )
    )
}
