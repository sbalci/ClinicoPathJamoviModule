# ============================================================================
# ggstatsplot UTILITY FUNCTIONS
# ============================================================================
# Shared helpers for the analyses that wrap ggstatsplot / statsExpressions.

#' Run an expression with base R's `as.character()` restored for formulas
#'
#' This is not defensive programming, it is a live session-wide bug.
#'
#' Any package loaded in the session can register an `as.character.formula` S3
#' method, and `formula.tools` does exactly that - returning ONE deparsed string
#' where base R returns three elements:
#'
#' ```
#' as.character(v ~ g)   # base:          c("~", "v", "g")   length 3
#' as.character(v ~ g)   # formula.tools: "v ~ g"            length 1
#' ```
#'
#' In this umbrella repo `formula.tools` arrives via `logistf` (an Import, used by
#' the odds-ratio and Firth regression analyses). The generated jjstatsplot module
#' ships neither, but the shield stays: the method is registered session-wide by
#' whoever loads it, so a user with `formula.tools` (or any package depending on
#' it) attached breaks these plots just the same.
#'
#' `stats::oneway.test()` does `dp <- as.character(formula)` and rejects
#' anything of length != 3 with "a two-sided formula is required". So once that
#' method is registered, Welch's ANOVA is broken for the whole R session, not just
#' inside this package.
#'
#' The damage is silent. `ggstatsplot` swallows the failure and hands back a plot
#' whose `subtitle` is NULL, so a user who ticks "Statistical results in plot"
#' gets a figure with no statistics on it and nothing saying why. Measured on
#' three groups of 40 with ClinicoPath loaded:
#'
#' | call                              | subtitle |
#' |-----------------------------------|----------|
#' | `ggbetweenstats()`, 3 groups      | NULL     |
#' | `ggbetweenstats()`, 2 groups      | present  |
#' | `ggwithinstats()`, 3 measurements | present  |
#'
#' Only the 3-or-more-group BETWEEN-subjects parametric path routes through
#' `oneway.test`; the two-group `t.test` path and the repeated-measures path
#' (which goes through afex) are unharmed.
#'
#' The S3 methods table is an ordinary unlocked environment, so the fix is to
#' swap the method for the duration of the call and put it back on exit. Wrap
#' every `ggbetweenstats()`, `grouped_ggbetweenstats()` and
#' `statsExpressions::oneway_anova()` call in an analysis with this.
#'
#' Usage: wrap the call, e.g.
#' `withBaseFormulaChar(ggstatsplot::ggbetweenstats(data, x = g, y = v))`.
#'
#' Internal and unexported on purpose: no NAMESPACE entry and no .Rd file, so it
#' needs neither `jmvtools::prepare()` nor `devtools::document()` to take effect.
#'
#' @param expr Expression to evaluate. Lazily evaluated inside the shield.
#' @return The value of `expr`.
#' @noRd
#' @keywords internal
withBaseFormulaChar <- function(expr) {
    tbl <- tryCatch(get(".__S3MethodsTable__.", envir = asNamespace("base")),
                    error = function(e) NULL)

    # Only intervene when the offending method is actually registered, and only
    # when the binding can be written back. Never leave the table modified.
    shield <- !is.null(tbl) &&
        exists("as.character.formula", envir = tbl, inherits = FALSE) &&
        !environmentIsLocked(tbl) &&
        !isTRUE(tryCatch(bindingIsLocked("as.character.formula", tbl),
                         error = function(e) TRUE))

    if (shield) {
        old <- get("as.character.formula", envir = tbl, inherits = FALSE)
        assign("as.character.formula",
               function(x, ...) as.character(unclass(x)), envir = tbl)
        # add = TRUE so this still runs if `expr` itself registers an on.exit,
        # and it runs on the error path as well as the happy one.
        on.exit(assign("as.character.formula", old, envir = tbl), add = TRUE)
    }

    force(expr)
}

#' Does a ggstatsplot / statsExpressions result change with the random seed?
#'
#' Measured on statsExpressions 2.1.1 by computing each test under two seeds:
#' parametric results never change; robust ones do (bootstrap intervals), except for
#' correlations and contingency tables; Bayesian ones do for one- and two-sample tests,
#' correlations and contingency tables (posterior draws); nonparametric ones only for
#' three or more independent groups (the bootstrap interval of epsilon-squared).
#'
#' @param type `typestatistics`: "parametric", "nonparametric", "robust" or "bayes".
#' @param design "between", "within", "one_sample", "correlation" or "contingency".
#' @param k Number of groups or measurements compared.
#' @return TRUE when the reported numbers depend on the seed.
#' @noRd
#' @keywords internal
statsSeedMatters <- function(type, design, k = 2L) {
    switch(design,
        between     = type == "robust" || (type == "bayes" && k == 2) || (type == "nonparametric" && k >= 3),
        within      = type == "robust" || (type == "bayes" && k == 2),
        one_sample  = type %in% c("robust", "bayes"),
        correlation = type == "bayes",
        contingency = type == "bayes",
        FALSE)
}

#' Does the Bayes-factor CAPTION under a plot depend on the random seed?
#'
#' ggstatsplot writes a `bf.message` caption only for a PARAMETRIC test, and that caption
#' carries a posterior median and a credible interval, both sampled. Measured on
#' ggstatsplot 0.13.x with two seeds: the caption differs for a one-sample test, a
#' correlation, a contingency table, a paired comparison with any number of measurements,
#' and an independent comparison of exactly two groups; an independent comparison of three
#' or more groups draws no caption at all.
#'
#' @param type `typestatistics`.
#' @param design "between", "within", "one_sample", "correlation" or "contingency".
#' @param k Number of groups or measurements compared.
#' @param bfmessage Whether the Bayes-factor message is switched on.
#' @return TRUE when the caption is drawn AND its numbers depend on the seed.
#' @noRd
#' @keywords internal
captionSeedMatters <- function(type, design, k = 2L, bfmessage = FALSE) {
    if (!isTRUE(bfmessage) || !identical(type, "parametric")) return(FALSE)
    switch(design,
        between     = k == 2,
        within      = TRUE,
        one_sample  = TRUE,
        correlation = TRUE,
        contingency = TRUE,
        FALSE)
}

#' Name the random seed under a plot whose statistics depend on it
#'
#' Appended to an existing text caption; a combined (patchwork) plot gets it as the
#' overall caption.
#'
#' @param plot A ggplot or patchwork object.
#' @param self The analysis, for the translation of the caption.
#' @param seed The seed the statistics were computed with.
#' @return The plot with the caption added.
#' @noRd
#' @keywords internal
addSeedCaption <- function(plot, self, seed) {
    text <- jmvcore::format(.("Random seed: {seed}"), seed = seed)
    if (inherits(plot, "patchwork"))
        return(plot + patchwork::plot_annotation(caption = text))
    old <- plot$labels$caption
    caption <- if (is.character(old) && any(nzchar(old))) {
        paste(c(old, text), collapse = "\n")
    } else if (is.language(old) || is.expression(old)) {
        # The Bayes-factor caption is plotmath, not text: stack the seed under it with atop()
        # rather than replacing it, or the figure loses the Bayes factor it was drawing.
        expr <- if (is.expression(old)) old[[1]] else old
        as.expression(bquote(atop(.(expr), .(text))))
    } else {
        text
    }
    plot + ggplot2::labs(caption = caption)
}
