# ============================================================================
# CLINICOPATH ggstatsplot UTILITY FUNCTIONS
# ============================================================================
# Shared helpers for the analyses that wrap ggstatsplot / statsExpressions.

#' Run an expression with base R's `as.character()` restored for formulas
#'
#' This is not defensive programming, it is a live session-wide bug.
#'
#' `logistf` is in this package's Imports (the odds-ratio analysis needs it) and
#' pulls in `formula.tools`, which registers an `as.character.formula` S3 method
#' returning ONE deparsed string where base R returns three elements:
#'
#' ```
#' as.character(v ~ g)   # base:          c("~", "v", "g")   length 3
#' as.character(v ~ g)   # formula.tools: "v ~ g"            length 1
#' ```
#'
#' `stats::oneway.test()` does `dp <- as.character(formula)` and rejects
#' anything of length != 3 with "a two-sided formula is required". So merely
#' loading ClinicoPath breaks Welch's ANOVA for the whole R session, not just
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
