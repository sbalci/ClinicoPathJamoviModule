# ============================================================================
# FORMULA / MODEL-MATRIX HELPERS
# ============================================================================
# Variable-name quoting for formula strings, and the allow-listed survival
# formula parser. Used by the survival, penalised-regression and ROC analyses.

#' Backtick-quote variable names for use in a formula string
#'
#' The inverse of [.stripBackticks()]: takes raw jamovi variable names and
#' returns them quoted wherever R would otherwise fail to parse them as a
#' symbol. Use it when BUILDING a formula string, never as a `data[[...]]` key
#' -- a backticked name is not a column name, and the lookup returns `NULL`.
#'
#' This delegates to [jmvcore::composeTerm()] rather than testing
#' `grepl("[^a-zA-Z0-9._]", x)` and wrapping in backticks by hand, which is what
#' it did until 2026-09-06. That hand-rolled rule quoted only names containing a
#' character outside `[A-Za-z0-9._]`, and so silently produced formulas that do
#' not parse -- or, worse, parse into something else:
#'
#'   * `1stGrade` -- leading digit, no "special" character, left unquoted; the
#'     formula fails to parse.
#'   * `if`, `for`, `function` -- reserved words, left unquoted; parse error.
#'   * `` a`b `` -- wrapped to produce `` `a`b` ``, three backticks; parse error.
#'   * `TRUE` -- left unquoted, so the term parses as the literal `TRUE` rather
#'     than as the column. That one is the dangerous case: it fails silently.
#'
#' `jmvcore::composeTerm()` is jamovi's own escaper, handles all four, and
#' agrees with the old rule on every name the old rule got right -- so this is a
#' strict improvement, not a behaviour change, for existing analyses.
#'
#' Matching backticked names against fitted-model coefficients (as
#' `multisurvival-interactions.R` does) stays correct: `coxph()` derives its
#' coefficient names by deparsing the same terms, and `composeTerm()`'s output
#' has been verified to match those names for plain, spaced, punctuated and
#' digit-leading variable names alike.
#'
#' @param var_names Character vector of raw variable names.
#' @return Character vector, quoted where quoting is needed. Length and order
#'   are preserved, so it is safe in `paste()`/`paste0()` alongside other
#'   parallel vectors.
#' @keywords internal
.escapeVariableNames <- function(var_names) {
    if (length(var_names) == 0L) return(character(0))
    # composeTerm() is scalar (composeTerms() takes a list of TERM components,
    # which is a different thing), so map over the vector.
    vapply(as.character(var_names), jmvcore::composeTerm,
           character(1), USE.NAMES = FALSE)
}

#' Strip formula backticks from design-matrix column names
#'
#' `model.matrix()` builds its column names from the terms of a formula, and
#' `terms()` DEPARSES a non-syntactic data-frame column name -- so a column
#' called `Ki-67 (%)` arrives as `` `Ki-67 (%)` `` (backticks at both ends) and
#' a factor `Tumor Grade` with level `Low` arrives as `` `Tumor Grade`Low ``
#' (the closing backtick in the MIDDLE, which is why an anchored `^`|`$` strip
#' is not enough). jamovi variable names routinely contain spaces, hyphens,
#' parentheses and percent signs, and `jmvcore` deliberately restores those raw
#' names into `self$data`, so this is the normal case, not an exotic one.
#'
#' Left alone the backticks are printed verbatim in results tables and plot
#' labels, and they break every lookup that matches a design-matrix column
#' against the original variable name (`==`, `%in%`, `match()`, `startsWith()`),
#' which silently drops values or falls back to a wrong default.
#'
#' Backticks are quoting, never part of a name, so they are removed outright.
#' Stripping can in principle collide two distinct columns onto one name (a
#' numeric `Tumor GradeLow` beside the factor dummy above), so the result is
#' de-duplicated -- every downstream consumer looks columns up by name.
#'
#' @param x A matrix (its `colnames` are cleaned) or a character vector.
#' @return The same object with backticks removed and names made unique.
#' @keywords internal
.stripBackticks <- function(x) {
    clean <- function(nm) {
        nm <- gsub("`", "", nm, fixed = TRUE)
        if (anyDuplicated(nm)) nm <- make.unique(nm, sep = "_")
        nm
    }
    if (is.null(x)) return(x)
    if (is.matrix(x) || is.data.frame(x)) {
        if (is.null(colnames(x))) return(x)
        colnames(x) <- clean(colnames(x))
        return(x)
    }
    clean(as.character(x))
}


#' Build a survival formula safely via jmvcore::asFormula
#'
#' Wraps `jmvcore::asFormula` with the function allow-list extended to cover
#' common survival modelling helpers (`Surv`, `strata`, `cluster`, `frailty`,
#' `tt`, `pspline`, `ns`, `bs`, `I`, `const`, `finegray`). Use this instead of
#' base `stats::as.formula()` in survival / Cox / Fine-Gray paths so the formula
#' goes through jmvcore's allow-listed parser.
#'
#' The returned formula's environment is set to the CALLER's frame. `asFormula`
#' otherwise leaves it pointing at its own evaluation frame, which does not hold
#' the caller's `mydata`. Model fitting still works, because `coxph(fml, data=)`
#' is handed the data directly — but any method that later re-evaluates the
#' model call does not get it. `cox.zph()` is the visible casualty: ticking
#' "Proportional hazards assumption" aborted the entire analysis with
#' "object 'mydata' not found", taking every other result down with it.
#'
#' @param x A character formula string (e.g. `"survival::Surv(t, d) ~ x"`).
#' @param env Environment to attach to the returned formula. Defaults to the
#'   calling frame, which is what downstream re-evaluation needs.
#' @return A parsed formula object.
#' @keywords internal
.asSurvivalFormula <- function(x, env = parent.frame()) {
    fml <- jmvcore::asFormula(
        x,
        additional_allowed_functions = c(
            "Surv", "strata", "cluster", "frailty", "tt",
            "pspline", "ns", "bs", "I", "const", "finegray"
        )
    )
    if (inherits(fml, "formula")) environment(fml) <- env
    fml
}
