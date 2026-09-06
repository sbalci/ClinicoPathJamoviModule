# ============================================================================
# FORMULA / MODEL-MATRIX HELPERS
# ============================================================================
# Moved from R/utils.R (2026-09-06 ClinicoPathDescriptives audit). Callers are the
# penalised/regularised regression analyses (jsurvival, meddecide, JamoviTest), so
# this file is listed in those modules' `r_files` only - it is not shipped to the
# descriptives module, where it was dead weight.

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
