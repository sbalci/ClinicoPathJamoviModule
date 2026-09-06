# ============================================================================
# CLINICOPATH UTILITY FUNCTIONS
# ============================================================================
# This file contains shared utility functions used across the ClinicoPath module
# Functions are organized by category and should be generic and reusable

# ============================================================================
# PACKAGE DEPENDENCIES AND OPERATORS
# ============================================================================

#' @importFrom stats binomial qbeta glm predict quantile cov var
#' @importFrom utils sessionInfo tail
NULL

# Suppress R CMD CHECK notes for global variables used in NSE / auto-generated
# class references that are defined in other sub-packages or lazily.
utils::globalVariables(c(
    # NSE / ggplot2 aesthetics used inside functions
    "label",
    "lo_y",
    "type",
    "x",
    # aes() columns of the Firth forest plot in R/oddsratio.b.R (.firthOrPlot)
    "level",
    "lower",
    "n_show",
    "or_text",
    "upper",
    "var_show",
    "y"
))

#' Run third-party code without leaking package chatter into the results
#'
#' jamovi's engine captures `message()` and `warning()` conditions raised while
#' an analysis runs and renders them in the "Analysis Notes" panel, where users
#' see them. Third-party modelling packages emit a lot of chatter that is
#' meaningless to a pathologist reading their results -- glmnet's `cox.ties`
#' migration notice, for example, appeared twelve times in a single Lasso-Cox
#' run.
#'
#' Wrap a third-party call in `.quietly()` to keep that noise out of the
#' results pane. It suppresses ALL messages (package chatter is never the user's
#' problem) but muffles only *deprecation-flavoured* warnings, matched by
#' `deprecation_pattern`. Substantive warnings -- non-convergence, NAs
#' introduced, rank deficiency -- still propagate, because those change how the
#' output should be read and must not be hidden.
#'
#' @param expr Expression to evaluate.
#' @param deprecation_pattern Regex matched against warning messages; matches are
#'   muffled. Defaults to the usual deprecation/migration vocabulary.
#' @return The value of `expr`.
#' @keywords internal
#' @examples
#' \dontrun{
#' fit <- .quietly(glmnet::cv.glmnet(x, y, family = "cox"))
#' }
.quietly <- function(expr,
                     deprecation_pattern = paste(
                         "deprecat", "defunct", "superseded", "will change from",
                         "is no longer", "renamed", "future version", "startup",
                         sep = "|")) {
    withCallingHandlers(
        suppressMessages(suppressPackageStartupMessages(expr)),
        warning = function(w) {
            if (grepl(deprecation_pattern, conditionMessage(w), ignore.case = TRUE))
                invokeRestart("muffleWarning")
        }
    )
}

#' Interpolate a translated string without risking an unbounded substitution loop
#'
#' `jmvcore::format()` re-scans the ENTIRE string from position 1 after each
#' substitution. If a substituted value contains its own placeholder -- e.g.
#' `jmvcore::format("LR ({value})", value = "x {value} y")` -- the substituter finds
#' that placeholder again, substitutes again, and never terminates. The loop runs in
#' code that does not poll R's interrupt handler, so it survives `setTimeLimit()` and
#' has to be SIGKILLed; inside jamovi it freezes the analysis engine rather than
#' raising an error.
#'
#' Two realistic ways in: a translator copies a `{placeholder}` into the msgstr of the
#' very string that placeholder belongs to, or a dataset carries a column/level named
#' literally `{n}` that is then interpolated by name.
#'
#' This wrapper is a pass-through. When no supplied value contains a brace -- the
#' overwhelming majority of calls -- it delegates untouched and the output is
#' byte-identical to calling `jmvcore::format()` directly. Only when a value actually
#' contains a `{` are that value's braces neutralised, so a pathological input
#' degrades to slightly different text instead of hanging.
#'
#' Verified trigger conditions (R, jmvcore 2.7.x): a value containing its OWN
#' placeholder name hangs; a value containing a DIFFERENT supplied name substitutes and
#' terminates; an UNKNOWN `{name}` renders as an ellipsis; a bare `{ }` is left literal.
#'
#' @param .format_string Format string, normally wrapped in `.()`.
#' @param ... Named placeholder values.
#' @return The interpolated string.
#' @keywords internal
.fmt <- function(.format_string, ...) {
    values <- list(...)
    risky <- vapply(values, function(v) {
        v <- tryCatch(as.character(v), error = function(e) "")
        length(v) > 0L && any(grepl("{", v, fixed = TRUE), na.rm = TRUE)
    }, logical(1))

    if (any(risky)) {
        # Only the offending values are touched, and only their braces. A brace in a
        # variable name or an error message is display text, never markup, so replacing
        # it with a lookalike keeps the message readable and cannot re-enter the loop.
        values[risky] <- lapply(values[risky], function(v) {
            v <- as.character(v)
            v <- gsub("{", "(", v, fixed = TRUE)
            gsub("}", ")", v, fixed = TRUE)
        })
        return(do.call(jmvcore::format, c(list(.format_string), values)))
    }

    do.call(jmvcore::format, c(list(.format_string), values))
}


#' Null-coalescing operator
#' @name null_coalescing
#' @aliases %||%
#' @param x Left-hand side value
#' @param y Right-hand side default value
#' @return `x` if it is not `NULL`, otherwise `y`.
#' @keywords internal
#' @export
# Defined locally rather than re-exported from rlang. rlang's %||% was deprecated
# once base R 4.4 gained its own, and taking it from rlang made loading this
# package depend on that one symbol still being exported: devtools::document()
# failed here with "object '%||%' is not exported by 'namespace:rlang'", which
# blocks EVERY regeneration in the module, not just this file. The operator is
# one line; owning it removes the coupling.
#
# Plain `#` comments on purpose - roxygen consumes any `#'` lines after @export
# as that tag's VALUE, which is the "@export must be only 1 line long" warning.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Pipe operator
#' @name %>%
#' @rdname pipe
#' @param lhs A value passed into the right-hand side function.
#' @param rhs A function call to which `lhs` is supplied as the first argument.
#' @return The result of calling `rhs` with `lhs` as its first argument.
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# ============================================================================
# BOOTSTRAP UTILITIES
# ============================================================================
