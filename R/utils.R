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
    "x"
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

#' Escape variable names containing special characters for formulas
#'
#' Adds backticks around names that contain anything other than
#' `[A-Za-z0-9._]`. Centralises the helper that previously lived in
#' multiple `.b.R` files so the implementation stays in one place.
#'
#' @param var_names Character vector of variable names.
#' @return Character vector with non-syntactic names backtick-quoted.
#' @keywords internal
.escapeVariableNames <- function(var_names) {
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    var_names
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
# ROC ANALYSIS UTILITIES
# ============================================================================

#' Convert raw test values to predicted probabilities using ROC curve
#' @description Maps raw test values to probabilities based on their position in the ROC curve
#' @param values Raw test values
#' @param actual Binary outcomes (0/1)
#' @param direction Direction of test (">=" or "<=")
#' @keywords internal
#' @return Vector of predicted probabilities
#' @param warn Logical; emit a consolidated warning when logistic calibration
#'   shows separation, non-convergence, or fitting failure. Bootstrap callers
#'   set this to `FALSE` and report one aggregate diagnostic instead.
#' @export
raw_to_prob <- function(values, actual, direction = ">=", warn = TRUE) {
    # Validate inputs
    if (length(values) != length(actual)) {
        stop("Values and actual must have the same length")
    }

    if (!direction %in% c(">=", "<=")) {
        stop("Direction must be '>=' or '<='")
    }

    # Remove missing values
    complete_cases <- !is.na(values) & !is.na(actual)
    values_clean <- values[complete_cases]
    actual_clean <- actual[complete_cases]

    if (length(values_clean) == 0) {
        warning("No complete cases found")
        return(rep(NA, length(values)))
    }

    # Initialize probabilities vector
    probs <- rep(NA, length(values))

    # Use logistic regression to convert raw values to predicted probabilities
    # This is the statistically correct approach for IDI/NRI calculations
    predictor <- if (direction == "<=") -values_clean else values_clean

    fit_warnings <- character(0)
    glm_result <- tryCatch(
        withCallingHandlers({
            model <- stats::glm(actual_clean ~ predictor, family = binomial(link = "logit"))
            new_predictor <- if (direction == "<=") -values else values
            stats::predict(model, newdata = data.frame(predictor = new_predictor), type = "response")
        }, warning = function(w) {
            fit_warnings <<- c(fit_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }),
        error = function(e) {
        # Fail rather than substitute a non-probability.
        #
        # The previous fallback returned rank(x)/(n+1) -- the empirical
        # percentile of the PREDICTOR. It never touched `actual`, so its mean is
        # ~0.5 whatever the outcome prevalence and it is not an estimate of
        # P(Y = 1 | x) in any sense. Feeding that to IDI (a difference of mean
        # predicted risk between events and non-events) or to NRI category cuts
        # silently swaps a calibrated risk scale for a uniform rank scale.
        fit_warnings <<- c(fit_warnings, paste("fit failed:", conditionMessage(e)))
        rep(NA_real_, length(values))
    })

    probs <- as.numeric(glm_result)
    fit_warnings <- unique(fit_warnings)
    attr(probs, "fit_warnings") <- fit_warnings

    if (isTRUE(warn) && length(fit_warnings) > 0L) {
        warning(sprintf(
            paste0(
                "raw_to_prob: logistic calibration reported %s. Predicted probabilities ",
                "may be unreliable (separation or non-convergence); interpret IDI/NRI ",
                "with caution."
            ),
            paste(sprintf("'%s'", fit_warnings), collapse = "; ")
        ), call. = FALSE)
    }

    # Ensure probabilities are in [0,1] range
    probs[!is.na(probs) & probs < 0] <- 0
    probs[!is.na(probs) & probs > 1] <- 1

    return(probs)
}

# ============================================================================
# BOOTSTRAP UTILITIES
# ============================================================================

#' Bootstrap IDI calculation with confidence intervals
#' @description Calculates Integrated Discrimination Improvement with bootstrap confidence intervals
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction (">=" or "<=")
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @keywords internal
#' @return List with IDI, confidence intervals, and p-value
#' @export
bootstrapIDI <- function(new_values, ref_values, actual,
                         direction = ">=", n_boot = 1000,
                         conf_level = 0.95) {
    # Validate inputs
    n <- length(actual)
    if (length(new_values) != n || length(ref_values) != n) {
        stop("All input vectors must have the same length")
    }
    
    if (n_boot < 100) {
        warning("Low number of bootstrap iterations may produce unreliable results")
    }
    
    # Ensure actual is binary
    if (!all(actual %in% c(0, 1))) {
        stop("Actual values must be binary (0 or 1)")
    }
    
    # Original IDI calculation
    new_probs <- raw_to_prob(new_values, actual, direction, warn = FALSE)
    ref_probs <- raw_to_prob(ref_values, actual, direction, warn = FALSE)
    original_fit_warning <- length(attr(new_probs, "fit_warnings")) > 0L ||
        length(attr(ref_probs, "fit_warnings")) > 0L
    
    # Calculate discrimination slopes
    events <- actual == 1
    non_events <- actual == 0
    
    # Check for events and non-events
    if (sum(events) == 0 || sum(non_events) == 0) {
        warning("No events or non-events found in data")
        return(list(idi = NA, ci_lower = NA, ci_upper = NA, p_value = NA))
    }
    
    # Original IDI
    original_idi <- (mean(new_probs[events], na.rm = TRUE) -
                     mean(new_probs[non_events], na.rm = TRUE)) -
        (mean(ref_probs[events], na.rm = TRUE) -
         mean(ref_probs[non_events], na.rm = TRUE))
    
    # Bootstrap
    boot_idi <- numeric(n_boot)
    valid_boots <- 0
    warning_boots <- 0L
    
    for (i in 1:n_boot) {
        boot_idx <- sample(n, n, replace = TRUE)
        
        boot_new <- new_values[boot_idx]
        boot_ref <- ref_values[boot_idx]
        boot_actual <- actual[boot_idx]
        
        # Skip if no events or non-events in bootstrap sample
        if (sum(boot_actual == 1) == 0 || sum(boot_actual == 0) == 0) {
            boot_idi[i] <- NA
            next
        }
        
        # Calculate probabilities for bootstrap sample.
        #
        # The result is assigned from the value of tryCatch(), NOT from inside
        # the error handler. `boot_idi[i] <- NA` written in the handler assigns
        # into the handler function's own frame and never reaches this one, so
        # every failed replicate silently kept its preallocated 0.0 -- dragging
        # the estimate and the CI toward zero and counting in BOTH p-value
        # tails. It also meant the "many bootstraps failed" warning below could
        # never fire from this path.
        boot_idi[i] <- tryCatch({
            boot_new_probs <- raw_to_prob(boot_new, boot_actual, direction, warn = FALSE)
            boot_ref_probs <- raw_to_prob(boot_ref, boot_actual, direction, warn = FALSE)
            if (length(attr(boot_new_probs, "fit_warnings")) > 0L ||
                length(attr(boot_ref_probs, "fit_warnings")) > 0L) {
                warning_boots <- warning_boots + 1L
            }

            # Calculate IDI
            boot_events <- boot_actual == 1
            boot_non_events <- boot_actual == 0

            val <- (mean(boot_new_probs[boot_events], na.rm = TRUE) -
                    mean(boot_new_probs[boot_non_events], na.rm = TRUE)) -
                (mean(boot_ref_probs[boot_events], na.rm = TRUE) -
                 mean(boot_ref_probs[boot_non_events], na.rm = TRUE))
            if (is.finite(val)) {
                valid_boots <- valid_boots + 1
                val
            } else NA_real_
        }, error = function(e) NA_real_)
    }
    
    # Remove failed bootstrap samples
    boot_idi_valid <- boot_idi[!is.na(boot_idi)]
    
    if (length(boot_idi_valid) == 0) {
        # Nothing to summarise. Returning quantile(numeric(0)) and
        # mean(logical(0)) would hand back ci = NA and p = NaN with no
        # indication that every replicate had failed.
        warning("All bootstrap replicates failed; no confidence interval or p-value can be computed.",
                call. = FALSE)
        return(list(idi = original_idi, ci_lower = NA_real_, ci_upper = NA_real_,
                    p_value = NA_real_, n_valid_boots = 0,
                    fit_warning = original_fit_warning,
                    fit_warning_boots = warning_boots))
    }

    if (length(boot_idi_valid) < n_boot * 0.5) {
        warning(sprintf("Only %d of %d bootstrap replicates succeeded - results may be unreliable",
                        length(boot_idi_valid), n_boot), call. = FALSE)
    }
    if (original_fit_warning || warning_boots > 0L) {
        warning(sprintf(
            paste0(
                "Logistic calibration showed separation or non-convergence in the original ",
                "fit or %d of %d bootstrap replicates; IDI and its interval may be unstable."
            ),
            warning_boots, n_boot
        ), call. = FALSE)
    }
    
    # Calculate confidence intervals
    alpha <- 1 - conf_level
    ci_lower <- quantile(boot_idi_valid, alpha/2, na.rm = TRUE)
    ci_upper <- quantile(boot_idi_valid, 1 - alpha/2, na.rm = TRUE)
    
    # Calculate p-value (two-sided test for IDI = 0).
    #
    # (1 + count) / (B + 1) rather than count / B: a bootstrap p-value can never
    # legitimately be exactly 0, and the uncorrected form returned 0 whenever
    # every replicate fell on one side, implying impossible precision from a
    # finite number of replicates.
    B <- length(boot_idi_valid)
    p_value <- min(1, 2 * min(
        (1 + sum(boot_idi_valid <= 0)) / (B + 1),
        (1 + sum(boot_idi_valid >= 0)) / (B + 1)
    ))
    
    return(list(
        idi = original_idi,
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper),
        p_value = p_value,
        n_valid_boots = valid_boots,
        fit_warning = original_fit_warning,
        fit_warning_boots = warning_boots
    ))
}

# ============================================================================
# GENERAL UTILITIES
# ============================================================================

#' Package startup message
#'
#' Returns the package author / website banner. Called by `.onAttach()` (see
#' `R/zzz.R`) via `packageStartupMessage()`, which routes to the message stream
#' and respects `suppressPackageStartupMessages()`. Available as an exported
#' function so users can print the banner explicitly.
#'
#' @keywords internal
#' @return Invisible NULL (called for side effects).
#' @export
clinicopath_startup_message <- function() {
    packageStartupMessage(
        "Serdar Balci MD Pathologist\nhttps://www.serdarbalci.com/\n"
    )
    invisible(NULL)
}
