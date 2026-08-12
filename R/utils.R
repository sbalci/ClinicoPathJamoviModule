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
#' @return Vector of predicted probabilities
#' @export
raw_to_prob <- function(values, actual, direction = ">=") {
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

    glm_result <- tryCatch({
        model <- stats::glm(actual_clean ~ predictor, family = binomial(link = "logit"))
        # Predict for ALL original values (including those with NA in actual)
        new_predictor <- if (direction == "<=") -values else values
        stats::predict(model, newdata = data.frame(predictor = new_predictor), type = "response")
    }, warning = function(w) {
        # Refit with warnings muted, but PASS THE WARNING ON.
        #
        # This used to swallow it silently. The two warnings glm raises here are
        # exactly the ones the caller needs to know about: "fitted probabilities
        # numerically 0 or 1 occurred" means separation, and "algorithm did not
        # converge" means the fit is unusable. Both produce predicted risks that
        # look plausible and are not, and IDI/NRI are computed from them.
        warning(sprintf(
            "raw_to_prob: logistic fit reported '%s'. Predicted probabilities may be unreliable (separation or non-convergence); interpret IDI/NRI with caution.",
            conditionMessage(w)), call. = FALSE)
        suppressWarnings({
            model <- stats::glm(actual_clean ~ predictor, family = binomial(link = "logit"))
            new_predictor <- if (direction == "<=") -values else values
            stats::predict(model, newdata = data.frame(predictor = new_predictor), type = "response")
        })
    }, error = function(e) {
        # Fail rather than substitute a non-probability.
        #
        # The previous fallback returned rank(x)/(n+1) -- the empirical
        # percentile of the PREDICTOR. It never touched `actual`, so its mean is
        # ~0.5 whatever the outcome prevalence and it is not an estimate of
        # P(Y = 1 | x) in any sense. Feeding that to IDI (a difference of mean
        # predicted risk between events and non-events) or to NRI category cuts
        # silently swaps a calibrated risk scale for a uniform rank scale.
        warning(sprintf(
            "raw_to_prob: logistic regression failed (%s); returning NA rather than a rank-based substitute, which would not be a probability.",
            conditionMessage(e)), call. = FALSE)
        rep(NA_real_, length(values))
    })

    probs <- as.numeric(glm_result)

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
    new_probs <- raw_to_prob(new_values, actual, direction)
    ref_probs <- raw_to_prob(ref_values, actual, direction)
    
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
            boot_new_probs <- raw_to_prob(boot_new, boot_actual, direction)
            boot_ref_probs <- raw_to_prob(boot_ref, boot_actual, direction)

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
                    p_value = NA_real_, n_valid_boots = 0))
    }

    if (length(boot_idi_valid) < n_boot * 0.5) {
        warning(sprintf("Only %d of %d bootstrap replicates succeeded - results may be unreliable",
                        length(boot_idi_valid), n_boot), call. = FALSE)
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
        n_valid_boots = valid_boots
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
#' @return Invisible NULL (called for side effects).
#' @export
clinicopath_startup_message <- function() {
    packageStartupMessage(
        "Serdar Balci MD Pathologist\nhttps://www.serdarbalci.com/\n"
    )
    invisible(NULL)
}
