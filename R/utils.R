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
#' @importFrom rlang %||%
`%||%` <- rlang::`%||%`

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
        # Get fitted probabilities for the complete cases
        fitted_probs <- model$fitted.values
        # Predict for ALL original values (including those with NA in actual)
        new_predictor <- if (direction == "<=") -values else values
        stats::predict(model, newdata = data.frame(predictor = new_predictor), type = "response")
    }, warning = function(w) {
        # glm may warn about convergence or fitted probabilities near 0/1
        # Suppress and continue
        suppressWarnings({
            model <- stats::glm(actual_clean ~ predictor, family = binomial(link = "logit"))
            new_predictor <- if (direction == "<=") -values else values
            stats::predict(model, newdata = data.frame(predictor = new_predictor), type = "response")
        })
    }, error = function(e) {
        # Fallback: use empirical CDF-based rank probabilities
        # This handles perfect separation and other glm failures
        warning("Logistic regression failed; using rank-based probability estimates")
        rank_probs <- rep(NA, length(values))
        ranks <- rank(values_clean, ties.method = "average")
        normalized_ranks <- ranks / (length(ranks) + 1)
        # Map clean ranks to original positions
        for (i in seq_along(values)) {
            if (!is.na(values[i])) {
                match_idx <- which(values_clean == values[i])
                if (length(match_idx) > 0) {
                    rank_probs[i] <- mean(normalized_ranks[match_idx])
                }
            }
        }
        if (direction == "<=") rank_probs <- 1 - rank_probs
        rank_probs
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
        
        # Calculate probabilities for bootstrap sample
        tryCatch({
            boot_new_probs <- raw_to_prob(boot_new, boot_actual, direction)
            boot_ref_probs <- raw_to_prob(boot_ref, boot_actual, direction)
            
            # Calculate IDI
            boot_events <- boot_actual == 1
            boot_non_events <- boot_actual == 0
            
            boot_idi[i] <- (mean(boot_new_probs[boot_events], na.rm = TRUE) -
                            mean(boot_new_probs[boot_non_events], na.rm = TRUE)) -
                (mean(boot_ref_probs[boot_events], na.rm = TRUE) -
                 mean(boot_ref_probs[boot_non_events], na.rm = TRUE))
            valid_boots <- valid_boots + 1
        }, error = function(e) {
            boot_idi[i] <- NA
        })
    }
    
    # Remove failed bootstrap samples
    boot_idi_valid <- boot_idi[!is.na(boot_idi)]
    
    if (length(boot_idi_valid) < n_boot * 0.5) {
        warning("Many bootstrap samples failed - results may be unreliable")
    }
    
    # Calculate confidence intervals
    alpha <- 1 - conf_level
    ci_lower <- quantile(boot_idi_valid, alpha/2, na.rm = TRUE)
    ci_upper <- quantile(boot_idi_valid, 1 - alpha/2, na.rm = TRUE)
    
    # Calculate p-value (two-sided test for IDI = 0)
    p_value <- 2 * min(
        mean(boot_idi_valid <= 0, na.rm = TRUE),
        mean(boot_idi_valid >= 0, na.rm = TRUE)
    )
    
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
