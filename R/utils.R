# ============================================================================
# CLINICOPATH UTILITY FUNCTIONS
# ============================================================================
# This file contains shared utility functions used across the ClinicoPath module
# Functions are organized by category and should be generic and reusable

# ============================================================================
# PACKAGE DEPENDENCIES AND OPERATORS
# ============================================================================

#' @importFrom stats binomial qbeta glm predict quantile
#' @importFrom utils sessionInfo tail
NULL

# Suppress R CMD CHECK notes for global variables used in NSE / auto-generated
# class references that are defined in other sub-packages or lazily.
utils::globalVariables(c(
    # Auto-generated R6 class references (defined in sub-packages / lazy loading)
    "alluvial2Class",
    "alluvial3Class",
    "diagnosticperformanceClass",
    "intervalcensorcureClass",
    "jjriverplotClass",
    "thresholdregressionClass",
    "treatmentswitchingClass",
    # NSE / ggplot2 aesthetics used inside functions
    "label",
    "lo_y",
    "type",
    "x"
))

#' Build a survival formula safely via jmvcore::asFormula
#'
#' Wraps `jmvcore::asFormula` with the function allow-list extended to cover
#' common survival modelling helpers (`Surv`, `strata`, `cluster`, `frailty`,
#' `tt`, `pspline`, `ns`, `bs`, `I`, `const`, `finegray`). Use this instead of
#' base `stats::as.formula()` in survival / Cox / Fine-Gray paths so the formula
#' goes through jmvcore's allow-listed parser.
#'
#' @param x A character formula string (e.g. `"survival::Surv(t, d) ~ x"`).
#' @return A parsed formula object.
#' @keywords internal
.asSurvivalFormula <- function(x) {
    jmvcore::asFormula(
        x,
        additional_allowed_functions = c(
            "Surv", "strata", "cluster", "frailty", "tt",
            "pspline", "ns", "bs", "I", "const", "finegray"
        )
    )
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

#' Build a survival model formula from variable names
#'
#' Consolidated helper used across the survival-analysis backends (e.g.
#' `multisurvival.b.R`) to assemble a `survival::Surv(...) ~ ...` formula
#' from raw variable names, with safe escaping of non-syntactic names via
#' `.escapeVariableNames()` and safe parsing via `.asSurvivalFormula()`.
#'
#' @param time_var Character. Time variable name (or start time for
#'   `"standard"`/`"interval"` types).
#' @param outcome_var Character. Event/outcome variable name.
#' @param predictors Character vector of predictor (main-effect) variable
#'   names.
#' @param survival_type One of `"standard"`, `"counting"`, `"interval"`.
#' @param start_var Character. Start-time variable name (required when
#'   `survival_type = "counting"`).
#' @param stop_var Character. Stop-time variable name (required when
#'   `survival_type` is `"counting"` or `"interval"`).
#' @param strata_vars Character vector of stratification variable names.
#' @param interaction_terms Character vector of already-escaped, `:`-joined
#'   interaction terms (e.g. `` "`Arm`:`Bio`" ``) appended to the right-hand
#'   side after main effects and before strata.
#' @return A parsed formula object (see `.asSurvivalFormula()`).
#' @keywords internal
.buildSurvivalFormula <- function(time_var, outcome_var, predictors, survival_type = "standard", start_var = NULL, stop_var = NULL, strata_vars = NULL, interaction_terms = NULL) {
  # Escape all variable names for safe formula construction
  escaped_time <- .escapeVariableNames(time_var)
  escaped_outcome <- .escapeVariableNames(outcome_var)
  escaped_predictors <- .escapeVariableNames(predictors)

  # Build left-hand side based on survival type
  lhs <- switch(survival_type,
    "standard" = paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ")"),
    "counting" = {
      if (is.null(start_var) || is.null(stop_var)) {
        jmvcore::reject("Start and stop variables required for counting process format")
      }
      escaped_start <- .escapeVariableNames(start_var)
      escaped_stop <- .escapeVariableNames(stop_var)
      paste0("survival::Surv(", escaped_start, ", ", escaped_stop, ", ", escaped_outcome, ")")
    },
    "interval" = {
      if (is.null(stop_var)) {
        jmvcore::reject("Stop time variable required for interval censoring")
      }
      escaped_stop <- .escapeVariableNames(stop_var)
      paste0("survival::Surv(", escaped_time, ", ", escaped_stop, ", ", escaped_outcome, ")")
    },
    jmvcore::reject("Unknown survival type: ", survival_type)
  )

  # Build right-hand side: main effects + interaction terms (already escaped)
  main_terms <- if (length(escaped_predictors) == 0) character(0) else escaped_predictors
  int_terms  <- if (length(interaction_terms) == 0) character(0) else interaction_terms
  rhs_terms  <- c(main_terms, int_terms)

  if (length(rhs_terms) == 0) {
    rhs <- "1"  # Null model
  } else {
    rhs <- paste(rhs_terms, collapse = " + ")
  }

  # Add stratification if specified (applies whether or not predictors exist)
  if (!is.null(strata_vars) && length(strata_vars) > 0) {
    escaped_strata <- .escapeVariableNames(strata_vars)
    strata_term <- paste0("strata(", paste(escaped_strata, collapse = ", "), ")")
    rhs <- if (identical(rhs, "1")) strata_term else paste(rhs, strata_term, sep = " + ")
  }

  formula_string <- paste0(lhs, " ~ ", rhs)
  return(.asSurvivalFormula(formula_string))
}

#' Check whether a required package is available
#'
#' Thin wrapper around `requireNamespace()` that returns a boolean. Packages a
#' jamovi module needs must be listed in `DESCRIPTION` `Imports:` so they are
#' installed at install time - this helper does NOT install anything (auto-
#' installing dependencies at runtime is a CRAN policy violation and a security
#' hazard).
#'
#' @param package_name Character string with package name.
#' @param install_if_missing Ignored. Retained for backwards compatibility
#'   with earlier signatures of this function. A warning is issued when set
#'   to anything other than its default.
#' @return Logical indicating whether the package is available.
#' @export
load_required_package <- function(package_name, install_if_missing = FALSE) {
    if (!isFALSE(install_if_missing)) {
        warning(
            "load_required_package(install_if_missing = TRUE) is no longer ",
            "supported; declare the dependency in DESCRIPTION Imports instead."
        )
    }
    requireNamespace(package_name, quietly = TRUE)
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

#' NA-coalescing operator
#' @name na-coalescing
#' @rdname na-coalescing
#' @param x Left-hand side value
#' @param y Right-hand side default value
#' @return `x` if it is not `NA`, otherwise `y`.
#' @keywords internal
#' @export
`%|%` <- function(x, y) {
    if (is.na(x)) y else x
}

#' Not-in operator
#' @name not-in
#' @rdname not-in
#' @param x Values to check
#' @param table Values to check against
#' @return A logical vector, `TRUE` where an element of `x` is not present in `table`.
#' @keywords internal
#' @export
`%notin%` <- function(x, table) !(x %in% table)

#' Alternative not-in operator
#' @name not-in-alt
#' @rdname not-in-alt
#' @param x Values to check
#' @param table Values to check against
#' @return A logical vector, `TRUE` where an element of `x` is not present in `table`.
#' @keywords internal
#' @export
`%!in%` <- function(x, table) !(x %in% table)

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
# DIAGNOSTIC TEST STATISTICS
# ============================================================================

#' Calculate test sensitivity
#' @description Calculates sensitivity (true positive rate) from confusion matrix values
#' @param tp Number of true positives
#' @param fn Number of false negatives
#' @return Numeric sensitivity value or NA when inputs are not valid
#' @export
calculate_sensitivity <- function(tp, fn) {
    if (is.na(tp) || is.na(fn) || (tp + fn) == 0) {
        return(NA_real_)
    }
    tp / (tp + fn)
}

#' Calculate test specificity
#' @description Calculates specificity (true negative rate) from confusion matrix values
#' @param tn Number of true negatives
#' @param fp Number of false positives
#' @return Numeric specificity value or NA when inputs are not valid
#' @export
calculate_specificity <- function(tn, fp) {
    if (is.na(tn) || is.na(fp) || (tn + fp) == 0) {
        return(NA_real_)
    }
    tn / (tn + fp)
}

#' Calculate positive predictive value (PPV)
#' @description Calculates PPV from confusion matrix values
#' @param tp Number of true positives
#' @param fp Number of false positives
#' @return Numeric PPV value or NA when inputs are not valid
#' @export
calculate_ppv <- function(tp, fp) {
    if (is.na(tp) || is.na(fp) || (tp + fp) == 0) {
        return(NA_real_)
    }
    tp / (tp + fp)
}

#' Calculate negative predictive value (NPV)
#' @description Calculates NPV from confusion matrix values
#' @param tn Number of true negatives
#' @param fn Number of false negatives
#' @return Numeric NPV value or NA when inputs are not valid
#' @export
calculate_npv <- function(tn, fn) {
    if (is.na(tn) || is.na(fn) || (tn + fn) == 0) {
        return(NA_real_)
    }
    tn / (tn + fn)
}

#' Calculate positive likelihood ratio
#' @description Calculates positive likelihood ratio from sensitivity and specificity
#' @param sens Sensitivity value
#' @param spec Specificity value
#' @return Numeric positive likelihood ratio or NA when inputs are not valid
#' @export
calculate_plr <- function(sens, spec) {
    if (is.na(sens) || is.na(spec) || (1 - spec) == 0) {
        return(NA_real_)
    }
    sens / (1 - spec)
}

#' Calculate negative likelihood ratio
#' @description Calculates negative likelihood ratio from sensitivity and specificity
#' @param sens Sensitivity value
#' @param spec Specificity value
#' @return Numeric negative likelihood ratio or NA when inputs are not valid
#' @export
calculate_nlr <- function(sens, spec) {
    if (is.na(sens) || is.na(spec) || spec == 0) {
        return(NA_real_)
    }
    (1 - sens) / spec
}

#' Approximate AUC from sensitivity and specificity
#' @description Uses a simplified formula to approximate AUC from sensitivity and specificity
#' @param sens Sensitivity of the test
#' @param spec Specificity of the test
#' @return Numeric AUC value or NA when inputs are not valid
#' @export
calculate_auc <- function(sens, spec) {
    if (is.na(sens) || is.na(spec)) {
        return(NA_real_)
    }
    0.5 * (sens * (1 - spec)) +
        0.5 * (1 * (1 - (1 - spec))) +
        0.5 * ((1 - sens) * spec)
}

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

#' Validate inputs for ROC analysis
#' @description Comprehensive validation of ROC analysis inputs
#' @param x Test values
#' @param class_var Classification labels
#' @param pos_class Positive class label
#' @return List with validation results and cleaned data
#' @export
validateROCInputs <- function(x, class_var, pos_class = NULL) {
    errors <- character(0)
    warnings <- character(0)
    
    # Check for missing inputs
    if (missing(x) || is.null(x)) {
        errors <- c(errors, "Test values (x) are required")
    }
    if (missing(class_var) || is.null(class_var)) {
        errors <- c(errors, "Class labels are required")
    }
    
    # Return early if critical inputs missing
    if (length(errors) > 0) {
        return(list(
            valid = FALSE,
            errors = errors,
            warnings = warnings
        ))
    }
    
    # Check input lengths
    if (length(x) != length(class_var)) {
        errors <- c(errors, "Test values and class labels must have the same length")
    }
    
    # Check for sufficient data
    if (length(x) < 10) {
        warnings <- c(warnings, "Small sample size (n < 10) may produce unreliable results")
    }
    
    # Check for missing values
    missing_x <- sum(is.na(x))
    missing_class <- sum(is.na(class_var))
    
    if (missing_x > 0) {
        warnings <- c(warnings, paste(missing_x, "missing values in test scores will be removed"))
    }
    
    if (missing_class > 0) {
        warnings <- c(warnings, paste(missing_class, "missing values in class labels will be removed"))
    }
    
    # Remove missing values
    complete_cases <- !is.na(x) & !is.na(class_var)
    x_clean <- x[complete_cases]
    class_clean <- class_var[complete_cases]
    
    # Check class variable
    unique_classes <- unique(class_clean)
    if (length(unique_classes) != 2) {
        errors <- c(errors, paste("Class variable must have exactly 2 levels, found:",
                                  length(unique_classes)))
    }
    
    # Validate positive class
    if (is.null(pos_class)) {
        pos_class <- as.character(unique_classes[1])
        warnings <- c(warnings, paste("Using", pos_class, "as positive class"))
    } else if (!pos_class %in% unique_classes) {
        errors <- c(errors, paste("Specified positive class", pos_class, "not found in data"))
    }
    
    # Check for constant test values
    if (length(unique(x_clean)) == 1) {
        errors <- c(errors, "Test values are constant - cannot calculate ROC curve")
    }
    
    # Check class balance
    if (length(errors) == 0 && length(unique_classes) == 2) {
        pos_count <- sum(class_clean == pos_class)
        neg_count <- sum(class_clean != pos_class)
        
        if (pos_count == 0) {
            errors <- c(errors, "No positive cases found")
        }
        if (neg_count == 0) {
            errors <- c(errors, "No negative cases found")
        }
        
        # Warn about extreme imbalance
        total_n <- length(class_clean)
        if (total_n > 0 && min(pos_count, neg_count) / total_n < 0.05) {
            warnings <- c(warnings, "Severe class imbalance detected - results may be unstable")
        }
    }
    
    return(list(
        valid = length(errors) == 0,
        errors = errors,
        warnings = warnings,
        x_clean = x_clean,
        class_clean = class_clean,
        pos_class = pos_class,
        n_pos = if(exists("pos_count")) pos_count else NA,
        n_neg = if(exists("neg_count")) neg_count else NA,
        n_total = length(x_clean)
    ))
}

# ============================================================================
# HTML TABLE UTILITIES
# ============================================================================

#' Format HTML table for sensitivity/specificity results
#' @description Creates HTML table for confusion matrix visualization
#' @param x A list with elements Title, TP, FP, TN, FN
#' @param ... Additional arguments (ignored)
#' @return HTML string containing the formatted table
#' @export
print.sensSpecTable <- function(x, ...) {
    Title <- x$Title
    TP <- x$TP
    FP <- x$FP
    TN <- x$TN
    FN <- x$FN
    # Validate inputs
    if (any(!is.finite(c(TP, FP, TN, FN)))) {
        return("<p>Error: Invalid confusion matrix values</p>")
    }
    
    # Create HTML table with the confusion matrix results
    html <- paste0(
        "<style type='text/css'>
          .tg  {border-collapse:collapse;border-spacing:0;border-width:1px;border-style:solid;border-color:black;}
          .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
          .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
          .tg .tg-s6z2{text-align:center}
          .tg .tg-uys7{border-color:inherit;text-align:center}
          .tg .tg-h0x1{text-align:center}
        </style>
        <table class='tg'>
          <tr>
            <th class='tg-0lax' colspan='4'>", Title, "</th>
          </tr>
          <tr>
            <td class='tg-s6z2'></td>
            <td class='tg-uys7' colspan='3'>DECISION BASED ON MEASURE</td>
          </tr>
          <tr>
            <td class='tg-h0x1' rowspan='3'>CRITERION</td>
            <td class='tg-h0x1'></td>
            <td class='tg-h0x1'>Negative</td>
            <td class='tg-h0x1'>Positive</td>
          </tr>
          <tr>
            <td class='tg-s6z2'>Negative</td>
            <td class='tg-s6z2'>", TN, " (TN)</td>
            <td class='tg-s6z2'>", FP, " (FP)</td>
          </tr>
          <tr>
            <td class='tg-h0x1'>Positive</td>
            <td class='tg-h0x1'>", FN, " (FN)</td>
            <td class='tg-h0x1'>", TP, " (TP)</td>
          </tr>
          <tr>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
          </tr>
        </table>"
    )
    return(html)
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

#' Safe division function
#' @description Performs division with safe handling of division by zero
#' @param x Numerator
#' @param y Denominator
#' @param na_value Value to return when division by zero occurs
#' @return Result of x/y or na_value if y is zero
#' @export
safe_divide <- function(x, y, na_value = NA_real_) {
    ifelse(y == 0 | is.na(y), na_value, x / y)
}

#' Check if value is within valid range
#' @description Validates that a value is within specified bounds
#' @param x Value to check
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @param inclusive Whether bounds are inclusive
#' @return Logical indicating if value is within range
#' @export
is_in_range <- function(x, min_val, max_val, inclusive = TRUE) {
    if (is.na(x)) return(FALSE)
    if (inclusive) {
        return(x >= min_val & x <= max_val)
    } else {
        return(x > min_val & x < max_val)
    }
}

#' Convert proportion to percentage string
#' @description Converts numeric proportion to formatted percentage
#' @param x Numeric proportion (0-1)
#' @param digits Number of decimal places
#' @return Character string with percentage formatting
#' @export
prop_to_percent <- function(x, digits = 1) {
    if (is.na(x)) return("NA%")
    paste0(round(x * 100, digits), "%")
}

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
