# ============================================================================
# CLINICOPATH UTILITY FUNCTIONS
# ============================================================================
# This file contains shared utility functions used across the ClinicoPath module
# Functions are organized by category and should be generic and reusable

# ============================================================================
# PACKAGE DEPENDENCIES AND OPERATORS
# ============================================================================

#' Load required packages with error handling
#' @param package_name Character string with package name
#' @param install_if_missing Logical, whether to install if package is missing
#' @return Logical indicating success
load_required_package <- function(package_name, install_if_missing = TRUE) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
        if (install_if_missing) {
            install.packages(package_name)
            return(requireNamespace(package_name, quietly = TRUE))
        }
        return(FALSE)
    }
    return(TRUE)
}

# Load essential packages
load_required_package("rlang")
load_required_package("magrittr")

#' Null-coalescing operator
#' @name null-coalescing
#' @rdname null-coalescing
#' @keywords internal
#' @export
#' @importFrom rlang %||%
#' @usage lhs \%||\% rhs
`%||%` <- rlang::`%||%`

#' NA-coalescing operator
#' @name na-coalescing
#' @rdname na-coalescing
#' @keywords internal
#' @export
#' @usage lhs \%|\% rhs
`%|%` <- function(x, y) {
    if (is.na(x)) y else x
}

#' Not-in operator
#' @name not-in
#' @rdname not-in
#' @keywords internal
#' @export
#' @usage lhs \%notin\% rhs
`%notin%` <- Negate("%in%")

#' Alternative not-in operator
#' @name not-in-alt
#' @rdname not-in-alt
#' @keywords internal
#' @export
#' @usage lhs \%!in\% rhs
`%!in%` <- Negate("%in%")

#' Pipe operator
#' @name %>%
#' @rdname pipe
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
    
    # Get unique sorted values for thresholds
    sorted_values <- sort(unique(values_clean))
    
    # Initialize probabilities vector
    probs <- rep(NA, length(values))
    
    # Calculate sensitivity for each unique value as proxy for probability
    for (i in seq_along(sorted_values)) {
        threshold <- sorted_values[i]
        
        # Get predictions based on direction
        if (direction == ">=") {
            predicted_pos <- values_clean >= threshold
        } else {
            predicted_pos <- values_clean <= threshold
        }
        
        # Calculate sensitivity at this threshold
        tp <- sum(predicted_pos & actual_clean == 1)
        fn <- sum(!predicted_pos & actual_clean == 1)
        
        # Calculate sensitivity (avoid division by zero)
        if ((tp + fn) > 0) {
            sensitivity <- tp / (tp + fn)
        } else {
            sensitivity <- 0
        }
        
        # Assign probability based on sensitivity
        value_indices <- which(values == threshold)
        if (direction == ">=") {
            probs[value_indices] <- sensitivity
        } else {
            probs[value_indices] <- 1 - sensitivity
        }
    }
    
    # Ensure probabilities are in [0,1] range
    probs[probs < 0] <- 0
    probs[probs > 1] <- 1
    
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

#' Print formatted HTML table for sensitivity/specificity results
#' @description Creates HTML table for confusion matrix visualization
#' @param Title Title for the confusion matrix table
#' @param TP Number of true positives
#' @param FP Number of false positives
#' @param TN Number of true negatives
#' @param FN Number of false negatives
#' @return HTML string containing the formatted table
#' @export
print.sensSpecTable <- function(Title, TP, FP, TN, FN) {
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
#' @description Displays information about the package author and website
#' @return Invisible NULL (called for side effects)
#' @export
clinicopath_startup_message <- function() {
    cat("Serdar Balci MD Pathologist", "\n",
        "https://www.serdarbalci.com/", "\n", "\n")
    invisible(NULL)
}

# Display startup message when package loads
clinicopath_startup_message()