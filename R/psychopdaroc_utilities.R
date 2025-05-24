# ============================================================================
# UTILITY FUNCTIONS FOR PSYCHOPDAROC
# ============================================================================
# This file contains shared utility functions used by the psychopdaroc module
# Functions here should be generic and reusable, not class-specific

#' Print formatted HTML table for sensitivity/specificity results
#'
#' @param Title Title for the confusion matrix table
#' @param TP Number of true positives
#' @param FP Number of false positives
#' @param TN Number of true negatives
#' @param FN Number of false negatives
#' @return HTML string containing the formatted table
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

#' Convert raw test values to predicted probabilities using ROC curve
#'
#' This function maps raw test values to probabilities based on their
#' position in the ROC curve. Used for IDI/NRI calculations.
#'
#' @param values Raw test values
#' @param actual Binary outcomes (0/1)
#' @param direction Direction of test (">=" or "<=")
#' @return Vector of predicted probabilities
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
        # For values equal to this threshold
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

#' Bootstrap IDI calculation with confidence intervals
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction (">=" or "<=")
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @return List with IDI, confidence intervals, and p-value
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

#' Bootstrap NRI calculation with confidence intervals
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction (">=" or "<=")
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @return List with NRI components and confidence intervals
bootstrapNRI <- function(new_values, ref_values, actual,
                         direction = ">=", thresholds = NULL,
                         n_boot = 1000, conf_level = 0.95) {
    # Validate inputs
    n <- length(actual)
    if (length(new_values) != n || length(ref_values) != n) {
        stop("All input vectors must have the same length")
    }

    if (!all(actual %in% c(0, 1))) {
        stop("Actual values must be binary (0 or 1)")
    }

    # Validate thresholds if provided
    if (!is.null(thresholds)) {
        thresholds <- sort(thresholds)
        if (any(thresholds <= 0) || any(thresholds >= 1)) {
            stop("Thresholds must be between 0 and 1")
        }
    }

    # Original NRI calculation
    original_nri <- computeNRI(new_values, ref_values, actual, direction, thresholds)

    # Bootstrap
    boot_nri <- numeric(n_boot)
    boot_event_nri <- numeric(n_boot)
    boot_non_event_nri <- numeric(n_boot)
    valid_boots <- 0

    for (i in 1:n_boot) {
        boot_idx <- sample(n, n, replace = TRUE)

        boot_new <- new_values[boot_idx]
        boot_ref <- ref_values[boot_idx]
        boot_actual <- actual[boot_idx]

        # Skip if no events or non-events
        if (sum(boot_actual == 1) == 0 || sum(boot_actual == 0) == 0) {
            boot_nri[i] <- NA
            boot_event_nri[i] <- NA
            boot_non_event_nri[i] <- NA
            next
        }

        tryCatch({
            boot_result <- computeNRI(boot_new, boot_ref, boot_actual, direction, thresholds)
            boot_nri[i] <- boot_result$nri
            boot_event_nri[i] <- boot_result$event_nri
            boot_non_event_nri[i] <- boot_result$non_event_nri
            valid_boots <- valid_boots + 1
        }, error = function(e) {
            boot_nri[i] <- NA
            boot_event_nri[i] <- NA
            boot_non_event_nri[i] <- NA
        })
    }

    # Remove failed iterations
    valid_idx <- !is.na(boot_nri)
    boot_nri <- boot_nri[valid_idx]
    boot_event_nri <- boot_event_nri[valid_idx]
    boot_non_event_nri <- boot_non_event_nri[valid_idx]

    if (length(boot_nri) < n_boot * 0.5) {
        warning("Many bootstrap samples failed - results may be unreliable")
    }

    # Calculate confidence intervals
    alpha <- 1 - conf_level
    ci_lower <- quantile(boot_nri, alpha/2, na.rm = TRUE)
    ci_upper <- quantile(boot_nri, 1 - alpha/2, na.rm = TRUE)

    # Calculate p-value
    p_value <- 2 * min(
        mean(boot_nri <= 0, na.rm = TRUE),
        mean(boot_nri >= 0, na.rm = TRUE)
    )

    return(list(
        nri = original_nri$nri,
        event_nri = original_nri$event_nri,
        non_event_nri = original_nri$non_event_nri,
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper),
        p_value = p_value,
        n_valid_boots = valid_boots
    ))
}

#' Compute Net Reclassification Index (NRI)
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @return List containing NRI components
computeNRI <- function(new_values, ref_values, actual,
                       direction = ">=", thresholds = NULL) {
    # Convert to probabilities
    new_probs <- raw_to_prob(new_values, actual, direction)
    ref_probs <- raw_to_prob(ref_values, actual, direction)

    # Identify events and non-events
    events <- actual == 1
    non_events <- actual == 0

    # Check for sufficient data
    if (sum(events) == 0 || sum(non_events) == 0) {
        return(list(nri = NA, event_nri = NA, non_event_nri = NA))
    }

    if (is.null(thresholds) || length(thresholds) == 0) {
        # Continuous NRI
        up_events <- sum(new_probs[events] > ref_probs[events], na.rm = TRUE)
        down_events <- sum(new_probs[events] < ref_probs[events], na.rm = TRUE)
        up_non_events <- sum(new_probs[non_events] > ref_probs[non_events], na.rm = TRUE)
        down_non_events <- sum(new_probs[non_events] < ref_probs[non_events], na.rm = TRUE)

        # Calculate proportions
        p_up_events <- up_events / sum(events)
        p_down_events <- down_events / sum(events)
        p_up_non_events <- up_non_events / sum(non_events)
        p_down_non_events <- down_non_events / sum(non_events)

    } else {
        # Categorical NRI
        # Create risk categories with proper handling of boundaries
        breaks <- c(0, thresholds, 1)
        labels <- 1:length(breaks - 1)

        ref_cats <- cut(ref_probs,
                        breaks = breaks,
                        labels = labels,
                        include.lowest = TRUE)

        new_cats <- cut(new_probs,
                        breaks = breaks,
                        labels = labels,
                        include.lowest = TRUE)

        # Calculate movement between categories
        move_up_event <- sum(as.numeric(new_cats[events]) > as.numeric(ref_cats[events]), na.rm = TRUE)
        move_down_event <- sum(as.numeric(new_cats[events]) < as.numeric(ref_cats[events]), na.rm = TRUE)
        move_up_non_event <- sum(as.numeric(new_cats[non_events]) > as.numeric(ref_cats[non_events]), na.rm = TRUE)
        move_down_non_event <- sum(as.numeric(new_cats[non_events]) < as.numeric(ref_cats[non_events]), na.rm = TRUE)

        # Calculate proportions
        p_up_events <- move_up_event / sum(events)
        p_down_events <- move_down_event / sum(events)
        p_up_non_events <- move_up_non_event / sum(non_events)
        p_down_non_events <- move_down_non_event / sum(non_events)
    }

    # Calculate NRI components
    event_nri <- p_up_events - p_down_events
    non_event_nri <- p_down_non_events - p_up_non_events
    overall_nri <- event_nri + non_event_nri

    return(list(
        nri = overall_nri,
        event_nri = event_nri,
        non_event_nri = non_event_nri,
        p_up_events = p_up_events,
        p_down_events = p_down_events,
        p_up_non_events = p_up_non_events,
        p_down_non_events = p_down_non_events
    ))
}

#' Validate inputs for ROC analysis
#'
#' @param x Test values
#' @param class_var Classification labels
#' @param pos_class Positive class label
#' @return List with validation results and cleaned data
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
