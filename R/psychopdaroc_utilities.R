# ============================================================================
# ADDITIONAL UTILITY FUNCTIONS FOR PSYCHOPDAROC
# ============================================================================
# These functions provide complete implementations for advanced features
# that were placeholders in the main class

#' Calculate precision-recall curve data
#'
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @return List containing precision, recall, thresholds, and AUPRC
calculatePrecisionRecall <- function(x, class, pos_class) {

    # Convert to binary response (1 = positive, 0 = negative)
    response <- as.numeric(class == pos_class)

    # Sort values and create thresholds
    sorted_values <- sort(unique(x), decreasing = TRUE)
    precision <- numeric(length(sorted_values))
    recall <- numeric(length(sorted_values))

    # Calculate precision and recall for each threshold
    for (i in seq_along(sorted_values)) {
        threshold <- sorted_values[i]
        predicted_pos <- x >= threshold

        # Compute TP, FP, FN
        tp <- sum(predicted_pos & response == 1)
        fp <- sum(predicted_pos & response == 0)
        fn <- sum(!predicted_pos & response == 1)

        # Calculate precision and recall
        precision[i] <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
        recall[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    }

    # Add points for complete curve (0,1) and (1,0)
    precision <- c(1, precision, 0)
    recall <- c(0, recall, 1)
    sorted_values <- c(max(x, na.rm = TRUE) + 1, sorted_values, min(x, na.rm = TRUE) - 1)

    # Calculate AUPRC using trapezoidal rule
    auprc <- 0
    for (i in 2:length(recall)) {
        # Only add positive areas
        if (recall[i] > recall[i-1]) {
            auprc <- auprc + 0.5 * (precision[i] + precision[i-1]) * (recall[i] - recall[i-1])
        }
    }

    return(list(
        threshold = sorted_values,
        precision = precision,
        recall = recall,
        auprc = auprc
    ))
}

#' Calculate comprehensive classifier performance metrics
#'
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @return List containing comprehensive performance metrics
calculateComprehensiveMetrics <- function(x, class, pos_class) {

    # Validate inputs
    if (length(x) != length(class)) {
        stop("Test values and class labels must have same length")
    }

    # Remove missing values
    complete_cases <- !is.na(x) & !is.na(class)
    x <- x[complete_cases]
    class <- class[complete_cases]

    if (length(x) < 10) {
        warning("Very small sample size may produce unreliable results")
    }

    # Convert to binary response
    response <- as.numeric(class == pos_class)

    # Calculate basic ROC metrics first
    thresholds <- sort(unique(x))
    best_youden <- -Inf
    best_threshold <- NA
    best_sens <- NA
    best_spec <- NA
    auc <- 0

    # Store all ROC points for AUC calculation
    roc_points <- data.frame(
        fpr = numeric(length(thresholds)),
        tpr = numeric(length(thresholds))
    )

    for (i in seq_along(thresholds)) {
        threshold <- thresholds[i]
        predicted_pos <- x >= threshold

        tp <- sum(predicted_pos & response == 1)
        fp <- sum(predicted_pos & response == 0)
        tn <- sum(!predicted_pos & response == 0)
        fn <- sum(!predicted_pos & response == 1)

        # Calculate rates
        sensitivity <- tp / (tp + fn)
        specificity <- tn / (tn + fp)
        youden <- sensitivity + specificity - 1

        # Store for AUC calculation
        roc_points$tpr[i] <- sensitivity
        roc_points$fpr[i] <- 1 - specificity

        # Update best threshold based on Youden's index
        if (youden > best_youden) {
            best_youden <- youden
            best_threshold <- threshold
            best_sens <- sensitivity
            best_spec <- specificity
        }
    }

    # Calculate AUC using trapezoidal rule
    roc_points <- roc_points[order(roc_points$fpr), ]
    for (i in 2:nrow(roc_points)) {
        width <- roc_points$fpr[i] - roc_points$fpr[i-1]
        height <- (roc_points$tpr[i] + roc_points$tpr[i-1]) / 2
        auc <- auc + width * height
    }

    # Get predictions at optimal threshold
    predicted_pos <- x >= best_threshold

    # Calculate confusion matrix at optimal threshold
    tp <- sum(predicted_pos & response == 1)
    fp <- sum(predicted_pos & response == 0)
    tn <- sum(!predicted_pos & response == 0)
    fn <- sum(!predicted_pos & response == 1)

    # Calculate comprehensive metrics
    accuracy <- (tp + tn) / (tp + fp + tn + fn)
    balanced_accuracy <- (best_sens + best_spec) / 2

    # Calculate precision and F1 score
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall <- best_sens  # Same as sensitivity
    f1_score <- ifelse(precision + recall > 0,
                       2 * precision * recall / (precision + recall), 0)

    # Calculate Brier score (requires probability predictions)
    # Use a simple approach: convert test values to probabilities
    prob_predictions <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    prob_predictions[prob_predictions < 0] <- 0
    prob_predictions[prob_predictions > 1] <- 1
    brier_score <- mean((prob_predictions - response)^2, na.rm = TRUE)

    # Calculate AUPRC
    pr_results <- calculatePrecisionRecall(x, class, pos_class)
    auprc <- pr_results$auprc

    return(list(
        auc = auc,
        auprc = auprc,
        brier = brier_score,
        f1_score = f1_score,
        accuracy = accuracy,
        balanced_accuracy = balanced_accuracy,
        sensitivity = best_sens,
        specificity = best_spec,
        precision = precision,
        optimal_threshold = best_threshold
    ))
}

#' Calculate partial AUC using numerical integration
#'
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @param spec_from Lower bound of specificity range
#' @param spec_to Upper bound of specificity range
#' @return List containing partial AUC results
calculatePartialAUC <- function(x, class, pos_class, spec_from = 0.8, spec_to = 1.0) {

    # Validate inputs
    if (spec_from >= spec_to) {
        stop("spec_from must be less than spec_to")
    }
    if (spec_from < 0 || spec_to > 1) {
        stop("Specificity bounds must be between 0 and 1")
    }

    # Convert to binary response
    response <- as.numeric(class == pos_class)

    # Calculate ROC curve points
    thresholds <- sort(unique(x))
    roc_data <- data.frame(
        threshold = thresholds,
        sensitivity = numeric(length(thresholds)),
        specificity = numeric(length(thresholds))
    )

    for (i in seq_along(thresholds)) {
        threshold <- thresholds[i]
        predicted_pos <- x >= threshold

        tp <- sum(predicted_pos & response == 1)
        fp <- sum(predicted_pos & response == 0)
        tn <- sum(!predicted_pos & response == 0)
        fn <- sum(!predicted_pos & response == 1)

        roc_data$sensitivity[i] <- tp / (tp + fn)
        roc_data$specificity[i] <- tn / (tn + fp)
    }

    # Filter for specificity range
    in_range <- roc_data$specificity >= spec_from & roc_data$specificity <= spec_to

    if (sum(in_range) < 2) {
        warning("Insufficient data points in specified specificity range")
        return(list(
            pAUC = NA,
            pAUC_normalized = NA,
            ci_lower = NA,
            ci_upper = NA,
            spec_range = paste0("(", spec_from, " - ", spec_to, ")")
        ))
    }

    # Extract points in range and sort by specificity
    partial_data <- roc_data[in_range, ]
    partial_data <- partial_data[order(partial_data$specificity), ]

    # Calculate partial AUC using trapezoidal rule
    partial_auc <- 0
    for (i in 2:nrow(partial_data)) {
        # Width in FPR space (1 - specificity)
        width <- (1 - partial_data$specificity[i-1]) - (1 - partial_data$specificity[i])
        # Average height (sensitivity)
        height <- (partial_data$sensitivity[i] + partial_data$sensitivity[i-1]) / 2
        partial_auc <- partial_auc + width * height
    }

    # Normalize partial AUC to [0,1] range
    max_possible_pauc <- (1 - spec_from) - (1 - spec_to)  # Width of FPR range
    normalized_pauc <- partial_auc / max_possible_pauc

    # Simple confidence interval (would need bootstrap for better estimates)
    n <- length(x)
    se_approx <- sqrt(partial_auc * (1 - partial_auc) / n)
    ci_lower <- max(0, partial_auc - 1.96 * se_approx)
    ci_upper <- min(max_possible_pauc, partial_auc + 1.96 * se_approx)

    return(list(
        pAUC = partial_auc,
        pAUC_normalized = normalized_pauc,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        spec_range = paste0("(", spec_from, " - ", spec_to, ")")
    ))
}

#' Calculate bootstrap confidence intervals for ROC metrics
#'
#' @param x Numeric vector of test values
#' @param class Binary class vector
#' @param pos_class Positive class label
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @return List containing bootstrap confidence intervals
calculateBootstrapCI <- function(x, class, pos_class, n_boot = 2000, conf_level = 0.95) {

    # Validate inputs
    if (n_boot < 100) {
        warning("Very few bootstrap iterations may produce unstable results")
    }

    # Remove missing values
    complete_cases <- !is.na(x) & !is.na(class)
    x <- x[complete_cases]
    class <- class[complete_cases]
    n <- length(x)

    if (n < 20) {
        warning("Small sample size may produce unreliable bootstrap results")
    }

    # Initialize storage for bootstrap results
    boot_auc <- numeric(n_boot)
    boot_threshold <- numeric(n_boot)
    boot_sensitivity <- numeric(n_boot)
    boot_specificity <- numeric(n_boot)

    # Original metrics for comparison
    original_metrics <- calculateComprehensiveMetrics(x, class, pos_class)

    # Bootstrap sampling
    for (i in 1:n_boot) {
        # Sample with replacement
        boot_indices <- sample(n, n, replace = TRUE)
        x_boot <- x[boot_indices]
        class_boot <- class[boot_indices]

        # Calculate metrics for bootstrap sample
        tryCatch({
            boot_metrics <- calculateComprehensiveMetrics(x_boot, class_boot, pos_class)
            boot_auc[i] <- boot_metrics$auc
            boot_threshold[i] <- boot_metrics$optimal_threshold
            boot_sensitivity[i] <- boot_metrics$sensitivity
            boot_specificity[i] <- boot_metrics$specificity
        }, error = function(e) {
            # Handle bootstrap samples that fail
            boot_auc[i] <- NA
            boot_threshold[i] <- NA
            boot_sensitivity[i] <- NA
            boot_specificity[i] <- NA
        })
    }

    # Remove failed bootstrap samples
    valid_boots <- !is.na(boot_auc)
    boot_auc <- boot_auc[valid_boots]
    boot_threshold <- boot_threshold[valid_boots]
    boot_sensitivity <- boot_sensitivity[valid_boots]
    boot_specificity <- boot_specificity[valid_boots]

    if (length(boot_auc) < n_boot * 0.5) {
        warning("Many bootstrap samples failed - results may be unreliable")
    }

    # Calculate confidence intervals using percentile method
    alpha <- 1 - conf_level
    lower_pct <- alpha / 2
    upper_pct <- 1 - alpha / 2

    # Helper function to calculate CI
    calc_ci <- function(boot_values, original_value) {
        if (length(boot_values) < 10) {
            return(list(estimate = original_value, ci_lower = NA, ci_upper = NA))
        }

        ci_lower <- quantile(boot_values, lower_pct, na.rm = TRUE)
        ci_upper <- quantile(boot_values, upper_pct, na.rm = TRUE)

        list(
            estimate = original_value,
            ci_lower = as.numeric(ci_lower),
            ci_upper = as.numeric(ci_upper)
        )
    }

    return(list(
        auc = calc_ci(boot_auc, original_metrics$auc),
        threshold = calc_ci(boot_threshold, original_metrics$optimal_threshold),
        sensitivity = calc_ci(boot_sensitivity, original_metrics$sensitivity),
        specificity = calc_ci(boot_specificity, original_metrics$specificity)
    ))
}

#' Bootstrap IDI calculation with confidence intervals
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector
#' @param direction Classification direction
#' @param n_boot Number of bootstrap iterations
#' @return List with IDI and confidence intervals
bootstrapIDI <- function(new_values, ref_values, actual, direction = ">=", n_boot = 1000) {

    n <- length(actual)
    boot_idi <- numeric(n_boot)

    # Original IDI calculation
    new_probs <- rawToProb(new_values, actual, direction)
    ref_probs <- rawToProb(ref_values, actual, direction)

    original_idi <- (mean(new_probs[actual == 1]) - mean(new_probs[actual == 0])) -
        (mean(ref_probs[actual == 1]) - mean(ref_probs[actual == 0]))

    # Bootstrap
    for (i in 1:n_boot) {
        boot_idx <- sample(n, n, replace = TRUE)

        boot_new <- new_values[boot_idx]
        boot_ref <- ref_values[boot_idx]
        boot_actual <- actual[boot_idx]

        # Calculate probabilities for bootstrap sample
        boot_new_probs <- rawToProb(boot_new, boot_actual, direction)
        boot_ref_probs <- rawToProb(boot_ref, boot_actual, direction)

        # Calculate IDI
        boot_idi[i] <- (mean(boot_new_probs[boot_actual == 1]) - mean(boot_new_probs[boot_actual == 0])) -
            (mean(boot_ref_probs[boot_actual == 1]) - mean(boot_ref_probs[boot_actual == 0]))
    }

    # Calculate confidence intervals
    ci_lower <- quantile(boot_idi, 0.025, na.rm = TRUE)
    ci_upper <- quantile(boot_idi, 0.975, na.rm = TRUE)

    # Calculate p-value (two-sided test for IDI = 0)
    p_value <- 2 * min(mean(boot_idi <= 0, na.rm = TRUE),
                       mean(boot_idi >= 0, na.rm = TRUE))

    return(list(
        idi = original_idi,
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper),
        p_value = p_value
    ))
}

#' Bootstrap NRI calculation with confidence intervals
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector
#' @param direction Classification direction
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @param n_boot Number of bootstrap iterations
#' @return List with NRI components and confidence intervals
bootstrapNRI <- function(new_values, ref_values, actual, direction = ">=",
                         thresholds = NULL, n_boot = 1000) {

    n <- length(actual)
    boot_nri <- numeric(n_boot)
    boot_event_nri <- numeric(n_boot)
    boot_non_event_nri <- numeric(n_boot)

    # Original NRI calculation
    original_nri <- computeNRI(new_values, ref_values, actual, direction, thresholds)

    # Bootstrap
    for (i in 1:n_boot) {
        boot_idx <- sample(n, n, replace = TRUE)

        boot_new <- new_values[boot_idx]
        boot_ref <- ref_values[boot_idx]
        boot_actual <- actual[boot_idx]

        tryCatch({
            boot_result <- computeNRI(boot_new, boot_ref, boot_actual, direction, thresholds)
            boot_nri[i] <- boot_result$nri
            boot_event_nri[i] <- boot_result$event_nri
            boot_non_event_nri[i] <- boot_result$non_event_nri
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

    # Calculate confidence intervals
    ci_lower <- quantile(boot_nri, 0.025, na.rm = TRUE)
    ci_upper <- quantile(boot_nri, 0.975, na.rm = TRUE)

    # Calculate p-value
    p_value <- 2 * min(mean(boot_nri <= 0, na.rm = TRUE),
                       mean(boot_nri >= 0, na.rm = TRUE))

    return(list(
        nri = original_nri$nri,
        event_nri = original_nri$event_nri,
        non_event_nri = original_nri$non_event_nri,
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper),
        p_value = p_value
    ))
}

#' Convert raw test values to predicted probabilities using ROC curve
#'
#' @param values Raw test values
#' @param actual Binary outcomes (0/1)
#' @param direction Direction of test (">=", "<=")
#' @return Vector of predicted probabilities
rawToProb <- function(values, actual, direction = ">=") {

    # Get unique sorted values for thresholds
    sorted_values <- sort(unique(values))

    # Initialize probabilities vector
    probs <- numeric(length(values))

    # Calculate sensitivity for each unique value as proxy for probability
    for (i in seq_along(sorted_values)) {
        threshold <- sorted_values[i]

        if (direction == ">=") {
            predicted_pos <- values >= threshold
        } else {
            predicted_pos <- values <= threshold
        }

        # Calculate sensitivity at this threshold
        tp <- sum(predicted_pos & actual == 1)
        fn <- sum(!predicted_pos & actual == 1)

        sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), 0)

        # Assign probability based on sensitivity
        value_indices <- which(values == threshold)
        probs[value_indices] <- ifelse(direction == ">=", sensitivity, 1 - sensitivity)
    }

    # Ensure probabilities are in [0,1] range
    probs[probs < 0] <- 0
    probs[probs > 1] <- 1

    return(probs)
}

#' Compute Net Reclassification Index (NRI)
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector
#' @param direction Classification direction
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @return List containing NRI components
computeNRI <- function(new_values, ref_values, actual, direction = ">=", thresholds = NULL) {

    # Convert to probabilities
    new_probs <- rawToProb(new_values, actual, direction)
    ref_probs <- rawToProb(ref_values, actual, direction)

    # Identify events and non-events
    events <- actual == 1
    non_events <- actual == 0

    if (is.null(thresholds) || length(thresholds) == 0) {
        # Continuous NRI
        up_events <- sum(new_probs[events] > ref_probs[events])
        down_events <- sum(new_probs[events] < ref_probs[events])
        up_non_events <- sum(new_probs[non_events] > ref_probs[non_events])
        down_non_events <- sum(new_probs[non_events] < ref_probs[non_events])

        # Calculate proportions
        p_up_events <- up_events / sum(events)
        p_down_events <- down_events / sum(events)
        p_up_non_events <- up_non_events / sum(non_events)
        p_down_non_events <- down_non_events / sum(non_events)

    } else {
        # Categorical NRI
        # Create risk categories
        ref_cats <- cut(ref_probs,
                        breaks = c(0, thresholds, 1),
                        labels = 1:(length(thresholds) + 1),
                        include.lowest = TRUE)

        new_cats <- cut(new_probs,
                        breaks = c(0, thresholds, 1),
                        labels = 1:(length(thresholds) + 1),
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
        non_event_nri = non_event_nri
    ))
}

# ============================================================================
# ENHANCED PLOTTING FUNCTIONS
# ============================================================================

#' Create enhanced ROC plot with confidence bands
#'
#' @param roc_data Data frame with ROC curve points
#' @param show_ci Whether to show confidence intervals
#' @param ci_level Confidence level for intervals
#' @return ggplot object
createEnhancedROCPlot <- function(roc_data, show_ci = FALSE, ci_level = 0.95) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package is required for plotting")
    }

    # Base plot
    p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
        ggplot2::geom_line(size = 1.2, color = "steelblue") +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::labs(
            x = "1 - Specificity (False Positive Rate)",
            y = "Sensitivity (True Positive Rate)",
            title = "ROC Curve"
        ) +
        ggplot2::theme_minimal()

    # Add confidence intervals if requested
    if (show_ci && all(c("sens_lower", "sens_upper") %in% names(roc_data))) {
        p <- p + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = sens_lower, ymax = sens_upper),
            alpha = 0.2, fill = "steelblue"
        )
    }

    return(p)
}

#' Create precision-recall plot
#'
#' @param pr_data Data frame with precision-recall data
#' @param show_baseline Whether to show baseline (prevalence) line
#' @return ggplot object
createPrecisionRecallPlot <- function(pr_data, show_baseline = TRUE) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package is required for plotting")
    }

    p <- ggplot2::ggplot(pr_data, ggplot2::aes(x = recall, y = precision)) +
        ggplot2::geom_line(size = 1.2, color = "darkgreen") +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::labs(
            x = "Recall (Sensitivity)",
            y = "Precision (Positive Predictive Value)",
            title = "Precision-Recall Curve"
        ) +
        ggplot2::theme_minimal()

    # Add baseline (prevalence) line if requested
    if (show_baseline && "prevalence" %in% names(pr_data)) {
        prevalence <- unique(pr_data$prevalence)[1]
        p <- p + ggplot2::geom_hline(
            yintercept = prevalence,
            linetype = "dashed",
            alpha = 0.7,
            color = "red"
        ) +
            ggplot2::annotate(
                "text", x = 0.8, y = prevalence + 0.05,
                label = paste("Baseline =", round(prevalence, 3)),
                color = "red"
            )
    }

    return(p)
}

# ============================================================================
# VALIDATION FUNCTIONS
# ============================================================================

#' Validate inputs for ROC analysis
#'
#' @param x Test values
#' @param class Classification labels
#' @param pos_class Positive class label
#' @return List with validation results and cleaned data
validateROCInputs <- function(x, class, pos_class = NULL) {

    errors <- character(0)
    warnings <- character(0)

    # Check for missing inputs
    if (missing(x)) errors <- c(errors, "Test values (x) are required")
    if (missing(class)) errors <- c(errors, "Class labels are required")

    # Check input lengths
    if (length(x) != length(class)) {
        errors <- c(errors, "Test values and class labels must have the same length")
    }

    # Check for sufficient data
    if (length(x) < 10) {
        warnings <- c(warnings, "Small sample size (n < 10) may produce unreliable results")
    }

    # Check for missing values
    missing_x <- sum(is.na(x))
    missing_class <- sum(is.na(class))

    if (missing_x > 0) {
        warnings <- c(warnings, paste(missing_x, "missing values in test scores will be removed"))
    }

    if (missing_class > 0) {
        warnings <- c(warnings, paste(missing_class, "missing values in class labels will be removed"))
    }

    # Check class variable
    unique_classes <- unique(class[!is.na(class)])
    if (length(unique_classes) != 2) {
        errors <- c(errors, "Class variable must have exactly 2 levels")
    }

    # Validate positive class
    if (is.null(pos_class)) {
        pos_class <- unique_classes[1]
        warnings <- c(warnings, paste("Using", pos_class, "as positive class"))
    } else if (!pos_class %in% unique_classes) {
        errors <- c(errors, paste("Specified positive class", pos_class, "not found in data"))
    }

    # Remove missing values
    complete_cases <- !is.na(x) & !is.na(class)
    x_clean <- x[complete_cases]
    class_clean <- class[complete_cases]

    # Check for constant test values
    if (length(unique(x_clean)) == 1) {
        errors <- c(errors, "Test values are constant - cannot calculate ROC curve")
    }

    # Check class balance
    pos_count <- sum(class_clean == pos_class)
    neg_count <- sum(class_clean != pos_class)

    if (pos_count == 0) {
        errors <- c(errors, "No positive cases found")
    }
    if (neg_count == 0) {
        errors <- c(errors, "No negative cases found")
    }

    # Warn about extreme imbalance
    total_n <- pos_count + neg_count
    if (min(pos_count, neg_count) / total_n < 0.05) {
        warnings <- c(warnings, "Severe class imbalance detected - results may be unstable")
    }

    return(list(
        valid = length(errors) == 0,
        errors = errors,
        warnings = warnings,
        x_clean = x_clean,
        class_clean = class_clean,
        pos_class = pos_class,
        n_pos = pos_count,
        n_neg = neg_count
    ))
}
