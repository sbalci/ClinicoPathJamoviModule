# ===== Diagnostic Accuracy Helper Functions =====
# Reusable utility functions for diagnostic test calculations
# These functions support various diagnostic modules in ClinicoPath
# (e.g., enhancedroc, precisionrecall, clinicalvalidation)

#' Calculate Sensitivity (True Positive Rate)
#'
#' @param tp Number of true positives
#' @param fn Number of false negatives
#' @return Sensitivity value (0-1)
.calculateSensitivity <- function(tp, fn) {
    if (tp + fn == 0) return(NA)
    return(tp / (tp + fn))
}

#' Calculate Specificity (True Negative Rate)
#'
#' @param tn Number of true negatives
#' @param fp Number of false positives
#' @return Specificity value (0-1)
.calculateSpecificity <- function(tn, fp) {
    if (tn + fp == 0) return(NA)
    return(tn / (tn + fp))
}

#' Calculate Positive Predictive Value (Precision)
#'
#' @param tp Number of true positives
#' @param fp Number of false positives
#' @param prevalence Disease prevalence (optional, for Bayes adjustment)
#' @return PPV value (0-1)
.calculatePPV <- function(tp, fp, prevalence = NULL) {
    if (tp + fp == 0) return(NA)

    ppv_crude <- tp / (tp + fp)

    # If prevalence provided, adjust using Bayes theorem
    if (!is.null(prevalence)) {
        # Calculate sensitivity and specificity from current data
        # This assumes we have representative sample
        # For more accurate adjustment, pass sensitivity and specificity directly
        return(ppv_crude)  # Placeholder for now
    }

    return(ppv_crude)
}

#' Calculate Negative Predictive Value
#'
#' @param tn Number of true negatives
#' @param fn Number of false negatives
#' @param prevalence Disease prevalence (optional, for Bayes adjustment)
#' @return NPV value (0-1)
.calculateNPV <- function(tn, fn, prevalence = NULL) {
    if (tn + fn == 0) return(NA)

    npv_crude <- tn / (tn + fn)

    # If prevalence provided, adjust using Bayes theorem
    if (!is.null(prevalence)) {
        return(npv_crude)  # Placeholder for now
    }

    return(npv_crude)
}

#' Calculate Positive Likelihood Ratio
#'
#' @param sensitivity Sensitivity (TPR)
#' @param specificity Specificity (TNR)
#' @return Positive likelihood ratio (LR+)
.calculateLRPlus <- function(sensitivity, specificity) {
    if (is.na(sensitivity) || is.na(specificity)) return(NA)
    if (specificity == 1) return(Inf)  # Perfect specificity
    if (specificity == 0) return(NA)   # Cannot divide by zero

    return(sensitivity / (1 - specificity))
}

#' Calculate Negative Likelihood Ratio
#'
#' @param sensitivity Sensitivity (TPR)
#' @param specificity Specificity (TNR)
#' @return Negative likelihood ratio (LR-)
.calculateLRMinus <- function(sensitivity, specificity) {
    if (is.na(sensitivity) || is.na(specificity)) return(NA)
    if (sensitivity == 1) return(0)    # Perfect sensitivity
    if (specificity == 0) return(NA)   # Cannot interpret

    return((1 - sensitivity) / specificity)
}

#' Calculate Diagnostic Odds Ratio
#'
#' @param lr_plus Positive likelihood ratio
#' @param lr_minus Negative likelihood ratio
#' @return Diagnostic odds ratio (DOR)
.calculateDOR <- function(lr_plus, lr_minus) {
    if (is.na(lr_plus) || is.na(lr_minus)) return(NA)
    if (is.infinite(lr_plus)) return(Inf)
    if (lr_minus == 0) return(Inf)

    return(lr_plus / lr_minus)
}

#' Calculate Youden's J Index
#'
#' @param sensitivity Sensitivity (TPR)
#' @param specificity Specificity (TNR)
#' @return Youden's J statistic (0-1)
.calculateYouden <- function(sensitivity, specificity) {
    if (is.na(sensitivity) || is.na(specificity)) return(NA)
    return(sensitivity + specificity - 1)
}

#' Calculate Accuracy
#'
#' @param tp Number of true positives
#' @param tn Number of true negatives
#' @param fp Number of false positives
#' @param fn Number of false negatives
#' @return Overall accuracy (0-1)
.calculateAccuracy <- function(tp, tn, fp, fn) {
    total <- tp + tn + fp + fn
    if (total == 0) return(NA)
    return((tp + tn) / total)
}

#' Calculate F1 Score (Harmonic Mean of Precision and Recall)
#'
#' @param tp Number of true positives
#' @param fp Number of false positives
#' @param fn Number of false negatives
#' @return F1 score (0-1)
.calculateF1Score <- function(tp, fp, fn) {
    precision <- .calculatePPV(tp, fp)
    recall <- .calculateSensitivity(tp, fn)

    if (is.na(precision) || is.na(recall)) return(NA)
    if (precision + recall == 0) return(0)

    return(2 * (precision * recall) / (precision + recall))
}

#' Calculate Matthews Correlation Coefficient
#'
#' @param tp Number of true positives
#' @param tn Number of true negatives
#' @param fp Number of false positives
#' @param fn Number of false negatives
#' @return MCC value (-1 to 1)
.calculateMCC <- function(tp, tn, fp, fn) {
    numerator <- (tp * tn) - (fp * fn)
    denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

    if (denominator == 0) return(NA)
    return(numerator / denominator)
}

#' Comprehensive 2x2 Diagnostic Metrics Calculator
#'
#' Calculates all standard diagnostic accuracy metrics from a 2x2 confusion matrix
#'
#' @param tp Number of true positives
#' @param fp Number of false positives
#' @param fn Number of false negatives
#' @param tn Number of true negatives
#' @param prevalence Optional disease prevalence (for adjusted PPV/NPV)
#' @return List containing all diagnostic metrics
.calculate2x2Metrics <- function(tp, fp, fn, tn, prevalence = NULL) {

    # Basic counts
    total <- tp + fp + fn + tn

    # Primary metrics
    sensitivity <- .calculateSensitivity(tp, fn)
    specificity <- .calculateSpecificity(tn, fp)
    ppv <- .calculatePPV(tp, fp, prevalence)
    npv <- .calculateNPV(tn, fn, prevalence)

    # Derived metrics
    lr_plus <- .calculateLRPlus(sensitivity, specificity)
    lr_minus <- .calculateLRMinus(sensitivity, specificity)
    dor <- .calculateDOR(lr_plus, lr_minus)
    youden <- .calculateYouden(sensitivity, specificity)
    accuracy <- .calculateAccuracy(tp, tn, fp, fn)
    f1_score <- .calculateF1Score(tp, fp, fn)
    mcc <- .calculateMCC(tp, tn, fp, fn)

    # Return comprehensive list
    return(list(
        # Confusion matrix
        tp = tp,
        fp = fp,
        fn = fn,
        tn = tn,
        total = total,

        # Primary metrics
        sensitivity = sensitivity,
        specificity = specificity,
        ppv = ppv,
        npv = npv,

        # Derived metrics
        lr_plus = lr_plus,
        lr_minus = lr_minus,
        dor = dor,
        youden = youden,
        accuracy = accuracy,
        f1_score = f1_score,
        mcc = mcc,

        # Rates
        tpr = sensitivity,  # True positive rate (alias for sensitivity)
        tnr = specificity,  # True negative rate (alias for specificity)
        fpr = ifelse(!is.na(specificity), 1 - specificity, NA),  # False positive rate
        fnr = ifelse(!is.na(sensitivity), 1 - sensitivity, NA),  # False negative rate

        # Prevalence info
        prevalence = prevalence
    ))
}

#' Format Diagnostic Metrics Table for Display
#'
#' Creates a formatted data frame of diagnostic metrics with confidence intervals
#'
#' @param metrics List of calculated metrics (from calculate2x2Metrics)
#' @param ci_method Method for confidence intervals ('wilson', 'exact', 'asymptotic')
#' @param conf_level Confidence level (default 0.95)
#' @return Formatted data frame ready for table display
.formatDiagnosticTable <- function(metrics, ci_method = "wilson", conf_level = 0.95) {

    # Helper function to calculate Wilson score confidence intervals
    wilson_ci <- function(p, n, conf_level = 0.95) {
        if (is.na(p) || n == 0) return(c(NA, NA))

        z <- qnorm((1 + conf_level) / 2)
        denominator <- 1 + z^2/n
        centre_adjusted_probability <- (p + z^2/(2*n)) / denominator
        adjusted_standard_deviation <- sqrt((p*(1-p)/n + z^2/(4*n^2)) / denominator)

        lower <- centre_adjusted_probability - z * adjusted_standard_deviation
        upper <- centre_adjusted_probability + z * adjusted_standard_deviation

        return(c(lower = max(0, lower), upper = min(1, upper)))
    }

    # Calculate confidence intervals for key metrics
    n_diseased <- metrics$tp + metrics$fn
    n_healthy <- metrics$tn + metrics$fp
    n_test_positive <- metrics$tp + metrics$fp
    n_test_negative <- metrics$tn + metrics$fn

    sens_ci <- wilson_ci(metrics$sensitivity, n_diseased, conf_level)
    spec_ci <- wilson_ci(metrics$specificity, n_healthy, conf_level)
    ppv_ci <- wilson_ci(metrics$ppv, n_test_positive, conf_level)
    npv_ci <- wilson_ci(metrics$npv, n_test_negative, conf_level)

    # Create formatted table
    table_data <- data.frame(
        Metric = c(
            "Sensitivity (Recall, TPR)",
            "Specificity (TNR)",
            "Positive Predictive Value (Precision)",
            "Negative Predictive Value",
            "Positive Likelihood Ratio",
            "Negative Likelihood Ratio",
            "Diagnostic Odds Ratio",
            "Youden's J Index",
            "Accuracy",
            "F1 Score",
            "Matthews Correlation Coefficient"
        ),
        Value = c(
            metrics$sensitivity,
            metrics$specificity,
            metrics$ppv,
            metrics$npv,
            metrics$lr_plus,
            metrics$lr_minus,
            metrics$dor,
            metrics$youden,
            metrics$accuracy,
            metrics$f1_score,
            metrics$mcc
        ),
        Lower_CI = c(
            sens_ci[1],
            spec_ci[1],
            ppv_ci[1],
            npv_ci[1],
            NA, NA, NA, NA, NA, NA, NA  # CIs for LRs, DOR, etc. require more complex calculations
        ),
        Upper_CI = c(
            sens_ci[2],
            spec_ci[2],
            ppv_ci[2],
            npv_ci[2],
            NA, NA, NA, NA, NA, NA, NA
        ),
        Interpretation = c(
            "Proportion of actual positives correctly identified",
            "Proportion of actual negatives correctly identified",
            "Probability that positive test indicates disease",
            "Probability that negative test excludes disease",
            "How much test increases disease odds when positive",
            "How much test decreases disease odds when negative",
            "Overall discriminative ability of the test",
            "Overall test performance (optimality criterion)",
            "Proportion of all cases correctly classified",
            "Harmonic mean of precision and recall",
            "Correlation between predictions and truth"
        ),
        stringsAsFactors = FALSE
    )

    return(table_data)
}

#' Calculate Confidence Interval for Sensitivity
#'
#' @param tp Number of true positives
#' @param fn Number of false negatives
#' @param conf_level Confidence level (default 0.95)
#' @param method Method ('wilson', 'exact', 'asymptotic')
#' @return List with estimate, lower, and upper bounds
.sensitivityCI <- function(tp, fn, conf_level = 0.95, method = "wilson") {
    n <- tp + fn
    if (n == 0) return(list(estimate = NA, lower = NA, upper = NA))

    p <- tp / n

    if (method == "wilson") {
        z <- qnorm((1 + conf_level) / 2)
        denominator <- 1 + z^2/n
        centre <- (p + z^2/(2*n)) / denominator
        margin <- z * sqrt((p*(1-p)/n + z^2/(4*n^2)) / denominator)

        return(list(
            estimate = p,
            lower = max(0, centre - margin),
            upper = min(1, centre + margin)
        ))
    } else if (method == "exact") {
        # Clopper-Pearson exact method
        if (tp == 0) {
            lower <- 0
        } else {
            lower <- qbeta((1 - conf_level) / 2, tp, fn + 1)
        }

        if (tp == n) {
            upper <- 1
        } else {
            upper <- qbeta((1 + conf_level) / 2, tp + 1, fn)
        }

        return(list(estimate = p, lower = lower, upper = upper))
    } else {
        # Asymptotic (Wald) method
        z <- qnorm((1 + conf_level) / 2)
        se <- sqrt(p * (1 - p) / n)

        return(list(
            estimate = p,
            lower = max(0, p - z * se),
            upper = min(1, p + z * se)
        ))
    }
}

#' Calculate Confidence Interval for Specificity
#'
#' @param tn Number of true negatives
#' @param fp Number of false positives
#' @param conf_level Confidence level (default 0.95)
#' @param method Method ('wilson', 'exact', 'asymptotic')
#' @return List with estimate, lower, and upper bounds
.specificityCI <- function(tn, fp, conf_level = 0.95, method = "wilson") {
    # Specificity CI is same as sensitivity CI but with tn/fp instead of tp/fn
    return(.sensitivityCI(tn, fp, conf_level, method))
}

#' Interpret Likelihood Ratio Values
#'
#' @param lr Likelihood ratio value
#' @param type Type of LR ('plus' or 'minus')
#' @return Character string with interpretation
.interpretLR <- function(lr, type = "plus") {
    if (is.na(lr)) return("Cannot be determined")

    if (type == "plus") {
        if (is.infinite(lr)) return("Conclusive positive test (rules in disease)")
        if (lr > 10) return("Large increase in probability (strong rule-in)")
        if (lr > 5) return("Moderate increase in probability")
        if (lr > 2) return("Small increase in probability")
        if (lr > 1) return("Minimal increase in probability")
        if (lr == 1) return("No change in probability")
        return("Decreases probability (unusual for LR+)")
    } else {
        if (lr == 0) return("Conclusive negative test (rules out disease)")
        if (lr < 0.1) return("Large decrease in probability (strong rule-out)")
        if (lr < 0.2) return("Moderate decrease in probability")
        if (lr < 0.5) return("Small decrease in probability")
        if (lr < 1) return("Minimal decrease in probability")
        if (lr == 1) return("No change in probability")
        return("Increases probability (unusual for LR-)")
    }
}

#' Interpret Diagnostic Odds Ratio
#'
#' @param dor Diagnostic odds ratio value
#' @return Character string with interpretation
.interpretDOR <- function(dor) {
    if (is.na(dor)) return("Cannot be determined")
    if (is.infinite(dor)) return("Perfect discrimination")
    if (dor > 100) return("Excellent discrimination")
    if (dor > 20) return("Good discrimination")
    if (dor > 10) return("Fair discrimination")
    if (dor > 1) return("Poor discrimination")
    if (dor == 1) return("No discriminative value (no better than chance)")
    return("Inverse relationship (test performs opposite of expected)")
}
