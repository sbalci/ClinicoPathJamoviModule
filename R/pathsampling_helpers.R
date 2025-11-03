# Pathsampling Helper Functions
# Version: 2.0.0
# Date: 2025-10-31
#
# These functions implement the Phase 2 improvements for pathsampling.
# They should be added as private methods in pathsamplingClass in pathsampling.b.R

# ==============================================================================
# 1. HETEROGENEITY TESTING
# ==============================================================================

#' Test for heterogeneity in detection probability across groups
#'
#' Uses likelihood ratio test to compare:
#' H0: Single q for all groups (pooled model)
#' H1: Separate q for each group (stratified model)
#'
#' @param first_detection Numeric vector of first detection positions
#' @param groups Factor or character vector of group labels
#' @return List with statistic, df, p_value, interpretation
#' @keywords internal
.testHeterogeneity <- function(first_detection, groups) {

  # Remove NAs
  valid_idx <- !is.na(first_detection) & !is.na(groups)
  first_detection <- first_detection[valid_idx]
  groups <- groups[valid_idx]

  if (length(first_detection) < 10) {
    return(list(
      statistic = NA,
      df = NA,
      pValue = NA,
      interpretation = "Insufficient data (n < 10)"
    ))
  }

  # Calculate pooled q (H0: null model)
  mean_first_pooled <- mean(first_detection, na.rm = TRUE)
  q_pooled <- 1 / mean_first_pooled

  # Log-likelihood for pooled model (geometric distribution)
  ll_null <- sum(log(dgeom(first_detection - 1, q_pooled)), na.rm = TRUE)

  # Calculate group-specific q (H1: alternative model)
  ll_alt <- 0
  unique_groups <- unique(groups)
  n_groups <- length(unique_groups)

  for (group in unique_groups) {
    group_data <- first_detection[groups == group]
    if (length(group_data) > 0) {
      mean_first_group <- mean(group_data, na.rm = TRUE)
      q_group <- 1 / mean_first_group
      ll_alt <- ll_alt + sum(log(dgeom(group_data - 1, q_group)), na.rm = TRUE)
    }
  }

  # Likelihood ratio statistic
  lr_stat <- 2 * (ll_alt - ll_null)
  df <- n_groups - 1
  p_value <- pchisq(lr_stat, df, lower.tail = FALSE)

  # Interpretation
  if (p_value < 0.001) {
    interp <- "Strong evidence of heterogeneity (p < 0.001)"
  } else if (p_value < 0.01) {
    interp <- "Significant heterogeneity detected (p < 0.01)"
  } else if (p_value < 0.05) {
    interp <- "Moderate heterogeneity detected (p < 0.05)"
  } else {
    interp <- "No significant heterogeneity (p ≥ 0.05)"
  }

  list(
    statistic = lr_stat,
    df = df,
    pValue = p_value,
    interpretation = interp
  )
}


# ==============================================================================
# 2. GEOMETRIC CI CALCULATION
# ==============================================================================

#' Calculate confidence intervals using geometric model
#'
#' Computes cumulative detection probability CI from q CI
#' P(detect in ≤n) = 1 - (1-q)^n
#'
#' @param n_samples Number of samples
#' @param q_mle Point estimate of q
#' @param q_ci_lower Lower bound of q CI
#' @param q_ci_upper Upper bound of q CI
#' @return List with point, lower, upper probabilities
#' @keywords internal
.calculateGeometricCI <- function(n_samples, q_mle, q_ci_lower, q_ci_upper) {

  # Cumulative detection probability
  prob_point <- 1 - (1 - q_mle)^n_samples
  prob_lower <- 1 - (1 - q_ci_lower)^n_samples
  prob_upper <- 1 - (1 - q_ci_upper)^n_samples

  list(
    point = prob_point,
    lower = prob_lower,
    upper = prob_upper,
    method = "geometric"
  )
}


# ==============================================================================
# 3. MODEL FIT ASSESSMENT
# ==============================================================================

#' Test goodness of fit for geometric distribution
#'
#' Chi-square test comparing observed vs expected frequencies
#'
#' @param first_detection Numeric vector of first detection positions
#' @param q_estimate Estimated detection probability
#' @return List with chi_sq, df, p_value, fit_quality
#' @keywords internal
.testModelFit <- function(first_detection, q_estimate) {

  # Remove NAs
  first_detection <- first_detection[!is.na(first_detection)]

  if (length(first_detection) < 10) {
    return(list(
      chiSquare = NA,
      df = NA,
      pValue = NA,
      fitQuality = "Insufficient data"
    ))
  }

  # Get unique values and frequencies
  first_vals <- sort(unique(first_detection))
  observed <- sapply(first_vals, function(x) sum(first_detection == x))

  # Expected frequencies under geometric distribution
  n_total <- length(first_detection)
  expected <- sapply(first_vals, function(x) {
    n_total * dgeom(x - 1, q_estimate)
  })

  # Combine small expected frequencies (< 5)
  if (any(expected < 5)) {
    # Simple approach: group all rare categories
    keep_idx <- expected >= 5
    if (sum(keep_idx) > 1) {
      observed <- c(observed[keep_idx], sum(observed[!keep_idx]))
      expected <- c(expected[keep_idx], sum(expected[!keep_idx]))
    }
  }

  # Chi-square goodness of fit
  chi_sq <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1 - 1  # -1 for total, -1 for estimated parameter

  if (df < 1) df <- 1  # Minimum df = 1

  p_value <- pchisq(chi_sq, df, lower.tail = FALSE)

  # Fit quality assessment
  if (p_value >= 0.10) {
    fit_quality <- "Good fit (p ≥ 0.10)"
  } else if (p_value >= 0.05) {
    fit_quality <- "Acceptable fit (p ≥ 0.05)"
  } else if (p_value >= 0.01) {
    fit_quality <- "Marginal fit (p < 0.05)"
  } else {
    fit_quality <- "Poor fit (p < 0.01)"
  }

  list(
    chiSquare = chi_sq,
    df = df,
    pValue = p_value,
    fitQuality = fit_quality
  )
}


# ==============================================================================
# 4. OBSERVED VS PREDICTED COMPARISON
# ==============================================================================

#' Calculate observed vs predicted detection rates
#'
#' @param first_detection Numeric vector of first detection positions
#' @param q_estimate Estimated detection probability
#' @param max_samples Maximum number of samples to evaluate
#' @return Data frame with observed, predicted, difference, assessment
#' @keywords internal
.calculateObsPred <- function(first_detection, q_estimate, max_samples) {

  # Remove NAs
  valid_data <- first_detection[!is.na(first_detection)]
  n_positive <- length(valid_data)

  if (n_positive == 0) {
    return(data.frame(
      nSamples = integer(0),
      observed = numeric(0),
      predicted = numeric(0),
      difference = numeric(0),
      assessment = character(0)
    ))
  }

  results <- data.frame(
    nSamples = 1:max_samples,
    observed = NA_real_,
    predicted = NA_real_,
    difference = NA_real_,
    assessment = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in 1:max_samples) {
    # Observed cumulative detection
    n_detected_by_i <- sum(valid_data <= i)
    obs <- n_detected_by_i / n_positive

    # Predicted (geometric model)
    pred <- 1 - (1 - q_estimate)^i

    # Difference
    diff <- obs - pred

    # Assessment
    abs_diff <- abs(diff)
    if (abs_diff < 0.05) {
      assess <- "✅ Excellent fit"
    } else if (abs_diff < 0.10) {
      assess <- "✅ Good fit"
    } else if (abs_diff < 0.15) {
      assess <- "⚠️ Fair fit"
    } else {
      assess <- "❌ Poor fit"
    }

    results[i, ] <- list(i, obs, pred, diff, assess)
  }

  results
}


# ==============================================================================
# 5. APPEND CALCULATED VARIABLES
# ==============================================================================

#' Append calculated detection probabilities to dataset
#'
#' Creates new columns with cumulative probabilities, detection categories, etc.
#'
#' @param data Original dataset (data frame)
#' @param q_estimate Estimated detection probability
#' @param recommended_samples Recommended number of samples for target confidence
#' @param first_detection Vector of first detection positions
#' @param prefix Prefix for new variable names (default: "ps_")
#' @return Modified data frame with new columns
#' @keywords internal
.appendCalculatedVariables <- function(data, q_estimate, recommended_samples,
                                       first_detection, prefix = "ps_") {

  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    warning("Data must be a data frame for appendVariables")
    return(data)
  }

  n_rows <- nrow(data)

  # 1. Add cumulative probabilities for common sample sizes
  for (i in c(1, 2, 3, 4, 5, 7, 10)) {
    var_name <- paste0(prefix, "cumulative_prob_", i)
    prob <- 1 - (1 - q_estimate)^i
    data[[var_name]] <- rep(prob, n_rows)
  }

  # 2. Add detection category
  detection_cat <- rep(NA_character_, n_rows)
  for (idx in 1:n_rows) {
    fd <- first_detection[idx]
    if (is.na(fd)) {
      detection_cat[idx] <- "Negative"
    } else if (fd <= 2) {
      detection_cat[idx] <- "Early (1-2)"
    } else if (fd <= 5) {
      detection_cat[idx] <- "Standard (3-5)"
    } else {
      detection_cat[idx] <- "Late (>5)"
    }
  }
  data[[paste0(prefix, "detection_category")]] <- factor(
    detection_cat,
    levels = c("Early (1-2)", "Standard (3-5)", "Late (>5)", "Negative")
  )

  # 3. Add recommended samples (constant for all rows)
  data[[paste0(prefix, "recommended_samples")]] <- rep(recommended_samples, n_rows)

  # 4. Add "detected by N" flags
  for (n in c(3, 5, 7, 10)) {
    var_name <- paste0(prefix, "detected_by_", n)
    detected <- !is.na(first_detection) & first_detection <= n
    data[[var_name]] <- detected
  }

  # 5. Add detection efficiency (how early detected relative to recommended)
  efficiency <- rep(NA_real_, n_rows)
  for (idx in 1:n_rows) {
    fd <- first_detection[idx]
    if (!is.na(fd) && !is.na(recommended_samples) && recommended_samples > 0) {
      efficiency[idx] <- fd / recommended_samples
    }
  }
  data[[paste0(prefix, "detection_efficiency")]] <- efficiency

  return(data)
}


# ==============================================================================
# 6. AUTO-DETECT HETEROGENEITY (COMPOSITION ANALYSIS)
# ==============================================================================

#' Automatically detect and report population heterogeneity
#'
#' Calculates CV of q across groups and warns if high
#'
#' @param first_detection Numeric vector of first detection positions
#' @param groups Factor or character vector of group labels
#' @return List with composition, cv, warning flag, summary text
#' @keywords internal
.autoDetectHeterogeneity <- function(first_detection, groups) {

  # Remove NAs
  valid_idx <- !is.na(first_detection) & !is.na(groups)
  first_detection <- first_detection[valid_idx]
  groups <- groups[valid_idx]

  if (length(first_detection) < 10) {
    return(list(
      warning = FALSE,
      message = "Insufficient data for heterogeneity assessment (n < 10)"
    ))
  }

  # Calculate q for each group
  unique_groups <- unique(groups)
  group_stats <- list()
  q_values <- numeric(length(unique_groups))

  for (i in seq_along(unique_groups)) {
    group <- unique_groups[i]
    group_data <- first_detection[groups == group]
    n_group <- length(group_data)

    if (n_group >= 3) {
      mean_first <- mean(group_data)
      q_group <- 1 / mean_first
      q_values[i] <- q_group

      group_stats[[as.character(group)]] <- list(
        n = n_group,
        q = q_group,
        mean_first = mean_first
      )
    } else {
      q_values[i] <- NA
    }
  }

  # Remove NA q values
  q_values <- q_values[!is.na(q_values)]

  if (length(q_values) < 2) {
    return(list(
      warning = FALSE,
      message = "Only one group has sufficient data for q estimation"
    ))
  }

  # Calculate coefficient of variation
  mean_q <- mean(q_values)
  sd_q <- sd(q_values)
  cv_q <- sd_q / mean_q

  # Warning threshold
  warning_flag <- cv_q > 0.30

  # Create summary message
  if (warning_flag) {
    severity <- if (cv_q > 0.50) "HIGH" else "MODERATE"
    message <- sprintf(
      "⚠️ %s heterogeneity detected (CV = %.2f). Detection probability varies substantially across groups. Consider stratified analysis.",
      severity, cv_q
    )
  } else {
    message <- sprintf(
      "✅ Low heterogeneity (CV = %.2f). Pooled analysis is appropriate.",
      cv_q
    )
  }

  list(
    warning = warning_flag,
    cv = cv_q,
    mean_q = mean_q,
    sd_q = sd_q,
    group_stats = group_stats,
    message = message
  )
}


# ==============================================================================
# END OF HELPER FUNCTIONS
# ==============================================================================

# NOTE: These functions should be added to the private section of
# pathsamplingClass in pathsampling.b.R using the pattern:
#
# private = list(
#   .testHeterogeneity = function(...) { ... },
#   .calculateGeometricCI = function(...) { ... },
#   ... etc ...
# )
