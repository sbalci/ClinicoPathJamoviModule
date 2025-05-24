#' @title Statistical Utility Functions
#' @description Functions for calculating confidence intervals and test statistics

#' Calculate AUC confidence intervals
#' @param auc Area under curve value
#' @param n_pos Number of positive cases
#' @param n_neg Number of negative cases
#' @param conf_level Confidence level (default 0.95)
#' @return Vector containing lower and upper CI bounds
#' @export
auc_ci <- function(auc, n_pos, n_neg, conf_level = 0.95) {
  if (!is.numeric(auc) || auc < 0 || auc > 1) {
    stop("AUC must be between 0 and 1")
  }
  if (!is.numeric(n_pos) || !is.numeric(n_neg) ||
      n_pos < 1 || n_neg < 1) {
    stop("Sample sizes must be positive integers")
  }

  se <- sqrt((auc * (1-auc)) / min(n_pos, n_neg))
  z <- qnorm(1 - (1-conf_level)/2)
  ci <- c(auc - z*se, auc + z*se)
  ci <- pmax(0, pmin(1, ci)) # Constrain to [0,1]
  return(ci)
}


#' Bootstrap confidence intervals for diagnostic metrics
#' @param data Data frame containing test results
#' @param metric Function to calculate desired metric
#' @param R Number of bootstrap iterations
#' @return List containing point estimate and confidence intervals
#' @export
bootstrap_ci <- function(data, metric, R = 1000) {
  boot_results <- boot::boot(data, metric, R = R)
  ci <- boot::boot.ci(boot_results, type = "bca")
  return(list(
    estimate = boot_results$t0,
    ci_lower = ci$bca[4],
    ci_upper = ci$bca[5]
  ))
}
