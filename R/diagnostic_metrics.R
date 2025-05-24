#' Diagnostic metric calculations
#'
#' Utility functions for basic diagnostic test statistics.
#'
#' @param tp Number of true positives
#' @param fn Number of false negatives
#'
#' @return Numeric sensitivity value or `NA` when inputs are not valid.
#' @export
calculate_sensitivity <- function(tp, fn) {
  if (is.na(tp) || is.na(fn) || (tp + fn) == 0) {
    return(NA_real_)
  }
  tp / (tp + fn)
}

#' Calculate test specificity
#'
#' @param tn Number of true negatives
#' @param fp Number of false positives
#'
#' @return Numeric specificity value or `NA` when inputs are not valid.
#' @export
calculate_specificity <- function(tn, fp) {
  if (is.na(tn) || is.na(fp) || (tn + fp) == 0) {
    return(NA_real_)
  }
  tn / (tn + fp)
}

#' Approximate AUC from sensitivity and specificity
#'
#' Uses the formula shown in the package documentation example:
#' `0.5 * (sens * (1 - spec)) + 0.5 * (1 * (1 - (1 - spec))) + 0.5 * ((1 - sens) * spec)`.
#'
#' @param sens Sensitivity of the test
#' @param spec Specificity of the test
#'
#' @return Numeric AUC value or `NA` when inputs are not valid.
#' @export
calculate_auc <- function(sens, spec) {
  if (is.na(sens) || is.na(spec)) {
    return(NA_real_)
  }
  0.5 * (sens * (1 - spec)) +
    0.5 * (1 * (1 - (1 - spec))) +
    0.5 * ((1 - sens) * spec)
}
