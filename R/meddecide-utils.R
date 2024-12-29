#' @title Utility functions for ROC curve analysis and comparison
#' @description Functions for ROC curve analysis, including DeLong's test and bootstrap comparisons
#' @name roc_utils
#' @importFrom stats pnorm pt qnorm pchisq friedman.test t.test p.adjust cov
#' @importFrom Matrix nearPD
#' @importFrom boot boot
#' @importFrom stats quantile
#' @importFrom tidyr gather
#' @importFrom RColorBrewer brewer.pal
NULL

#' Calculate confidence intervals for ROC curve
#' @keywords internal
calculate_ci_roc <- function(data, indices) {
  if (!all(c("test_pos", "disease_pos") %in% names(data))) {
    stop("Data must contain 'test_pos' and 'disease_pos' columns")
  }

  d <- data[indices, ]
  tp <- sum(d$test_pos & d$disease_pos, na.rm = TRUE)
  fp <- sum(d$test_pos & !d$disease_pos, na.rm = TRUE)
  tn <- sum(!d$test_pos & !d$disease_pos, na.rm = TRUE)
  fn <- sum(!d$test_pos & d$disease_pos, na.rm = TRUE)

  sens <- if(tp + fn > 0) tp / (tp + fn) else 0
  spec <- if(tn + fp > 0) tn / (tn + fp) else 0

  return(c(sens = sens, spec = spec))
}

#' Compare AUCs of two diagnostic tests
#' @keywords internal
compare_auc <- function(test1, test2) {
  required_fields <- c("sens", "spec", "n_pos", "n_neg")
  if (!all(required_fields %in% names(test1)) ||
      !all(required_fields %in% names(test2))) {
    stop("Test data must contain sensitivity, specificity, and sample sizes")
  }

  auc1 <- calculate_auc(test1$sens, test1$spec)
  auc2 <- calculate_auc(test2$sens, test2$spec)

  se1 <- calculate_se(auc1, test1$n_pos, test1$n_neg)
  se2 <- calculate_se(auc2, test2$n_pos, test2$n_neg)

  z <- (auc1 - auc2) / sqrt(se1^2 + se2^2)
  p_value <- 2 * (1 - pnorm(abs(z)))

  return(list(
    auc_diff = auc1 - auc2,
    z_stat = z,
    p_value = p_value,
    se = sqrt(se1^2 + se2^2)
  ))
}

#' Calculate AUC from sensitivity and specificity
#' @keywords internal
calculate_auc <- function(sens, spec) {
  0.5 * (sens * (1-spec)) + 0.5 * (1 * (1-(1-spec))) + 0.5 * ((1-sens) * spec)
}

#' Calculate standard error for AUC
#' @keywords internal
calculate_se <- function(auc, n_pos, n_neg) {
  sqrt((auc * (1-auc)) / min(n_pos, n_neg))
}

#' Calculate DeLong covariance matrix
#' @keywords internal
calculate_delong_cov <- function(V10, V01) {
  if (!is.matrix(V10) || !is.matrix(V01)) {
    stop("Placement values must be matrices")
  }

  n1 <- nrow(V10)
  n2 <- nrow(V01)

  S10 <- if(n1 > 1) stats::cov(V10) / n1 else matrix(0, 1, 1)
  S01 <- if(n2 > 1) stats::cov(V01) / n2 else matrix(0, 1, 1)

  return(S10 + S01)
}

#' Compare multiple ROC curves using DeLong's test
#' @param test_data List of test results
#' @param paired Logical indicating whether tests are paired
#' @return A list containing test results with the following components:
#'   \item{overall}{Overall test results including chi-square statistic and p-value}
#'   \item{pairwise}{Matrix of pairwise comparison p-values}
#'   \item{aucs}{Vector of AUC values for each test}
#'   \item{covariance}{Covariance matrix of the tests}
#' @export
delong_test <- function(test_data, paired = TRUE) {
  if (!is.list(test_data) || length(test_data) < 2) {
    stop("test_data must be a list containing at least two tests")
  }

  n_tests <- length(test_data)
  test_names <- names(test_data)
  if (is.null(test_names)) {
    test_names <- paste0("Test", 1:n_tests)
  }

  placement_values <- calculate_placement_values(test_data)
  V10 <- placement_values$V10
  V01 <- placement_values$V01

  cov_matrix <- tryCatch({
    cov_mat <- calculate_delong_cov(V10, V01)
    as.matrix(Matrix::nearPD(cov_mat)$mat)
  }, error = function(e) {
    warning("Error in covariance calculation, using diagonal matrix")
    diag(n_tests)
  })

  results <- calculate_delong_results(test_data, cov_matrix, test_names)

  return(results)
}

#' Calculate placement values for DeLong's test
#' @keywords internal
calculate_placement_values <- function(test_data) {
  n_tests <- length(test_data)
  n1 <- test_data[[1]]$n_pos
  n2 <- test_data[[1]]$n_neg

  V10 <- matrix(0, nrow = n1, ncol = n_tests)
  V01 <- matrix(0, nrow = n2, ncol = n_tests)

  for(i in seq_len(n_tests)) {
    test <- test_data[[i]]
    V10[,i] <- test$placement_values_diseased
    V01[,i] <- test$placement_values_healthy
  }

  list(V10 = V10, V01 = V01)
}

#' Compare ROC curves using bootstrap method
#' @param test_data List of test results
#' @param n_bootstrap Number of bootstrap iterations
#' @return A list containing bootstrap comparison results:
#'   \item{overall}{Overall comparison results using Friedman test}
#'   \item{pairwise}{Matrix of pairwise comparison p-values}
#' @export
bootstrap_roc_test <- function(test_data, n_bootstrap = 1000) {
  if (n_bootstrap < 100) {
    warning("Number of bootstrap iterations may be too low")
  }

  boot_data <- prepare_bootstrap_data(test_data)
  boot_results <- perform_bootstrap(boot_data, n_bootstrap)

  results <- calculate_bootstrap_results(boot_results, names(test_data))

  return(results)
}

#' Prepare data for bootstrap analysis
#' @keywords internal
prepare_bootstrap_data <- function(test_data) {
  if (!is.list(test_data) || length(test_data) < 2) {
    stop("test_data must be a list containing at least two tests")
  }

  data.frame(
    test_pos = rep(TRUE, sum(sapply(test_data, function(x) x$n_pos))),
    disease_pos = rep(TRUE, sum(sapply(test_data, function(x) x$n_pos)))
  )
}

#' Calculate bootstrap results
#' @keywords internal
calculate_bootstrap_results <- function(boot_results, test_names) {
  n_tests <- length(test_names)

  auc_matrix <- extract_auc_matrix(boot_results, n_tests)

  results <- list(
    overall = calculate_overall_comparison(auc_matrix),
    pairwise = calculate_pairwise_comparisons(auc_matrix, test_names)
  )

  return(results)
}


#' Compare multiple diagnostic tests
#' @param tests List of test results
#' @param method Method for comparison ("delong" or "bootstrap")
#' @return List containing test results
compare_tests <- function(tests, method = "delong") {
  if (method == "delong") {
    results <- delong_test(tests)
  } else {
    results <- bootstrap_roc_test(tests)
  }

  return(results)
}

#' DeLong's test for comparing ROC curves
#' @param roc1 First ROC curve data
#' @param roc2 Second ROC curve data
#' @return List containing test statistic and p-value
delong_test <- function(roc1, roc2) {
  # Calculate variance-covariance matrix
  v1 <- var(roc1$scores[roc1$labels == 1])
  v2 <- var(roc2$scores[roc2$labels == 1])
  cov12 <- cov(roc1$scores[roc1$labels == 1],
               roc2$scores[roc2$labels == 1])

  # Calculate test statistic
  z <- (roc1$auc - roc2$auc) /
    sqrt(v1/roc1$n + v2/roc2$n - 2*cov12/sqrt(roc1$n*roc2$n))

  # Calculate p-value
  p <- 2 * (1 - pnorm(abs(z)))

  return(list(
    statistic = z,
    p.value = p
  ))
}

