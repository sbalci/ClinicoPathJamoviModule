# Tests for generalizedroc function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("generalizedroc works with basic settings", {
  set.seed(123)
  n <- 50
  
  data <- data.frame(
    outcome = factor(sample(c('Control', 'Case'), n, replace = TRUE), levels=c('Control', 'Case')),
    predictor = rnorm(n),
    cov1 = rnorm(n)
  )
  
  expect_no_error({
    result <- generalizedroc(
      data = data,
      outcome = 'outcome',
      predictor = 'predictor',
      assume_equal_variance = FALSE,
      transformation = 'none',
      distribution_model = 'normal',
      calculate_auc = TRUE,
      confidence_intervals = TRUE,
      ci_method = 'bootstrap',
      bootstrap_samples = 100,
      confidence_level = 0.95,
      show_diagnostics = TRUE,
      variance_test = 'levene',
      optimal_threshold = TRUE,
      plot_roc = FALSE,
      plot_distributions = FALSE,
      plot_diagnostic = FALSE,
      use_tram = FALSE,
      covariates = NULL,
      tram_model = 'Colr',
      plot_covariate_roc = FALSE,
      plot_auc_vs_covariate = FALSE,
      n_covariate_points = 50,
      random_seed = 42
    )
  })
})

test_that("generalizedroc returns results object", {
  set.seed(456)
  n <- 30
  
  data <- data.frame(
    y = factor(sample(c('0', '1'), n, replace = TRUE), levels=c('0', '1')),
    x = rnorm(n)
  )
  
  result <- generalizedroc(
    data = data,
    outcome = 'y',
    predictor = 'x',
    assume_equal_variance = TRUE,
    calculate_auc = TRUE,
    bootstrap_samples = 100
  )
  
  expect_true(inherits(result, "generalizedrocResults"))
})
