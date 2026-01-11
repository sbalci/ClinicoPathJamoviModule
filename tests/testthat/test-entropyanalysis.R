# Tests for entropyanalysis function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("entropyanalysis works with basic settings", {
  set.seed(123)
  n <- 50
  
  # Ensure probabilities sum to roughly 1 (normalized internally anyway)
  p1 <- runif(n, 0, 1)
  p2 <- 1 - p1
  
  data <- data.frame(
    outcome = factor(sample(c('ClassA', 'ClassB'), n, replace = TRUE), levels=c('ClassA', 'ClassB')),
    prob_A = p1,
    prob_B = p2,
    predictor = factor(sample(c('Pred1', 'Pred2'), n, replace = TRUE))
  )
  
  expect_no_error({
    result <- entropyanalysis(
      data = data,
      outcome = 'outcome',
      probability_vars = c('prob_A', 'prob_B'),
      predictor_var = 'predictor',
      calculate_entropy = TRUE,
      calculate_conditional_entropy = TRUE,
      calculate_mutual_information = TRUE,
      calculate_kl_divergence = FALSE,
      uncertainty_threshold = 0.5,
      normalize_entropy = TRUE,
      binning_method = 'equal_width',
      n_bins = 5,
      show_case_level = FALSE,
      flag_uncertain = TRUE,
      plot_entropy_distribution = FALSE,
      plot_uncertainty_by_class = FALSE,
      plot_mi_heatmap = FALSE,
      random_seed = 42
    )
  })
})

test_that("entropyanalysis returns results object", {
  set.seed(456)
  n <- 30
  
  # 3 classes
  p1 <- runif(n)
  p2 <- runif(n)
  p3 <- runif(n)
  
  # Normalize
  total <- p1 + p2 + p3
  p1 <- p1 / total
  p2 <- p2 / total
  p3 <- p3 / total
  
  data <- data.frame(
    y = factor(sample(c('C1', 'C2', 'C3'), n, replace = TRUE), levels=c('C1', 'C2', 'C3')),
    p_C1 = p1,
    p_C2 = p2,
    p_C3 = p3,
    x = rnorm(n)
  )
  
  result <- entropyanalysis(
    data = data,
    outcome = 'y',
    probability_vars = c('p_C1', 'p_C2', 'p_C3'),
    predictor_var = 'x',
    calculate_entropy = TRUE,
    calculate_mutual_information = FALSE,
    binning_method = 'equal_width',
    n_bins = 5,
    random_seed = 123
  )
  
  expect_true(inherits(result, "entropyanalysisResults"))
})
