# Tests for prauc (Precision-Recall AUC) function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("prauc works with basic inputs", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    outcome = factor(sample(c("Positive", "Negative"), n, replace = TRUE)),
    predictor = runif(n, 0, 1)
  )
  
  expect_no_error({
    result <- prauc(
      data = data,
      outcome = "outcome",
      predictor = "predictor",
      prevalence = 0,
      calculate_auc = TRUE,
      calculate_fscore = FALSE,
      confidence_intervals = FALSE,
      compare_to_roc = FALSE,
      baseline_comparison = FALSE,
      plot_pr_curve = FALSE,
      plot_comparison = FALSE,
      plot_fscore = FALSE
    )
  })
})

test_that("prauc returns results object", {
  set.seed(456)
  n <- 80
  data <- data.frame(
    outcome = factor(sample(c("Yes", "No"), n, replace = TRUE)),
    predictor = runif(n, 0, 1)
  )
  
  result <- prauc(
    data = data,
    outcome = "outcome",
    predictor = "predictor",
    prevalence = 0,
    calculate_auc = TRUE,
    plot_pr_curve = FALSE
  )
  
  expect_true(inherits(result, "praucResults"))
})

test_that("prauc handles AUC calculation", {
  set.seed(789)
  n <- 100
  # Create data with better separation
  data <- data.frame(
    outcome = factor(c(rep("Positive", 50), rep("Negative", 50))),
    predictor = c(runif(50, 0.5, 1), runif(50, 0, 0.5))
  )
  
  result <- prauc(
    data = data,
    outcome = "outcome", 
    predictor = "predictor",
    calculate_auc = TRUE,
    plot_pr_curve = FALSE
  )
  
  expect_true(inherits(result, "praucResults"))
})
