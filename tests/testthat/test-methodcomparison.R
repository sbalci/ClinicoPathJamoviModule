# Test suite for methodcomparison function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("methodcomparison runs without error on basic data", {
  set.seed(123)
  n <- 50
  data <- data.frame(
    method1 = runif(n, 1, 100),
    method2 = runif(n, 1, 100),
    grouping = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  
  expect_no_error({
    result <- methodcomparison(
      data = data,
      method1 = "method1",
      method2 = "method2",
      grouping = "grouping",
      comparison_method = "bland_altman",
      plots_options = FALSE,
      bland_altman_plot = FALSE,
      scatter_plot = FALSE
    )
  })
})

test_that("methodcomparison returns results object", {
  set.seed(456)
  n <- 40
  data <- data.frame(
    method1 = rnorm(n, 50, 10),
    method2 = rnorm(n, 52, 10),
    grp = factor(sample(c("X", "Y"), n, replace = TRUE))
  )
  
  result <- methodcomparison(
    data = data,
    method1 = "method1",
    method2 = "method2",
    grouping = NULL,  # Explicitly NULL
    comparison_method = "bland_altman",
    plots_options = FALSE,
    bland_altman_plot = FALSE,
    scatter_plot = FALSE
  )
  
  expect_true(inherits(result, "methodcomparisonResults"))
})

test_that("methodcomparison populates descriptive stats table", {
  set.seed(789)
  n <- 30
  data <- data.frame(
    method1 = rnorm(n, 100, 15),
    method2 = rnorm(n, 100, 15) + rnorm(n, 0, 5)
  )
  
  result <- methodcomparison(
    data = data,
    method1 = "method1", 
    method2 = "method2",
    grouping = NULL,
    comparison_method = "bland_altman",
    correlation_analysis = TRUE,
    concordance_correlation = TRUE,
    plots_options = FALSE,
    bland_altman_plot = FALSE,
    scatter_plot = FALSE
  )
  
  expect_true(result$descriptiveStats$rowCount >= 1)
})

test_that("methodcomparison Passing-Bablok works", {
  set.seed(111)
  n <- 50
  data <- data.frame(
    method1 = rnorm(n, 50, 10),
    method2 = rnorm(n, 50, 10)
  )
  
  expect_no_error({
    result <- methodcomparison(
      data = data,
      method1 = "method1",
      method2 = "method2",
      grouping = NULL,
      comparison_method = "passing_bablok",
      passing_bablok_options = TRUE,
      plots_options = FALSE,
      bland_altman_plot = FALSE,
      scatter_plot = FALSE
    )
  })
})
