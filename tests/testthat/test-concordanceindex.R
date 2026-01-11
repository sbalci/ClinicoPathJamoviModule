# Tests for concordanceindex function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("concordanceindex works with basic inputs", {
  skip_if_not_installed("survival")
  
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    event = factor(rbinom(n, 1, 0.6)),
    risk_score = rnorm(n)
  )
  
  # Run analysis  
  results <- concordanceindex(
    data = test_data,
    time = "time",
    event = "event",
    event_code = "1",
    predictor = "risk_score",
    stratify_by = NULL,
    additional_predictors = NULL
  )
  
  expect_true(inherits(results, "concordanceindexResults"))
  expect_true(!is.null(results$cindexSummary))
})

test_that("concordanceindex returns valid C-index", {
  skip_if_not_installed("survival")
  
  set.seed(456)
  n <- 80
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    event = factor(rbinom(n, 1, 0.6)),
    risk_score = rnorm(n)
  )
  
  results <- concordanceindex(
    data = test_data,
    time = "time",
    event = "event",
    event_code = "1",
    predictor = "risk_score",
    stratify_by = NULL,
    additional_predictors = NULL
  )
  
  # Check result is created
  expect_true(inherits(results, "concordanceindexResults"))
  expect_true(results$cindexSummary$rowCount >= 1)
})

test_that("concordanceindex handles Somers D option", {
  skip_if_not_installed("survival")
  
  set.seed(789)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    event = factor(rbinom(n, 1, 0.6)),
    risk_score = rnorm(n)
  )
  
  results <- concordanceindex(
    data = test_data,
    time = "time",
    event = "event",
    event_code = "1",
    predictor = "risk_score",
    somers_d = TRUE,
    stratify_by = NULL,
    additional_predictors = NULL
  )
  
  expect_true(!is.null(results$somersD))
})
