# ===============================================================
# Basic Functionality Tests: lassocox (Lasso-Cox Regression)
# ===============================================================
#
# Tests that the lassocox function exists, runs with minimal arguments,
# errors appropriately on missing required arguments, and produces
# expected output structure.
#
# Uses package test datasets:
#   - lassocox_breast_cancer  (n=250, standard clinicopathological)
#   - lassocox_small_cohort   (n=75, minimal viable scenario)

library(testthat)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
skip_lassocox_deps <- function() {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
}

load_breast_cancer <- function() {
  candidates <- c(
    file.path("data", "lassocox_breast_cancer.rda"),
    file.path("..", "..", "data", "lassocox_breast_cancer.rda"),
    system.file("data", "lassocox_breast_cancer.rda", package = "ClinicoPath")
  )
  data_path <- candidates[nzchar(candidates) & file.exists(candidates)][1]
  if (file.exists(data_path)) {
    env <- new.env()
    load(data_path, envir = env)
    return(env$lassocox_breast_cancer)
  }
  skip("lassocox_breast_cancer.rda not found")
}

load_small_cohort <- function() {
  candidates <- c(
    file.path("data", "lassocox_small_cohort.rda"),
    file.path("..", "..", "data", "lassocox_small_cohort.rda"),
    system.file("data", "lassocox_small_cohort.rda", package = "ClinicoPath")
  )
  data_path <- candidates[nzchar(candidates) & file.exists(candidates)][1]
  if (file.exists(data_path)) {
    env <- new.env()
    load(data_path, envir = env)
    return(env$lassocox_small_cohort)
  }
  skip("lassocox_small_cohort.rda not found")
}

# ---------------------------------------------------------------
# Tests
# ---------------------------------------------------------------

test_that("lassocox function exists in ClinicoPath namespace", {
  skip_lassocox_deps()
  expect_true(is.function(lassocox))
})

test_that("lassocox runs with breast cancer dataset (standard scenario)", {
  skip_lassocox_deps()
  data <- load_breast_cancer()

  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "survival_months",
      outcome = "death",
      outcomeLevel = "Dead",
      explanatory = c("age", "tumor_size_cm", "grade", "stage",
                       "lymph_nodes_positive", "ki67_percent",
                       "er_status", "her2_status", "lvi"),
      censorLevel = "Alive"
    )
  })
})

test_that("lassocox runs with small cohort dataset (minimal viable)", {
  skip_lassocox_deps()
  data <- load_small_cohort()

  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time_months",
      outcome = "event_occurred",
      outcomeLevel = "Yes",
      explanatory = c("age", "gender", "biomarker_a", "biomarker_b",
                       "biomarker_c", "treatment_group", "severity_score"),
      censorLevel = "No"
    )
  })
})

test_that("lassocox produces expected output items", {
  skip_lassocox_deps()
  data <- load_breast_cancer()

  result <- lassocox(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = c("age", "tumor_size_cm", "grade",
                     "lymph_nodes_positive", "ki67_percent"),
    cv_plot = FALSE,
    coef_plot = FALSE,
    survival_plot = FALSE,
    censorLevel = "Alive"
  )

  # Should have results object

  expect_true(result$modelSummary$rowCount > 0)

  # Model summary table should exist
  expect_true(!is.null(result$modelSummary))

  # Coefficients table should exist
  expect_true(!is.null(result$coefficients))

  # Performance table should exist
  expect_true(!is.null(result$performance))
})

test_that("lassocox with lambda.min vs lambda.1se both produce usable model summaries", {
  skip_lassocox_deps()
  data <- load_breast_cancer()

  explanatory_vars <- c("age", "tumor_size_cm", "grade",
                         "lymph_nodes_positive", "ki67_percent",
                         "er_status", "her2_status")

  result_min <- do.call(lassocox, list(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = explanatory_vars,
    lambda = "lambda.min",
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE,
    censorLevel = "Alive"
  ))

  result_1se <- do.call(lassocox, list(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = explanatory_vars,
    lambda = "lambda.1se",
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE,
    censorLevel = "Alive"
  ))

  # Both should complete without error
  expect_true(result_min$modelSummary$rowCount > 0)
  expect_true(result_1se$modelSummary$rowCount > 0)
})
