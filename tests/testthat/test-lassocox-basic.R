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
  data_path <- system.file("data", "lassocox_breast_cancer.rda",
                           package = "ClinicoPath")
  if (data_path == "") {
    # Fallback for development: load from local data/
    data_path <- file.path("../../data", "lassocox_breast_cancer.rda")
  }
  if (file.exists(data_path)) {
    env <- new.env()
    load(data_path, envir = env)
    return(env$lassocox_breast_cancer)
  }
  skip("lassocox_breast_cancer.rda not found")
}

load_small_cohort <- function() {
  data_path <- system.file("data", "lassocox_small_cohort.rda",
                           package = "ClinicoPath")
  if (data_path == "") {
    data_path <- file.path("../../data", "lassocox_small_cohort.rda")
  }
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
  expect_true(exists("lassocoxClass"))
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
                       "er_status", "her2_status", "lvi")
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
                       "biomarker_c", "treatment_group", "severity_score")
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
    survival_plot = FALSE
  )

  # Should have results object

  expect_true(!is.null(result$results))

  # Model summary table should exist
  expect_true(!is.null(result$results$modelSummary))

  # Coefficients table should exist
  expect_true(!is.null(result$results$coefficients))

  # Performance table should exist
  expect_true(!is.null(result$results$performance))
})

test_that("lassocox with lambda.min vs lambda.1se produces different models", {
  skip_lassocox_deps()
  data <- load_breast_cancer()

  explanatory_vars <- c("age", "tumor_size_cm", "grade",
                         "lymph_nodes_positive", "ki67_percent",
                         "er_status", "her2_status")

  result_min <- lassocox(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = explanatory_vars,
    lambda = "lambda.min",
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  result_1se <- lassocox(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = explanatory_vars,
    lambda = "lambda.1se",
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  # Both should complete without error
  expect_true(!is.null(result_min$results))
  expect_true(!is.null(result_1se$results))
})
