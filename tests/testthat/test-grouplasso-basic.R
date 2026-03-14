# ===============================================================
# Basic Functionality Tests: grouplasso (Group LASSO Cox)
# ===============================================================

library(testthat)

test_that("grouplasso class can be instantiated", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  expect_no_error({
    options <- grouplassoOptions$new(
      time = "time",
      event = "status",
      outcomeLevel = "Dead",
      censorLevel = "Alive",
      predictors = c("age", "bmi", "tumor_size", "ki67", "er", "albumin"),
      suitabilityCheck = FALSE,
      plot_regularization_path = FALSE,
      plot_cv_curve = FALSE,
      plot_group_importance = FALSE,
      cv_folds = 5,
      n_lambda = 20,
      random_seed = 42
    )
  })
})

test_that("grouplasso runs with continuous predictors", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time",
    event = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("age", "bmi", "tumor_size", "ki67", "er", "albumin", "ldh"),
    suitabilityCheck = FALSE,
    show_path_summary = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5,
    n_lambda = 20,
    random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)

  expect_no_error(results$run())
})

test_that("grouplasso runs with mixed continuous and factor predictors", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time",
    event = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("age", "tumor_size", "grade", "her2", "ki67"),
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5,
    n_lambda = 20,
    random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)

  expect_no_error(results$run())
})

test_that("grouplasso returns without error when inputs are missing", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  # No predictors selected
  options <- grouplassoOptions$new(
    time = "time",
    event = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive"
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)

  # Should return silently (no error) without running
  expect_no_error(results$run())
})
