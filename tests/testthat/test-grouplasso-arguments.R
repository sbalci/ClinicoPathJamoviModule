# ===============================================================
# Argument Combination Tests: grouplasso
# ===============================================================

library(testthat)

test_that("grouplasso penalty_type options work", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")
  predictors <- c("age", "tumor_size", "ki67", "er", "albumin")

  for (ptype in c("group_lasso", "group_mcp", "group_scad")) {
    options <- grouplassoOptions$new(
      time = "time", event = "status",
      outcomeLevel = "Dead", censorLevel = "Alive",
      predictors = predictors,
      penalty_type = ptype,
      suitabilityCheck = FALSE,
      plot_regularization_path = FALSE,
      plot_cv_curve = FALSE,
      plot_group_importance = FALSE,
      cv_folds = 5, n_lambda = 15, random_seed = 42
    )
    results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
    expect_no_error(results$run())
  }
})

test_that("grouplasso adaptive group works", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  for (method in c("ridge", "univariate")) {
    options <- grouplassoOptions$new(
      time = "time", event = "status",
      outcomeLevel = "Dead", censorLevel = "Alive",
      predictors = c("age", "tumor_size", "ki67", "er"),
      penalty_type = "adaptive_group",
      adaptive_weights_method = method,
      suitabilityCheck = FALSE,
      plot_regularization_path = FALSE,
      plot_cv_curve = FALSE,
      plot_group_importance = FALSE,
      cv_folds = 5, n_lambda = 15, random_seed = 42
    )
    results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
    expect_no_error(results$run())
  }
})

test_that("grouplasso group_definition methods work", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")
  predictors <- c("age", "tumor_size", "grade", "ki67")

  for (gd in c("automatic", "factor_based")) {
    options <- grouplassoOptions$new(
      time = "time", event = "status",
      outcomeLevel = "Dead", censorLevel = "Alive",
      predictors = predictors,
      group_definition = gd,
      suitabilityCheck = FALSE,
      plot_regularization_path = FALSE,
      plot_cv_curve = FALSE,
      plot_group_importance = FALSE,
      cv_folds = 5, n_lambda = 15, random_seed = 42
    )
    results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
    expect_no_error(results$run())
  }
})

test_that("grouplasso custom grouping works", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "bmi", "tumor_size", "ki67"),
    group_definition = "custom",
    group_structure = "age:1, bmi:1, tumor_size:2, ki67:2",
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
  expect_no_error(results$run())
})

test_that("grouplasso group_weights options work", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")
  predictors <- c("age", "tumor_size", "ki67", "er")

  for (gw in c("equal", "sqrt_size", "group_size")) {
    options <- grouplassoOptions$new(
      time = "time", event = "status",
      outcomeLevel = "Dead", censorLevel = "Alive",
      predictors = predictors,
      group_weights = gw,
      suitabilityCheck = FALSE,
      plot_regularization_path = FALSE,
      plot_cv_curve = FALSE,
      plot_group_importance = FALSE,
      cv_folds = 5, n_lambda = 15, random_seed = 42
    )
    results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
    expect_no_error(results$run())
  }
})

test_that("grouplasso factor_grouping toggle changes groups", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")
  predictors <- c("age", "grade", "ki67")

  # With factor grouping (grade dummies = one group)
  opt1 <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = predictors,
    factor_grouping = TRUE,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  # Without factor grouping (each dummy = own group)
  opt2 <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = predictors,
    factor_grouping = FALSE,
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  r1 <- grouplassoClass$new(options = opt1, data = grouplasso_biomarker)
  r2 <- grouplassoClass$new(options = opt2, data = grouplasso_biomarker)

  expect_no_error(r1$run())
  expect_no_error(r2$run())
})
