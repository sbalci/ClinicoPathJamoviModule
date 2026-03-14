# ===============================================================
# Edge Cases and Error Handling Tests: grouplasso
# ===============================================================

library(testthat)

test_that("grouplasso handles small dataset", {
  skip_if_not_installed("grpreg")

  data(grouplasso_small, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "hemoglobin"),
    suitabilityCheck = TRUE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_small)
  expect_no_error(results$run())
})

test_that("grouplasso handles all-censored data gracefully", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  # Make all observations censored
  all_censored <- grouplasso_biomarker
  all_censored$status <- factor("Alive", levels = c("Alive", "Dead"))

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "ki67"),
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = all_censored)

  # Should not crash; should produce error notice and return
  expect_no_error(results$run())
})

test_that("grouplasso handles single predictor", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age"),
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_biomarker)
  expect_no_error(results$run())
})

test_that("grouplasso handles high-dimensional genomic data", {
  skip_if_not_installed("grpreg")

  data(grouplasso_genomic, package = "ClinicoPath")

  # All 30 gene features
  gene_vars <- setdiff(names(grouplasso_genomic), c("time", "status"))

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Progressed", censorLevel = "Stable",
    predictors = gene_vars,
    suitabilityCheck = TRUE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 20, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = grouplasso_genomic)
  expect_no_error(results$run())
})

test_that("grouplasso handles negative time values", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  bad_data <- grouplasso_biomarker
  bad_data$time[1:3] <- -1

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size"),
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = bad_data)

  # Should produce error notice, not crash
  expect_no_error(results$run())
})

test_that("grouplasso handles missing data", {
  skip_if_not_installed("grpreg")

  data(grouplasso_biomarker, package = "ClinicoPath")

  # Add missing values
  na_data <- grouplasso_biomarker
  na_data$age[1:10] <- NA
  na_data$ki67[5:15] <- NA

  options <- grouplassoOptions$new(
    time = "time", event = "status",
    outcomeLevel = "Dead", censorLevel = "Alive",
    predictors = c("age", "tumor_size", "ki67"),
    suitabilityCheck = FALSE,
    plot_regularization_path = FALSE,
    plot_cv_curve = FALSE,
    plot_group_importance = FALSE,
    cv_folds = 5, n_lambda = 15, random_seed = 42
  )

  results <- grouplassoClass$new(options = options, data = na_data)

  # Should run after dropping NA rows
  expect_no_error(results$run())
})
