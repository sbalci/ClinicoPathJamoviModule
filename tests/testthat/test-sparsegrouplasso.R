# ===============================================================
# Core Tests: sparsegrouplasso (Sparse Group LASSO Cox)
# ===============================================================

library(testthat)

test_that("sparsegrouplasso class can be instantiated", {
  skip_if_not_installed("glmnet")

  data(sparsegrouplasso_lung, package = "ClinicoPath")

  expect_no_error({
    options <- sparsegrouplassoOptions$new(
      time_var = "time",
      event_var = "status",
      outcomeLevel = "Dead",
      censorLevel = "Alive",
      pred_vars = c("age", "smoking_py", "tumor_size", "pdl1"),
      pathway_info = NULL,
      suitabilityCheck = FALSE,
      plot_cv_error = FALSE,
      plot_coefficients = FALSE,
      plot_groups = FALSE,
      plot_sparsity = FALSE,
      plot_stability = FALSE,
      showExplanations = FALSE,
      cv_folds = 5,
      n_lambda = 20,
      seed_value = 42
    )
  })
})

test_that("sparsegrouplasso runs with continuous predictors", {
  skip_if_not_installed("glmnet")

  data(sparsegrouplasso_lung, package = "ClinicoPath")

  options <- sparsegrouplassoOptions$new(
    time_var = "time",
    event_var = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    pred_vars = c("age", "smoking_py", "tumor_size", "pdl1",
                  "crp", "nlr", "albumin", "ldh"),
    pathway_info = NULL,
    suitabilityCheck = FALSE,
    plot_cv_error = FALSE,
    plot_coefficients = FALSE,
    plot_groups = FALSE,
    plot_sparsity = FALSE,
    plot_stability = FALSE,
    showExplanations = FALSE,
    cv_folds = 5,
    n_lambda = 20,
    seed_value = 42
  )

  analysis <- sparsegrouplassoClass$new(options = options, data = sparsegrouplasso_lung)
  expect_no_error(analysis$run())

  # Verify outputs populated
  expect_true(nrow(analysis$results$summary$asDF) > 0)
  expect_true(nrow(analysis$results$coefficients$asDF) > 0)
  expect_true(nrow(analysis$results$groupStructure$asDF) > 0)
  expect_true(nrow(analysis$results$performance$asDF) > 0)
})

test_that("sparsegrouplasso runs with mixed continuous and factor predictors", {
  skip_if_not_installed("glmnet")

  data(sparsegrouplasso_lung, package = "ClinicoPath")

  options <- sparsegrouplassoOptions$new(
    time_var = "time",
    event_var = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    pred_vars = c("age", "smoking_py", "sex", "ecog", "histology"),
    pathway_info = NULL,
    group_definition = "factor_based",
    suitabilityCheck = FALSE,
    plot_cv_error = FALSE,
    plot_coefficients = FALSE,
    plot_groups = FALSE,
    plot_sparsity = FALSE,
    plot_stability = FALSE,
    showExplanations = FALSE,
    cv_folds = 5,
    n_lambda = 20,
    seed_value = 42
  )

  analysis <- sparsegrouplassoClass$new(options = options, data = sparsegrouplasso_lung)
  expect_no_error(analysis$run())

  # Verify factor dummies are grouped together
  g <- analysis$results$groupStructure$asDF
  ecog_group <- g[grep("ecog", g$variables_list), ]
  expect_true(nrow(ecog_group) == 1)  # All ecog dummies in one group
  expect_true(ecog_group$n_variables >= 2)  # Multiple dummy columns
})

test_that("sparsegrouplasso wrapper works with pathway_info = NULL", {
  skip_if_not_installed("glmnet")

  data(sparsegrouplasso_lung, package = "ClinicoPath")

  expect_no_error({
    result <- sparsegrouplasso(
      data = sparsegrouplasso_lung,
      time_var = "time",
      event_var = "status",
      outcomeLevel = "Dead",
      censorLevel = "Alive",
      pred_vars = c("age", "smoking_py", "tumor_size", "pdl1"),
      pathway_info = NULL,
      cv_folds = 5,
      n_lambda = 20,
      plot_cv_error = FALSE,
      plot_coefficients = FALSE,
      plot_groups = FALSE,
      plot_sparsity = FALSE,
      plot_stability = FALSE
    )
  })
})
