# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: plscox
# ═══════════════════════════════════════════════════════════
#
# Tests core plscox execution with metabolomics and small datasets.
# Validates R6 class creation, result structure, and basic output.

library(testthat)

test_that("plscox runs with metabolomics dataset (n=120, p=80)", {
  data(plscox_metabolomics, package = "ClinicoPath")

  metab_cols <- grep("^METAB_", names(plscox_metabolomics), value = TRUE)

  result <- plscox(
    data = plscox_metabolomics,
    time = "survival_months",
    status = "death",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = metab_cols,
    pls_components = 3,
    cross_validation = "k5",
    component_selection = "cv_loglik",
    scaling_method = "standardize",
    bootstrap_validation = FALSE,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = TRUE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
  expect_true(!is.null(result$results))
})

test_that("plscox runs with small dataset (n=50, p=25)", {
  data(plscox_small, package = "ClinicoPath")

  marker_cols <- grep("^MARKER_", names(plscox_small), value = TRUE)

  result <- plscox(
    data = plscox_small,
    time = "time_months",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = marker_cols,
    pls_components = 2,
    cross_validation = "k5",
    component_selection = "cv_loglik",
    scaling_method = "standardize",
    bootstrap_validation = FALSE,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = TRUE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox shows welcome message when no variables selected", {
  data(plscox_small, package = "ClinicoPath")

  # With no predictors specified, should show instructions
  result <- plscox(
    data = plscox_small,
    time = "time_months",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = character(0),
    pls_components = 5,
    cross_validation = "k10",
    component_selection = "cv_loglik",
    scaling_method = "standardize",
    bootstrap_validation = FALSE,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = TRUE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox with LOO cross-validation completes", {
  data(plscox_small, package = "ClinicoPath")

  marker_cols <- grep("^MARKER_", names(plscox_small), value = TRUE)

  result <- plscox(
    data = plscox_small,
    time = "time_months",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = marker_cols,
    pls_components = 3,
    cross_validation = "loo",
    component_selection = "cv_loglik",
    scaling_method = "standardize",
    bootstrap_validation = FALSE,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = FALSE,
    prediction_accuracy = FALSE,
    confidence_intervals = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})
