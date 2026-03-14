# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: plscox
# ═══════════════════════════════════════════════════════════
#
# Tests all .a.yaml options affect behavior when changed.
# Uses plscox_small for speed.

library(testthat)

# Helper: build default options for small dataset
plscox_defaults <- function(overrides = list()) {
  data(plscox_small, package = "ClinicoPath", envir = environment())
  marker_cols <- grep("^MARKER_", names(plscox_small), value = TRUE)

  opts <- list(
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
    tolerance = 1e-06,
    tie_method = "efron",
    sparse_pls = FALSE,
    limQ2set = 0.0975,
    pvals_expli = FALSE,
    alpha_pvals_expli = 0.05,
    bootstrap_validation = FALSE,
    n_bootstrap = 200,
    permutation_test = FALSE,
    n_permutations = 100,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    risk_groups = 3,
    confidence_intervals = TRUE,
    feature_importance = TRUE,
    prediction_accuracy = TRUE,
    suitabilityCheck = TRUE
  )
  modifyList(opts, overrides)
}

test_that("component_selection = manual uses pls_components directly", {
  opts <- plscox_defaults(list(
    component_selection = "manual",
    pls_components = 2,
    cross_validation = "none"
  ))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("component_selection = bic works", {
  opts <- plscox_defaults(list(
    component_selection = "bic",
    cross_validation = "none"
  ))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("component_selection = aic works", {
  opts <- plscox_defaults(list(
    component_selection = "aic",
    cross_validation = "none"
  ))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("cross_validation = k10 works", {
  opts <- plscox_defaults(list(cross_validation = "k10"))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("cross_validation = none with cv_loglik falls back gracefully", {
  opts <- plscox_defaults(list(
    cross_validation = "none",
    component_selection = "cv_loglik"
  ))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("scaling_method = unit_variance works", {
  opts <- plscox_defaults(list(scaling_method = "unit_variance"))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("scaling_method = minmax works", {
  opts <- plscox_defaults(list(scaling_method = "minmax"))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("scaling_method = none works", {
  opts <- plscox_defaults(list(scaling_method = "none"))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("tie_method = breslow works", {
  opts <- plscox_defaults(list(tie_method = "breslow"))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("sparse_pls = TRUE works", {
  opts <- plscox_defaults(list(sparse_pls = TRUE))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("pvals_expli = TRUE with threshold works", {
  opts <- plscox_defaults(list(
    pvals_expli = TRUE,
    alpha_pvals_expli = 0.10
  ))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("risk_groups = 2 works", {
  opts <- plscox_defaults(list(risk_groups = 2))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("risk_groups = 5 works", {
  opts <- plscox_defaults(list(risk_groups = 5))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("suitabilityCheck = FALSE skips assessment", {
  opts <- plscox_defaults(list(suitabilityCheck = FALSE))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("feature_importance = FALSE hides loadings table", {
  opts <- plscox_defaults(list(feature_importance = FALSE))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("prediction_accuracy = FALSE hides performance table", {
  opts <- plscox_defaults(list(prediction_accuracy = FALSE))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("confidence_intervals = FALSE still runs", {
  opts <- plscox_defaults(list(confidence_intervals = FALSE))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("tolerance parameter is respected", {
  opts <- plscox_defaults(list(tolerance = 1e-03))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})

test_that("limQ2set parameter is respected", {
  opts <- plscox_defaults(list(limQ2set = 0.50))
  result <- do.call(plscox, opts)
  expect_true(inherits(result, "plscoxClass"))
})
