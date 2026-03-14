# ═══════════════════════════════════════════════════════════
# plscox Tests — Consolidated Entry Point
# ═══════════════════════════════════════════════════════════
#
# Comprehensive tests are split into:
#   test-plscox-basic.R        — Core execution
#   test-plscox-arguments.R    — All option combinations
#   test-plscox-edge-cases.R   — Boundary conditions
#   test-plscox-integration.R  — Bootstrap, permutation, plots
#
# This file provides a quick smoke test.

library(testthat)

test_that("plscox smoke test with continuous predictors", {
  set.seed(42)
  n <- 50
  df <- data.frame(
    time = runif(n, 1, 60),
    status = factor(sample(c("Event", "Censored"), n, replace = TRUE,
                           prob = c(0.55, 0.45))),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
    x4 = rnorm(n), x5 = rnorm(n)
  )

  result <- plscox(
    data = df,
    time = "time",
    status = "status",
    outcomeLevel = "Event",
    censorLevel = "Censored",
    predictors = c("x1", "x2", "x3", "x4", "x5"),
    pls_components = 2,
    cross_validation = "none",
    component_selection = "manual",
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
