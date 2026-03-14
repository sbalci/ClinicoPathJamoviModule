# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: plscox
# ═══════════════════════════════════════════════════════════
#
# Tests boundary conditions, error paths, and unusual data.

library(testthat)

test_that("plscox handles numeric 0/1 status (non-factor)", {
  # plscox_genomic uses numeric os_event (0/1)
  data(plscox_genomic, package = "ClinicoPath")

  # Use first 30 genes to keep it fast
  gene_cols <- grep("^GENE_", names(plscox_genomic), value = TRUE)[1:30]

  result <- plscox(
    data = plscox_genomic,
    time = "os_time",
    status = "os_event",
    outcomeLevel = "1",
    censorLevel = "0",
    predictors = gene_cols,
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

test_that("plscox handles p >> n genomic data", {
  data(plscox_genomic, package = "ClinicoPath")

  # Use all 200 genes (p=200, n=60)
  gene_cols <- grep("^GENE_", names(plscox_genomic), value = TRUE)

  result <- plscox(
    data = plscox_genomic,
    time = "os_time",
    status = "os_event",
    outcomeLevel = "1",
    censorLevel = "0",
    predictors = gene_cols,
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
})

test_that("plscox handles very few events gracefully", {
  set.seed(99)
  n <- 40
  # Only 5 events
  df <- data.frame(
    time = runif(n, 1, 50),
    status = factor(c(rep("Dead", 5), rep("Alive", n - 5))),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
    x4 = rnorm(n), x5 = rnorm(n)
  )

  # Should run (may produce warnings/notes about low events)
  result <- plscox(
    data = df,
    time = "time",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
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
    confidence_intervals = FALSE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox handles single predictor", {
  set.seed(42)
  n <- 60
  df <- data.frame(
    time = runif(n, 1, 50),
    status = factor(sample(c("Dead", "Alive"), n, replace = TRUE)),
    biomarker = rnorm(n)
  )

  result <- plscox(
    data = df,
    time = "time",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = "biomarker",
    pls_components = 1,
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

test_that("plscox handles data with missing values in predictors", {
  data(plscox_genomic, package = "ClinicoPath")

  # plscox_genomic has ~3% missing in gene columns
  gene_cols <- grep("^GENE_", names(plscox_genomic), value = TRUE)[1:20]

  # Should handle NAs via complete-case or imputation
  result <- plscox(
    data = plscox_genomic,
    time = "os_time",
    status = "os_event",
    outcomeLevel = "1",
    censorLevel = "0",
    predictors = gene_cols,
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

test_that("plscox handles all censored data gracefully", {
  set.seed(42)
  n <- 40
  df <- data.frame(
    time = runif(n, 1, 50),
    status = factor(rep("Alive", n), levels = c("Alive", "Dead")),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n)
  )

  # Should produce an error message, not crash
  result <- plscox(
    data = df,
    time = "time",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("x1", "x2", "x3"),
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
    confidence_intervals = FALSE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox handles constant predictor without crash", {
  set.seed(42)
  n <- 50
  df <- data.frame(
    time = runif(n, 1, 50),
    status = factor(sample(c("Dead", "Alive"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x_const = rep(5, n)  # zero variance
  )

  result <- plscox(
    data = df,
    time = "time",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("x1", "x2", "x_const"),
    pls_components = 1,
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
    confidence_intervals = FALSE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox handles risk_groups > unique risk scores", {
  set.seed(42)
  n <- 20
  df <- data.frame(
    time = runif(n, 1, 50),
    status = factor(sample(c("Dead", "Alive"), n, replace = TRUE)),
    x1 = rnorm(n), x2 = rnorm(n)
  )

  # risk_groups=5 with n=20 may cause quantile collapse
  result <- plscox(
    data = df,
    time = "time",
    status = "status",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("x1", "x2"),
    pls_components = 1,
    cross_validation = "none",
    component_selection = "manual",
    scaling_method = "standardize",
    risk_groups = 5,
    bootstrap_validation = FALSE,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = FALSE,
    prediction_accuracy = FALSE,
    confidence_intervals = FALSE
  )

  expect_true(inherits(result, "plscoxClass"))
})
