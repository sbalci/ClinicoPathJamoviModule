# ═══════════════════════════════════════════════════════════
# Integration Tests: plscox
# ═══════════════════════════════════════════════════════════
#
# Tests bootstrap validation, permutation tests, and plot generation.
# These are slower tests that exercise the full analysis pipeline.

library(testthat)

test_that("plscox bootstrap validation produces results", {
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
    cross_validation = "none",
    component_selection = "manual",
    scaling_method = "standardize",
    bootstrap_validation = TRUE,
    n_bootstrap = 50,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = FALSE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox permutation test produces results", {
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
    cross_validation = "none",
    component_selection = "manual",
    scaling_method = "standardize",
    bootstrap_validation = FALSE,
    permutation_test = TRUE,
    n_permutations = 50,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = FALSE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox with all plots enabled completes", {
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
    plot_components = TRUE,
    plot_loadings = TRUE,
    plot_scores = TRUE,
    plot_validation = TRUE,
    plot_survival = TRUE,
    risk_groups = 3,
    feature_importance = TRUE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox full pipeline with metabolomics data", {
  data(plscox_metabolomics, package = "ClinicoPath")

  # Use a subset of metabolites for speed
  metab_cols <- grep("^METAB_", names(plscox_metabolomics), value = TRUE)[1:30]

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
    sparse_pls = FALSE,
    risk_groups = 3,
    bootstrap_validation = FALSE,
    permutation_test = FALSE,
    plot_components = FALSE,
    plot_loadings = FALSE,
    plot_scores = FALSE,
    plot_validation = FALSE,
    plot_survival = FALSE,
    feature_importance = TRUE,
    prediction_accuracy = TRUE,
    confidence_intervals = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "plscoxClass"))
})

test_that("plscox with sparse PLS on genomic data", {
  data(plscox_genomic, package = "ClinicoPath")

  gene_cols <- grep("^GENE_", names(plscox_genomic), value = TRUE)[1:50]

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
    sparse_pls = TRUE,
    limQ2set = 0.0975,
    risk_groups = 2,
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

test_that("plscox with all advanced PLS options", {
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
    cross_validation = "k5",
    component_selection = "cv_cindex",
    scaling_method = "unit_variance",
    tie_method = "breslow",
    tolerance = 1e-04,
    sparse_pls = FALSE,
    limQ2set = 0.20,
    pvals_expli = TRUE,
    alpha_pvals_expli = 0.10,
    risk_groups = 4,
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
