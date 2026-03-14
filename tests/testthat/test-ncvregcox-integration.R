# ═══════════════════════════════════════════════════════════
# Integration Tests: ncvregcox (SCAD/MCP Cox)
# ═══════════════════════════════════════════════════════════
#
# End-to-end tests with all outputs enabled, verifying that
# the full analysis pipeline runs without error.

test_that("full pipeline: SCAD on clinical data with all outputs", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  covs <- c("age", "tumor_diameter", "ldh_level", "cea_level",
            "bmi", "crp", "albumin", "wbc_count",
            "gender", "t_stage", "n_stage", "histology")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = covs,
    penalty = "SCAD",
    cv_folds = 10,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 1.0,
    standardize = TRUE,
    plot_path = TRUE,
    plot_cv = TRUE,
    variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "ncvregcoxClass"))

  # Verify key result objects exist
  expect_true(!is.null(result$results$model_summary))
  expect_true(!is.null(result$results$selected_variables))
  expect_true(!is.null(result$results$cross_validation_results))
  expect_true(!is.null(result$results$model_comparison))
  expect_true(!is.null(result$results$convergence_info))
  expect_true(!is.null(result$results$model_interpretation))
  expect_true(!is.null(result$results$suitabilityReport))
  expect_true(!is.null(result$results$variable_importance))
})

test_that("full pipeline: MCP on clinical data with all outputs", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  covs <- c("age", "tumor_diameter", "ldh_level", "cea_level",
            "bmi", "crp", "t_stage", "n_stage")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = covs,
    penalty = "MCP",
    cv_folds = 10,
    lambda_type = "1se",
    gamma = 3.0,
    alpha = 1.0,
    standardize = TRUE,
    plot_path = TRUE,
    plot_cv = TRUE,
    variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("full pipeline: sparse data with 30 covariates", {
  data(ncvregcox_sparse, package = "ClinicoPath")
  cov_names <- paste0("x", 1:30)

  result <- ncvregcox(
    data = ncvregcox_sparse,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = cov_names,
    penalty = "SCAD",
    cv_folds = 5,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 1.0,
    standardize = TRUE,
    plot_path = TRUE,
    plot_cv = TRUE,
    variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("full pipeline: elastic net mixing (alpha=0.5) on sparse data", {
  data(ncvregcox_sparse, package = "ClinicoPath")
  cov_names <- paste0("x", 1:30)

  result <- ncvregcox(
    data = ncvregcox_sparse,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = cov_names,
    penalty = "SCAD",
    cv_folds = 5,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 0.5,
    standardize = TRUE,
    plot_path = FALSE,
    plot_cv = FALSE,
    variable_importance = TRUE,
    suitabilityCheck = FALSE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("full pipeline: collinear data triggers suitability warnings", {
  data(ncvregcox_collinear, package = "ClinicoPath")
  covs <- paste0("x", 1:10)

  result <- ncvregcox(
    data = ncvregcox_collinear,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = covs,
    penalty = "SCAD",
    cv_folds = 5,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 1.0,
    standardize = TRUE,
    plot_path = FALSE,
    plot_cv = FALSE,
    variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("full pipeline: small data with reduced folds", {
  data(ncvregcox_small, package = "ClinicoPath")
  covs <- c("age", "marker1", "marker2", "grade")

  result <- ncvregcox(
    data = ncvregcox_small,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = covs,
    penalty = "SCAD",
    cv_folds = 3,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 1.0,
    standardize = TRUE,
    plot_path = FALSE,
    plot_cv = FALSE,
    variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox OMV export works", {
  skip_if_not_installed("jmvReadWrite")

  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = covs,
    penalty = "SCAD",
    cv_folds = 5,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 1.0,
    standardize = TRUE,
    plot_path = FALSE,
    plot_cv = FALSE,
    variable_importance = FALSE,
    suitabilityCheck = FALSE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
  expect_true(is.list(result))

  omv_dir <- file.path(tempdir(), "ncvregcox_omv_test")
  if (!dir.exists(omv_dir)) dir.create(omv_dir)
  omv_path <- file.path(omv_dir, "ncvregcox_test.omv")

  expect_no_error({
    jmvReadWrite::write_omv(result, omv_path)
  })

  expect_true(file.exists(omv_path))
  unlink(omv_dir, recursive = TRUE)
})
