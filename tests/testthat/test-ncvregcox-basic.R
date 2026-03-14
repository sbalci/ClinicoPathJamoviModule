# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: ncvregcox (SCAD/MCP Cox)
# ═══════════════════════════════════════════════════════════

test_that("ncvregcox runs with SCAD penalty on clinical data", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("age", "tumor_diameter", "ldh_level", "bmi"),
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
})

test_that("ncvregcox runs with MCP penalty", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("age", "tumor_diameter", "ldh_level"),
    penalty = "MCP",
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
})

test_that("ncvregcox runs on high-dimensional sparse data", {
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
    plot_path = FALSE,
    plot_cv = FALSE,
    variable_importance = FALSE,
    suitabilityCheck = FALSE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles factor covariates", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("age", "t_stage", "n_stage"),
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
})

test_that("ncvregcox produces model_summary table", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("age", "tumor_diameter", "ldh_level"),
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

  # model_summary should exist and have 1 row
  expect_true(!is.null(result$results$model_summary))
})

test_that("ncvregcox returns silently when required vars missing", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  # No covariates -> should return silently (no error)
  result <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = character(0),
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
})
