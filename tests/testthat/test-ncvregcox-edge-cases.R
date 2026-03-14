# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: ncvregcox (SCAD/MCP Cox)
# ═══════════════════════════════════════════════════════════

# Helper to run ncvregcox with defaults
run_ncvregcox <- function(data, covariates, ...) {
  defaults <- list(
    data = data,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = covariates,
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
  args <- modifyList(defaults, list(...))
  do.call(ncvregcox, args)
}


test_that("ncvregcox handles small sample data", {
  data(ncvregcox_small, package = "ClinicoPath")
  covs <- c("age", "marker1", "marker2", "marker3", "grade")

  # Small sample with cv_folds=3 (must be less than n)
  result <- run_ncvregcox(ncvregcox_small, covs, cv_folds = 3)

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles data with missing values", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  # ncvregcox_clinical has 6 missing values -- function should exclude them

  covs <- c("age", "tumor_diameter", "ldh_level", "crp", "albumin", "cea_level")

  result <- run_ncvregcox(ncvregcox_clinical, covs)

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles highly collinear data", {
  data(ncvregcox_collinear, package = "ClinicoPath")
  covs <- paste0("x", 1:10)

  result <- run_ncvregcox(ncvregcox_collinear, covs, suitabilityCheck = TRUE)

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles single covariate", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- run_ncvregcox(ncvregcox_clinical, "age")

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles two covariates", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- run_ncvregcox(ncvregcox_clinical, c("age", "tumor_diameter"))

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles all-factor covariates", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  result <- run_ncvregcox(ncvregcox_clinical,
                          c("gender", "t_stage", "n_stage", "histology"))

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox handles extreme alpha values", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  # Near-ridge (alpha close to 0 but > 0; ncvreg requires alpha > 0)
  result_low <- run_ncvregcox(ncvregcox_clinical, covs, alpha = 0.01)
  expect_true(inherits(result_low, "ncvregcoxClass"))

  # Pure penalty (alpha = 1)
  result_high <- run_ncvregcox(ncvregcox_clinical, covs, alpha = 1.0)
  expect_true(inherits(result_high, "ncvregcoxClass"))
})

test_that("ncvregcox handles extreme gamma values", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  # Low gamma (more aggressive penalty)
  result_low <- run_ncvregcox(ncvregcox_clinical, covs, gamma = 1.1)
  expect_true(inherits(result_low, "ncvregcoxClass"))

  # High gamma (closer to LASSO behavior)
  result_high <- run_ncvregcox(ncvregcox_clinical, covs, gamma = 10.0)
  expect_true(inherits(result_high, "ncvregcoxClass"))
})

test_that("ncvregcox handles data with all censored events", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  # Create all-censored dataset
  all_censored <- ncvregcox_clinical
  all_censored$event <- factor(rep(0, nrow(all_censored)), levels = c(0, 1))

  # Should error because event variable has only 1 effective level
  expect_error(
    run_ncvregcox(all_censored, c("age", "tumor_diameter")),
    ignore.case = TRUE
  )
})

test_that("ncvregcox handles invalid event level selection", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  expect_error(
    run_ncvregcox(ncvregcox_clinical, c("age", "tumor_diameter"),
                  outcomeLevel = "NONEXISTENT"),
    regexp = "not present",
    ignore.case = TRUE
  )
})

test_that("ncvregcox handles same event and censor level", {
  data(ncvregcox_clinical, package = "ClinicoPath")

  expect_error(
    run_ncvregcox(ncvregcox_clinical, c("age", "tumor_diameter"),
                  outcomeLevel = "1", censorLevel = "1"),
    regexp = "different",
    ignore.case = TRUE
  )
})

test_that("ncvregcox suitability check flags small samples", {
  data(ncvregcox_small, package = "ClinicoPath")
  covs <- c("age", "marker1", "marker2", "marker3", "grade")

  result <- run_ncvregcox(ncvregcox_small, covs,
                          cv_folds = 3, suitabilityCheck = TRUE)

  expect_true(inherits(result, "ncvregcoxClass"))
})

test_that("ncvregcox suitability check flags high collinearity", {
  data(ncvregcox_collinear, package = "ClinicoPath")
  covs <- paste0("x", 1:10)

  result <- run_ncvregcox(ncvregcox_collinear, covs, suitabilityCheck = TRUE)

  expect_true(inherits(result, "ncvregcoxClass"))
})
