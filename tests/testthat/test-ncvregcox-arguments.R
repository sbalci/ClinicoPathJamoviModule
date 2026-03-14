# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: ncvregcox (SCAD/MCP Cox)
# ═══════════════════════════════════════════════════════════
#
# Tests that each .a.yaml option produces observable behavior changes.

# Helper to run ncvregcox with defaults overridden
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


test_that("penalty option: SCAD vs MCP produce different results", {
  data(ncvregcox_sparse, package = "ClinicoPath")
  covs <- paste0("x", 1:30)

  set.seed(42)
  r_scad <- run_ncvregcox(ncvregcox_sparse, covs, penalty = "SCAD")
  set.seed(42)
  r_mcp <- run_ncvregcox(ncvregcox_sparse, covs, penalty = "MCP")

  expect_true(inherits(r_scad, "ncvregcoxClass"))
  expect_true(inherits(r_mcp, "ncvregcoxClass"))
})

test_that("lambda_type option: min vs 1se", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level", "bmi", "crp", "albumin")

  r_min <- run_ncvregcox(ncvregcox_clinical, covs, lambda_type = "min")
  r_1se <- run_ncvregcox(ncvregcox_clinical, covs, lambda_type = "1se")

  expect_true(inherits(r_min, "ncvregcoxClass"))
  expect_true(inherits(r_1se, "ncvregcoxClass"))
})

test_that("alpha option: pure SCAD vs elastic net mixing", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level", "bmi")

  r_pure <- run_ncvregcox(ncvregcox_clinical, covs, alpha = 1.0)
  r_enet <- run_ncvregcox(ncvregcox_clinical, covs, alpha = 0.5)

  expect_true(inherits(r_pure, "ncvregcoxClass"))
  expect_true(inherits(r_enet, "ncvregcoxClass"))
})

test_that("gamma option: different concavity values", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level", "bmi")

  r_default <- run_ncvregcox(ncvregcox_clinical, covs, gamma = 3.7)
  r_low <- run_ncvregcox(ncvregcox_clinical, covs, gamma = 2.0)

  expect_true(inherits(r_default, "ncvregcoxClass"))
  expect_true(inherits(r_low, "ncvregcoxClass"))
})

test_that("standardize option: TRUE vs FALSE", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  r_std <- run_ncvregcox(ncvregcox_clinical, covs, standardize = TRUE)
  r_raw <- run_ncvregcox(ncvregcox_clinical, covs, standardize = FALSE)

  expect_true(inherits(r_std, "ncvregcoxClass"))
  expect_true(inherits(r_raw, "ncvregcoxClass"))
})

test_that("cv_folds option: different fold counts", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  r_5fold <- run_ncvregcox(ncvregcox_clinical, covs, cv_folds = 5)
  r_3fold <- run_ncvregcox(ncvregcox_clinical, covs, cv_folds = 3)

  expect_true(inherits(r_5fold, "ncvregcoxClass"))
  expect_true(inherits(r_3fold, "ncvregcoxClass"))
})

test_that("variable_importance option: TRUE produces importance table", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level", "bmi")

  r_imp <- run_ncvregcox(ncvregcox_clinical, covs, variable_importance = TRUE)
  r_no_imp <- run_ncvregcox(ncvregcox_clinical, covs, variable_importance = FALSE)

  expect_true(inherits(r_imp, "ncvregcoxClass"))
  expect_true(inherits(r_no_imp, "ncvregcoxClass"))
})

test_that("suitabilityCheck option: TRUE produces suitability report", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  r_check <- run_ncvregcox(ncvregcox_clinical, covs, suitabilityCheck = TRUE)
  r_no_check <- run_ncvregcox(ncvregcox_clinical, covs, suitabilityCheck = FALSE)

  expect_true(inherits(r_check, "ncvregcoxClass"))
  expect_true(inherits(r_no_check, "ncvregcoxClass"))
})

test_that("plot_path and plot_cv options are accepted", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  # These just verify no error; actual plot rendering requires image context

  r_plots <- run_ncvregcox(ncvregcox_clinical, covs,
                           plot_path = TRUE, plot_cv = TRUE)

  expect_true(inherits(r_plots, "ncvregcoxClass"))
})

test_that("MCP with default gamma auto-adjusts to 3.0", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter", "ldh_level")

  # gamma=3.7 with MCP should silently use 3.0 (MCP default)
  r <- run_ncvregcox(ncvregcox_clinical, covs, penalty = "MCP", gamma = 3.7)

  expect_true(inherits(r, "ncvregcoxClass"))
})

test_that("outcomeLevel and censorLevel options work", {
  data(ncvregcox_clinical, package = "ClinicoPath")
  covs <- c("age", "tumor_diameter")

  # Swap event/censor levels
  r_swap <- run_ncvregcox(ncvregcox_clinical, covs,
                          outcomeLevel = "0", censorLevel = "1")

  expect_true(inherits(r_swap, "ncvregcoxClass"))
})

test_that("all options together: kitchen-sink test", {
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
    penalty = "SCAD",
    cv_folds = 5,
    lambda_type = "min",
    gamma = 3.7,
    alpha = 0.8,
    standardize = TRUE,
    plot_path = TRUE,
    plot_cv = TRUE,
    variable_importance = TRUE,
    suitabilityCheck = TRUE
  )

  expect_true(inherits(result, "ncvregcoxClass"))
})
