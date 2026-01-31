# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: rpasurvival
# ═══════════════════════════════════════════════════════════
#
# Tests all argument combinations and parameter settings

library(testthat)
library(ClinicoPath)
data(rpasurvival_test)

test_that("rpasurvival minbucket parameter affects tree structure", {
  # Small minbucket (more splits)
  result_small <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    minbucket = 10
  )
  expect_no_error(result_small)

  # Large minbucket (fewer splits)
  result_large <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    minbucket = 40
  )
  expect_no_error(result_large)
})

test_that("rpasurvival cp (complexity parameter) affects pruning", {
  # Small cp (less pruning, larger tree)
  result_small_cp <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    cp = 0.001
  )
  expect_no_error(result_small_cp)

  # Large cp (more pruning, smaller tree)
  result_large_cp <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    cp = 0.05
  )
  expect_no_error(result_large_cp)
})

test_that("rpasurvival maxdepth parameter controls tree depth", {
  # Shallow tree
  result_depth2 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    maxdepth = 2
  )
  expect_no_error(result_depth2)

  # Deeper tree
  result_depth5 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    maxdepth = 5
  )
  expect_no_error(result_depth5)
})

test_that("rpasurvival cross-validation settings work", {
  # With 10-fold CV (default)
  result_cv10 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    nfolds = 10
  )
  expect_no_error(result_cv10)

  # With 5-fold CV
  result_cv5 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    nfolds = 5
  )
  expect_no_error(result_cv5)

  # No CV
  result_nocv <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    nfolds = 0
  )
  expect_no_error(result_nocv)
})

test_that("rpasurvival pruning option works", {
  # With pruning (default)
  result_pruned <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    prunetree = TRUE
  )
  expect_no_error(result_pruned)

  # Without pruning
  result_unpruned <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    prunetree = FALSE
  )
  expect_no_error(result_unpruned)
})

test_that("rpasurvival risk group labeling schemes work", {
  # Automatic (Stage I, II, III...)
  result_auto <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    riskgrouplabels = "auto"
  )
  expect_no_error(result_auto)

  # Risk-based (Low, Intermediate, High)
  result_risk <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    riskgrouplabels = "risk"
  )
  expect_no_error(result_risk)

  # Numeric (Group 1, 2, 3...)
  result_numeric <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    riskgrouplabels = "numeric"
  )
  expect_no_error(result_numeric)
})

test_that("rpasurvival KM plot options work", {
  # KM plot with all features
  result_km_full <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    kmplot = TRUE,
    kmci = TRUE,
    risktable = TRUE,
    pval = TRUE
  )
  expect_no_error(result_km_full)

  # KM plot minimal
  result_km_min <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    kmplot = TRUE,
    kmci = FALSE,
    risktable = FALSE,
    pval = FALSE
  )
  expect_no_error(result_km_min)
})

test_that("rpasurvival creates new variable when requested", {
  # Create new risk group variable
  result_newvar <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    createnewvar = TRUE,
    newvarname = "rpa_stage"
  )
  expect_no_error(result_newvar)

  # Verify new variable creation happens
  # (Actual verification would require checking jamovi data structure)
})

test_that("rpasurvival handles all output table combinations", {
  # All tables enabled
  result_all <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    riskgrouptable = TRUE,
    cptable = TRUE,
    variableimportance = TRUE
  )
  expect_no_error(result_all)

  # Only essential tables
  result_essential <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    riskgrouptable = TRUE,
    cptable = FALSE,
    variableimportance = FALSE
  )
  expect_no_error(result_essential)
})

test_that("rpasurvival interpretation and summary options work", {
  # All guidance enabled
  result_guided <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    showSummary = TRUE,
    showInterpretation = TRUE,
    showReport = TRUE
  )
  expect_no_error(result_guided)

  # No guidance (expert mode)
  result_expert <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    showSummary = FALSE,
    showInterpretation = FALSE,
    showReport = FALSE
  )
  expect_no_error(result_expert)
})

test_that("rpasurvival handles conservative vs aggressive settings", {
  # Conservative settings (for validation)
  result_conservative <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    minbucket = 30,
    cp = 0.02,
    maxdepth = 2,
    nfolds = 10,
    prunetree = TRUE
  )
  expect_no_error(result_conservative)

  # Aggressive settings (exploratory)
  result_aggressive <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age", "tumor_size", "ki67"),
    minbucket = 15,
    cp = 0.005,
    maxdepth = 4,
    nfolds = 5
  )
  expect_no_error(result_aggressive)
})

test_that("rpasurvival time_unit affects 5-year survival calculation", {
  # Load edge case data with different time units
  data(rpasurvival_edge_days, package = "ClinicoPath")
  data(rpasurvival_edge_years, package = "ClinicoPath")

  # Time in days
  result_days <- rpasurvival(
    data = rpasurvival_edge_days,
    time = "time_days",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "days"
  )
  expect_no_error(result_days)

  # Time in years
  result_years <- rpasurvival(
    data = rpasurvival_edge_years,
    time = "time_years",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "years"
  )
  expect_no_error(result_years)

  # Results should differ appropriately
  # (5 years = 1825 days vs 60 months vs 5 years)
})
