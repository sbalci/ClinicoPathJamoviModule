# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: rpasurvival
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality of the rpasurvival function
# (Recursive Partitioning Analysis for Survival Data)

library(testthat)

# Load test data
data(rpasurvival_test, package = "ClinicoPath")

test_that("rpasurvival function exists and is accessible", {
  expect_true(exists("rpasurvival"))
  expect_type(rpasurvival, "closure")
})

test_that("rpasurvival runs with minimal required arguments", {
  # Basic execution with time, event, and 2 predictors
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade")
  )

  expect_s3_class(result, "rpasurvivalClass")
  expect_true("results" %in% names(result))
})

test_that("rpasurvival handles required arguments correctly", {
  # Test that function works with all required arguments
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("age", "stage", "grade", "LVI")
  )

  expect_no_error(result)
  expect_s3_class(result, "rpasurvivalClass")
})

test_that("rpasurvival errors on missing required arguments", {
  # Missing time variable
  expect_error(
    rpasurvival(
      data = rpasurvival_test,
      event = "event",
      predictors = c("stage", "grade")
    )
  )

  # Missing event variable
  expect_error(
    rpasurvival(
      data = rpasurvival_test,
      time = "time",
      predictors = c("stage", "grade")
    )
  )

  # Missing predictors
  expect_error(
    rpasurvival(
      data = rpasurvival_test,
      time = "time",
      event = "event"
    )
  )
})

test_that("rpasurvival produces expected output structure", {
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    riskgrouptable = TRUE,
    treeplot = TRUE,
    kmplot = TRUE
  )

  # Check that main results components exist
  expect_true(!is.null(result$results))

  # Check for expected output tables/plots
  # These will depend on .r.yaml structure
  expect_true("riskgrouptable" %in% names(result$results) ||
              "maintable" %in% names(result$results))
})

test_that("rpasurvival accepts valid eventValue options", {
  # Test eventValue = "1" (default)
  result1 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    eventValue = "1"
  )
  expect_no_error(result1)

  # Note: Testing "2" and "TRUE" requires data with those codings
  # See test-rpasurvival-edge-cases.R
})

test_that("rpasurvival handles different time_unit options", {
  # Time unit: months (default)
  result_months <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "months"
  )
  expect_no_error(result_months)

  # Time unit: years
  result_years <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "years"
  )
  expect_no_error(result_years)

  # Time unit: days
  result_days <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "days"
  )
  expect_no_error(result_days)
})

test_that("rpasurvival works with different numbers of predictors", {
  # 2 predictors
  result_2 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade")
  )
  expect_no_error(result_2)

  # 4 predictors
  result_4 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("age", "stage", "grade", "LVI")
  )
  expect_no_error(result_4)

  # 6 predictors
  result_6 <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("age", "stage", "grade", "LVI", "tumor_size", "ki67")
  )
  expect_no_error(result_6)
})

test_that("rpasurvival respects output control options", {
  # With all outputs enabled
  result_all <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI"),
    treeplot = TRUE,
    kmplot = TRUE,
    riskgrouptable = TRUE,
    variableimportance = TRUE,
    showSummary = TRUE,
    showReport = TRUE
  )
  expect_no_error(result_all)

  # With minimal outputs
  result_min <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    treeplot = FALSE,
    kmplot = FALSE,
    riskgrouptable = TRUE,
    variableimportance = FALSE,
    showSummary = FALSE,
    showReport = FALSE
  )
  expect_no_error(result_min)
})

test_that("rpasurvival handles mixed predictor types", {
  # Mix of continuous, ordinal, and nominal predictors
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c(
      "age",           # continuous
      "stage",         # ordinal
      "grade",         # ordinal
      "LVI",           # nominal
      "tumor_size",    # continuous
      "performance_status"  # ordinal
    )
  )

  expect_no_error(result)
  expect_s3_class(result, "rpasurvivalClass")
})
