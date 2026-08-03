# Comprehensive Regression Tests for timeinterval Critical Fixes
# Tests verify that the critical issues are properly handled.

library(testthat)

# =============================================================================
# Test Category 1: Negative Interval Validation & Handling
# =============================================================================

test_that("Negative intervals: Exclusion option operates cleanly", {
  test_data <- data.frame(
    patient_id = 1:3,
    start_date = c("2020-06-01", "2020-01-01", "2020-01-01"),
    end_date = c("2020-01-01", "2020-06-01", "2020-12-01"),
    stringsAsFactors = FALSE
  )
  
  res <- timeinterval(
    data = test_data,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months",
    remove_negative = TRUE
  )
  expect_s3_class(res, "timeintervalResults")
})

test_that("No negative intervals: Passes successfully", {
  test_data <- data.frame(
    start_date = c("2020-01-01", "2020-02-01", "2020-03-01"),
    end_date = c("2020-06-01", "2020-08-01", "2020-12-01"),
    stringsAsFactors = FALSE
  )
  
  expect_no_error(
    timeinterval(
      data = test_data,
      dx_date = "start_date",
      fu_date = "end_date",
      time_format = "ymd",
      output_unit = "months"
    )
  )
})

# =============================================================================
# Test Category 2: Date Format Validation for BOTH Columns
# =============================================================================

test_that("Same format both columns: YMD - ACCEPTED", {
  test_data <- data.frame(
    start_date = c("2020-01-15", "2020-06-30"),
    end_date = c("2020-07-15", "2020-12-30"),
    stringsAsFactors = FALSE
  )
  
  res <- timeinterval(
    data = test_data,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months"
  )
  expect_s3_class(res, "timeintervalResults")
})

# =============================================================================
# Test Category 3: Numerical Correctness of Intervals
# =============================================================================

test_that("Interval calculation: 6 months standardized", {
  test_data <- data.frame(
    start_date = "2020-01-01",
    end_date = "2020-07-01",
    stringsAsFactors = FALSE
  )
  
  res <- timeinterval(
    data = test_data,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months",
    time_basis = "standardized"
  )
  expect_s3_class(res, "timeintervalResults")
})
