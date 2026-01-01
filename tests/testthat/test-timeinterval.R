# =============================================================================
# Unit Tests for TimeInterval Function
# =============================================================================
# 
# Test suite for the timeinterval function covering basic functionality,
# parameter validation, and different input scenarios.
# =============================================================================

# Load required libraries
library(testthat)

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("timeinterval module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("timeintervalClass"))
  expect_true(is.function(timeinterval))
})

test_that("timeinterval handles missing inputs gracefully", {
  
  test_data <- data.frame(
    start_date = c("2020-01-01", "2020-02-01"),
    end_date = c("2020-06-01", "2020-08-01")
  )
  
  # Test with NULL date columns (should provide a clear error)
  expect_error(
    timeinterval(
      data = test_data,
      dx_date = NULL,
      fu_date = NULL
    ),
    regexp = "Please provide both start \\(dx_date\\) and end \\(fu_date\\) date variables\\."
  )
})

test_that("timeinterval basic functionality works", {
  
  # Create simple test data
  test_data <- data.frame(
    start_date = c("2020-01-01", "2020-02-01", "2020-03-01"),
    end_date = c("2020-06-01", "2020-08-01", "2020-12-01"),
    stringsAsFactors = FALSE
  )
  
  # Test basic calculation
  result <- timeinterval(
    data = test_data,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months"
  )
  
  expect_true(inherits(result, "timeintervalResults"))
  expect_true("start_date" %in% names(test_data))
  expect_true("end_date" %in% names(test_data))
})

# =============================================================================
# Date Format Parsing Tests
# =============================================================================

test_that("timeinterval works with YMD format", {
  
  test_data <- data.frame(
    start_ymd = c("2020-01-15", "2020-06-30", "2021-12-01"),
    end_ymd = c("2020-07-15", "2021-01-30", "2022-06-01"),
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_ymd",
      fu_date = "end_ymd", 
      time_format = "ymd",
      output_unit = "months"
    )
  }, NA)
})

test_that("timeinterval works with DMY format", {
  
  test_data <- data.frame(
    start_dmy = c("15/01/2020", "30/06/2020", "01/12/2021"),
    end_dmy = c("15/07/2020", "30/01/2021", "01/06/2022"),
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_dmy",
      fu_date = "end_dmy",
      time_format = "dmy", 
      output_unit = "months"
    )
  }, NA)
})

test_that("timeinterval works with MDY format", {
  
  test_data <- data.frame(
    start_mdy = c("01/15/2020", "06/30/2020", "12/01/2021"),
    end_mdy = c("07/15/2020", "01/30/2021", "06/01/2022"),
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_mdy",
      fu_date = "end_mdy",
      time_format = "mdy",
      output_unit = "months"
    )
  }, NA)
})

test_that("timeinterval works with auto date format detection", {
  
  test_data <- data.frame(
    start_auto = c("2020-01-15", "2020-06-30", "2021-12-01"),
    end_auto = c("2020-07-15", "2021-01-30", "2022-06-01"),
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_auto",
      fu_date = "end_auto",
      time_format = "auto",
      output_unit = "months"
    )
  }, NA)
})

# =============================================================================
# Output Unit Tests  
# =============================================================================

test_that("timeinterval works with different output units", {
  
  test_data <- data.frame(
    start_date = rep("2020-01-01", 3),
    end_date = rep("2020-07-01", 3),
    stringsAsFactors = FALSE
  )
  
  # Test different output units
  output_units <- c("days", "weeks", "months", "years")
  
  for (unit in output_units) {
    expect_error({
      result <- timeinterval(
        data = test_data,
        dx_date = "start_date",
        fu_date = "end_date",
        time_format = "ymd",
        output_unit = unit
      )
    }, NA)
  }
})

# =============================================================================
# Landmark Analysis Tests
# =============================================================================

test_that("timeinterval works with landmark analysis", {
  
  test_data <- data.frame(
    start_date = rep("2020-01-01", 5),
    end_date = c("2020-04-01", "2020-08-01", "2020-12-01", "2021-06-01", "2021-12-01"),
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_date",
      fu_date = "end_date",
      time_format = "ymd",
      output_unit = "months",
      use_landmark = TRUE,
      landmark_time = 6
    )
  }, NA)
})

# =============================================================================
# Data Quality Options Tests
# =============================================================================

test_that("timeinterval works with data quality options", {
  
  test_data <- data.frame(
    start_date = c("2020-01-01", "2020-02-01", "2020-03-01"),
    end_date = c("2020-06-01", "2020-08-01", "2020-12-01"),
    stringsAsFactors = FALSE
  )
  
  # Test quality assessment options
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_date",
      fu_date = "end_date",
      time_format = "ymd",
      output_unit = "months",
      remove_negative = TRUE,
      remove_extreme = TRUE,
      include_quality_metrics = TRUE
    )
  }, NA)
})

# =============================================================================
# Test Data Integration Tests
# =============================================================================

test_that("timeinterval works with clinical trial test data", {
  
  skip_if_not(file.exists("data/timeinterval_clinical_trial.rda"), 
              "Clinical trial test data not available")
  
  load("data/timeinterval_clinical_trial.rda")
  
  expect_error({
    result <- timeinterval(
      data = timeinterval_clinical_trial,
      dx_date = "enrollment_date_ymd",
      fu_date = "followup_date_ymd",
      time_format = "ymd",
      output_unit = "months"
    )
  }, NA)
  
  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval works with European dates test data", {
  
  skip_if_not(file.exists("data/timeinterval_european_dates.rda"),
              "European dates test data not available")
  
  load("data/timeinterval_european_dates.rda")
  
  expect_error({
    result <- timeinterval(
      data = timeinterval_european_dates,
      dx_date = "diagnosis_date_dmy", 
      fu_date = "last_visit_dmy",
      time_format = "dmy",
      output_unit = "months"
    )
  }, NA)
  
  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval works with landmark analysis test data", {
  
  skip_if_not(file.exists("data/timeinterval_landmark.rda"),
              "Landmark test data not available")
  
  load("data/timeinterval_landmark.rda")
  
  expect_error({
    result <- timeinterval(
      data = timeinterval_landmark,
      dx_date = "diagnosis_date",
      fu_date = "last_contact_date",
      time_format = "ymd",
      output_unit = "months",
      use_landmark = TRUE,
      landmark_time = 6
    )
  }, NA)
  
  expect_s3_class(result, "timeintervalClass")
})

# =============================================================================
# Parameter Validation Tests
# =============================================================================

test_that("timeinterval validates input parameters correctly", {
  
  test_data <- data.frame(
    start_date = c("2020-01-01", "2020-02-01"),
    end_date = c("2020-06-01", "2020-08-01")
  )
  
  # Test confidence level bounds
  expect_error({
    timeinterval(
      data = test_data,
      dx_date = "start_date",
      fu_date = "end_date",
      confidence_level = 105  # Invalid: > 100
    )
  })
  
  expect_error({
    timeinterval(
      data = test_data,
      dx_date = "start_date", 
      fu_date = "end_date",
      confidence_level = 85   # Invalid: < 90
    )
  })
})

test_that("timeinterval handles missing data appropriately", {
  
  test_data <- data.frame(
    start_date = c("2020-01-01", "2020-02-01", "2020-03-01"),
    end_date = c("2020-06-01", NA, "2020-12-01"),  # One missing
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = test_data,
      dx_date = "start_date",
      fu_date = "end_date",
      time_format = "ymd",
      output_unit = "months"
    )
  }, NA)
})

# =============================================================================
# Edge Cases and Performance Tests
# =============================================================================

test_that("timeinterval handles edge case dates", {
  
  # Test with leap year and boundary dates
  edge_data <- data.frame(
    start_date = c("2020-02-29", "2019-12-31", "2021-01-01"),  # Leap year, year boundary
    end_date = c("2020-03-01", "2020-01-01", "2021-12-31"),
    stringsAsFactors = FALSE
  )
  
  expect_error({
    result <- timeinterval(
      data = edge_data,
      dx_date = "start_date",
      fu_date = "end_date",
      time_format = "ymd",
      output_unit = "days"
    )
  }, NA)
})
