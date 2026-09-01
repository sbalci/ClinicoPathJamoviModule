# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: timeinterval
# ═══════════════════════════════════════════════════════════

library(testthat)

data(timeinterval_test, package = "ClinicoPath")
data(timeinterval_ymd, package = "ClinicoPath")
data(timeinterval_dmy, package = "ClinicoPath")
data(timeinterval_small, package = "ClinicoPath")

test_that("timeinterval function exists and is callable", {
  expect_true(exists("timeinterval"))
  expect_true(is.function(timeinterval))
})

test_that("timeinterval runs with minimal required arguments", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )

  expect_s3_class(result, "timeintervalResults")
  expect_true(!is.null(result$summary))
})

test_that("timeinterval accepts data.frame and tibble", {
  df_data <- as.data.frame(timeinterval_test)
  result_df <- timeinterval(
    data = df_data,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_s3_class(result_df, "timeintervalResults")

  result_tbl <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_s3_class(result_tbl, "timeintervalResults")
})

test_that("timeinterval validates variable names exist in data", {
  expect_error(
    timeinterval(
      data = timeinterval_test,
      dx_date = "nonexistent_start",
      fu_date = "followup_date"
    )
  )

  expect_error(
    timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "nonexistent_end"
    )
  )
})

test_that("timeinterval handles YMD format", {
  result <- timeinterval(
    data = timeinterval_ymd,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd"
  )
  expect_s3_class(result, "timeintervalResults")
})

test_that("timeinterval handles DMY format", {
  result <- timeinterval(
    data = timeinterval_dmy,
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "dmy"
  )
  expect_s3_class(result, "timeintervalResults")
})

test_that("timeinterval handles auto format detection", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_format = "auto"
  )
  expect_s3_class(result, "timeintervalResults")
})

test_that("timeinterval handles different output units", {
  units <- c("days", "weeks", "months", "years")

  for (unit in units) {
    result <- timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "followup_date",
      output_unit = unit
    )
    expect_s3_class(result, "timeintervalResults")
  }
})

test_that("timeinterval handles standardized and calendar time basis", {
  result_std <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_basis = "standardized",
    output_unit = "months"
  )
  expect_s3_class(result_std, "timeintervalResults")

  result_cal <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_basis = "calendar",
    output_unit = "months"
  )
  expect_s3_class(result_cal, "timeintervalResults")
})

test_that("timeinterval produces expected output structure", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_s3_class(result, "timeintervalResults")
  expect_true(!is.null(result$summary))
})

test_that("timeinterval options and flags run smoothly", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    include_quality_metrics = TRUE,
    confidence_level = 95,
    show_summary = TRUE,
    show_glossary = TRUE
  )
  expect_s3_class(result, "timeintervalResults")
})

test_that("timeinterval handles small datasets and single row", {
  result_small <- timeinterval(
    data = timeinterval_small,
    dx_date = "start",
    fu_date = "end"
  )
  expect_s3_class(result_small, "timeintervalResults")

  single_row <- timeinterval_test[1, ]
  result_single <- timeinterval(
    data = single_row,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_s3_class(result_single, "timeintervalResults")
})
