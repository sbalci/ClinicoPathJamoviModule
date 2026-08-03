# ═══════════════════════════════════════════════════════════
# Edge Case Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the datetimeconverter jamovi function

library(testthat)

# Load test data
data(datetimeconverter_test, package = "ClinicoPath")
data(datetimeconverter_edge, package = "ClinicoPath")
data(datetimeconverter_small, package = "ClinicoPath")

test_that("datetimeconverter handles edge dataset with auto-detection", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_string",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles small dataset with YMD dates", {
  result <- datetimeconverter(
    data = datetimeconverter_small,
    datetime_var = "datetime_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles small dataset with DMY dates", {
  result <- datetimeconverter(
    data = datetimeconverter_small,
    datetime_var = "datetime_dmy",
    datetime_format = "dmy"
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles small dataset with Excel serials", {
  result <- datetimeconverter(
    data = datetimeconverter_small,
    datetime_var = "excel_serial",
    datetime_format = "excel_serial"
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles small dataset with Unix epoch", {
  result <- datetimeconverter(
    data = datetimeconverter_small,
    datetime_var = "unix_epoch",
    datetime_format = "unix_epoch"
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles missing values in dates", {
  df <- data.frame(
    dates = c("2024-01-15", NA, "2024-03-20", "", "  ")
  )
  result <- datetimeconverter(
    data = df,
    datetime_var = "dates",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles single-row data frames", {
  df <- data.frame(
    dates = "2024-05-10 10:30:00"
  )
  result <- datetimeconverter(
    data = df,
    datetime_var = "dates",
    datetime_format = "ymdhms",
    extract_year = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})
