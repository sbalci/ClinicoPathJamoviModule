# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the datetimeconverter jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(datetimeconverter_test, package = "ClinicoPath")

test_that("datetimeconverter function exists and is accessible", {
  expect_true(exists("datetimeconverter"))
  expect_type(datetimeconverter, "closure")
})

test_that("datetimeconverter runs with minimal required arguments", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
  expect_true("results" %in% names(result))
})

test_that("datetimeconverter handles YMD format", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles DMY format", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_dmy",
    datetime_format = "dmy"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles MDY format", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_mdy",
    datetime_format = "mdy"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles datetime with HMS", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    datetime_format = "ymdhms"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles auto-detection", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "auto"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles Excel serial dates", {
  data(datetimeconverter_excel, package = "ClinicoPath")
  
  result <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles Unix epoch timestamps", {
  data(datetimeconverter_unix, package = "ClinicoPath")
  
  result <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_timestamp",
    datetime_format = "unix_epoch"
  )
  
  expect_s3_class(result, "datetimeconverter")
})

test_that("datetimeconverter extracts year component", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_year = TRUE
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter extracts month component", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_month = TRUE
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter extracts day name", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_dayname = TRUE
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter extracts multiple components", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles timezone specification", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    datetime_format = "ymdhms",
    timezone = "UTC"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter shows quality metrics", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    show_quality_metrics = TRUE
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter runs with small dataset", {
  data(datetimeconverter_small, package = "ClinicoPath")
  
  result <- datetimeconverter(
    data = datetimeconverter_small,
    datetime_var = "datetime_ymd"
  )
  
  expect_s3_class(result, "datetimeconverterClass")
})
