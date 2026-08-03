# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════

library(testthat)

data(datetimeconverter_test, package = "ClinicoPath")
data(datetimeconverter_excel, package = "ClinicoPath")
data(datetimeconverter_unix, package = "ClinicoPath")
data(datetimeconverter_mixed, package = "ClinicoPath")
data(datetimeconverter_clinical, package = "ClinicoPath")

test_that("datetimeconverter handles all datetime format options", {
  # Test auto format
  result_auto <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "auto"
  )
  expect_s3_class(result_auto, "datetimeconverterResults")

  # Test ymd format
  result_ymd <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(result_ymd, "datetimeconverterResults")

  # Test dmy format
  result_dmy <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_dmy",
    datetime_format = "dmy"
  )
  expect_s3_class(result_dmy, "datetimeconverterResults")

  # Test mdy format
  result_mdy <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_mdy",
    datetime_format = "mdy"
  )
  expect_s3_class(result_mdy, "datetimeconverterResults")
})

test_that("datetimeconverter handles datetime with time components", {
  result_ymdhms <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    datetime_format = "ymdhms"
  )
  expect_s3_class(result_ymdhms, "datetimeconverterResults")

  result_dmyhms <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_dmyhms",
    datetime_format = "dmyhms"
  )
  expect_s3_class(result_dmyhms, "datetimeconverterResults")

  result_mdyhms <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_mdyhms",
    datetime_format = "mdyhms"
  )
  expect_s3_class(result_mdyhms, "datetimeconverterResults")
})

test_that("datetimeconverter handles Excel serial date formats", {
  result_excel <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial"
  )
  expect_s3_class(result_excel, "datetimeconverterResults")
})

test_that("datetimeconverter handles Unix epoch timestamps", {
  result_unix <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_timestamp",
    datetime_format = "unix_epoch"
  )
  expect_s3_class(result_unix, "datetimeconverterResults")
})

test_that("datetimeconverter extracts individual components", {
  result_year <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_year = TRUE
  )
  expect_s3_class(result_year, "datetimeconverterResults")

  result_month <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_month = TRUE
  )
  expect_s3_class(result_month, "datetimeconverterResults")

  result_monthname <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_monthname = TRUE
  )
  expect_s3_class(result_monthname, "datetimeconverterResults")

  result_day <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_day = TRUE
  )
  expect_s3_class(result_day, "datetimeconverterResults")

  result_dayname <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_dayname = TRUE
  )
  expect_s3_class(result_dayname, "datetimeconverterResults")

  result_weeknum <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_weeknum = TRUE
  )
  expect_s3_class(result_weeknum, "datetimeconverterResults")

  result_quarter <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_quarter = TRUE
  )
  expect_s3_class(result_quarter, "datetimeconverterResults")

  result_dayofyear <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    extract_dayofyear = TRUE
  )
  expect_s3_class(result_dayofyear, "datetimeconverterResults")
})

test_that("datetimeconverter extracts time components", {
  result_hour <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    extract_hour = TRUE
  )
  expect_s3_class(result_hour, "datetimeconverterResults")

  result_minute <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    extract_minute = TRUE
  )
  expect_s3_class(result_minute, "datetimeconverterResults")

  result_second <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    extract_second = TRUE
  )
  expect_s3_class(result_second, "datetimeconverterResults")
})

test_that("datetimeconverter handles multiple component extractions", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_monthname = TRUE,
    extract_day = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE,
    extract_dayofyear = TRUE,
    extract_hour = TRUE,
    extract_minute = TRUE,
    extract_second = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles timezone specifications", {
  timezones <- c("UTC", "America/New_York", "Europe/London", "Asia/Tokyo")

  for (tz in timezones) {
    result <- datetimeconverter(
      data = datetimeconverter_test,
      datetime_var = "datetime_ymdhms",
      datetime_format = "ymdhms",
      timezone = tz
    )
    expect_s3_class(result, "datetimeconverterResults")
  }
})

test_that("datetimeconverter handles timezone with component extraction", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    datetime_format = "ymdhms",
    timezone = "America/New_York",
    extract_hour = TRUE,
    extract_dayname = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles quality metrics display", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    show_quality_metrics = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles preview options", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    preview_rows = 10
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles clinical date scenarios", {
  result_surgery <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_dayname = TRUE
  )
  expect_s3_class(result_surgery, "datetimeconverterResults")

  result_lab <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "lab_timestamp",
    datetime_format = "ymdhms",
    timezone = "UTC",
    extract_hour = TRUE
  )
  expect_s3_class(result_lab, "datetimeconverterResults")
})

test_that("datetimeconverter handles auto-detection with mixed formats", {
  result <- datetimeconverter(
    data = datetimeconverter_mixed,
    datetime_var = "mixed_datetime",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter handles comprehensive clinical workflow", {
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd",
    timezone = "America/New_York",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_monthname = TRUE,
    extract_day = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE,
    show_quality_metrics = TRUE,
    preview_rows = 10
  )
  expect_s3_class(result, "datetimeconverterResults")
})
