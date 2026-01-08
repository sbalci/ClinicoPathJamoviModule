# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Tests all valid argument combinations, option interactions,
# and parameter ranges for the datetimeconverter jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(datetimeconverter_test, package = "ClinicoPath")
data(datetimeconverter_excel, package = "ClinicoPath")
data(datetimeconverter_unix, package = "ClinicoPath")
data(datetimeconverter_mixed, package = "ClinicoPath")
data(datetimeconverter_clinical, package = "ClinicoPath")
data(datetimeconverter_timezone, package = "ClinicoPath")
data(datetimeconverter_components, package = "ClinicoPath")

test_that("datetimeconverter handles all datetime format options", {
  formats <- c("auto", "ymd", "dmy", "mdy", "ymdhms", "dmyhms", "mdyhms",
               "ymdh", "dmyh", "mdyh", "ymdhm", "dmyhm")

  # Test auto format
  result_auto <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "auto"
  )
  expect_s3_class(result_auto, "datetimeconverterClass")

  # Test ymd format
  result_ymd <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(result_ymd, "datetimeconverterClass")

  # Test dmy format
  result_dmy <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_dmy",
    datetime_format = "dmy"
  )
  expect_s3_class(result_dmy, "datetimeconverterClass")

  # Test mdy format
  result_mdy <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_mdy",
    datetime_format = "mdy"
  )
  expect_s3_class(result_mdy, "datetimeconverterClass")
})

test_that("datetimeconverter handles datetime with time components", {
  # ymdhms format
  result_ymdhms <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_ymdhms",
    datetime_format = "ymdhms"
  )
  expect_s3_class(result_ymdhms, "datetimeconverterClass")

  # dmyhms format
  result_dmyhms <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_dmyhms",
    datetime_format = "dmyhms"
  )
  expect_s3_class(result_dmyhms, "datetimeconverterClass")

  # mdyhms format
  result_mdyhms <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_mdyhms",
    datetime_format = "mdyhms"
  )
  expect_s3_class(result_mdyhms, "datetimeconverterClass")
})

test_that("datetimeconverter handles Excel serial date formats", {
  # Excel serial dates (Windows)
  result_excel <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial"
  )
  expect_s3_class(result_excel, "datetimeconverterClass")

  # Excel 1904 date system (Mac)
  result_excel_1904 <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_1904_date",
    datetime_format = "excel_1904"
  )
  expect_s3_class(result_excel_1904, "datetimeconverterClass")
})

test_that("datetimeconverter handles Unix epoch timestamps", {
  # Unix epoch seconds
  result_unix <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_timestamp",
    datetime_format = "unix_epoch"
  )
  expect_s3_class(result_unix, "datetimeconverterClass")

  # Unix epoch milliseconds
  result_unix_ms <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_milliseconds",
    datetime_format = "unix_milliseconds"
  )
  expect_s3_class(result_unix_ms, "datetimeconverterClass")
})

test_that("datetimeconverter handles ISO 8601 formats", {
  result_iso <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "datetime_iso8601",
    datetime_format = "iso8601"
  )
  expect_s3_class(result_iso, "datetimeconverterClass")
})

test_that("datetimeconverter extracts individual components", {
  # Year extraction
  result_year <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_year = TRUE
  )
  expect_s3_class(result_year, "datetimeconverterClass")

  # Month extraction
  result_month <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_month = TRUE
  )
  expect_s3_class(result_month, "datetimeconverterClass")

  # Month name extraction
  result_monthname <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_monthname = TRUE
  )
  expect_s3_class(result_monthname, "datetimeconverterClass")

  # Day extraction
  result_day <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_day = TRUE
  )
  expect_s3_class(result_day, "datetimeconverterClass")

  # Day name extraction
  result_dayname <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_dayname = TRUE
  )
  expect_s3_class(result_dayname, "datetimeconverterClass")

  # Week number extraction
  result_weeknum <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_weeknum = TRUE
  )
  expect_s3_class(result_weeknum, "datetimeconverterClass")

  # Quarter extraction
  result_quarter <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_quarter = TRUE
  )
  expect_s3_class(result_quarter, "datetimeconverterClass")

  # Day of year extraction
  result_dayofyear <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "date_standard",
    extract_dayofyear = TRUE
  )
  expect_s3_class(result_dayofyear, "datetimeconverterClass")
})

test_that("datetimeconverter extracts time components", {
  # Hour extraction
  result_hour <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "datetime_full",
    extract_hour = TRUE
  )
  expect_s3_class(result_hour, "datetimeconverterClass")

  # Minute extraction
  result_minute <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "datetime_full",
    extract_minute = TRUE
  )
  expect_s3_class(result_minute, "datetimeconverterClass")

  # Second extraction
  result_second <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "datetime_full",
    extract_second = TRUE
  )
  expect_s3_class(result_second, "datetimeconverterClass")
})

test_that("datetimeconverter handles multiple component extractions", {
  result <- datetimeconverter(
    data = datetimeconverter_components,
    datetime_var = "datetime_full",
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
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles timezone specifications", {
  timezones <- c("UTC", "America/New_York", "Europe/London", "Asia/Tokyo")

  for (tz in timezones) {
    result <- datetimeconverter(
      data = datetimeconverter_timezone,
      datetime_var = "datetime_utc",
      datetime_format = "ymdhms",
      timezone = tz
    )
    expect_s3_class(result, "datetimeconverterClass")
  }
})

test_that("datetimeconverter handles timezone with component extraction", {
  result <- datetimeconverter(
    data = datetimeconverter_timezone,
    datetime_var = "datetime_utc",
    datetime_format = "ymdhms",
    timezone = "America/New_York",
    extract_hour = TRUE,
    extract_dayname = TRUE
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles output format options", {
  # Output as text
  result_text <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    output_as_text = TRUE
  )
  expect_s3_class(result_text, "datetimeconverterClass")

  # Output as numeric
  result_numeric <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    output_as_numeric = TRUE
  )
  expect_s3_class(result_numeric, "datetimeconverterClass")
})

test_that("datetimeconverter handles custom output format", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    custom_output_format = "%Y-%m-%d %H:%M:%S"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles quality metrics display", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    show_quality_metrics = TRUE
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles preview option", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    show_preview = TRUE,
    preview_rows = 10
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles clinical date scenarios", {
  # Admission date
  result_admission <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_dayname = TRUE
  )
  expect_s3_class(result_admission, "datetimeconverterClass")

  # Surgery datetime
  result_surgery <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "SurgeryDateTime",
    datetime_format = "ymdhms",
    timezone = "UTC",
    extract_hour = TRUE
  )
  expect_s3_class(result_surgery, "datetimeconverterClass")

  # Lab result timestamp
  result_lab <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "LabTimestamp",
    datetime_format = "ymdhms"
  )
  expect_s3_class(result_lab, "datetimeconverterClass")
})

test_that("datetimeconverter handles auto-detection with mixed formats", {
  result <- datetimeconverter(
    data = datetimeconverter_mixed,
    datetime_var = "mixed_dates",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles comprehensive clinical workflow", {
  # Complete analysis with all features
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
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
    show_preview = TRUE,
    preview_rows = 5
  )
  expect_s3_class(result, "datetimeconverterClass")
})
