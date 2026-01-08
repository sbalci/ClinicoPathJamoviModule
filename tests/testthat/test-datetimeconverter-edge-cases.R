# ═══════════════════════════════════════════════════════════
# Edge Case Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the datetimeconverter jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(datetimeconverter_test, package = "ClinicoPath")
data(datetimeconverter_edge, package = "ClinicoPath")
data(datetimeconverter_small, package = "ClinicoPath")

test_that("datetimeconverter handles missing datetime values", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_missing",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles all NA values", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_all_na",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles invalid date formats", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_invalid",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles ambiguous dates", {
  # Dates like 01/02/2020 - could be Jan 2 or Feb 1
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_ambiguous",
    datetime_format = "dmy"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles leap year dates", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_leapyear",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles year 2000 transition dates", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_y2k",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles very old dates (1900s)", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_old",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles future dates", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_future",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles month boundary dates", {
  # Dates like Feb 28, Feb 29, Mar 1, Mar 31, Apr 1
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_month_boundary",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles year boundary dates", {
  # Dec 31, Jan 1 transitions
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_year_boundary",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles single row dataset", {
  single_row <- datetimeconverter_small[1, ]

  result <- datetimeconverter(
    data = single_row,
    datetime_var = "datetime_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles very small dataset", {
  result <- datetimeconverter(
    data = datetimeconverter_small,
    datetime_var = "datetime_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles dates with leading/trailing spaces", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_spaces",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles mixed valid and invalid dates", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_mixed_validity",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles extreme Excel serial dates", {
  # Very early Excel dates (near 1900) and recent dates (2024+)
  data(datetimeconverter_excel, package = "ClinicoPath")

  result <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_extreme",
    datetime_format = "excel_serial"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles extreme Unix timestamps", {
  # Very early Unix epoch (1970s) and recent timestamps
  data(datetimeconverter_unix, package = "ClinicoPath")

  result <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_extreme",
    datetime_format = "unix_epoch"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles timezone edge cases", {
  data(datetimeconverter_timezone, package = "ClinicoPath")

  # Daylight saving time transition dates
  result_dst <- datetimeconverter(
    data = datetimeconverter_timezone,
    datetime_var = "datetime_dst",
    datetime_format = "ymdhms",
    timezone = "America/New_York"
  )
  expect_s3_class(result_dst, "datetimeconverterClass")
})

test_that("datetimeconverter handles midnight times (00:00:00)", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_midnight",
    datetime_format = "ymdhms"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles end-of-day times (23:59:59)", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_endofday",
    datetime_format = "ymdhms"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles duplicate datetime values", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_duplicates",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles non-standard separators", {
  # Dates with / . - or space separators
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_separators",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles century ambiguity", {
  # Two-digit years like 98 (could be 1998 or 2098)
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_twodigit_year",
    datetime_format = "auto"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles February 29 in non-leap years", {
  # Should fail gracefully for invalid dates like 2023-02-29
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_invalid_feb29",
    datetime_format = "ymd"
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter handles component extraction with missing values", {
  result <- datetimeconverter(
    data = datetimeconverter_edge,
    datetime_var = "datetime_missing",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE
  )
  expect_s3_class(result, "datetimeconverterClass")
})
