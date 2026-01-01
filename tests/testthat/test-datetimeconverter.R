library(testthat)
library(ClinicoPath)

make_clean_data <- function() {
    data.frame(ts = c("2023-01-15 14:30:00", "2024-02-20 09:00:00", "2023-03-10 12:00:00"))
}

make_messy_data <- function() {
    data.frame(
        ymd_hms_var = c("2023-01-15 14:30:00", "2024-02-20 09:00:00", NA, "", "2023-03-10 12:00:00"),
        dmy_var = c("15/01/2023", "20/02/2024", "10/03/2023", NA, ""),
        mdy_var = c("01-15-2023", "02-20-2024", "03-10-2023", NA, ""),
        excel_var = c(44940.6041666667, 45341.375, NA, NA, 45000),
        excel_char = c("44940.6041666667", "45341.375", NA, "", "45000"),
        unix_var = c(1673800200, 1708419600, NA, NA, 1678459200),
        ambiguous = c("01-02-2023", "02-03-2023", "04-05-2023", NA, "")
    )
}

expect_datetime_numeric <- function(res, row, expected, tz = "UTC") {
    df <- res$corrected_datetime_numeric$asDF
    expect_type(df[row, "corrected_datetime_numeric"], "double")
    expect_equal(df[row, "corrected_datetime_numeric"], as.numeric(expected), tolerance = 1)
}

# -----------------------------------------------------------------------------
# Core parsing paths
# -----------------------------------------------------------------------------

test_that("auto-detection parses clean ISO datetimes", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    data <- make_clean_data()
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        corrected_datetime_numeric = TRUE)

    expect_datetime_numeric(res, 1, as.POSIXct("2023-01-15 14:30:00", tz = Sys.timezone()))
})

test_that("manual format specification works", {
    data <- data.frame(ts = c("15/01/2023", "20/02/2024"))
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        datetime_format = "dmy",
        corrected_datetime_numeric = TRUE)

    expect_datetime_numeric(res, 1, as.POSIXct("2023-01-15 00:00:00", tz = Sys.timezone()))
})

# -----------------------------------------------------------------------------
# Numeric inputs (Excel / Unix / character serials)
# -----------------------------------------------------------------------------

test_that("Excel serial numbers are detected", {
    data <- data.frame(ts = c(44940.6041666667, 45341.375))
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        corrected_datetime_numeric = TRUE)

    expect_datetime_numeric(res, 1, as.POSIXct("2023-01-15 14:30:00", tz = "UTC"))
})

test_that("Excel serials stored as character are auto-converted", {
    data <- data.frame(ts = c("44940.6041666667", "45341.375", "45000"))
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        corrected_datetime_numeric = TRUE)

    expect_datetime_numeric(res, 3, as.POSIXct("2023-03-07 00:00:00", tz = "UTC"))
})

test_that("Unix epoch seconds are detected", {
    data <- data.frame(ts = c(1673800200, 1678459200))
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        corrected_datetime_numeric = TRUE)

    expect_datetime_numeric(res, 1, as.POSIXct(1673800200, origin = "1970-01-01", tz = "UTC"))
})

# -----------------------------------------------------------------------------
# Component extraction and outputs
# -----------------------------------------------------------------------------

test_that("component extraction populates requested outputs", {
    data <- make_clean_data()
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        year_out = TRUE,
        month_out = TRUE,
        day_out = TRUE,
        hour_out = TRUE,
        minute_out = TRUE,
        second_out = TRUE,
        dayname_out = TRUE,
        weeknum_out = TRUE,
        quarter_out = TRUE,
        dayofyear_out = TRUE)

    expect_equal(res$year_out$asDF[1, "year_out"], 2023)
    expect_equal(res$month_out$asDF[1, "month_out"], 1)
    expect_equal(res$day_out$asDF[1, "day_out"], 15)
    expect_equal(res$hour_out$asDF[1, "hour_out"], 14)
    expect_equal(res$minute_out$asDF[1, "minute_out"], 30)
    expect_equal(res$second_out$asDF[1, "second_out"], 0)
    expect_equal(res$dayname_out$asDF[1, "dayname_out"], "Sunday")
    expect_equal(res$weeknum_out$asDF[1, "weeknum_out"], 2)
    expect_equal(res$quarter_out$asDF[1, "quarter_out"], 1)
    expect_equal(res$dayofyear_out$asDF[1, "dayofyear_out"], 15)
})

# -----------------------------------------------------------------------------
# Timezone handling
# -----------------------------------------------------------------------------

test_that("custom timezone strings are honoured", {
    data <- make_clean_data()
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        timezone = "Europe/Istanbul",
        corrected_datetime_numeric = TRUE)

    expect_datetime_numeric(res, 1, as.POSIXct("2023-01-15 14:30:00", tz = "Europe/Istanbul"))
})

# -----------------------------------------------------------------------------
# Ambiguity + validation
# -----------------------------------------------------------------------------

test_that("ambiguous DMY/MDY formats surface warnings", {
    data <- data.frame(ts = c("01-02-2023", "02-03-2023", "04-05-2023"))
    res <- datetimeconverter(
        data = data,
        datetime_var = ts,
        datetime_format = "auto",
        show_quality_metrics = TRUE,
        show_summary = TRUE)

    format_html <- res$formatInfo$getContent()
    expect_match(format_html, "Ambiguous")
})

# -----------------------------------------------------------------------------
# Error handling
# -----------------------------------------------------------------------------

test_that("missing datetime variable triggers informative error", {
    data <- make_clean_data()
    expect_error(
        datetimeconverter(data = data, datetime_var = missing_col),
        "not found")
})
