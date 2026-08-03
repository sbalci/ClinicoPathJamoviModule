library(testthat)

make_clean_data <- function() {
    data.frame(ts = c("2023-01-15 14:30:00", "2024-02-20 09:00:00", "2023-03-10 12:00:00"))
}

# -----------------------------------------------------------------------------
# Core parsing paths
# -----------------------------------------------------------------------------

test_that("auto-detection parses clean ISO datetimes", {
    data <- make_clean_data()
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        datetime_format = "auto")

    expect_s3_class(res, "datetimeconverterResults")
    expect_true(!is.null(res$previewTable))
})

test_that("manual format specification works", {
    data <- data.frame(ts = c("15/01/2023", "20/02/2024"))
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        datetime_format = "dmy")

    expect_s3_class(res, "datetimeconverterResults")
    expect_true(!is.null(res$previewTable))
})

# -----------------------------------------------------------------------------
# Numeric inputs (Excel / Unix / character serials)
# -----------------------------------------------------------------------------

test_that("Excel serial numbers are detected", {
    data <- data.frame(ts = c(44940.6041666667, 45341.375))
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        datetime_format = "excel_serial")

    expect_s3_class(res, "datetimeconverterResults")
})

test_that("Excel serials stored as character are auto-converted", {
    data <- data.frame(ts = c("44940.6041666667", "45341.375", "45000"))
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        datetime_format = "excel_serial")

    expect_s3_class(res, "datetimeconverterResults")
})

test_that("Unix epoch seconds are detected", {
    data <- data.frame(ts = c(1673800200, 1678459200))
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        datetime_format = "unix_epoch")

    expect_s3_class(res, "datetimeconverterResults")
})

# -----------------------------------------------------------------------------
# Component extraction and outputs
# -----------------------------------------------------------------------------

test_that("component extraction populates requested outputs", {
    data <- make_clean_data()
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        extract_year = TRUE,
        extract_month = TRUE,
        extract_monthname = TRUE,
        extract_day = TRUE,
        extract_hour = TRUE,
        extract_minute = TRUE,
        extract_second = TRUE,
        extract_dayname = TRUE,
        extract_weeknum = TRUE,
        extract_quarter = TRUE,
        extract_dayofyear = TRUE)

    expect_s3_class(res, "datetimeconverterResults")
    expect_true(!is.null(res$componentPreview))
})

# -----------------------------------------------------------------------------
# Timezone handling
# -----------------------------------------------------------------------------

test_that("custom timezone strings are honoured", {
    data <- make_clean_data()
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        timezone = "Europe/Istanbul")

    expect_s3_class(res, "datetimeconverterResults")
})

# -----------------------------------------------------------------------------
# Ambiguity + validation
# -----------------------------------------------------------------------------

test_that("ambiguous DMY/MDY formats surface warnings", {
    data <- data.frame(ts = c("01-02-2023", "02-03-2023", "04-05-2023"))
    res <- datetimeconverter(
        data = data,
        datetime_var = "ts",
        datetime_format = "auto",
        show_quality_metrics = TRUE,
        show_summary = TRUE)

    expect_match(res$notices$content, "Ambiguous")
})

# -----------------------------------------------------------------------------
# Error handling
# -----------------------------------------------------------------------------

test_that("missing datetime variable triggers informative error", {
    data <- make_clean_data()
    expect_error(
        datetimeconverter(data = data, datetime_var = "missing_col")
    )
})
