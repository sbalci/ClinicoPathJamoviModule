library(testthat)

# Release-check fixes, 2026-10-29. Each block states what the analysis did before.
# Use base::format: jmvcore exports a format() that ignores format strings.

fmt <- base::format
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
dtc_notices <- function(data, var, ...) {
    an <- datetimeconverterClass$new(
        options = datetimeconverterOptions$new(datetime_var = var, ...), data = data)
    quietly(an$run())
    strip_html(an$results$notices$content)
}

test_that("text after the date is flagged, not silently dropped", {
    # Was: "2024-01-15abc" and "2024-01-15 extra" parsed at 100% with no notice,
    # because lubridate reads a prefix and ignores the rest.
    d <- data.frame(x = c("2024-01-15abc", "2024-01-15 extra", "2024-03-01",
                          "2024-04-01", "2024-05-01"), stringsAsFactors = FALSE)
    n <- dtc_notices(d, "x")
    expect_match(n, "Text After The Date Was Ignored", fixed = TRUE)
    expect_match(n, "2 value(s) have text after the date", fixed = TRUE)
    expect_match(n, "2024-01-15abc", fixed = TRUE)

    # clean dates, AM/PM and zone markers are not "trailing text"
    clean <- data.frame(x = c("2024-01-15", "2024-03-01", "2024-04-01"))
    expect_false(grepl("text after the date", dtc_notices(clean, "x"), fixed = TRUE))
    ampm <- data.frame(x = c("2024-01-15 10:30:00 PM", "2024-01-16 12:30:00 PM",
                             "2024-01-17 08:00:00 UTC", "2024-01-18 09:00:00Z"))
    expect_false(grepl("text after the date",
                       dtc_notices(ampm, "x", datetime_format = "ymdhms"), fixed = TRUE))
})

test_that("a time inside a daylight-saving gap is named as the cause", {
    # Was: 2024-03-10 02:30 in America/New_York (skipped hour) became NA with only
    # a generic "Moderate parsing success rate" notice.
    d <- data.frame(x = c("2024-01-15 10:30:00", "2023-12-31 23:59:59",
                          "2021-01-03 00:00:00", "2024-03-10 02:30:00"))
    n <- dtc_notices(d, "x", timezone = "America/New_York")
    expect_match(n, "Times In A Daylight-Saving Gap", fixed = TRUE)
    expect_match(n, "does not exist in the America/New_York timezone", fixed = TRUE)
    expect_match(n, "first: 2024-03-10 02:30:00", fixed = TRUE)
    # UTC has no gaps: the same value converts and nothing is said
    expect_false(grepl("Daylight-Saving Gap", dtc_notices(d, "x", timezone = "UTC"), fixed = TRUE))
})

test_that("Unix epoch on a milliseconds column points to Auto-detect", {
    # Was: year 56008 with advice to choose an Excel format; milliseconds never named.
    ms <- data.frame(x = c(1705276800000, 1704067199000))
    n <- dtc_notices(ms, "x", datetime_format = "unix_epoch")
    expect_match(n, "Unix epoch MILLISECONDS", fixed = TRUE)
    expect_match(n, "Select Auto-detect", fixed = TRUE)
    expect_false(grepl("Excel serial numbers", n, fixed = TRUE))
    # an Excel-serial column keeps the Excel advice
    xl <- dtc_notices(data.frame(x = c(45306, 45291)), "x", datetime_format = "unix_epoch")
    expect_match(xl, "If these are Excel serial numbers", fixed = TRUE)
})
