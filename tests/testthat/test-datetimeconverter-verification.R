library(testthat)

# Numerical verification for `datetimeconverter()`.
#
# The previous version of this file wrote the correct answers in comments
# ("45000 ... corresponds to 2023-03-15", "1700000000 = 2023-11-14 22:13:20 UTC")
# and then asserted only `expect_true(!is.null(res$previewTable))` -- so none of
# those stated facts was ever checked. They are checked here.
#
# NOTE ON format(): ClinicoPath does a blanket `import(jmvcore)`, and jmvcore
# exports its own format() which ignores format strings. Always use base::format
# in this file (and in the package -- see R/datetimeconverter.b.R).

fmt <- base::format

quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}

# `Output`-type options never reach the public wrapper, so drive the analysis
# object the way jamovi does in order to observe the converted vector itself.
dtc_parsed <- function(data, var, ...) {
    opts <- datetimeconverterOptions$new(datetime_var = var, ...)
    an <- datetimeconverterClass$new(options = opts, data = data)
    quietly(an$run())
    p <- an$.__enclos_env__$private
    prep <- p$.prepareDatetimeInput(data[[var]])
    if (isTRUE(prep$already_parsed)) return(prep$parsed_dates)
    tzr <- p$.resolveTimezone()
    f <- opts$datetime_format
    if (identical(f, "auto")) f <- p$.detectDatetimeFormat(prep$parsing_vector)$format
    if (identical(f, "unsure")) return(NULL)
    suppressWarnings(p$.parseDatetime(prep$parsing_vector, f, tz = tzr$tz))
}

test_that("Excel serial numbers convert on the 1899-12-30 origin", {
    pv <- dtc_parsed(data.frame(s = c(45000, 45001, 61)), "s",
                     datetime_format = "excel_serial")

    expect_s3_class(pv, "POSIXct")
    expect_equal(attr(pv, "tzone"), "UTC")
    # The value the old test only claimed in a comment:
    expect_equal(fmt(pv[1], "%Y-%m-%d", tz = "UTC"), "2023-03-15")
    expect_equal(fmt(pv[2], "%Y-%m-%d", tz = "UTC"), "2023-03-16")
    # Serial 61 is 1900-03-01: Excel's phantom 29 Feb 1900 (serial 60) is what
    # makes the 1899-12-30 origin correct from here onward.
    expect_equal(fmt(pv[3], "%Y-%m-%d", tz = "UTC"), "1900-03-01")

    ref <- as.POSIXct(c(45000, 45001, 61) * 86400, origin = "1899-12-30", tz = "UTC")
    expect_equal(as.numeric(pv), as.numeric(ref))
})

test_that("Unix epoch round-trips exactly and lands on the right UTC instant", {
    input <- c(1609459200, 1700000000)
    pv <- dtc_parsed(data.frame(e = input), "e", datetime_format = "unix_epoch")

    expect_equal(as.numeric(pv), input)
    expect_equal(fmt(pv[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2021-01-01 00:00:00")
    expect_equal(fmt(pv[2], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2023-11-14 22:13:20")
})

test_that("the numeric output is epoch SECONDS, not days", {
    # A Date (rather than POSIXct) would make as.numeric() return days and every
    # downstream survival time would be wrong by a factor of 86400.
    pv <- dtc_parsed(data.frame(d = c("2021-01-01 00:00:00", "2021-01-02 00:00:00")),
                     "d", datetime_format = "ymdhms", timezone = "UTC")
    expect_s3_class(pv, "POSIXct")
    expect_equal(diff(as.numeric(pv)), 86400)
    expect_equal(as.numeric(pv)[1], 1609459200)
})

test_that("the epoch output depends on the Timezone option, as documented", {
    # Regression guard on a documented fact, not a bug: a date with no time is
    # midnight IN THE SELECTED ZONE. The .a.yaml used to claim this output was
    # "timezone-independent", which it is not.
    df <- data.frame(d = "2024-01-15")
    utc <- as.numeric(dtc_parsed(df, "d", datetime_format = "ymd", timezone = "UTC"))
    ist <- as.numeric(dtc_parsed(df, "d", datetime_format = "ymd", timezone = "Europe/Istanbul"))
    nyc <- as.numeric(dtc_parsed(df, "d", datetime_format = "ymd", timezone = "America/New_York"))

    expect_equal(utc, as.numeric(as.POSIXct("2024-01-15", tz = "UTC")))
    expect_equal(ist, as.numeric(as.POSIXct("2024-01-15", tz = "Europe/Istanbul")))
    expect_equal(nyc, as.numeric(as.POSIXct("2024-01-15", tz = "America/New_York")))
    expect_false(utc == ist)
    expect_equal(utc - ist, 10800)   # Istanbul is UTC+3
})

test_that("an unrecognised timezone falls back to system with a warning", {
    opts <- datetimeconverterOptions$new(datetime_var = "d", datetime_format = "ymd",
                                         timezone = "Mars/Olympus_Mons")
    an <- datetimeconverterClass$new(options = opts, data = data.frame(d = "2024-01-15"))
    quietly(an$run())
    expect_match(strip_html(an$results$notices$content), "not a recognised Olson timezone")
})

test_that("datetime components match lubridate", {
    df <- data.frame(d = c("2021-01-01 13:45:30", "2024-02-29 00:00:00", "2020-12-31 23:59:59"))
    opts <- datetimeconverterOptions$new(
        datetime_var = "d", datetime_format = "ymdhms", timezone = "UTC",
        extract_year = TRUE, extract_month = TRUE, extract_day = TRUE,
        extract_hour = TRUE, extract_minute = TRUE, extract_second = TRUE,
        extract_quarter = TRUE, extract_dayofyear = TRUE, extract_weeknum = TRUE)
    an <- datetimeconverterClass$new(options = opts, data = df)
    quietly(an$run())
    pv <- dtc_parsed(df, "d", datetime_format = "ymdhms", timezone = "UTC")
    comp <- an$.__enclos_env__$private$.extractComponents(pv)

    ref <- as.POSIXct(df$d, tz = "UTC")
    expect_equal(as.numeric(comp$year),      as.numeric(lubridate::year(ref)))
    expect_equal(as.numeric(comp$month),     as.numeric(lubridate::month(ref)))
    expect_equal(as.numeric(comp$day),       as.numeric(lubridate::day(ref)))
    expect_equal(as.numeric(comp$hour),      as.numeric(lubridate::hour(ref)))
    expect_equal(as.numeric(comp$minute),    as.numeric(lubridate::minute(ref)))
    expect_equal(as.numeric(comp$second),    as.numeric(lubridate::second(ref)))
    expect_equal(as.numeric(comp$quarter),   as.numeric(lubridate::quarter(ref)))
    # leap day is day 60 of 2024; 2020 is a leap year so 31 Dec is day 366
    expect_equal(as.numeric(comp$dayofyear), c(1, 60, 366))
})

test_that("week number is lubridate::week, not the ISO week (documented)", {
    df <- data.frame(d = c("2021-01-01", "2024-02-29"))
    opts <- datetimeconverterOptions$new(datetime_var = "d", datetime_format = "ymd",
                                         timezone = "UTC", extract_weeknum = TRUE)
    an <- datetimeconverterClass$new(options = opts, data = df)
    quietly(an$run())
    pv <- dtc_parsed(df, "d", datetime_format = "ymd", timezone = "UTC")
    comp <- an$.__enclos_env__$private$.extractComponents(pv)
    ref <- as.POSIXct(df$d, tz = "UTC")

    expect_equal(as.numeric(comp$weeknum), as.numeric(lubridate::week(ref)))
    # and it genuinely differs from ISO at the turn of the year
    expect_equal(as.numeric(comp$weeknum)[1], 1)
    expect_equal(as.numeric(lubridate::isoweek(ref))[1], 53)
})

test_that("an impossible calendar date becomes NA", {
    pv <- dtc_parsed(data.frame(d = c("2024-02-29", "2023-02-29", "2024-03-31")),
                     "d", datetime_format = "ymd", timezone = "UTC")
    expect_false(is.na(pv[1]))   # 2024 is a leap year
    expect_true(is.na(pv[2]))    # 2023 is not
    expect_false(is.na(pv[3]))
})

test_that("ambiguous day/month order is chosen day-first AND flagged", {
    df <- data.frame(d = c("01/02/2024", "03/04/2024", "05/06/2024"))
    opts <- datetimeconverterOptions$new(datetime_var = "d", datetime_format = "auto")
    an <- datetimeconverterClass$new(options = opts, data = df)
    quietly(an$run())

    pv <- dtc_parsed(df, "d", datetime_format = "auto")
    # Parsed in the default (system) timezone, so read it back in that zone:
    # the same instant rendered in UTC would show the previous calendar day.
    ptz <- attr(pv, "tzone"); if (is.null(ptz) || !nzchar(ptz)) ptz <- Sys.timezone()
    expect_equal(fmt(pv, "%Y-%m-%d", tz = ptz),
                 fmt(lubridate::dmy(df$d), "%Y-%m-%d"))
    expect_match(strip_html(an$results$notices$content), "Ambiguous day/month order")
})

# ---------------------------------------------------------------- regressions

test_that("valid dates do NOT raise a false 'Implausible Dates' warning", {
    # Regression: a bare format() resolved to jmvcore::format, which ignores the
    # "%Y" format string. as.integer() of the whole datetime string produced the
    # epoch seconds (1710498030 for 2024-03-15) and the upper bound came out as
    # 21033, so EVERY ordinary date compared "> 21033" and this warning fired on
    # 100%-successful conversions.
    opts <- datetimeconverterOptions$new(datetime_var = "d", datetime_format = "ymdhms")
    an <- datetimeconverterClass$new(
        options = opts,
        data = data.frame(d = c("2024-03-15 10:20:30", "2022-01-01 00:00:00")))
    quietly(an$run())
    expect_false(grepl("Implausible", strip_html(an$results$notices$content)))
})

test_that("genuinely implausible dates still DO raise the warning", {
    # The fix must not turn a noisy check into a dead one.
    run_one <- function(dates) {
        opts <- datetimeconverterOptions$new(datetime_var = "d", datetime_format = "ymd")
        an <- datetimeconverterClass$new(options = opts, data = data.frame(d = dates))
        quietly(an$run())
        grepl("Implausible", strip_html(an$results$notices$content))
    }
    expect_true(run_one(c("1850-06-01", "2024-03-15")))   # before 1900
    expect_true(run_one(c("2100-06-01", "2024-03-15")))   # far future
    expect_false(run_one(c("2022-01-01", "2024-03-15")))  # ordinary
})

test_that("large numeric inputs are shown in full, not scientific notation", {
    # Regression: format(x, scientific = FALSE) also hit jmvcore::format, which
    # ignores the argument -- so a Unix epoch appeared in the "Original Value"
    # column as "1.7e+09". In the numeric fallback branch that same string is
    # what gets PARSED, so a mangled number becomes an unparseable date.
    res <- quietly(datetimeconverter(
        data = data.frame(e = c(1700000000, 1700086400)),
        datetime_var = "e", datetime_format = "unix_epoch", preview_rows = 10))
    preview <- strip_html(res$previewTable$content)
    expect_match(preview, "1700000000", fixed = TRUE)
    expect_false(grepl("1.7e+09", preview, fixed = TRUE))
})

test_that("YYYYMMDD-style numeric input survives the string fallback", {
    # The numeric fallback stringifies before parsing; scientific notation there
    # would silently turn every value into NA.
    pv <- dtc_parsed(data.frame(d = c(20240315, 20231120)), "d", datetime_format = "ymd")
    tzone <- attr(pv, "tzone")
    if (is.null(tzone) || !nzchar(tzone)) tzone <- Sys.timezone()
    expect_equal(fmt(pv, "%Y-%m-%d", tz = tzone), c("2024-03-15", "2023-11-20"))
})
