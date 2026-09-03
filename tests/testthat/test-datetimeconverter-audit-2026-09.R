# Regressions for the 2026-09 audit pass on datetimeconverter.
#
# 1  "Corrected DateTime (Text)" and the preview used as.character(<POSIXct>),
#    which on R >= 4.3 drops the clock for midnight rows -> mixed
#    "2024-01-15" / "2024-01-15 10:30:00" in one "standardized" column.
# 2  The "format ignored because the column is already stored as datetime
#    values" note also fired for a NUMERIC column auto-read as an Excel serial.
# 3  "text format selected but column is numeric, choose Excel/Unix" fired for a
#    compact yyyymmdd integer column that `ymd` parses at 100%.
# 4  Date-class input became UTC midnight with the Timezone option ignored.
# 5  "Implausible Dates Detected" carried no count and no example.
#
# The private helpers are taken off the R6 generator and run against a stub
# self/private, so the assertions are about the shipped code. Every test runs
# unconditionally when R/datetimeconverter.b.R is present; under an
# installed-package check the source is gone and the file skips.

library(testthat)

.dtc_src <- function() {
  for (p in c("../../R/datetimeconverter.b.R", "../R/datetimeconverter.b.R",
              "R/datetimeconverter.b.R"))
    if (file.exists(p)) return(p)
  testthat::skip("R/datetimeconverter.b.R not available (installed-package check)")
}

# Real methods, stubbed self/private. `rec$notices` records .addNotice() calls.
.dtc_stub <- function(data = data.frame(v = 1), options = list()) {
  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(.dtc_src(), envir = e)))
  pm <- e$datetimeconverterClass$private_methods

  rec <- new.env(parent = emptyenv())
  rec$notices <- list()

  stub <- new.env(parent = globalenv())
  bind <- function(f) { environment(f) <- stub; f }
  stub$. <- function(x, ...) x                       # translation shim
  stub$.fmt <- function(s, ...) {                    # {name} interpolation shim
    for (nm in names(list(...)))
      s <- gsub(paste0("{", nm, "}"), as.character(list(...)[[nm]]), s, fixed = TRUE)
    s
  }
  stub$`%||%` <- function(a, b) if (is.null(a)) b else a
  opts <- utils::modifyList(list(datetime_var = "v", datetime_format = "auto",
                                 timezone = "system"), options)
  stub$self <- list(options = opts, data = data)
  stub$private <- list(
    .addNotice = function(type, title, content, class = NULL) {
      rec$notices[[length(rec$notices) + 1L]] <- list(type = type, title = title,
                                                     content = content)
      invisible(NULL)
    },
    .resolveTimezone      = bind(pm$.resolveTimezone),
    .getParser            = bind(pm$.getParser),
    .processNumericVector = bind(pm$.processNumericVector),
    .prepareDatetimeInput = bind(pm$.prepareDatetimeInput),
    .parseDatetime        = bind(pm$.parseDatetime),
    .detectMisuse         = bind(pm$.detectMisuse),
    .datetimeText         = bind(pm$.datetimeText),
    .safeCharacterConversion = bind(pm$.safeCharacterConversion),
    .generatePreviewTable = bind(pm$.generatePreviewTable)
  )
  list(p = stub$private, rec = rec)
}

quietly <- function(expr) {
  f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}


# 1 -- midnight rows keep the clock -------------------------------------------

test_that("the text column keeps the clock on midnight rows and NA stays NA", {
  s <- .dtc_stub()
  x <- as.POSIXct(c("2024-01-15 00:00:00", "2024-01-15 10:30:00", NA), tz = "UTC")
  # the defect: element-wise as.character() drops the clock on the midnight row
  expect_equal(as.character(x)[1], "2024-01-15")
  expect_equal(s$p$.datetimeText(x),
               c("2024-01-15 00:00:00", "2024-01-15 10:30:00", NA))
  expect_equal(s$p$.safeCharacterConversion(s$p$.datetimeText(x)),
               c("2024-01-15 00:00:00", "2024-01-15 10:30:00", NA))
  # a downstream re-parse works on every non-missing row
  expect_false(anyNA(lubridate::ymd_hms(s$p$.datetimeText(x)[1:2])))
})

test_that("the text column is written in the timezone the conversion used", {
  s <- .dtc_stub()
  x <- as.POSIXct("2024-01-15 00:00:00", tz = "Europe/Istanbul")
  expect_equal(s$p$.datetimeText(x), "2024-01-15 00:00:00")
  expect_equal(s$p$.datetimeText(lubridate::with_tz(x, "UTC")), "2024-01-14 21:00:00")
})

test_that("the preview shows the full clock on a midnight row", {
  s <- .dtc_stub()
  x <- as.POSIXct(c("2024-01-15 00:00:00", "2024-01-15 10:30:00"), tz = "UTC")
  html <- s$p$.generatePreviewTable(c("2024-01-15", "2024-01-15 10:30"), x, n = 2)
  expect_match(html, "2024-01-15 00:00:00", fixed = TRUE)
})


# 2 -- "already stored as datetime values" only for POSIXct/Date --------------

test_that("a numeric column read as an Excel serial is not 'stored as datetime values'", {
  # Prerequisite the gate depends on: the numeric branch returns already_parsed
  # with a serial hint when a text format is selected, so an ungated
  # `format_hint != datetime_format` fires for it.
  s <- .dtc_stub(data = data.frame(v = c(44197, 44562, 45000)),
                 options = list(datetime_format = "ymd"))
  prep <- quietly(s$p$.prepareDatetimeInput(c(44197, 44562, 45000)))
  expect_true(prep$already_parsed)
  expect_equal(prep$format_hint, "excel_serial")

  # The gate itself lives in .run(); pin it at source level.
  src <- readLines(.dtc_src(), warn = FALSE)
  msg <- grep("was ignored because the column is already stored as datetime values", src)
  gate <- grep('prepared$format_hint %in% c("posixct", "date")', src, fixed = TRUE)
  expect_length(msg, 1)
  expect_true(any(gate < msg & gate >= msg - 5))
})


# 3 -- yyyymmdd integers are not a "numeric column" misuse --------------------

test_that("a compact yyyymmdd integer column parsed by ymd raises no numeric-column warning", {
  v <- c(20240115L, 20240220L, 20240301L, 20240415L)
  s <- .dtc_stub(data = data.frame(v = v), options = list(datetime_format = "ymd"))
  prep <- quietly(s$p$.prepareDatetimeInput(v))
  expect_false(isTRUE(prep$already_parsed))          # handed to the text parser
  parsed <- quietly(s$p$.parseDatetime(prep$parsing_vector, "ymd", tz = "UTC"))
  expect_false(anyNA(parsed))
  expect_length(s$p$.detectMisuse(parsed, prep$format_hint), 0)
})

test_that("the numeric-column warning still fires when the text parser fails or a serial reading took over", {
  v <- c(44197, 44562, 45000, 45100)
  s <- .dtc_stub(data = data.frame(v = v), options = list(datetime_format = "ymd"))
  # serial reading overrode the selection (format_hint set, parse rate 100%)
  prep <- quietly(s$p$.prepareDatetimeInput(v))
  expect_match(s$p$.detectMisuse(prep$parsed_dates, prep$format_hint),
               "column is numeric")
  # text parser mostly failed
  failed <- as.POSIXct(rep(NA_real_, 4), origin = "1970-01-01", tz = "UTC")
  expect_match(s$p$.detectMisuse(failed, NULL), "column is numeric")
  # a NULL parsed vector (defensive path) still warns
  expect_match(s$p$.detectMisuse(NULL, NULL), "column is numeric")
})


# 4 -- Date input honours the Timezone option ---------------------------------

test_that("Date-class input is midnight in the selected timezone, not UTC", {
  d <- as.Date(c("2024-01-15", "2024-06-01"))
  s <- .dtc_stub(data = data.frame(v = d), options = list(timezone = "Europe/Istanbul"))
  prep <- quietly(s$p$.prepareDatetimeInput(d))
  expect_equal(prep$format_hint, "date")
  expect_equal(as.numeric(prep$parsed_dates),
               as.numeric(as.POSIXct(c("2024-01-15", "2024-06-01"), tz = "Europe/Istanbul")))
  expect_equal(base::format(prep$parsed_dates, "%H:%M"), c("00:00", "00:00"))
  expect_equal(as.numeric(prep$parsed_dates)[1], 1705266000)   # was 1705276800 (UTC)

  # system default: local midnight, the same instant the text parser gives
  s2 <- .dtc_stub(data = data.frame(v = d))
  prep2 <- quietly(s2$p$.prepareDatetimeInput(d))
  expect_equal(as.numeric(prep2$parsed_dates), as.numeric(as.POSIXct(c("2024-01-15", "2024-06-01"))))
})


# 5 -- Implausible Dates Detected is quantified -------------------------------

test_that("the implausible-dates notice carries a count, a denominator and the first example", {
  s <- .dtc_stub()
  quietly(s$p$.parseDatetime(c("1850-01-01", "2024-01-01", "2999-12-31", NA), "ymd", tz = "UTC"))
  n <- Filter(function(x) x$title == "Implausible Dates Detected", s$rec$notices)
  expect_length(n, 1)
  expect_match(n[[1]]$content, "2 of 3 parsed date", fixed = TRUE)
  expect_match(n[[1]]$content, "1850-01-01", fixed = TRUE)
})

test_that("valid dates raise no implausible-dates notice", {
  s <- .dtc_stub()
  quietly(s$p$.parseDatetime(c("2022-03-05", "2024-01-01"), "ymd", tz = "UTC"))
  expect_length(s$rec$notices, 0)
})
