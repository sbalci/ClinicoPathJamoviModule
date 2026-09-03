# Regressions for the 2026-09 timeinterval audit remediation.
#
#  1. The "multiplier x 99th percentile" extreme rule is inert for small n
#     (type-7 quantile() folds the maximum into its own threshold). The rule is
#     unchanged by design; the analysis must now SAY so, with the threshold and
#     the count, instead of reporting "0 removed" as if the data were screened.
#  2. lubridate pivots two-digit years at 68 (22/09/68 -> 2068). Any parsed
#     value written with a two-digit year must be disclosed with a count.
#  3. The appended column title moved from sprintf() to .fmt(.()) -- same text.
#
# The private methods are taken off the R6 generator with sys.source() and run
# against a recording stub, so this file needs neither jamovi nor an installed
# ClinicoPath. Skips only when R/timeinterval.b.R itself is absent.

library(testthat)

.tisrc <- function(file) {
    for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
        if (file.exists(p)) return(p)
    testthat::skip(paste0("R/", file, " not available (installed-package check)"))
}

# .fmt() lives in R/utils.R; use the real one when present, a glue shim otherwise.
.ti_fmt <- function() {
    for (p in c("../../R/utils.R", "../R/utils.R", "R/utils.R")) {
        if (file.exists(p)) {
            e <- new.env(parent = globalenv())
            suppressWarnings(suppressMessages(sys.source(p, envir = e)))
            if (is.function(e$.fmt)) return(e$.fmt)
        }
    }
    function(.format_string, ...) {
        vals <- list(...)
        for (nm in names(vals))
            .format_string <- gsub(paste0("{", nm, "}"), as.character(vals[[nm]]),
                                   .format_string, fixed = TRUE)
        .format_string
    }
}

# Build a stub `self`/`private` environment and bind the shipped private
# methods to it. Returns the bound method table plus the recorder.
.ti_harness <- function(options) {
    src <- .tisrc("timeinterval.b.R")
    e <- new.env(parent = globalenv())
    suppressWarnings(suppressMessages(sys.source(src, envir = e)))
    pm <- e$timeintervalClass$private_methods
    pf <- e$timeintervalClass$private_fields

    rec <- new.env(parent = emptyenv())
    rec$content <- list(); rec$titles <- list(); rec$rownums <- NULL; rec$values <- NULL

    item <- function(nm) list(
        setContent  = function(x) { rec$content[[nm]] <- paste(x, collapse = " "); invisible(NULL) },
        setTitle    = function(x) { rec$titles[[nm]] <- x; invisible(NULL) },
        setVisible  = function(...) invisible(NULL),
        isNotFilled = function() TRUE,
        setRowNums  = function(x) { rec$rownums <- x; invisible(NULL) },
        setValues   = function(x) { rec$values <- x; invisible(NULL) }
    )

    stub <- new.env(parent = globalenv())
    stub$. <- function(x, ...) x
    stub$.fmt <- .ti_fmt()
    stub$self <- list(
        options = options,
        data    = options$data,
        results = stats::setNames(lapply(
            c("messages", "todo", "aboutPanel", "personTimeInfo", "qualityAssessment",
              "caveatsPanel", "summary", "nlSummary", "glossaryPanel", "calculated_time"),
            item),
            c("messages", "todo", "aboutPanel", "personTimeInfo", "qualityAssessment",
              "caveatsPanel", "summary", "nlSummary", "glossaryPanel", "calculated_time"))
    )
    priv <- new.env(parent = emptyenv())
    for (nm in names(pf)) assign(nm, pf[[nm]], envir = priv)
    for (nm in names(pm)) { f <- pm[[nm]]; environment(f) <- stub; assign(nm, f, envir = priv) }
    priv$.checkpoint <- function(...) invisible(NULL)
    stub$private <- priv
    list(p = priv, rec = rec)
}

.ti_opts <- function(df, ...) {
    o <- list(dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months",
              time_basis = "standardized", use_landmark = FALSE, landmark_time = 0,
              remove_negative = FALSE, remove_extreme = FALSE, extreme_multiplier = 2,
              timezone = "system", confidence_level = 95, include_quality_metrics = TRUE,
              show_summary = FALSE, show_glossary = FALSE, calculated_time = FALSE)
    o[names(list(...))] <- list(...)
    o$data <- df
    o
}

.ti_run <- function(df, ...) {
    h <- .ti_harness(.ti_opts(df, ...))
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE)
    suppressWarnings(h$p$.run())
    h
}

strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
MONTH_DAYS <- 365.25 / 12

# ------------------------------------------------------------- extreme rule --

test_that("small-n extreme rule: says it cannot act, with threshold and 0 removed", {
    # n = 30 at 2x: q99 sits between the 29th and 30th order statistics, so the
    # lone absurd date inflates its own threshold and survives. Was: "0 removed",
    # silently. Now: a banner and a filter line naming n, the threshold and the count.
    df <- data.frame(s = rep("2020-01-01", 30),
                     e = c(rep("2020-07-01", 29), "2400-01-01"), stringsAsFactors = FALSE)
    ref <- as.numeric(difftime(lubridate::ymd(df$e), lubridate::ymd(df$s), units = "days")) / MONTH_DAYS
    expect_lt(max(ref), 2 * stats::quantile(ref, 0.99, names = FALSE))   # the property

    h <- .ti_run(df, remove_extreme = TRUE)
    msgs <- strip_html(h$rec$content$messages)
    summ <- strip_html(h$rec$content$summary)

    expect_match(summ, "Number of observations: 30")          # rule unchanged: nothing removed
    expect_match(msgs, "Extreme-value filtering cannot act on these data")
    expect_match(msgs, "with 30 intervals")
    expect_match(msgs, "none was removed")
    expect_match(msgs, "threshold of [0-9.]+ months")
    expect_match(summ, "Extreme-value filtering cannot act")   # also in "Filters applied"
})

test_that("inert-rule boundary follows multiplier x (101 - n) >= 100", {
    mk <- function(n) data.frame(s = rep("2020-01-01", n),
                                 e = c(rep("2020-07-01", n - 1), "2400-01-01"),
                                 stringsAsFactors = FALSE)
    # n = 51 at 2x: 2 * 50 = 100 -> inert
    h51 <- .ti_run(mk(51), remove_extreme = TRUE)
    expect_match(strip_html(h51$rec$content$messages), "cannot act on these data")
    expect_match(strip_html(h51$rec$content$summary), "Number of observations: 51")
    # n = 52 at 2x: 2 * 49 = 98 -> the maximum CAN be flagged, and here it is
    h52 <- .ti_run(mk(52), remove_extreme = TRUE)
    expect_false(grepl("cannot act on these data", strip_html(h52$rec$content$messages)))
    expect_match(strip_html(h52$rec$content$summary), "Number of observations: 51")
    expect_match(strip_html(h52$rec$content$summary), "1 extreme interval\\(s\\) removed")
    # and the at-most-one-row limitation for n < 101 is stated
    expect_match(strip_html(h52$rec$content$summary), "at most the single longest one")
})

test_that("a working extreme filter that removes nothing still states its threshold", {
    df <- data.frame(s = rep("2020-01-01", 150), e = rep("2020-07-01", 150),
                     stringsAsFactors = FALSE)
    df$e[1:5] <- "2020-09-01"
    h <- .ti_run(df, remove_extreme = TRUE)
    summ <- strip_html(h$rec$content$summary)
    expect_match(summ, "No interval exceeded the extreme threshold of [0-9.]+ months \\(0 removed\\)")
    expect_false(grepl("cannot act", summ))
    expect_false(grepl("at most the single longest", summ))   # n >= 101
})

test_that("q99 <= 0 skip reason keeps its wording and now also raises a banner", {
    n0 <- 200
    df <- data.frame(s = rep("2020-01-01", n0 + 2),
                     e = c(rep("2020-01-01", n0), "2020-07-01", "2021-01-01"),
                     stringsAsFactors = FALSE)
    h <- .ti_run(df, remove_extreme = TRUE)
    expect_match(strip_html(h$rec$content$summary), "Extreme-value filtering was skipped")
    expect_match(strip_html(h$rec$content$summary), "99th percentile of the intervals is 0")
    expect_match(strip_html(h$rec$content$messages), "Extreme-value filtering was skipped")
})

# --------------------------------------------------------- two-digit years --

test_that(".countTwoDigitYears catches text and packed-numeric two-digit years only", {
    h <- .ti_harness(.ti_opts(data.frame(s = "2020-01-01", e = "2020-02-01")))
    cnt <- h$p$.countTwoDigitYears
    expect_equal(cnt(c("22/09/68", "22/09/1968"), lubridate::dmy(c("22/09/68", "22/09/1968"))), 1L)
    expect_equal(cnt(c("2020-01-15", NA), lubridate::ymd(c("2020-01-15", NA))), 0L)
    expect_equal(cnt(c(200115, 20200115), lubridate::ymd(c(200115, 20200115))), 1L)
    expect_equal(cnt(factor(c("15/01/20", "15/01/2020")), lubridate::dmy(c("15/01/20", "15/01/2020"))), 1L)
    expect_equal(cnt(character(0), lubridate::ymd(character(0))), 0L)
})

test_that("a two-digit-year column raises one disclosure with the count across both columns", {
    expect_equal(lubridate::year(lubridate::dmy("22/09/68")), 2068)   # the pivot
    df <- data.frame(s = c("22/09/68", "01/01/2010", "05/05/2011"),
                     e = c("22/09/2069", "01/06/10", "05/05/2012"), stringsAsFactors = FALSE)
    h <- .ti_run(df, time_format = "dmy", remove_negative = TRUE)
    msgs <- strip_html(h$rec$content$messages)
    expect_match(msgs, "2 date value\\(s\\) were written with a two-digit year")
    expect_match(msgs, "1969-1999")

    h4 <- .ti_run(data.frame(s = c("2020-01-01", "2020-02-01"), e = c("2020-06-01", "2020-07-01"),
                             stringsAsFactors = FALSE))
    expect_false(grepl("two-digit year", strip_html(h4$rec$content$messages)))
})

# ---------------------------------------------------------- output column --

test_that("calculated_time column title carries unit and landmark, rows aligned to retained rows", {
    df <- data.frame(s = rep("2020-01-01", 4),
                     e = c("2020-02-01", "2020-07-01", "2021-01-01", "2021-07-01"),
                     stringsAsFactors = FALSE)
    rownames(df) <- c("10", "20", "30", "40")
    h <- .ti_run(df, use_landmark = TRUE, landmark_time = 3)
    expect_equal(h$rec$titles$calculated_time, "Calculated Time (months, from 3 months landmark)")
    expect_equal(h$rec$rownums, c("20", "30", "40"))
    expect_length(h$rec$values, 3)
    expect_true(all(h$rec$values >= 0))

    h0 <- .ti_run(df)
    expect_equal(h0$rec$titles$calculated_time, "Calculated Time (months)")
    expect_equal(h0$rec$rownums, rownames(df))
})
