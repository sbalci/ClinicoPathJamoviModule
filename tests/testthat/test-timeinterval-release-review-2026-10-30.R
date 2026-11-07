library(testthat)

# Independent review 2026-10-30, findings TI-1 to TI-5. Each block states what the analysis
# did before. The grid shared with datetimeconverter is in
# test-datetimeconverter-release-review-2026-10-30.R.
# Use base::format: jmvcore exports a format() that ignores format strings.

fmt <- base::format
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
ti <- function(s, e, ...) {
    r <- tryCatch(suppressWarnings(suppressMessages(ClinicoPath::timeinterval(
        data = data.frame(s = s, e = e, stringsAsFactors = FALSE),
        dx_date = "s", fu_date = "e", ...))), error = function(err) err)
    if (inherits(r, "error")) return(list(err = conditionMessage(r)))
    list(r = r, vals = r$calculated_time$.__enclos_env__$private$.values[[1]],
         msg = strip_html(r$messages$content), sum = strip_html(r$summary$content))
}

test_that("TI-1: DD/MM/YYYY HH:MM is read as written, with no negative row to give it away", {
    # Was: ymd_hms read 15/01/2020 14:30 as 2015-01-20 20:14:30; mean 149.4 months against a
    # truth of 16.9, with a copy-ready sentence and no warning naming the format.
    set.seed(11)
    n <- 40
    s0 <- as.POSIXct(sprintf("%d-%02d-%02d 09:30:00", sample(2018:2021, n, TRUE),
                             sample(1:12, n, TRUE), sample(1:9, n, TRUE)), tz = "UTC")
    f0 <- s0 + sample(200:800, n, TRUE) * 86400
    truth <- as.numeric(difftime(f0, s0, units = "days"))
    a <- ti(fmt(s0, "%d/%m/%Y %H:%M"), fmt(f0, "%d/%m/%Y %H:%M"), output_unit = "days")
    expect_null(a$err)
    expect_equal(a$vals, truth, tolerance = 1e-6)
    expect_match(a$sum, "Date format used: dmy_hm", fixed = TRUE)
})

test_that("TI-1: explicit ymdhms is refused on text that is not year-first", {
    # Was: the same wrong number as auto.
    x <- fmt(as.POSIXct("2019-02-15 08:00", tz = "UTC") + (1:20) * 86400, "%d/%m/%Y %H:%M")
    a <- ti(x, x, time_format = "ymdhms")
    expect_match(a$err, "ymdhms", fixed = TRUE)
    expect_match(a$err, "cannot have the date format used", fixed = TRUE)
})

test_that("TI-1: a day-first column keeps its mixed-order check when it carries a time", {
    # dmy_hm must not fall out of the only check that sees mis-typed rows
    p <- ClinicoPath:::timeintervalClass$new(
        options = ClinicoPath:::timeintervalOptions$new(dx_date = "s", fu_date = "e"),
        data = data.frame(s = "01/02/2020 10:00", e = "03/04/2021 10:00"))
    priv <- p$.__enclos_env__$private
    start <- .parseDateText("01/02/2020 10:00", "dmy_hm")
    end <- .parseDateText("03/04/2021 10:00", "dmy_hm")
    priv$.checkMixedDateOrder("01/02/2020 10:00", "03/04/2021 10:00", "dmy_hm", start, end)
    expect_identical(priv$.ambiguousExposure$swap, "mdy_hm")
})

test_that("TI-2: YYYY-MM spans the years written and says the day is not data", {
    # Was: ydm put every row in 2020 with the year's last digits as the day; mean 4.3
    # against 15.1 under remove_negative, right in a one-year pilot.
    s <- c("2019-02", "2019-11", "2020-03", "2021-07", "2018-12")
    e <- c("2020-10", "2021-01", "2020-09", "2022-02", "2021-03")
    a <- ti(s, e, output_unit = "days")
    truth <- as.numeric(as.Date(paste0(e, "-01")) - as.Date(paste0(s, "-01")))
    expect_equal(a$vals, truth)
    expect_match(a$msg, "10 date value(s) give a month and a year but no day", fixed = TRUE)
    expect_false(grepl("Overall Quality: Good", strip_html(
        ti(s, e, include_quality_metrics = TRUE)$r$qualityAssessment$content), fixed = TRUE))
})

test_that("TI-2: a partial value in a full-date column is empty, and named", {
    set.seed(3)
    d <- as.Date("2019-01-01") + sample(600, 20)
    s <- as.character(d); s[3] <- "2019-02"
    a <- ti(s, as.character(d + 100), output_unit = "days")
    expect_true(is.na(a$vals[3]))
    expect_match(a$msg, "1 date value(s) were left empty", fixed = TRUE)
    expect_match(a$msg, "2019-02", fixed = TRUE)
})

test_that("TI-3: the unit and landmark stay in the column name on a rerun that keeps it", {
    # Was: the title was set inside isNotFilled(), so one harmless click put the yaml
    # default back and jamovi renamed the column.
    set.seed(9)
    s0 <- as.Date("2019-01-01") + sample(700, 60, TRUE)
    dat <- data.frame(s = as.character(s0), e = as.character(s0 + sample(1:900, 60, TRUE)))
    mk <- function() {
        a <- ClinicoPath:::timeintervalClass$new(
            options = ClinicoPath:::timeintervalOptions$new(
                dx_date = "s", fu_date = "e", time_format = "ymd",
                use_landmark = TRUE, landmark_time = 6),
            data = dat)
        a$init()
        a
    }
    first <- mk(); suppressWarnings(first$run())
    again <- mk()
    p <- again$results$calculated_time$.__enclos_env__$private
    p$.stales <- FALSE
    suppressWarnings(again$run())
    expect_match(p$.titles, "landmark", fixed = TRUE)
    expect_identical(p$.titles, first$results$calculated_time$.__enclos_env__$private$.titles)
})

test_that("TI-4: an unsettled day/month order is not Good and gets no copy-ready sentence", {
    # Was: rows wrong by up to 295 days, the mean barely moved, "Overall Quality: Good" and a
    # sentence to paste.
    set.seed(4)
    n <- 30
    ss <- as.Date(sprintf("%d-%02d-01", sample(2015:2020, n, TRUE), sample(1:12, n, TRUE)))
    ee <- as.Date(mapply(function(a, k) as.character(seq(a, by = "month", length.out = k)[k]),
                         ss, sample(3:40, n, TRUE)))
    a <- ti(fmt(ss, "%m/%d/%Y"), fmt(ee, "%m/%d/%Y"),
            include_quality_metrics = TRUE, show_summary = TRUE)
    expect_false(grepl("Overall Quality: Good", strip_html(a$r$qualityAssessment$content), fixed = TRUE))
    expect_match(strip_html(a$r$nlSummary$content), "withheld", fixed = TRUE)
})

test_that("TI-4: ISO dates with every day at or under 12 are not called unsettled", {
    # ymd and ydm both fit them, and nobody writes YYYY-DD-MM.
    s <- c("2020-01-05", "2020-02-10", "2021-03-01", "2019-11-12", "2018-06-07")
    e <- c("2021-01-05", "2021-02-10", "2022-03-01", "2020-11-12", "2019-06-07")
    a <- ti(s, e, include_quality_metrics = TRUE, show_summary = TRUE)
    expect_false(grepl("ambiguous", a$msg, fixed = TRUE))
    expect_false(grepl("withheld", strip_html(a$r$nlSummary$content), fixed = TRUE))
})

test_that("TI-5: a clock time in a daylight-saving gap is named, not just missing", {
    # Was: counted under "Missing values" with no mention of the cause.
    old <- Sys.getenv("TZ")
    Sys.setenv(TZ = "America/New_York")
    on.exit(if (nzchar(old)) Sys.setenv(TZ = old) else Sys.unsetenv("TZ"))
    a <- ti(c("2020-03-08 02:30:00", rep("2020-01-01 10:00:00", 19)),
            rep("2020-06-01 10:00:00", 20), output_unit = "days")
    expect_true(is.na(a$vals[1]))
    expect_match(a$msg, "does not exist", fixed = TRUE)
    expect_match(a$msg, "2020-03-08 02:30:00", fixed = TRUE)
})

test_that("the utc notice is silent for a day-first time format, which it does reach", {
    x <- fmt(as.POSIXct("2019-02-15 08:00", tz = "UTC") + (1:20) * 86400, "%d/%m/%Y %H:%M")
    a <- ti(x, x, timezone = "utc")
    expect_false(grepl("timezone has no effect", a$msg, fixed = TRUE))
})

test_that("the default time basis gives calendar months, as the survival analyses compute them", {
    d <- data.frame(s = c("2016-01-15", "2019-02-01"), e = c("2017-01-15", "2019-03-01"),
                    stringsAsFactors = FALSE)
    a <- timeintervalClass$new(options = timeintervalOptions$new(dx_date = "s", fu_date = "e",
                                                                 time_format = "ymd",
                                                                 output_unit = "months"), data = d)
    suppressWarnings(suppressMessages(a$run()))
    expect_equal(a$options$time_basis, "calendar")
    expect_match(paste(a$results$summary$content, collapse = " "), "Calendar-aware", fixed = TRUE)
    calc <- a$.__enclos_env__$private$.calculateCalendarIntervals(as.Date(d$s), as.Date(d$e), "months")
    expect_equal(calc, c(12, 1))
})
