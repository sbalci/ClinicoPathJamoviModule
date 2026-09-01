# ═══════════════════════════════════════════════════════════
# Numerical Verification Tests: timeinterval
# ═══════════════════════════════════════════════════════════
#
# These tests compare the intervals the analysis ACTUALLY COMPUTES against
# values derived independently from base R and lubridate. The previous version
# of this file was titled "calculates standardized day, week, month, and year
# intervals correctly" and then asserted only `expect_s3_class(res, ...)`, so no
# interval value was ever checked -- and one test fed a negative interval with
# remove_negative = FALSE, which the backend rejects, yet still passed.
#
# NOTE ON format(): ClinicoPath does a blanket `import(jmvcore)` and jmvcore
# exports its own format(). Use base::format in this file.

library(testthat)

fmt <- base::format
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
# Drive the analysis object the way jamovi does, so private helpers are reachable.
ti_run <- function(df, ...) {
    o <- timeintervalOptions$new(...)
    a <- timeintervalClass$new(options = o, data = df)
    quietly(a$run())
    list(a = a, p = a$.__enclos_env__$private, r = a$results)
}
MONTH_DAYS <- 365.25 / 12   # lubridate duration month = 30.4375 days

test_that("standardized intervals use fixed 30.4375-day months and 365.25-day years", {
    h <- ti_run(data.frame(s = "2020-01-01", e = "2020-01-31", stringsAsFactors = FALSE),
                dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "days")
    s <- lubridate::ymd(c("2020-01-01", "2020-01-01", "2020-01-01"))
    e <- lubridate::ymd(c("2020-01-31", "2020-07-01", "2021-01-01"))
    days <- as.numeric(difftime(e, s, units = "days"))

    expect_equal(h$p$.calculateTimeIntervals(s, e, "days"),   days)
    expect_equal(h$p$.calculateTimeIntervals(s, e, "weeks"),  days / 7)
    expect_equal(h$p$.calculateTimeIntervals(s, e, "months"), days / MONTH_DAYS)
    expect_equal(h$p$.calculateTimeIntervals(s, e, "years"),  days / 365.25)

    # the two standardized units must be mutually consistent
    expect_equal(h$p$.calculateTimeIntervals(s, e, "months") / 12,
                 h$p$.calculateTimeIntervals(s, e, "years"))
})

test_that("calendar-aware months respect actual month lengths and end-of-month rollback", {
    h <- ti_run(data.frame(s = "2020-01-01", e = "2020-02-01", stringsAsFactors = FALSE),
                dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months")
    s <- lubridate::ymd(c("2020-01-31", "2020-01-01", "2021-01-01", "2020-02-29", "2020-01-15"))
    e <- lubridate::ymd(c("2020-02-29", "2020-02-01", "2022-01-01", "2021-02-28", "2020-03-15"))

    got <- h$p$.calculateCalendarIntervals(s, e, "months")
    # Jan 31 -> Feb 29 is one whole calendar month once %m+% rolls back to the
    # last valid day; without the rollback it would be NA.
    expect_equal(got, c(1, 1, 12, 12, 2))
    expect_equal(h$p$.calculateCalendarIntervals(s, e, "years"), c(1, 1, 12, 12, 2) / 12)
    # days/weeks are basis-independent
    expect_equal(h$p$.calculateCalendarIntervals(s, e, "days"),
                 as.numeric(difftime(e, s, units = "days")))
})

test_that("calendar and standardized bases differ, and only where they should", {
    h <- ti_run(data.frame(s = "2020-01-01", e = "2020-02-01", stringsAsFactors = FALSE),
                dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months")
    s <- lubridate::ymd("2020-01-01"); e <- lubridate::ymd("2020-02-01")
    cal <- h$p$.calculateCalendarIntervals(s, e, "months")
    std <- h$p$.calculateTimeIntervals(s, e, "months")
    expect_equal(cal, 1)                       # a whole calendar month
    expect_equal(std, 31 / MONTH_DAYS)         # 31 days / 30.4375
    expect_false(isTRUE(all.equal(cal, std)))
})

test_that("landmark analysis filters at the threshold and re-bases time to zero", {
    h <- ti_run(data.frame(s = "2020-01-01", e = "2020-03-01", stringsAsFactors = FALSE),
                dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months")
    t_raw <- c(1.97, 6.99, 12.02, 24.01)
    res <- h$p$.applyLandmarkAnalysis(t_raw, data.frame(x = 1:4), 6, "months")

    expect_equal(res$time, t_raw[t_raw >= 6] - 6)   # kept AND shifted
    expect_equal(res$final_n, 3L)
    expect_equal(res$below_excluded, 1)
    expect_equal(res$na_excluded, 0)
    # landmark 0 (or NULL) is a no-op
    expect_equal(h$p$.applyLandmarkAnalysis(t_raw, data.frame(x = 1:4), 0, "months")$time, t_raw)
    expect_equal(h$p$.applyLandmarkAnalysis(t_raw, data.frame(x = 1:4), NULL, "months")$time, t_raw)
})

test_that("landmark treats NA follow-up as excluded and counts it separately", {
    h <- ti_run(data.frame(s = "2020-01-01", e = "2020-03-01", stringsAsFactors = FALSE),
                dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months")
    t_raw <- c(2, NA, 10)
    res <- h$p$.applyLandmarkAnalysis(t_raw, data.frame(x = 1:3), 6, "months")
    expect_equal(res$final_n, 1L)
    expect_equal(res$na_excluded, 1)
    expect_equal(res$below_excluded, 1)
    expect_equal(res$excluded_count, 2)
})

test_that("the mean confidence interval matches stats::t.test", {
    h <- ti_run(data.frame(s = "2020-01-01", e = "2020-03-01", stringsAsFactors = FALSE),
                dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months")
    x <- c(3, 7, 11, 15, 20)
    for (lvl in c(90, 95, 99)) {
        got <- h$p$.calculateCI(mean(x), stats::sd(x), length(x), lvl)
        ref <- as.numeric(stats::t.test(x, conf.level = lvl / 100)$conf.int)
        # the untruncated limits must still be exactly the t interval
        expect_equal(got$raw_lower, ref[1], tolerance = 1e-10)
        expect_equal(got$upper, ref[2], tolerance = 1e-10)
        # the REPORTED lower limit is that interval intersected with [0, Inf):
        # a mean duration cannot be negative, and intersecting a CI with a set
        # that contains the true value with probability 1 preserves coverage
        expect_equal(got$lower, max(ref[1], 0), tolerance = 1e-10)
        expect_identical(got$truncated, ref[1] < 0)
    }
    # at 99% this sample's Wald limit is negative, so the clamp must engage
    expect_true(h$p$.calculateCI(mean(x), stats::sd(x), length(x), 99)$truncated)
    expect_true(is.na(h$p$.calculateCI(5, NA, 1, 95)$lower))   # n = 1 is not estimable
})

test_that("negative intervals are refused by default with an actionable message", {
    df <- data.frame(s = c("2020-01-01", "2020-06-01"),
                     e = c("2019-12-01", "2020-08-01"), stringsAsFactors = FALSE)
    r <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", remove_negative = FALSE)
    msg <- strip_html(r$r$messages$content)
    expect_match(msg, "Negative time intervals detected")
    # the message must name the checkbox that actually exists in the UI --
    # it used to say "Remove Negative Intervals", which is not a real label
    expect_match(msg, "Remove negative intervals")   # must quote the control label the GUI shows
    expect_match(msg, "1 of 2 rows")                      # count AND denominator
    expect_match(msg, "2020-01-01")                       # names the offending row
    # and no partial summary is emitted alongside the refusal
    expect_false(nzchar(trimws(strip_html(r$r$summary$content))))

    # with the option on, the negative row is dropped and the rest is analysed
    r2 <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd",
                 output_unit = "months", remove_negative = TRUE)
    expect_match(strip_html(r2$r$summary$content), "Number of observations: 1")
})

test_that("summary statistics match base R on the computed intervals", {
    df <- data.frame(s = rep("2020-01-01", 4),
                     e = c("2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01"),
                     stringsAsFactors = FALSE)
    r <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd", output_unit = "months")
    ref <- as.numeric(difftime(lubridate::ymd(df$e), lubridate::ymd(df$s), units = "days")) / MONTH_DAYS

    txt <- strip_html(r$r$summary$content)
    expect_match(txt, sprintf("Total person-time: %s", round(sum(ref), 2)), fixed = TRUE)
    expect_match(txt, sprintf("Median time: %s", round(stats::median(ref), 2)), fixed = TRUE)
    expect_match(txt, sprintf("Standard deviation: %s", round(stats::sd(ref), 2)), fixed = TRUE)
})

# ---------------------------------------------------------------- regressions

test_that("extreme filtering is skipped, with a reason, when the 99th percentile is not positive", {
    # Regression: the rule is "> multiplier x 99th percentile". At q99 == 0 the
    # threshold is 0, so EVERY non-zero interval counted as extreme -- in a cohort
    # where 99% of patients enter and exit on the same day that silently DELETED
    # the handful of genuine follow-ups.
    n0 <- 200
    df <- data.frame(s = c(rep("2020-01-01", n0), "2020-01-01", "2020-01-01"),
                     e = c(rep("2020-01-01", n0), "2020-07-01", "2021-01-01"),
                     stringsAsFactors = FALSE)
    ref <- as.numeric(difftime(lubridate::ymd(df$e), lubridate::ymd(df$s), units = "days")) / MONTH_DAYS
    expect_equal(stats::quantile(ref, 0.99, names = FALSE), 0)   # the triggering condition

    r <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", remove_extreme = TRUE, include_quality_metrics = TRUE)
    txt <- strip_html(r$r$summary$content)
    expect_match(txt, sprintf("Number of observations: %d", nrow(df)))  # nothing deleted
    expect_match(txt, "Extreme-value filtering was skipped")
    expect_match(txt, "99th percentile of the intervals is 0")
})

test_that("a positive 99th percentile still drives the extreme filter normally", {
    # The guard must not disable the feature where it is well defined.
    # n must be large enough that the 99th percentile is not itself interpolated
    # into the outlier -- see the next test for that limitation.
    n <- 200
    df <- data.frame(s = rep("2020-01-01", n),
                     e = c(rep("2020-07-01", n - 1), "2055-01-01"), stringsAsFactors = FALSE)
    r <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", remove_extreme = TRUE)
    txt <- strip_html(r$r$summary$content)
    expect_match(txt, sprintf("Number of observations: %d", n - 1))  # the absurd date removed
    expect_match(txt, "extreme interval\\(s\\) removed")
    expect_false(grepl("filtering was skipped", txt))
})

test_that("with small n the extreme rule cannot see a lone outlier (documented limitation)", {
    # Not a defect introduced here, but a property of a "> multiplier x 99th
    # percentile" rule: at n = 30 the 99th percentile interpolates BETWEEN the
    # 29th and 30th order statistics, so a single extreme value inflates the very
    # threshold meant to catch it and survives. Pinned so the behaviour is at
    # least visible rather than surprising.
    df <- data.frame(s = rep("2020-01-01", 30),
                     e = c(rep("2020-07-01", 29), "2400-01-01"), stringsAsFactors = FALSE)
    ref <- as.numeric(difftime(lubridate::ymd(df$e), lubridate::ymd(df$s), units = "days")) / MONTH_DAYS
    q99 <- stats::quantile(ref, 0.99, names = FALSE)
    expect_gt(q99, stats::median(ref) * 100)      # q99 is dragged into the outlier
    expect_lt(max(ref), q99 * 2)                  # so the outlier is below threshold

    r <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", remove_extreme = TRUE)
    expect_match(strip_html(r$r$summary$content), "Number of observations: 30")
})

test_that("a single-column dataset keeps its data.frame shape through filtering", {
    # Regression: `self$data` holds only the columns the analysis asked for, so it
    # has ONE column when the same variable is chosen as both start and end date.
    # data[valid, ] without drop = FALSE collapsed it to a vector, rownames()
    # returned NULL, and .run()'s setRowNums(rownames(filtered_data)) then wrote
    # the calculated-time column back with no row mapping.
    one <- data.frame(d = c("2020-01-01", "2020-02-01", "2020-03-01"), stringsAsFactors = FALSE)
    h <- ti_run(one, dx_date = "d", fu_date = "d", time_format = "ymd", output_unit = "months")

    res <- h$p$.applyLandmarkAnalysis(c(1, 5, 9), one, 6, "months")
    expect_s3_class(res$data, "data.frame")
    expect_equal(ncol(res$data), 1L)
    expect_false(is.null(rownames(res$data)))
    expect_equal(nrow(res$data), 1L)
})

test_that("choosing the same variable for both dates yields all-zero intervals, not an error", {
    one <- data.frame(d = c("2020-01-01", "2020-06-01", "2020-09-01"), stringsAsFactors = FALSE)
    expect_no_error({
        r <- ti_run(one, dx_date = "d", fu_date = "d", time_format = "ymd", output_unit = "months")
    })
    expect_match(strip_html(r$r$summary$content), "Number of observations: 3")
})
