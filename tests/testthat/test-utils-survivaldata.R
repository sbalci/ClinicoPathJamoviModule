# Shared survival data preparation (R/utils-survivaldata.R). The expected values are
# worked by hand from the rule each helper states, never read back from the helper.

unorder <- ClinicoPath:::.unorderFactors
dropempty <- ClinicoPath:::.dropEmptyLevels
landmark <- ClinicoPath:::.applyLandmark
classify <- ClinicoPath:::.classifyNumericDates
plausible <- ClinicoPath:::.checkDatePlausibility
parsefail <- ClinicoPath:::.countDateParseFailures

epoch <- function(d) as.numeric(as.POSIXct(d, tz = "UTC"))

test_that(".unorderFactors keeps order, empty levels and attributes", {
    o <- factor(c("low", "high", "low"), levels = c("low", "mid", "high"), ordered = TRUE)
    attr(o, "label") <- "Tumour grade"
    attr(o, "values") <- 1:3
    df <- data.frame(id = 1:3)
    df$g <- o
    df$plain <- factor(c("b", "a", "a"))
    df$n <- 3:1

    out <- unorder(df, c("g", "plain", "n", "absent"))
    expect_false(is.ordered(out$g))
    expect_identical(class(out$g), "factor")
    # factor(x, ordered = FALSE) would have lost "mid" here
    expect_identical(levels(out$g), c("low", "mid", "high"))
    expect_identical(as.character(out$g), c("low", "high", "low"))
    expect_identical(attr(out$g, "label"), "Tumour grade")
    expect_identical(attr(out$g, "values"), 1:3)
    expect_identical(out$plain, df$plain)
    expect_identical(out$n, df$n)

    expect_identical(unorder(df, NULL), df)
    expect_identical(unorder(df, character(0)), df)
    expect_true(is.ordered(unorder(df, "plain")$g))
})

test_that(".unorderFactors gives labelled reference comparisons in coxph", {
    d <- data.frame(
        time = c(2, 5, 7, 9, 11, 13, 4, 6, 8, 10, 12, 14),
        status = c(1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1),
        g = factor(rep(c("G1", "G2", "G3"), 4), levels = c("G1", "G2", "G3"), ordered = TRUE)
    )
    expect_named(coef(survival::coxph(survival::Surv(time, status) ~ g, d)), c("g.L", "g.Q"))
    expect_named(coef(survival::coxph(survival::Surv(time, status) ~ g, unorder(d, "g"))),
                 c("gG2", "gG3"))
})

test_that(".dropEmptyLevels reports a lost reference and what remains", {
    df <- data.frame(id = 1:4)
    df$grade <- factor(c("G2", "G3", "G3", "G2"), levels = c("G1", "G2", "G3"))
    df$site <- factor(c("a", "a", "b", "b"), levels = c("a", "unused", "b"))
    df$fine <- factor(c("x", "y", "x", "y"))
    df$text <- c("p", "q", "p", "q")

    res <- dropempty(df, c("grade", "site", "fine", "text", "absent"))
    expect_identical(levels(res$data$grade), c("G2", "G3"))
    expect_identical(as.character(res$data$grade), c("G2", "G3", "G3", "G2"))
    expect_identical(res$dropped, list(grade = "G1", site = "unused", fine = character(0)))
    expect_identical(res$old_reference, c(grade = "G1", site = "a", fine = "x"))
    expect_identical(res$new_reference, c(grade = "G2", site = "a", fine = "x"))
    expect_identical(res$reference_changed, c(grade = TRUE, site = FALSE, fine = FALSE))
    expect_identical(res$n_levels, c(grade = 2L, site = 2L, fine = 2L))
    expect_identical(res$too_few, character(0))
    expect_identical(res$data$text, df$text)
})

test_that(".dropEmptyLevels names a variable with fewer than two levels", {
    df <- data.frame(id = 1:2)
    df$one <- factor(c("b", "b"), levels = c("a", "b"))
    df$none <- factor(c(NA, NA), levels = c("a", "b"))
    res <- dropempty(df, c("one", "none"))
    expect_identical(res$too_few, c("one", "none"))
    expect_identical(res$n_levels, c(one = 1L, none = 0L))
    expect_identical(res$new_reference, c(one = "b", none = NA_character_))
    expect_identical(res$reference_changed, c(one = TRUE, none = TRUE))
    expect_identical(res$dropped$none, c("a", "b"))
})

test_that(".dropEmptyLevels keeps attributes aligned and the ordered class", {
    df <- data.frame(id = 1:2)
    g <- factor(c("low", "high"), levels = c("low", "mid", "high"), ordered = TRUE)
    attr(g, "label") <- "Tumour grade"
    attr(g, "values") <- c(10L, 20L, 30L)
    df$g <- g
    out <- dropempty(df, "g")$data$g
    expect_true(is.ordered(out))
    expect_identical(levels(out), c("low", "high"))
    expect_identical(attr(out, "label"), "Tumour grade")
    expect_identical(attr(out, "values"), c(10L, 30L))
    # jmvcore::toNumeric() reads the codes through `values`
    expect_identical(jmvcore::toNumeric(out), c(10L, 30L))
})

test_that(".dropEmptyLevels treats an explicit NA level like any other level", {
    df <- data.frame(id = 1:3)
    df$used <- addNA(factor(c("a", "b", NA), levels = c("a", "b", "c")))
    df$unused <- addNA(factor(c("a", "b", "a")))
    res <- dropempty(df, c("used", "unused"))
    expect_identical(levels(res$data$used), c("a", "b", NA))
    expect_identical(res$dropped$used, "c")
    expect_identical(levels(res$data$unused), c("a", "b"))
    expect_identical(res$dropped$unused, NA_character_)
    expect_identical(as.character(res$data$used), c("a", "b", NA))
})

test_that(".dropEmptyLevels survives empty input", {
    df <- data.frame(g = factor(character(0), levels = c("a", "b")))
    res <- dropempty(df, "g")
    expect_identical(res$too_few, "g")
    expect_identical(nlevels(res$data$g), 0L)

    no_levels <- data.frame(g = factor(character(0)))
    expect_identical(dropempty(no_levels, "g")$too_few, "g")

    for (none in list(NULL, character(0), "absent")) {
        res <- dropempty(df, none)
        expect_identical(res$data, df)
        expect_length(res$dropped, 0)
        expect_length(res$old_reference, 0)
        expect_length(res$reference_changed, 0)
        expect_identical(res$too_few, character(0))
    }
})

test_that(".dropEmptyLevels gives the correct Cox comparison when the reference empties", {
    set.seed(11)
    d <- data.frame(
        time = rexp(90, 0.1) + 0.1,
        status = rbinom(90, 1, 0.8),
        g = factor(rep(c("G2", "G3"), 45), levels = c("G1", "G2", "G3"))
    )
    aliased <- coef(survival::coxph(survival::Surv(time, status) ~ g, d))
    expect_true(anyNA(aliased))
    fixed <- coef(survival::coxph(survival::Surv(time, status) ~ g, dropempty(d, "g")$data))
    expect_named(fixed, "gG3")
    expect_equal(unname(fixed), -unname(aliased[["gG2"]]))
})

test_that(".applyLandmark keeps strictly later times and counts every row", {
    res <- landmark(c(5, 12, 12.5, NA, 30, NaN), 12)
    expect_true(res$valid)
    expect_identical(res$keep, c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
    expect_identical(res$time, c(NA, NA, 0.5, NA, 18, NA))
    expect_identical(res$n_kept, 2L)
    expect_identical(res$n_excluded, 2L)
    expect_identical(res$n_missing, 2L)
})

test_that(".applyLandmark matches a hand count on integer months", {
    set.seed(7)
    t <- sample(1:40, 200, replace = TRUE)
    res <- landmark(t, 12)
    expect_identical(res$n_kept, length(which(t > 12)))
    expect_identical(res$n_excluded, length(which(t <= 12)))
    expect_true(all(res$time[res$keep] > 0))
    expect_identical(res$time[res$keep], t[t > 12] - 12)
    expect_identical(res$n_kept + res$n_excluded + res$n_missing, 200L)
})

test_that(".applyLandmark excludes time zero at landmark zero, and accepts integers", {
    res <- landmark(c(0, 0, 3L), 0)
    expect_identical(res$keep, c(FALSE, FALSE, TRUE))
    expect_identical(res$n_excluded, 2L)
    expect_identical(res$time, c(NA, NA, 3))
    expect_identical(landmark(c(5L, 9L), 6L)$time, c(NA, 3L))
})

test_that(".applyLandmark refuses an invalid landmark and keeps nobody", {
    for (bad in list(-3, -0.001, NA, NA_real_, NaN, Inf, -Inf, NULL, numeric(0), c(1, 2), "12", TRUE)) {
        res <- landmark(c(5, 10, NA), bad)
        expect_false(res$valid)
        expect_identical(res$keep, c(FALSE, FALSE, FALSE))
        expect_identical(res$time, c(NA_real_, NA_real_, NA_real_))
        expect_identical(res$n_kept, 0L)
        expect_identical(res$n_missing, 1L)
    }
})

test_that(".applyLandmark handles empty and all-missing time", {
    res <- landmark(numeric(0), 3)
    expect_true(res$valid)
    expect_length(res$keep, 0)
    expect_identical(c(res$n_kept, res$n_excluded, res$n_missing), c(0L, 0L, 0L))

    res <- landmark(c(NA_real_, NA_real_), 3)
    expect_false(any(res$keep))
    expect_identical(res$n_missing, 2L)
    expect_identical(res$n_excluded, 0L)

    res <- landmark(c(1, 2), 100)
    expect_identical(res$n_kept, 0L)
    expect_identical(res$n_excluded, 2L)
})

test_that(".classifyNumericDates accepts plausible epoch seconds only", {
    res <- classify(epoch(c("2015-03-01", "2020-06-15", NA)))
    expect_identical(res$code, "epoch_seconds")
    expect_true(res$accepted)
    expect_identical(res$n_checked, 2L)
    expect_identical(res$n_refused, 0L)
    expect_identical(res$example, NA_real_)

    expect_true(classify(1e8)$accepted)
    expect_false(classify(99999999)$accepted)
    expect_true(classify(1e10)$accepted)
    expect_false(classify(1e10 + 1)$accepted)
    # 1950 as epoch seconds is a date, and it is negative
    expect_true(classify(epoch("1950-06-01"))$accepted)
    # the sentinel is left to .checkDatePlausibility(), which can name it
    expect_true(classify(epoch("2199-12-31"))$accepted)
})

test_that(".classifyNumericDates names the likely shape", {
    expect_identical(classify(c(20190115, 20200115, 20211231))$code, "yyyymmdd")
    expect_identical(classify(c(19991231, NA, 20000229))$code, "yyyymmdd")
    expect_identical(classify(c(2015, 2018, 2020))$code, "year")
    expect_identical(classify(c(43831, 44197, 44562))$code, "excel_serial")
    expect_identical(classify(c(43831.5, 44197.25))$code, "excel_serial")
    expect_identical(classify(as.numeric(as.Date(c("2015-01-01", "2021-01-01"))))$code, "day_count")
    expect_identical(classify(c(12, 36, 60))$code, "small_number")
    expect_identical(classify(c(0, 0))$code, "small_number")
    expect_identical(classify(c(2015.5, 2018.25))$code, "small_number")
    expect_identical(classify(c(150000, 900000))$code, "unknown")
    expect_identical(classify(c(99999999, 88888888))$code, "unknown")
    expect_identical(classify(1000 * epoch(c("2015-03-01", "2020-06-15")))$code, "too_large")
    expect_identical(classify(c(1.38e10 + 1e9, 2e10))$code, "too_large")

    res <- classify(c(20190115, 20200115))
    expect_false(res$accepted)
    expect_identical(res$n_refused, 2L)
    expect_identical(res$example, 20190115)
})

test_that(".classifyNumericDates refuses a column with any value out of range", {
    good <- epoch(c("2015-03-01", "2020-06-15", "2021-01-01"))
    for (bad in c(0, 20190115, 17000, -5, 1e12, Inf, -Inf)) {
        res <- classify(c(good, bad))
        expect_identical(res$code, "mixed")
        expect_false(res$accepted)
        expect_identical(res$n_checked, 4L)
        expect_identical(res$n_refused, 1L)
        expect_identical(res$example, bad)
    }
})

test_that(".classifyNumericDates handles empty and non-numeric input", {
    for (none in list(numeric(0), NA_real_, c(NaN, NA), integer(0))) {
        res <- classify(none)
        expect_identical(res$code, "empty")
        expect_false(res$accepted)
        expect_identical(res$n_checked, 0L)
    }
    for (other in list(c("2019-01-15"), factor("a"), as.Date("2019-01-15"),
                       as.POSIXct("2019-01-15", tz = "UTC"), NA, NULL)) {
        res <- classify(other)
        expect_identical(res$code, "not_numeric")
        expect_false(res$accepted)
    }
})

test_that(".checkDatePlausibility finds sentinels at the stated bounds", {
    d <- as.Date(c("2015-03-01", "2199-12-31", "1900-01-01", NA, "1901-01-01", "2198-12-31", "1899-12-30"))
    res <- plausible(d)
    expect_identical(res$n_checked, 6L)
    expect_identical(res$n_early, 2L)
    expect_identical(res$n_late, 1L)
    expect_identical(res$n_implausible, 3L)
    expect_identical(res$examples, c("2199-12-31", "1900-01-01", "1899-12-30"))
})

test_that(".checkDatePlausibility agrees on Date and date-time input", {
    txt <- c("2015-03-01", "2199-12-31", "0019-01-15")
    a <- plausible(as.Date(txt))
    b <- plausible(as.POSIXct(txt, tz = "UTC"))
    expect_identical(a, b)
    expect_identical(a$n_late, 1L)
    expect_identical(a$n_early, 1L)
    # a late-evening date in a zone behind UTC is still judged on the UTC year
    expect_identical(plausible(as.POSIXct("2015-06-01 23:00", tz = "America/New_York"))$n_implausible, 0L)
})

test_that(".checkDatePlausibility caps examples, deduplicates and tolerates no dates", {
    res <- plausible(as.Date(c(rep("2199-12-31", 5), "2300-01-01", "2400-01-01", "2500-01-01")))
    expect_identical(res$n_late, 8L)
    expect_identical(res$examples, c("2199-12-31", "2300-01-01", "2400-01-01"))

    for (none in list(as.Date(character(0)), as.Date(NA), as.POSIXct(NA_real_, tz = "UTC"), NA)) {
        res <- plausible(none)
        expect_identical(res$n_checked, 0L)
        expect_identical(res$n_implausible, 0L)
        expect_length(res$examples, 0)
    }
})

test_that(".checkDatePlausibility counts an infinite date as implausible", {
    x <- as.POSIXct(c(Inf, 1.6e9), origin = "1970-01-01", tz = "UTC")
    res <- plausible(x)
    expect_identical(res$n_checked, 2L)
    expect_identical(res$n_late, 1L)
    expect_identical(res$n_early, 0L)
})

test_that(".countDateParseFailures counts only failures of real input", {
    raw <- c("2019-01-15", "", "   ", NA, "garbage", "2019-13-45", "2020-02-30")
    parsed <- lubridate::ymd(raw, quiet = TRUE)
    expect_identical(parsefail(raw, parsed), list(n_failed = 3L, n_inputs = 4L))

    f <- factor(c("2019-01-15", "nope", NA))
    expect_identical(parsefail(f, lubridate::ymd(f, quiet = TRUE)), list(n_failed = 1L, n_inputs = 2L))

    all_bad <- c("x", "y")
    expect_identical(parsefail(all_bad, lubridate::ymd(all_bad, quiet = TRUE)), list(n_failed = 2L, n_inputs = 2L))

    expect_identical(parsefail(character(0), as.Date(character(0))), list(n_failed = 0L, n_inputs = 0L))
    expect_identical(parsefail(c(NA, NA), as.Date(c(NA, NA))), list(n_failed = 0L, n_inputs = 0L))
})

test_that("the four helpers agree on the review's two sentinel repros", {
    # F3: day counts are refused; F4: a 2199 sentinel passes the epoch test and is
    # then caught on its year, so the two checks have to be run in that order.
    days <- as.numeric(as.Date(c("2015-01-01", "2016-01-01")))
    expect_identical(classify(days)$code, "day_count")
    secs <- epoch(c("2015-01-01", "2199-12-31"))
    expect_true(classify(secs)$accepted)
    expect_identical(plausible(as.POSIXct(secs, origin = "1970-01-01", tz = "UTC"))$n_late, 1L)
})
