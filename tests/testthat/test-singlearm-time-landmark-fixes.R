# Regression tests for the singlearm time-unit, landmark, extrapolation and
# reporting-honesty fixes. Each test names the defect it locks down; if one
# fails, the corresponding bug is back.

run_singlearm <- function(...) {
    args <- list(...)
    # `type: Level` options are ALWAYS required arguments of the generated
    # wrapper: the jamovi compiler forbids `default:` on a Level, so jmvtools
    # emits them with no default at all. Every programmatic call must therefore
    # pass them explicitly, and NULL is the value that means "not set".
    for (lvl in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
        if (is.null(args[[lvl]])) args[lvl] <- list(NULL)
    do.call(singlearm, args)
}

test_that("event-level mappings are required wrapper arguments, and NULL is accepted", {
    # This is the CONTRACT that run_singlearm() above exists to satisfy; it is
    # not a wish. A previous pass asserted these arguments carry NULL defaults
    # and tried to add `default:` to the Level options to make that true -- the
    # compiler rejects it. Lock the real behaviour down in both directions so
    # nobody re-litigates it.
    for (lvl in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
        expect_true(rlang::is_missing(formals(singlearm)[[lvl]]),
                    info = paste(lvl, "must stay a no-default argument"))

    # ...and passing NULL explicitly is what makes the call work.
    d <- data.frame(time = 1:6, status = c(1L, 0L, 1L, 0L, 1L, 0L))
    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status")
    expect_equal(res$medianTable$getCell(rowNo = 1, "records")$value, 6)
})

strip_html <- function(x) gsub("<[^>]*>", " ", paste(x, collapse = " "))

simple_data <- function(times, dead) {
    data.frame(time = times,
               status = factor(ifelse(dead, "Dead", "Alive"),
                               levels = c("Alive", "Dead")))
}

test_that("events at time zero are retained instead of rejecting the analysis", {
    d <- data.frame(time = c(0, 1, 2, 3), status = c(1L, 0L, 1L, 0L))
    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         cutp = "0, 1, 2")

    expect_equal(res$medianTable$getCell(rowNo = 1, "records")$value, 4)
    expect_equal(res$survTable$getCell(rowNo = 1, "time")$value, 0)
    expect_equal(res$survTable$getCell(rowNo = 1, "surv")$value, 0.75)
    expect_false(grepl("strictly positive", strip_html(res$errors$content),
                       fixed = TRUE))
    expect_match(strip_html(res$warnings$content), "follow-up time zero")
})

test_that("Kaplan-Meier boundary intervals are not displayed as exact certainty", {
    d <- data.frame(
        time = 1:6,
        status = factor(rep("Alive", 6), levels = c("Alive", "Dead")))
    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", cutp = "1, 3, 6")

    for (i in seq_len(res$survTable$rowCount)) {
        expect_equal(res$survTable$getCell(rowNo = i, "surv")$value, 1)
        expect_true(is.na(res$survTable$getCell(rowNo = i, "lower")$value))
        expect_true(is.na(res$survTable$getCell(rowNo = i, "upper")$value))
    }
})

test_that("inactive retained date selections do not invalidate elapsed-time analysis", {
    d <- simple_data(1:6, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
    d$dx <- 1:6
    d$fu <- 7:12
    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", tint = FALSE,
                         dxdate = "dx", fudate = "fu")

    expect_equal(res$medianTable$getCell(rowNo = 1, "records")$value, 6)
})


test_that("a landmark past the longest follow-up reports a configuration error, not an R internals error", {
    # Was: nothing checked nrow() after filtering, so .assessDataQuality
    # evaluated 0/0 < 0.1 and the run aborted with
    # "missing value where TRUE/FALSE needed".
    d <- simple_data(1:6, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", uselandmark = TRUE, landmark = 100)

    msg <- strip_html(res$errors$content)
    expect_match(msg, "No subjects remain after the landmark")
    expect_match(msg, "longest follow-up was 6")
})


test_that("a negative landmark is rejected instead of shifting everyone's follow-up forward", {
    # Was: filter(mytime >= -5) kept everyone and mutate() ADDED 5 to every
    # time, silently inflating the whole cohort's follow-up.
    # Rejected twice over now: `min: 0` in singlearm.a.yaml makes jmvcore's
    # option check throw before .run() is ever reached (that is what fires
    # here), and .definemytime() still carries the R-level guard for any path
    # that bypasses the option check.
    d <- simple_data(1:6, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    expect_error(
        run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                      outcomeLevel = "Dead", uselandmark = TRUE, landmark = -5),
        "landmark must be between 0")
})


test_that("subjects whose follow-up ends exactly at the landmark are excluded", {
    # Was: `>=` kept them with a residual time of exactly zero, contradicting
    # the strictly-positive-time rule enforced in .definemytime().
    d <- simple_data(1:6, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", uselandmark = TRUE, landmark = 3)

    # times 1, 2 and 3 all go; 4, 5, 6 remain
    expect_equal(res$medianTable$getCell(rowNo = 1, "records")$value, 3)
})


test_that("requested time points beyond the longest follow-up are omitted, not extrapolated", {
    # Was: summary(fit, times = ..., extend = TRUE) carried the last estimate
    # forward forever, printing survival probabilities and CIs at 24 and 120
    # months on a cohort with 6 months of follow-up and n.risk = 0.
    d <- simple_data(c(1,2,3,4,5,6,6,5,4,3), rep(c(TRUE, FALSE), 5))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", cutp = "2, 24, 120")

    expect_equal(res$survTable$rowCount, 1)
    expect_equal(res$survTable$getCell(rowNo = 1, "time")$value, 2)
    expect_match(strip_html(res$warnings$content),
                 "beyond the longest follow-up")
})


test_that("cutpoints are used as entered under a non-month unit, with a note", {
    # The default string '12, 36, 60' is written in months, so selecting "years"
    # asks for survival at 12, 36 and 60 YEARS.
    #
    # A previous pass "fixed" this by substituting 1/3/5 whenever the string
    # equalled the default -- which also overrode a user who deliberately typed
    # 12, 36, 60 in years, and then told them "Enter your own values to override
    # this". jamovi cannot distinguish "untouched" from "typed the same thing",
    # so the values are now honoured and the note explains what they mean.
    d <- simple_data(c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5), rep(c(TRUE, FALSE), 5))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", timetypeoutput = "years")

    info <- strip_html(res$info$content)
    expect_match(info, "exactly as entered")
    expect_match(info, "1, 3, 5")               # says what to type instead
    expect_false(grepl("Enter your own values", info))

    # 12/36/60 years all lie past the 5-year follow-up, whose longest
    # observation is censored, so nothing can be estimated there.
    expect_equal(res$survTable$rowCount, 0)
    expect_match(strip_html(res$warnings$content), "beyond the longest follow-up")
})


test_that("explicit cutpoints are never rescaled", {
    d <- simple_data(c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5), rep(c(TRUE, FALSE), 5))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", timetypeoutput = "years",
                         cutp = "1, 2")

    expect_equal(vapply(1:res$survTable$rowCount,
                        function(i) res$survTable$getCell(rowNo = i, "time")$value,
                        numeric(1)),
                 c(1, 2))
})

test_that("fractional cutpoints are retained without integer coercion", {
    d <- simple_data(c(0.5, 1, 1.5, 2, 2.5, 3),
                     c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", cutp = "0.5, 1.5, 2.5")

    reported <- vapply(seq_len(res$survTable$rowCount),
                       function(i) res$survTable$getCell(rowNo = i, "time")$value,
                       numeric(1))
    expect_equal(reported, c(0.5, 1.5, 2.5))
})

test_that("fractional landmarks are accepted on a continuous time scale", {
    d <- simple_data(c(0.5, 1, 1.5, 2, 2.5, 3),
                     c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", uselandmark = TRUE,
                         landmark = 1.5, cutp = "0.5, 1")

    expect_equal(res$medianTable$getCell(rowNo = 1, "records")$value, 3)
})


test_that("numeric R Date columns are read as days, not Unix epoch seconds", {
    # Was: every numeric date column went to as.POSIXct(origin = "1970-01-01"),
    # which counts SECONDS. A 366-day interval came out as 0.000139 months.
    dx <- as.numeric(as.Date("2020-01-01"))
    fu <- as.numeric(as.Date("2021-01-01"))
    d <- data.frame(dxd = rep(dx, 6), fud = rep(fu, 6),
                    status = factor(c("Dead","Alive","Dead","Alive","Dead","Alive"),
                                    levels = c("Alive", "Dead")))

    res <- run_singlearm(data = d, tint = TRUE, dxdate = "dxd", fudate = "fud",
                         outcome = "status", outcomeLevel = "Dead")

    # 366 days is a little over 12 months, not 0.000139 of one.
    expect_equal(res$medianTable$getCell(rowNo = 1, "median")$value, 12, tolerance = 0.05)
})


test_that("Unix epoch seconds are still read as seconds", {
    dx <- as.numeric(as.POSIXct("2020-01-01", tz = "UTC"))
    fu <- as.numeric(as.POSIXct("2021-01-01", tz = "UTC"))
    d <- data.frame(dxd = rep(dx, 6), fud = rep(fu, 6),
                    status = factor(c("Dead","Alive","Dead","Alive","Dead","Alive"),
                                    levels = c("Alive", "Dead")))

    res <- run_singlearm(data = d, tint = TRUE, dxdate = "dxd", fudate = "fud",
                         outcome = "status", outcomeLevel = "Dead")

    expect_equal(res$medianTable$getCell(rowNo = 1, "median")$value, 12, tolerance = 0.05)
})


test_that("multievent runs with an unused category and with a numeric outcome", {
    # Was: .validateInputs() demanded all four mappings and checked them
    # against levels(), which is NULL for a numeric outcome. Both cases fell
    # back to the welcome screen with no explanation, contradicting the shared
    # recoder that explicitly allows an empty category.
    d <- data.frame(time = 1:6,
                    st = factor(c("DOD","DOOC","AWOD","DOD","AWOD","DOOC")))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "st",
                         outcomeLevel = "DOD", multievent = TRUE,
                         dod = "DOD", dooc = "DOOC", awod = "AWOD")
    expect_equal(res$medianTable$rowCount, 1)

    dnum <- data.frame(time = 1:6, st = c(1, 2, 3, 1, 3, 2))
    res_num <- run_singlearm(data = dnum, elapsedtime = "time", outcome = "st",
                             outcomeLevel = "1", multievent = TRUE,
                             dod = "1", dooc = "2", awod = "3")
    expect_equal(res_num$medianTable$rowCount, 1)
})


test_that("an incomplete multievent mapping produces an actionable error, not a blank screen", {
    d <- data.frame(time = 1:6,
                    st = factor(c("DOD","DOOC","AWOD","DOD","AWOD","DOOC")))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "st",
                         outcomeLevel = "DOD", multievent = TRUE, dod = "DOD")

    expect_match(strip_html(res$errors$content), "not assigned to any category")
})


test_that("data completeness is measured before incomplete rows are removed", {
    # Was: completeness was computed on cleanData, from which the incomplete
    # rows had already been dropped, so it was 100% by construction.
    d <- data.frame(time = c(1, NA, 3, 4),
                    status = factor(c("Dead", "Alive", NA, "Alive"),
                                    levels = c("Alive", "Dead")))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", advancedDiagnostics = TRUE)

    vals <- vapply(1:res$dataQualityTable$rowCount,
                   function(i) res$dataQualityTable$getCell(rowNo = i, "value")$value,
                   character(1))
    metrics <- vapply(1:res$dataQualityTable$rowCount,
                      function(i) res$dataQualityTable$getCell(rowNo = i, "metric")$value,
                      character(1))
    completeness <- vals[grepl("Completeness", metrics)]
    expect_length(completeness, 2)
    expect_true(all(completeness == "75%"))
})


test_that("median follow-up uses reverse Kaplan-Meier, not the median observed time", {
    # Was: median(observed event/censoring times), which in an event-heavy
    # cohort reports the median SURVIVAL and understates the observation window.
    set.seed(7)
    n <- 60
    d <- data.frame(time = round(stats::rexp(n, 1 / 20), 1),
                    status = factor(sample(c("Dead", "Alive"), n, TRUE, c(0.7, 0.3)),
                                    levels = c("Alive", "Dead")))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", advancedDiagnostics = TRUE)

    metrics <- vapply(1:res$dataQualityTable$rowCount,
                      function(i) res$dataQualityTable$getCell(rowNo = i, "metric")$value,
                      character(1))
    vals <- vapply(1:res$dataQualityTable$rowCount,
                   function(i) res$dataQualityTable$getCell(rowNo = i, "value")$value,
                   character(1))
    reported <- as.numeric(sub(" .*$", "", vals[grepl("Median Follow-up|Median Observed", metrics)]))

    expected <- summary(survival::survfit(
        survival::Surv(d$time, as.integer(d$status == "Alive")) ~ 1))$table[["median"]]

    expect_equal(reported, round(expected, 1))
    expect_false(isTRUE(all.equal(reported, round(stats::median(d$time), 1))))
})


test_that("the copy-ready clinical summary makes no prognosis claim", {
    # Was: a median of NA was reported as "favorable (median not reached)" and
    # medians were graded against 60/24 regardless of the time unit -- a
    # prognostic verdict in text explicitly labelled copy-ready for reports.
    set.seed(3)
    d <- data.frame(time = stats::runif(20, 0.5, 2),
                    status = factor(c("Dead", rep("Alive", 19)),
                                    levels = c("Alive", "Dead")))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", showSummaries = TRUE)

    summary_text <- strip_html(res$clinicalSummary$content)
    expect_false(grepl("favorable|concerning|moderate prognosis", summary_text))
    expect_false(grepl("prognosis for this patient population", summary_text))
    expect_match(summary_text, "Median event-free time was not reached")
    expect_match(summary_text, "do not establish prognosis")
})


test_that("the survival-table narrative counts events per interval against the real cohort size", {
    # Was: n.event (events since the previous requested time) was described as
    # cumulative, and divided by n.risk[1] -- the risk set at the FIRST
    # cutpoint -- which it called "the initial cohort".
    d <- simple_data(c(1,2,3,4,5,6,6,5,4,3), rep(c(TRUE, FALSE), 5))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", cutp = "2, 4", showSummaries = TRUE)

    txt <- paste(res$survTableSummary$content, collapse = " ")
    expect_match(txt, "of the 10 subjects were still at risk")
    expect_match(txt, "event\\(s\\) occurred between 0 and 2")
    expect_false(grepl("proportion of the initial cohort", txt))
})
