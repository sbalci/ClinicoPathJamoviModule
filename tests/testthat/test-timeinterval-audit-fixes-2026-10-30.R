library(testthat)

# Audit fixes, 2026-10-30. Each block states what the analysis did before.
# Use base::format: jmvcore exports a format() that ignores format strings.

fmt <- base::format
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
ti_new <- function(df, ...) {
    timeintervalClass$new(options = timeintervalOptions$new(...), data = df)
}

test_that("a validation reject reaches jamovi's error state instead of an Html banner", {
    # Was: a catch-all tryCatch around .calculate_survival_time() turned every
    # reject() into a red "Error:" div and the run finished normally.
    df <- data.frame(s = c("2020-01-01", "2020-03-01", "2020-05-01"),
                     e = c("2020-06-01", "2020-02-01", "2020-09-01"), stringsAsFactors = FALSE)
    expect_error(quietly(ti_new(df, dx_date = "s", fu_date = "e")$run()),
                 "Negative time intervals detected", fixed = TRUE)
    expect_error(quietly(timeinterval(data = df, dx_date = "s", fu_date = "e")),
                 "1 of 3 rows", fixed = TRUE)
})

test_that("the diagnostics computed before a rejection travel with the error", {
    # dmy and mdy tie, the chosen reading makes the end precede the start: the
    # ambiguity note used to be a separate banner; it is now part of the error.
    df <- data.frame(
        start = rep(c("10/06/2020", "11/07/2020", "12/08/2020"), each = 4),
        end   = rep(c("05/03/2020", "04/02/2020", "03/01/2020"), each = 4),
        stringsAsFactors = FALSE)
    e <- tryCatch(quietly(ti_new(df, dx_date = "start", fu_date = "end",
                                 output_unit = "days")$run()), error = function(e) e)
    expect_s3_class(e, "error")
    expect_match(conditionMessage(e), "Negative time intervals detected", fixed = TRUE)
    expect_match(conditionMessage(e), "Auto-detection is ambiguous", fixed = TRUE)
})

test_that("fatal input defects reject", {
    # Was: an Html error and return(), so the analysis looked complete.
    df <- data.frame(s = c(NA_character_, NA_character_), e = c("2020-01-01", "2020-02-01"),
                     stringsAsFactors = FALSE)
    expect_error(quietly(ti_new(df, dx_date = "s", fu_date = "e")$run()),
                 "Start date column contains only missing values", fixed = TRUE)
    # (the option backstops are pinned under run(noThrow = TRUE) in
    # test-timeinterval-validation.R, SEC-timeinterval-01)
})

test_that("panels are blanked with a non-empty Html on the getting-started path", {
    # The jamovi client skips an empty Html update, so "" left old text on screen.
    # End date not chosen yet -> the getting-started return.
    a <- ti_new(data.frame(s = c("2020-01-01", "2020-02-01"), e = c("2020-06-01", "2020-07-01")),
                dx_date = "s")
    quietly(a$run())
    for (item in c("messages", "summary", "aboutPanel", "personTimeInfo", "nlSummary"))
        expect_identical(a$results[[item]]$content, "<div></div>", info = item)
    expect_match(a$results$todo$content, "Welcome to Time Interval Calculator", fixed = TRUE)
})

test_that("Overall Quality is not 'Good' beside the mixed day/month-order warning", {
    # Was: the grade counted only .assessDataQuality's own warnings, so the
    # panel said "Good" under a strong warning that person-time was wrong.
    ok_s <- as.Date("2019-01-01") + 12:26 ; ok_e <- ok_s + 365
    us_s <- as.Date(c("2020-12-01", "2020-11-02", "2020-12-03", "2020-10-04", "2020-11-05"))
    us_e <- as.Date(c("2021-01-11", "2021-02-10", "2021-01-12", "2021-03-09", "2021-02-08"))
    df <- data.frame(start = c(fmt(ok_s, "%d/%m/%Y"), fmt(us_s, "%m/%d/%Y")),
                     end   = c(fmt(ok_e, "%d/%m/%Y"), fmt(us_e, "%m/%d/%Y")),
                     stringsAsFactors = FALSE)
    a <- ti_new(df, dx_date = "start", fu_date = "end", output_unit = "days",
                include_quality_metrics = TRUE)
    quietly(a$run())
    expect_match(strip_html(a$results$messages$content), "the day and month the other way round",
                 fixed = TRUE)
    q <- strip_html(a$results$qualityAssessment$content)
    expect_match(q, "Overall Quality: Poor", fixed = TRUE)
    expect_match(q, "rows may have the day and month entered the other way round", fixed = TRUE)
})

test_that("the negative-interval examples name spreadsheet rows, not positions", {
    # With a jamovi filter, self$data keeps the spreadsheet row numbers as rownames.
    df <- data.frame(s = c("2020-01-01", "2020-03-01", "2020-05-01"),
                     e = c("2020-06-01", "2020-09-01", "2020-02-01"), stringsAsFactors = FALSE)
    rownames(df) <- c("4", "9", "17")
    e <- tryCatch(quietly(ti_new(df, dx_date = "s", fu_date = "e")$run()), error = function(e) e)
    expect_match(conditionMessage(e), "Row 17: Start=2020-05-01", fixed = TRUE)
})
