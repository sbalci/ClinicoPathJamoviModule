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
ti_run <- function(df, ...) {
    a <- timeintervalClass$new(options = timeintervalOptions$new(...), data = df)
    quietly(a$run())
    a$results
}

test_that("a yy-mm-dd month-start column is not read day-first", {
    # Was: the tie-break PROMOTED the narrowest year span; dmy pins every row to
    # 2001 (span 0), won, and reported a mean of 4.0 months against a true ~20.
    d <- data.frame(
        s = c("16-01-01", "17-03-01", "18-05-01", "19-02-01", "20-01-01", "15-02-01"),
        e = c("17-05-01", "18-06-01", "19-07-01", "21-03-01", "22-09-01", "16-08-01"),
        stringsAsFactors = FALSE)
    truth <- as.numeric(as.Date(paste0("20", d$e)) - as.Date(paste0("20", d$s))) / 30.4375
    r <- ti_run(d, dx_date = "s", fu_date = "e", output_unit = "months")
    sm <- strip_html(r$summary$content)
    expect_match(sm, "Date format used: ymd", fixed = TRUE)
    expect_match(sm, sprintf("Mean time: %s", round(mean(truth), 2)), fixed = TRUE)
    # the tie survives demotion, so the user is still asked to verify
    msg <- strip_html(r$messages$content)
    expect_match(msg, "Auto-detection is ambiguous", fixed = TRUE)
    expect_false(grepl("narrowest range of years", msg, fixed = TRUE))
})

test_that("a two-digit-year rejection keeps its two-digit-year warning and names the cause", {
    # Was: "22/09/65" -> 2065 -> every interval negative; the rejection listed three
    # causes, none of them two-digit years, and the two-digit warning was discarded.
    d <- data.frame(s = c("22/09/65", "01/01/66", "15/06/64"),
                    e = c("22/09/20", "01/01/21", "15/06/22"), stringsAsFactors = FALSE)
    msg <- strip_html(ti_run(d, dx_date = "s", fu_date = "e", output_unit = "years")$messages$content)
    expect_match(msg, "Negative time intervals detected", fixed = TRUE)
    expect_match(msg, "6 date value(s) here were written with a two-digit year", fixed = TRUE)
    expect_match(msg, "6 date value(s) were written with a two-digit year. These are read", fixed = TRUE)

    # four-digit years: the two-digit sentence must not appear
    d4 <- data.frame(s = c("2020-05-01", "2020-06-01"), e = c("2020-04-01", "2020-07-01"))
    msg4 <- strip_html(ti_run(d4, dx_date = "s", fu_date = "e")$messages$content)
    expect_match(msg4, "Negative time intervals detected", fixed = TRUE)
    expect_false(grepl("two-digit year", msg4, fixed = TRUE))
})

test_that("text after the date is flagged, not silently dropped", {
    # Was: "2020-03-01abc" parsed as 2020-03-01 with no word to the user.
    d <- data.frame(s = c("2020-01-15", "2020-03-01abc", rep("2020-04-01", 3)),
                    e = rep("2021-01-01", 5), stringsAsFactors = FALSE)
    msg <- strip_html(ti_run(d, dx_date = "s", fu_date = "e", output_unit = "days")$messages$content)
    expect_match(msg, "1 date value(s) have text after the date, for example \"2020-03-01abc\"",
                 fixed = TRUE)

    # a month NAME ending a ydm value, and AM/PM / zone markers, are not trailing text
    ydm <- data.frame(s = c("2020-15-March", "2020-01-April"), e = c("2021-15-March", "2021-01-May"))
    expect_false(grepl("text after the date", strip_html(ti_run(
        ydm, dx_date = "s", fu_date = "e", time_format = "ydm")$messages$content), fixed = TRUE))
    hms <- data.frame(s = c("2020-03-01 10:30:00 PM", "2020-03-02 12:30:00 PM"),
                      e = c("2020-03-05 10:00:00Z", "2020-03-06 10:00:00 UTC"))
    expect_false(grepl("text after the date", strip_html(ti_run(
        hms, dx_date = "s", fu_date = "e", time_format = "ymdhms")$messages$content), fixed = TRUE))
})

test_that("the copy-ready sentence says the interval is observed and defines the unit", {
    # Was: "(mean 7.4 months, median 12 months)" -- no 'observed', no unit definition,
    # easily pasted as if it were a reverse-KM median follow-up.
    d <- data.frame(s = c("2020-01-15", "2020-02-29", "2021-03-31", "2022-06-10", "2023-01-01"),
                    e = c("2021-01-15", "2021-02-28", "2021-04-30", "2022-06-10", "2024-01-01"))
    nl <- strip_html(ti_run(d, dx_date = "s", fu_date = "e", output_unit = "months",
                            show_summary = TRUE)$nlSummary$content)
    expect_match(nl, "the observed time from the start date to the end date had a mean of", fixed = TRUE)
    expect_match(nl, "1 month = 30.4375 days", fixed = TRUE)
    expect_match(nl, "not the reverse Kaplan-Meier median follow-up", fixed = TRUE)

    nly <- strip_html(ti_run(d, dx_date = "s", fu_date = "e", output_unit = "years",
                             show_summary = TRUE)$nlSummary$content)
    expect_match(nly, "1 year = 365.25 days", fixed = TRUE)
    nlc <- strip_html(ti_run(d, dx_date = "s", fu_date = "e", output_unit = "months",
                             time_basis = "calendar", show_summary = TRUE)$nlSummary$content)
    expect_match(nlc, "; calendar months)", fixed = TRUE)
    expect_false(grepl("30.4375", nlc, fixed = TRUE))
})
