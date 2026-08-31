# ═══════════════════════════════════════════════════════════
# Regression tests for the timeinterval audit fixes
# ═══════════════════════════════════════════════════════════
#
# Each block pins one defect found in the 2026-08-30 deep audit. The comment
# above each test states what the analysis did BEFORE the fix, so a future
# refactor that reintroduces the behaviour fails here with a readable reason.
#
# library(ClinicoPath) -- the PACKAGE name, not the repo name. Using
# "ClinicoPathJamoviModule" makes testthat skip the whole file silently.

library(testthat)
library(ClinicoPath)

# ClinicoPath does a blanket import(jmvcore) and jmvcore exports its own format(),
# which does NOT accept a strptime template -- so every date built here uses
# base::format explicitly. Bare format() silently returns something else.

strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^<>]*>", " ", paste(as.character(x), collapse = " ")))

# Excel/spreadsheet day-count serials for a given date
xl <- function(from, n) as.numeric(as.Date(from) - as.Date("1899-12-30")) + seq_len(n) - 1

clean_df <- function(n = 40) {
    set.seed(2)
    s <- as.Date("2016-01-01") + sample(0:200, n, TRUE)
    data.frame(s = base::format(s, "%Y-%m-%d"),
               e = base::format(s + sample(60:1200, n, TRUE), "%Y-%m-%d"),
               stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------- CRITICAL --
# Was: a column of Excel serials auto-detected as "mdy" and reported
# "Mean time: 792.13 / Total person-time: 15842.63 person-months" for data
# whose true follow-up is 12 months. Only pushback was a future-dates warning.
test_that("spreadsheet day-count serials are rejected under every format setting", {
    df <- data.frame(start = xl("2016-01-01", 60), end = xl("2016-01-01", 60) + 366)
    for (fmt in c("auto", "ymd", "mdy")) {
        res <- timeinterval(data = df, dx_date = "start", fu_date = "end",
                            time_format = fmt, output_unit = "months")
        expect_true(grepl("cannot be read as dates unambiguously", res$messages$content, fixed = TRUE),
                    info = paste("format:", fmt))
        # the fabricated person-time must never be produced
        expect_false(grepl("person-months", res$summary$content, fixed = TRUE),
                     info = paste("format:", fmt))
    }
})

# The guard keys on DIGIT WIDTH, not on whether lubridate::ymd() happens to
# accept the number. An earlier ymd()-oracle version was wrong in both
# directions and each case below pins one of those failures.
test_that("legitimate numeric packed dates still run", {
    d1 <- as.Date("2020-01-01") + 0:29
    d2 <- d1 + 366
    e1 <- as.Date("1995-11-01") + 0:29
    e2 <- e1 + 366
    cases <- list(
        # 8-digit YYYYMMDD
        list(d = data.frame(start = as.numeric(base::format(d1, "%Y%m%d")),
                            end   = as.numeric(base::format(d2, "%Y%m%d"))), fmt = "auto"),
        # 6-digit YYMMDD
        list(d = data.frame(start = as.numeric(base::format(d1, "%y%m%d")),
                            end   = as.numeric(base::format(d2, "%y%m%d"))), fmt = "auto"),
        # 6-digit MMDDYY with mdy selected: ymd() rejects these, so the old
        # oracle-based guard falsely condemned them
        list(d = data.frame(start = as.numeric(base::format(e1, "%m%d%y")),
                            end   = as.numeric(base::format(e2, "%m%d%y"))), fmt = "mdy"),
        # 6-digit DDMMYY with dmy selected: same failure
        list(d = data.frame(start = as.numeric(base::format(e1, "%d%m%y")),
                            end   = as.numeric(base::format(e2, "%d%m%y"))), fmt = "dmy"))
    for (cs in cases) {
        res <- timeinterval(data = cs$d, dx_date = "start", fu_date = "end",
                            time_format = cs$fmt, output_unit = "months")
        expect_false(grepl("cannot be read as dates unambiguously",
                           res$messages$content, fixed = TRUE), info = cs$fmt)
    }
})

# Regression: the first version of the guard coerced the column with as.numeric()
# and DROPPED the NAs, so the 80% vote was taken over the numeric-coercible cells
# alone. One stray "99999" missing-code in 30 valid text dates made the
# denominator 1 and aborted the whole analysis with a false diagnosis.
test_that("a stray numeric cell in a text date column does not abort the analysis", {
    good <- base::format(as.Date("2016-01-01") + 0:29, "%Y-%m-%d")
    end  <- base::format(as.Date("2017-01-01") + 0:29, "%Y-%m-%d")
    for (col in list(replace(good, 30, "99999"), factor(replace(good, 30, "99999")))) {
        d <- data.frame(s = col, e = end, stringsAsFactors = FALSE)
        res <- timeinterval(data = d, dx_date = "s", fu_date = "e",
                            time_format = "ymd", output_unit = "months")
        expect_false(grepl("cannot be read as dates unambiguously",
                           res$messages$content, fixed = TRUE))
        expect_match(as.character(res$summary$content), "person-months", fixed = TRUE)
    }
})

# The ymd()-oracle version let this whole band through: ymd(40101) succeeds
# (reading it as year 4), so a 2009-2012 cohort exported as Excel serials was
# never flagged and reported person-time ~19x too large.
test_that("day-count serials in the 2009-2012 band are caught", {
    xl9 <- xl("2009-10-15", 30)
    d <- data.frame(start = xl9, end = xl9 + 730)
    res <- timeinterval(data = d, dx_date = "start", fu_date = "end",
                        time_format = "auto", output_unit = "months")
    expect_true(grepl("cannot be read as dates unambiguously",
                      res$messages$content, fixed = TRUE))
})

# R's own numeric Date origin (1970-01-01) lands in the same 5-digit band.
test_that("R numeric Date values are caught", {
    v <- as.numeric(as.Date("2005-01-01")) + 0:29
    d <- data.frame(start = v, end = v + 366)
    res <- timeinterval(data = d, dx_date = "start", fu_date = "end",
                        time_format = "auto", output_unit = "months")
    expect_true(grepl("cannot be read as dates unambiguously",
                      res$messages$content, fixed = TRUE))
})

# Backstop for a misparse the serial guard cannot see.
test_that("implausible follow-up duration raises a warning", {
    df <- data.frame(start = rep("1950-01-01", 30), end = rep("2020-01-01", 30))
    res <- timeinterval(data = df, dx_date = "start", fu_date = "end",
                        time_format = "ymd", output_unit = "months")
    expect_true(grepl("exceed the follow-up of essentially every clinical cohort",
                      res$messages$content, fixed = TRUE))
})


# ---------------------------------------------------------------- CRITICAL --
# Was: .applyLandmarkAnalysis() subtracts the landmark from every interval, but
# every label still said "follow-up" / "Total person-time". 30 patients with
# exactly 12.02 months of follow-up and landmark=6 reported "mean follow-up
# 6.0 months", and the copy-ready sentence -- built to be pasted into a
# manuscript -- said "Follow-up data were available for ... mean 6.0 months".
test_that("landmark labels every reported duration as measured from the landmark", {
    df <- clean_df()
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months", use_landmark = TRUE, landmark_time = 12,
                      show_summary = TRUE)
    s <- strip(r$summary$content)
    expect_match(s, "measured from the 12-month landmark", fixed = TRUE)
    expect_match(s, "Total post-landmark person-time", fixed = TRUE)
    expect_match(s, "Mean post-landmark time", fixed = TRUE)
    expect_false(grepl("Total person-time:", s, fixed = TRUE))

    # the pasteable sentence must disclose the reset
    nl <- strip(r$nlSummary$content)
    expect_match(nl, "measured from the landmark rather than from the start date", fixed = TRUE)
    expect_match(nl, "post-landmark person-months", fixed = TRUE)

    # and the completion notice too
    expect_match(strip(r$messages$content), "post-landmark person-time", fixed = TRUE)

    # ... while the plain labels come back with no landmark
    r0 <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                       output_unit = "months", show_summary = TRUE)
    s0 <- strip(r0$summary$content)
    expect_match(s0, "Total person-time:", fixed = TRUE)
    expect_false(grepl("post-landmark", s0, fixed = TRUE))
})

# Was: a landmark that excluded everyone printed the date-format checklist
# ("Date format settings match your data ...") and emitted NO message at all,
# sending the user to debug a parsing bug that did not exist.
test_that("a landmark that excludes everyone names the landmark as the cause", {
    r <- timeinterval(data = clean_df(), dx_date = "s", fu_date = "e",
                      time_format = "ymd", output_unit = "months",
                      use_landmark = TRUE, landmark_time = 9999)
    s <- strip(r$summary$content)
    expect_match(s, "excluded by the landmark", fixed = TRUE)
    expect_false(grepl("Date format settings match your data", s, fixed = TRUE))
    expect_match(strip(r$messages$content), "Landmark analysis excluded every participant",
                 fixed = TRUE)
})

# The second route into the same panel: filters, not the landmark, emptied it.
test_that("filters that remove every row are named as the cause", {
    s0 <- as.Date("2020-01-01") + 0:9
    df <- data.frame(s = base::format(s0, "%Y-%m-%d"), e = base::format(s0 - 30, "%Y-%m-%d"),
                     stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months", remove_negative = TRUE)
    s <- strip(r$summary$content)
    expect_match(s, "removed by the data quality filters", fixed = TRUE)
    expect_false(grepl("Date format settings match your data", s, fixed = TRUE))
})


# -------------------------------------------------------------------- HIGH --
# Was: `if (n < 10 && n > 1)` exempted the single most degenerate case, so n=1
# produced only "Info: Analysis completed using 1 observations" beside a
# summary reading "Standard deviation: NA".
test_that("n = 1 raises a strong warning rather than passing silently", {
    df <- data.frame(s = "2016-01-01", e = "2017-06-01", stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months")
    expect_match(strip(r$messages$content), "Only one interval could be calculated",
                 fixed = TRUE)
})

# Was: messages rendered in insertion order, so the "analysis completed" INFO
# (raised during summary computation) sat ABOVE data-quality warnings raised
# later. The reassuring banner appeared on top of the alarm.
test_that("messages render most severe first", {
    df <- data.frame(s = "2016-01-01", e = "2017-06-01", stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months")
    txt <- as.character(r$messages$content)
    expect_lt(regexpr("Strong Warning", txt, fixed = TRUE)[[1]],
              regexpr("Info", txt, fixed = TRUE)[[1]])
})


# ------------------------------------------------------------------ MEDIUM --
# Was: "Mean time: 0.04 (95% CI: -0.04 to 0.12)". A negative lower bound for a
# follow-up duration is not interpretable.
test_that("the mean CI is never reported below zero, and discloses the clamp", {
    n <- 300
    df <- data.frame(s = rep("2020-01-01", n),
                     e = c(rep("2020-01-01", n - 1), "2021-01-01"),
                     stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months")
    s <- strip(r$summary$content)
    expect_match(s, "CI: 0 to", fixed = TRUE)
    expect_false(grepl("CI: -", s, fixed = TRUE))
    # the clamp must be disclosed, with the value that was actually computed
    expect_match(s, "lower confidence limit is shown as 0, not as computed", fixed = TRUE)
})

# Was: the CI was computed only inside `if (include_quality_metrics)`, so an
# ordinary descriptive statistic was reachable only via a panel called
# "Data quality assessment" (which defaults to off).
test_that("the mean CI appears without enabling quality metrics", {
    r <- timeinterval(data = clean_df(), dx_date = "s", fu_date = "e",
                      time_format = "ymd", output_unit = "months")
    expect_match(as.character(r$summary$content), "95% CI", fixed = TRUE)

    # n = 1 is not estimable, so nothing is printed
    df1 <- data.frame(s = "2016-01-01", e = "2017-06-01", stringsAsFactors = FALSE)
    r1 <- timeinterval(data = df1, dx_date = "s", fu_date = "e", time_format = "ymd")
    expect_false(grepl("CI:", as.character(r1$summary$content), fixed = TRUE))
})

# Was: "Overall Quality: Good" on a cohort where 19 of 20 intervals were
# zero-length. zero_intervals was counted but never raised a warning.
test_that("a cohort dominated by zero-length intervals is not scored Good", {
    df <- data.frame(s = rep("2020-01-01", 20),
                     e = c(rep("2020-01-01", 19), "2021-01-01"),
                     stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months", include_quality_metrics = TRUE)
    q <- strip(r$qualityAssessment$content)
    expect_match(q, "zero-length intervals", fixed = TRUE)
    expect_false(grepl("Overall Quality: Good", q, fixed = TRUE))
})

# Was: silent. Selecting one column as both ends yields zero person-time,
# which is a zero denominator for any incidence rate.
test_that("the same variable on both ends is flagged", {
    df <- data.frame(s = base::format(as.Date("2016-01-01") + 0:19, "%Y-%m-%d"),
                     stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "s", time_format = "ymd",
                      output_unit = "months")
    expect_match(strip(r$messages$content),
                 "selected as both the start and the end date", fixed = TRUE)
})

# Was: the option claimed it "Ensures consistent time interval calculations
# across different systems and time zones", but lubridate returns a Date for
# every format except ymdhms, so the setting was inert and silent.
test_that("selecting UTC on a date-only format says the setting has no effect", {
    r <- timeinterval(data = clean_df(), dx_date = "s", fu_date = "e",
                      time_format = "ymd", output_unit = "months", timezone = "utc")
    expect_match(strip(r$messages$content), "timezone has no effect on the intervals",
                 fixed = TRUE)
})

# Was: "Flag extreme values" in the UI, but the option DELETES rows from every
# statistic -- person-time fell 1599.08 -> 942.00 (-41%) in the audit.
test_that("extreme-value removal warns that the person-time denominator shrank", {
    set.seed(1)
    s <- as.Date("2015-01-01") + sample(0:400, 60, TRUE)
    e <- s + sample(30:900, 60, TRUE)
    e[1:3] <- s[1:3] - 40
    e[4]   <- s[4] + 20000
    df <- data.frame(s = base::format(s, "%Y-%m-%d"), e = base::format(e, "%Y-%m-%d"),
                     stringsAsFactors = FALSE)
    r <- timeinterval(data = df, dx_date = "s", fu_date = "e", time_format = "ymd",
                      output_unit = "months", remove_negative = TRUE, remove_extreme = TRUE)
    expect_match(strip(r$messages$content), "Extreme-value removal dropped", fixed = TRUE)
})


# --------------------------------------------------------------- HYGIENE ----
# Was: 13 sites hardcoded a dark hex text colour on a translucent tint whose
# parent is `color: inherit`, so headings were invisible in jamovi's dark
# theme. tools/theme_safe_html.py cannot see this (it flags the mirror case).
test_that("no panel hardcodes a dark text colour", {
    src <- readLines(testthat::test_path("..", "..", "R", "timeinterval.b.R"), warn = FALSE)
    panel <- grep("color: #(004085|721c24|004080|333|555|4a148c|7f5006);", src, value = TRUE)
    expect_length(panel, 0)
})

# Was: two .() strings embedded "\n", which puts a multi-line unit into the
# translation catalogue. Sentences are now joined outside the translated units.
test_that("no translated string contains a newline", {
    src <- readLines(testthat::test_path("..", "..", "R", "timeinterval.b.R"), warn = FALSE)
    expect_length(grep("\\.\\(\"[^\"]*\\\\n", src, value = TRUE), 0)
})
