# ═══════════════════════════════════════════════════════════
# Regression tests: timeinterval delivery + notice coverage
# ═══════════════════════════════════════════════════════════
#
# Each block pins one defect found in the 2026-08-31 deep audit. The comment
# above each test states what the analysis did BEFORE the fix, so a refactor
# that reintroduces the behaviour fails here with a readable reason.
#
# NOTE ON THE CENTRAL ASSERTION. Every pre-existing timeinterval test asserted
# expect_no_error() or expect_s3_class(), and every one of them PASSED for the
# entire life of the delivery bug -- because the values really were computed and
# stored, and isFilled() really was TRUE. What was false was `enabled`, the one
# boolean Output$asProtoBuf() wraps the whole payload in. Assert `enabled`.
#
# NOTE ON format(): ClinicoPath does a blanket import(jmvcore) and jmvcore
# exports its own format(), which silently ignores a strptime template and
# returns a Date unchanged. Use base::format in this file.

library(testthat)

fmt <- base::format
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
# Drive the analysis the way jamovi does, so private helpers stay reachable.
ti_run <- function(df, ...) {
    o <- timeintervalOptions$new(...)
    a <- timeintervalClass$new(options = o, data = df)
    quietly(a$run())
    list(a = a, o = o, r = a$results)
}
# Simulate the user ticking the Output control. jamovi sends this list shape;
# OptionOutput's getter normalises it back to a scalar logical.
tick_output <- function(o, name = "calculated_time") {
    o$.__enclos_env__$private[[paste0("..", name)]]$value <-
        list(value = TRUE, vars = list(name), synced = TRUE)
    invisible(o)
}
mk_df <- function(n = 40, seed = 1) {
    set.seed(seed)
    d0 <- as.Date("2020-01-01") + sample(0:200, n, TRUE)
    data.frame(start = fmt(d0, "%Y-%m-%d"),
               end   = fmt(d0 + sample(30:900, n, TRUE), "%Y-%m-%d"),
               stringsAsFactors = FALSE)
}


# ------------------------------------------------------------------ DELIVERY --
# Was: jamovi/timeinterval.r.yaml declared the Output item `calculated_time`
# while jamovi/timeinterval.a.yaml declared a Bool called `add_times`.
# Output$enabled looks its gating option up by the RESULT ITEM'S OWN NAME, so
# options$get("calculated_time") returned NULL, enabled was permanently FALSE,
# and asProtoBuf() dropped the whole payload. Ticking "Add calculated times to
# dataset" computed 40 values and threw all of them away.

test_that("an option named after the result item exists at all", {
    o <- timeintervalOptions$new(dx_date = "start", fu_date = "end")
    expect_true(o$has("calculated_time"))   # FALSE before the fix -- the entire bug
})

test_that("ticking the Output control makes the column reach jamovi", {
    df <- mk_df()
    o <- tick_output(timeintervalOptions$new(dx_date = "start", fu_date = "end"))
    a <- timeintervalClass$new(options = o, data = df)
    quietly(a$run())
    ct <- a$results$calculated_time

    # enabled, NOT isFilled: isFilled was TRUE on the broken code too.
    expect_true(ct$enabled)
    expect_true(ct$isFilled())

    p <- ct$.__enclos_env__$private
    expect_length(p$.values[[1]], nrow(df))
    expect_equal(length(p$.rowNums), length(p$.values[[1]]))
})

test_that("the appended column is named, and names its unit", {
    df <- mk_df()
    for (unit in c("days", "months", "years")) {
        o <- tick_output(timeintervalOptions$new(dx_date = "start", fu_date = "end",
                                                 output_unit = unit))
        a <- timeintervalClass$new(options = o, data = df); quietly(a$run())
        ttl <- a$results$calculated_time$.__enclos_env__$private$.titles[1]
        # Before the fix this was the literal string "Output" (jmvcore's default
        # varTitle), so a survival dataset gained an unlabelled time column.
        expect_equal(ttl, sprintf("Calculated Time (%s)", unit))
        # jmvcore::format's placeholder regex is \{ *[A-Za-z][A-Za-z0-9]* *\} --
        # NO underscore -- so a `${ output_unit }` in the .r.yaml varTitle would
        # ship to jamovi verbatim as the user's column name. Guard against
        # someone "improving" the static title into an interpolated one.
        expect_false(grepl("[${]", ttl))
    }
})

test_that("row numbers stay aligned with values when a filter drops rows", {
    # This path had NEVER run in jamovi, because enabled was always FALSE.
    s <- as.Date("2020-01-01") + 0:11
    e <- s + 100
    e[c(5, 9)] <- s[c(5, 9)] - 30                       # two impossible rows
    df <- data.frame(s = fmt(s, "%Y-%m-%d"), e = fmt(e, "%Y-%m-%d"),
                     stringsAsFactors = FALSE)
    o <- tick_output(timeintervalOptions$new(dx_date = "s", fu_date = "e",
                                             time_format = "ymd", remove_negative = TRUE))
    a <- timeintervalClass$new(options = o, data = df); quietly(a$run())
    p <- a$results$calculated_time$.__enclos_env__$private

    truth <- as.numeric(e - s) / (365.25 / 12)
    expect_lt(length(p$.rowNums), nrow(df))             # rows really were dropped
    expect_false(any(c(5, 9) %in% p$.rowNums))          # and they were the right ones
    expect_equal(length(p$.rowNums), length(p$.values[[1]]))
    expect_equal(as.numeric(p$.values[[1]]), truth[p$.rowNums])
})

test_that("the landmark path keeps row numbers aligned too", {
    df <- mk_df()
    o <- tick_output(timeintervalOptions$new(dx_date = "start", fu_date = "end",
                                             use_landmark = TRUE, landmark_time = 6))
    a <- timeintervalClass$new(options = o, data = df); quietly(a$run())
    p <- a$results$calculated_time$.__enclos_env__$private
    expect_equal(length(p$.rowNums), length(p$.values[[1]]))
    expect_lt(length(p$.rowNums), nrow(df))
})

test_that("the R wrapper has no output argument, and does not need one", {
    # An Output option is filtered out of the generated wrapper signature, so
    # `add_times = TRUE` is now a hard `unused argument` error. Pinned so nobody
    # re-adds it as a Bool, which is what broke delivery in the first place.
    expect_false("add_times"       %in% names(formals(timeinterval)))
    expect_false("calculated_time" %in% names(formals(timeinterval)))

    # ...and the values are still computed headless, which is what gating on
    # isNotFilled() alone (rather than on self$options$calculated_time) buys.
    r <- timeinterval(data = mk_df(), dx_date = "start", fu_date = "end")
    expect_false(r$calculated_time$enabled)     # no wrapper arg can turn it on
    expect_true(r$calculated_time$isFilled())   # but the column was computed
})


# --------------------------------------------------------- MISSING-VALUE LIE --
# Was: with a landmark active, .applyLandmarkAnalysis() drops NA-interval rows,
# so summary_stats$missing was structurally 0 and the summary panel printed
# "Missing values: 0" directly below a banner reading "10 observations (25.0%)
# have missing time intervals" -- two contradictory statements on one screen.

mk_missing_df <- function(n = 40, n_na = 10, seed = 3) {
    set.seed(seed)
    s <- as.Date("2018-01-01") + sample(0:100, n, TRUE)
    e <- s + sample(30:900, n, TRUE)
    e[seq_len(n_na)] <- NA
    data.frame(s = fmt(s, "%Y-%m-%d"), e = fmt(e, "%Y-%m-%d"), stringsAsFactors = FALSE)
}

test_that("a landmark does not let the summary claim zero missing values", {
    h <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", use_landmark = TRUE, landmark_time = 6)
    txt <- strip_html(h$r$summary$content)
    expect_false(grepl("Missing values: 0", txt, fixed = TRUE))
    expect_match(txt, "Missing values: 10 \\(no interval could be calculated")
})

test_that("the summary and the messages panel agree on the missing count", {
    # The invariant that would have caught the original contradiction directly.
    h <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd",
                use_landmark = TRUE, landmark_time = 6)
    msg_n <- as.integer(sub(".*?([0-9]+) observations \\([0-9.]+%\\) have missing time intervals.*",
                            "\\1", strip_html(h$r$messages$content)))
    sum_n <- as.integer(sub(".*Missing values: ([0-9]+).*", "\\1", strip_html(h$r$summary$content)))
    expect_equal(sum_n, msg_n)
})

test_that("the landmark exclusion line separates short follow-up from missing follow-up", {
    # .applyLandmarkAnalysis() has always computed below_excluded and na_excluded
    # separately; only this line threw the split away and reported one number.
    h <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd",
                use_landmark = TRUE, landmark_time = 6)
    expect_match(strip_html(h$r$summary$content),
                 "excluded by landmark \\(6 months\\): [0-9]+ with follow-up shorter than 6 months and 10 with missing follow-up")
})

test_that("the same fact is not stated twice in the Clinical Summary", {
    on_lm  <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd",
                     use_landmark = TRUE, landmark_time = 6, show_summary = TRUE)
    off_lm <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd",
                     show_summary = TRUE)
    # With a landmark the exclusion is already itemised in the landmark clause.
    expect_false(grepl("missing values were detected", on_lm$r$nlSummary$content))
    expect_true(grepl("missing values were detected", off_lm$r$nlSummary$content))
})

test_that("the missing-value clause is unchanged when no landmark is set", {
    h <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd")
    txt <- strip_html(h$r$summary$content)
    expect_match(txt, "Missing values: 10 \\(no interval could be calculated")
    expect_match(txt, "Filters applied: None")
})

test_that("no surface emits a bare '<' that would break Word/PDF export", {
    h <- ti_run(mk_missing_df(), dx_date = "s", fu_date = "e", time_format = "ymd",
                use_landmark = TRUE, landmark_time = 6, show_summary = TRUE)
    for (pane in c("summary", "nlSummary", "messages")) {
        content <- as.character(h$r[[pane]]$content)
        # every "<" must open a tag; "follow-up < 6 months" must not appear raw
        expect_false(grepl("follow-up <[^b/]", content), info = pane)
        # ...and the escaped form must not leak into the htmlEscaped messages pane
        if (pane == "messages") expect_false(grepl("&amp;lt;", content))
    }
})


# ---------------------------------------------------------- NOTICE COVERAGE --
# Was: `remove_negative` silently deleted end-before-start rows from the
# person-time denominator, announced only by a green "Analysis completed"
# banner, while the far softer `remove_extreme` filter got a warning of its own.

mk_negatives <- function(n, n_neg, seed = 11) {
    set.seed(seed)
    s <- as.Date("2019-01-01") + sample(0:200, n, TRUE)
    e <- s + sample(60:800, n, TRUE)
    e[seq_len(n_neg)] <- s[seq_len(n_neg)] - 20
    data.frame(s = fmt(s, "%Y-%m-%d"), e = fmt(e, "%Y-%m-%d"), stringsAsFactors = FALSE)
}

test_that("removing negative intervals raises a notice at any count", {
    # One impossible row cannot be silent: with remove_negative OFF that same
    # single row is a hard rejection, so ticking the box must not make it vanish.
    h <- ti_run(mk_negatives(100, 1), dx_date = "s", fu_date = "e",
                time_format = "ymd", remove_negative = TRUE)
    txt <- strip_html(h$r$messages$content)
    expect_match(txt, "dropped 1 of 100 rows \\(1\\.0%\\)")
    expect_match(txt, "Warning:")
    expect_false(grepl("Strong Warning", txt))      # sporadic, not systematic
})

test_that("the negative-interval notice escalates when the fault is systematic", {
    below <- strip_html(ti_run(mk_negatives(100, 9), dx_date = "s", fu_date = "e",
                               time_format = "ymd", remove_negative = TRUE)$r$messages$content)
    at    <- strip_html(ti_run(mk_negatives(100, 10), dx_date = "s", fu_date = "e",
                               time_format = "ymd", remove_negative = TRUE)$r$messages$content)
    expect_false(grepl("Strong Warning", below))
    expect_true(grepl("Strong Warning", at))
    expect_match(at, "systematic rather than sporadic")
})

test_that("the negative-interval notice names the date format actually used", {
    # Its stated leading cause is a mis-detected day/month order, so "check the
    # Date Format setting" is only actionable if it says which format was used.
    h <- ti_run(mk_negatives(50, 15), dx_date = "s", fu_date = "e",
                time_format = "ymd", remove_negative = TRUE)
    expect_match(strip_html(h$r$messages$content), 'read with the "ymd" format')
})

test_that("severity ordering puts the negative notice above the completion info", {
    # render_messages() sorts by severity, not insertion order; the completion
    # INFO is raised last but must not appear first.
    txt <- strip_html(ti_run(mk_negatives(50, 15), dx_date = "s", fu_date = "e",
                             time_format = "ymd", remove_negative = TRUE)$r$messages$content)
    expect_lt(regexpr("Strong Warning", txt), regexpr("Info:", txt))
})


# ------------------------------------------------------- LANDMARK PLAUSIBILITY --
# A landmark excludes early cases BY DESIGN, so a bare attrition percentage is
# the wrong trigger: a 12-month landmark -- the commonest choice in oncology --
# excludes 59.7% of this package's own histopathology cohort, and a banner that
# fires on a correct analysis only teaches users to ignore banners. The
# diagnostic question is whether the landmark sits sensibly inside the follow-up
# the cohort actually has.

mk_followup <- function(n = 60, lo = 200, hi = 600, seed = 4) {
    set.seed(seed)
    s <- as.Date("2018-01-01") + sample(0:60, n, TRUE)
    data.frame(s = fmt(s, "%Y-%m-%d"), e = fmt(s + sample(lo:hi, n, TRUE), "%Y-%m-%d"),
               stringsAsFactors = FALSE)
}

test_that("a landmark inside the observed follow-up does not warn", {
    h <- ti_run(mk_followup(), dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", use_landmark = TRUE, landmark_time = 6)
    expect_false(grepl("later than the median", strip_html(h$r$messages$content)))
})

test_that("a landmark past the median observed interval warns, without misusing 'guarantee-time'", {
    h <- ti_run(mk_followup(), dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "months", use_landmark = TRUE, landmark_time = 15)
    txt <- strip_html(h$r$messages$content)
    # "median observed interval", not "median follow-up": this analysis has no
    # event indicator at all (zero outcome/status options in its .a.yaml), so
    # the reverse-Kaplan-Meier follow-up estimator used elsewhere in the module
    # cannot apply and the two quantities must not be conflated.
    expect_match(txt, "later than the median observed interval")
    expect_false(grepl("median follow-up", txt, fixed = TRUE))
    expect_match(txt, "denominator for that subgroup only")
    # Landmarking is the REMEDY for guarantee-time/immortal-time bias, not a
    # cause of it -- and the glossary panel in this same analysis says so.
    # Naming it here would contradict the glossary in one results pane.
    expect_false(grepl("guarantee", txt, ignore.case = TRUE))
    # This analysis has no event indicator, so it must not assert prognostic
    # selection it cannot distinguish from recent accrual.
    expect_match(txt, "no event indicator")
})

test_that("a landmark that excludes nobody but shortens everything is flagged", {
    # landmark_time is expressed in output_unit and defaults to 6. A user who
    # means "6 months" while the results are in days silently shortens every
    # interval by 6 days -- a wrong number that looks entirely reasonable.
    h <- ti_run(mk_followup(), dx_date = "s", fu_date = "e", time_format = "ymd",
                output_unit = "days", use_landmark = TRUE, landmark_time = 6)
    txt <- strip_html(h$r$messages$content)
    expect_match(txt, "excluded no participants")
    expect_match(txt, "same unit as the results")
})


# ------------------------------------------------------ TEXT-ENCODED SERIALS --
# Was: .checkDateSerial() inspected numeric columns only, so a spreadsheet
# day-count column re-exported as text or factor fell through to two dead-end
# messages ("Could not detect a common date format", "Date parsing failed") --
# both telling the user to choose a format manually when no format on the list
# can parse "42370".

xl_serial <- function(from, n) as.numeric(as.Date(from) - as.Date("1899-12-30")) + seq_len(n) - 1

test_that("day-count serials are rejected as serials whatever the column type", {
    sv <- xl_serial("2016-01-01", 30); ev <- sv + 365
    for (as_type in list(identity, as.character, factor)) {
        df <- data.frame(s = as_type(sv), e = as_type(ev))
        for (f in c("auto", "mdy", "ymd")) {
            h <- ti_run(df, dx_date = "s", fu_date = "e", time_format = f)
            txt <- strip_html(h$r$messages$content)
            expect_match(txt, "holds five-digit numbers")
            expect_false(grepl("Could not detect a common date format", txt))
            expect_false(grepl("Date parsing failed", txt))
        }
    }
})

test_that("the serial message does not present one epoch's date as the reading", {
    # Spreadsheets count from 1899-12-30, SAS and Stata from 1960-01-01, R from
    # 1970-01-01 -- all three land in the same five-digit band, so quoting one
    # reading unqualified can be wrong by decades.
    sv <- xl_serial("2016-01-01", 30)
    h <- ti_run(data.frame(s = sv, e = sv + 365), dx_date = "s", fu_date = "e")
    expect_match(strip_html(h$r$messages$content), "SAS and Stata count from 1960-01-01")
})

test_that("a stray numeric code in a text date column does not condemn the column", {
    # The string test, not coercion, is what makes widening to text safe:
    # as.numeric() would turn every real date into NA and take the vote over the
    # one stray cell. An SPSS/Excel missing code must score 1/30, not 1/1.
    for (as_type in list(as.character, factor)) {
        txt <- fmt(as.Date("2020-01-01") + 0:29, "%Y-%m-%d"); txt[7] <- "99999"
        df <- data.frame(s = as_type(txt),
                         e = as_type(fmt(as.Date("2020-06-01") + 0:29, "%Y-%m-%d")))
        h <- ti_run(df, dx_date = "s", fu_date = "e", time_format = "ymd")
        expect_false(grepl("five-digit", strip_html(h$r$messages$content)))
        expect_match(strip_html(h$r$summary$content), "Number of observations: 29")
    }
})

test_that("legitimate packed numeric dates are still accepted", {
    # YYYYMMDD is eight digits and YYMMDD/MMDDYY/DDMMYY are six; only day counts
    # are five, which is why digit width is the discriminator.
    for (v in list(20200101:20200130, 200101:200130)) {
        h <- ti_run(data.frame(s = v, e = v + 100), dx_date = "s", fu_date = "e")
        expect_false(grepl("five-digit", strip_html(h$r$messages$content)))
    }
})


# ------------------------------------------------------------- QUALITY PANEL --
# Was: .assessDataQuality() computed missing_start_dates and missing_end_dates
# and nothing ever read them, so the panel reported a missing-value total with
# no way to tell which of the two columns to go and fix.

test_that("the quality panel says which date column is missing or unreadable", {
    h <- ti_run(mk_missing_df(n = 40, n_na = 10), dx_date = "s", fu_date = "e",
                time_format = "ymd", include_quality_metrics = TRUE)
    txt <- strip_html(h$r$qualityAssessment$content)
    expect_match(txt, "Start dates missing or unreadable")
    expect_match(txt, "End dates missing or unreadable")
    # all 10 NAs are in the END column, so the split must not be symmetric
    expect_match(txt, "End dates missing or unreadable 10")
    expect_match(txt, "Start dates missing or unreadable 0")
})


# ------------------------------------------------- CLAIMS THE ANALYSIS MAKES --
# Was: the glossary told clinicians that landmark analysis selects "6-month
# survivors", and the always-visible person-time panel listed "Censoring:
# Accounts for participants leaving the study early". This analysis has NO event
# indicator -- it selects on LENGTH OF FOLLOW-UP and cannot tell an event date
# from a censoring date. Its own landmark warning says exactly that, so the
# results window contradicted itself, and the false claim also shipped in
# jamovi/timeinterval.a.yaml and therefore in man/timeinterval.Rd.

test_that("the glossary does not claim the landmark selects survivors", {
    h <- ti_run(mk_df(), dx_date = "start", fu_date = "end", show_glossary = TRUE)
    txt <- strip_html(h$r$glossaryPanel$content)
    expect_false(grepl("survivor", txt, ignore.case = TRUE))
    expect_match(txt, "no event indicator")
    expect_match(txt, "length of follow-up")
})

test_that("the person-time panel does not claim to account for censoring", {
    h <- ti_run(mk_df(), dx_date = "start", fu_date = "end")
    txt <- strip_html(h$r$personTimeInfo$content)
    expect_false(grepl("Accounts for participants leaving", txt, fixed = TRUE))
    expect_match(txt, "no event indicator")
})

test_that("the option help does not claim the landmark selects survivors", {
    a <- yaml::yaml.load_file(testthat::test_path("..", "..", "jamovi", "timeinterval.a.yaml"))
    for (o in a$options) {
        d <- tryCatch(o$description$R, error = function(e) NULL)
        if (is.null(d)) next
        expect_false(grepl("surviving past|survivors only", d),
                     info = paste("option", o$name, "still claims survival selection"))
    }
})


# -------------------------------------------- "DIVIDE BY THIS" MUST BE EARNED --
# Was: the Interpretation Example ended with an unconditional "serves as the
# denominator for calculating incidence rates" -- printed for a cohort whose
# total person-time was exactly 0 (an instruction to divide by zero), and on the
# same page as a strong warning saying a systematic date fault had just been
# filtered out and the surviving rows could not be assumed correct.

test_that("the incidence-rate instruction is withheld when person-time is zero", {
    same <- data.frame(s = rep("2020-01-01", 10), e = rep("2020-01-01", 10),
                       stringsAsFactors = FALSE)
    txt <- strip_html(ti_run(same, dx_date = "s", fu_date = "e",
                             time_format = "ymd")$r$summary$content)
    expect_match(txt, "is zero, so no incidence rate can be computed")
    expect_false(grepl("serves as the denominator", txt, fixed = TRUE))
})

test_that("the incidence-rate instruction is qualified after a systematic filter", {
    txt <- strip_html(ti_run(mk_negatives(50, 15), dx_date = "s", fu_date = "e",
                             time_format = "ymd", remove_negative = TRUE)$r$summary$content)
    expect_match(txt, "would normally serve as the denominator")
    expect_match(txt, "Resolve that first")
})

test_that("the incidence-rate instruction is given on a clean cohort", {
    txt <- strip_html(ti_run(mk_df(), dx_date = "start", fu_date = "end")$r$summary$content)
    expect_match(txt, "serves as the denominator for calculating incidence rates")
})


# ------------------------------------------------- DIAGNOSTICS ON ERROR PATHS --
# Was: .detectDateFormat() computes the ambiguity note BEFORE any rejection, but
# .run() emitted it only after `if (is.null(calculated_times)) return()`. So on a
# failed run the single most actionable sentence -- "ymd and dmy both fit these
# dates equally well; I used ymd" -- was computed and then discarded, exactly
# when the user is trying to work out what went wrong.

test_that("the format-ambiguity note survives a rejected run", {
    # Every component <= 12, so dmy and mdy tie at 100% and the ambiguity note is
    # raised; the end date precedes the start under the chosen reading, so the run
    # is then refused. The note must survive that refusal.
    df <- data.frame(
        start = rep(c("10/06/2020", "11/07/2020", "12/08/2020"), each = 4),
        end   = rep(c("05/03/2020", "04/02/2020", "03/01/2020"), each = 4),
        stringsAsFactors = FALSE)
    h <- ti_run(df, dx_date = "start", fu_date = "end", output_unit = "days")
    txt <- strip_html(h$r$messages$content)
    expect_match(txt, "Auto-detection is ambiguous")
    expect_match(txt, "Error:")          # the rejection itself is still shown
})

test_that("a two-digit-year column is no longer read as ymd", {
    # Was: the detector broke ties by candidate order and `ymd` was first, so any
    # DD/MM/YY column with years <= 2031 was read as yy-mm-dd -- the DAY became the
    # year. Measured before the fix on this exact fixture: mean 3775 days against a
    # true 493. Ties are now broken by which reading spans the narrowest range of
    # years, because a cohort spans a few years and a day-of-month spans thirty.
    set.seed(3)
    s <- as.Date("2018-01-05") + sample(0:300, 30, TRUE)
    e <- s + sample(300:700, 30, TRUE)
    df <- data.frame(start = fmt(s, "%d/%m/%y"), end = fmt(e, "%d/%m/%y"),
                     stringsAsFactors = FALSE)
    txt <- strip_html(ti_run(df, dx_date = "start", fu_date = "end",
                             output_unit = "days")$r$summary$content)
    expect_match(txt, "Date format used: dmy")
    expect_match(txt, sprintf("Number of observations: %d", nrow(df)))
    # the numbers must now be the true ones, not a 7.7x inflation
    expect_match(txt, sprintf("Mean time: %s", round(mean(as.numeric(e - s)), 2)))
    expect_match(txt, sprintf("Total person-time: %s person-days", sum(as.numeric(e - s))))
})

test_that("rows typed in the other day/month order raise a strong warning", {
    # The row set that made this the release blocker: 15 rows correctly typed
    # dd/mm/yyyy and 5 typed mm/dd/yyyy. The bad rows parse cleanly, stay positive,
    # sit inside 2x the 99th percentile and under the 50-year backstop, so before
    # this check the only banner was a green "Analysis completed" -- beside a
    # person-time 40.9% too high and a copy-ready manuscript sentence carrying it.
    ok_s <- as.Date("2019-01-01") + 12:26 ; ok_e <- ok_s + 365
    us_s <- as.Date(c("2020-12-01","2020-11-02","2020-12-03","2020-10-04","2020-11-05"))
    us_e <- as.Date(c("2021-01-11","2021-02-10","2021-01-12","2021-03-09","2021-02-08"))
    df <- data.frame(start = c(fmt(ok_s, "%d/%m/%Y"), fmt(us_s, "%m/%d/%Y")),
                     end   = c(fmt(ok_e, "%d/%m/%Y"), fmt(us_e, "%m/%d/%Y")),
                     stringsAsFactors = FALSE)
    h <- ti_run(df, dx_date = "start", fu_date = "end", output_unit = "days",
                include_quality_metrics = TRUE, show_summary = TRUE)
    txt <- strip_html(h$r$messages$content)
    expect_match(txt, "the day and month the other way round")
    expect_match(txt, "Strong Warning")

    # and the manuscript sentence must be withheld while the dates are suspect
    expect_match(strip_html(h$r$nlSummary$content), "Clinical Summary withheld")
    expect_false(grepl("contributing", strip_html(h$r$nlSummary$content), fixed = TRUE))
})

test_that("the ambiguity exposure is disclosed without crying wolf", {
    # A clean dd/mm column typically has a sixth of its rows ambiguous, so the
    # exposure is stated in the quality panel rather than raised as a banner -- a
    # warning that fires on most correct datasets teaches users to ignore warnings.
    set.seed(5); n <- 60
    s <- as.Date("2018-01-01") + sample(0:900, n, TRUE)
    e <- s + round(rgamma(n, shape = 2, scale = 180)) + 5
    df <- data.frame(a = fmt(s, "%d/%m/%Y"), b = fmt(e, "%d/%m/%Y"),
                     stringsAsFactors = FALSE)
    h <- ti_run(df, dx_date = "a", fu_date = "b", output_unit = "days",
                include_quality_metrics = TRUE)
    expect_false(grepl("the other way round", strip_html(h$r$messages$content)))
    expect_match(strip_html(h$r$qualityAssessment$content), "Day/month ambiguity")

    # ...and an unambiguous ISO column has nothing to disclose
    iso <- data.frame(a = fmt(s, "%Y-%m-%d"), b = fmt(e, "%Y-%m-%d"),
                      stringsAsFactors = FALSE)
    h2 <- ti_run(iso, dx_date = "a", fu_date = "b", include_quality_metrics = TRUE)
    expect_false(grepl("Day/month ambiguity", strip_html(h2$r$qualityAssessment$content)))
})

test_that("a landmark of zero with the box ticked says it did nothing", {
    txt <- strip_html(ti_run(mk_df(), dx_date = "start", fu_date = "end",
                             use_landmark = TRUE, landmark_time = 0)$r$messages$content)
    expect_match(txt, "landmark time is 0")
})

test_that("one observation is called a participant, not participants", {
    one <- data.frame(s = "2020-01-01", e = "2020-07-01", stringsAsFactors = FALSE)
    txt <- strip_html(ti_run(one, dx_date = "s", fu_date = "e", time_format = "ymd",
                             show_summary = TRUE)$r$nlSummary$content)
    expect_match(txt, "on 1 participant")
    expect_false(grepl("1 participants", txt, fixed = TRUE))
})


# ------------------------------------ THE WRITTEN COLUMN MUST NAME ITS ZERO --
# Was: with a landmark active every value written back to the spreadsheet is the
# landmark shorter than the interval its name promises, on a reduced cohort, and
# neither the column title nor varDescription said so. The Caveats panel does,
# but it is gated on include_quality_metrics, which defaults to FALSE.

test_that("the output column names the landmark it was rebased on", {
    df <- mk_df()
    o <- tick_output(timeintervalOptions$new(dx_date = "start", fu_date = "end",
                                             output_unit = "months",
                                             use_landmark = TRUE, landmark_time = 12))
    a <- timeintervalClass$new(options = o, data = df); quietly(a$run())
    ttl <- a$results$calculated_time$.__enclos_env__$private$.titles[1]
    expect_match(ttl, "landmark")
    expect_match(ttl, "months")

    # ...and does NOT say landmark when none is set
    o2 <- tick_output(timeintervalOptions$new(dx_date = "start", fu_date = "end",
                                              output_unit = "months"))
    a2 <- timeintervalClass$new(options = o2, data = df); quietly(a2$run())
    expect_false(grepl("landmark", a2$results$calculated_time$.__enclos_env__$private$.titles[1]))
})
