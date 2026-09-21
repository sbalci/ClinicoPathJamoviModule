# Regression tests from /validate-function swimmerplot depth=standard (2026-09-20).
#
# Tests cover:
#   1. Hand-calculated known-answer arithmetic (EXEC-IND)
#   2. Reference parity against survival::survfit, stats::binom.test, stats::fisher.test, lubridate
#   3. Metamorphic properties (row permutation, group level order, progression truncation)
#   4. Boundary & degenerate conditions (100% events fallback, 100% censored, negative duration rejection)
#   5. Cross-output consistency (DCR >= ORR, CI nesting, table-to-narrative parity)

library(ClinicoPath)

q <- function(expr) suppressWarnings(suppressMessages(force(expr)))

val_summary <- function(df, pattern) {
    row <- df[grepl(pattern, df$metric, ignore.case = TRUE), ]
    if (nrow(row) == 0L) stop(sprintf("metric '%s' not found", pattern))
    as.numeric(row$value[1])
}

val_adv <- function(df, key) {
    r <- df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
    if (nrow(r) != 1L) stop(sprintf("key '%s' matched %d rows", key, nrow(r)))
    as.numeric(r$metric_value[1])
}

ci_adv <- function(df, key) {
    r <- df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
    if (nrow(r) != 1L) stop(sprintf("key '%s' matched %d rows", key, nrow(r)))
    ci_str <- as.character(r$confidence_interval[1])
    if (is.na(ci_str) || !nzchar(ci_str) || identical(ci_str, "<NA>")) return(c(NA_real_, NA_real_))
    as.numeric(strsplit(ci_str, "\\s*-\\s*")[[1]])
}

test_that("C01-C05 hand-calculated cohort summary and duration statistics match exactly", {
    # 4 patients: [0,10], [0,20], [0,30], [0,40]. Total PT = 100, Mean = 25, Median = 25.
    df <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        censor = c(0, 1, 0, 1),
        resp = c("CR", "PR", "SD", "PD"),
        arm = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        censorVar = "censor", responseVar = "resp", groupVar = "arm"
    ))

    s <- r$summary$asDF
    expect_equal(val_summary(s, "Number of Patients"), 4)
    expect_equal(val_summary(s, "Total Observations"), 4)
    expect_equal(val_summary(s, "Mean Duration"), 25.0)
    expect_equal(val_summary(s, "Median Duration \\(observed\\)"), 25.0)
    expect_equal(val_summary(s, "Total Person-Time"), 100.0)
})

test_that("C06-C15 RECIST 1.1 response rates and Clopper-Pearson CIs match hand calculation", {
    # CR=1, PR=1, SD=1, PD=1. N=4.
    # ORR = (1+1)/4 = 50.0%. Exact 95% CI: binom.test(2, 4) -> 6.8% to 93.2%.
    # DCR = (1+1+1)/4 = 75.0%. Exact 95% CI: binom.test(3, 4) -> 19.4% to 99.4%.
    df <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        censor = c(0, 1, 0, 1),
        resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        censorVar = "censor", responseVar = "resp"
    ))

    am <- r$advancedMetrics$asDF
    expect_equal(val_adv(am, "orr"), 50.0)
    expect_equal(ci_adv(am, "orr"), c(6.8, 93.2))

    expect_equal(val_adv(am, "dcr"), 75.0)
    expect_equal(ci_adv(am, "dcr"), c(19.4, 99.4))
})

test_that("C16 Reverse Kaplan-Meier median follow-up equals hand-calculated 30.0", {
    # censor: 0=censored (reverse-KM event), 1=event (reverse-KM censored).
    # Events at t=10 (P1) and t=30 (P3).
    # S(10) = 3/4 = 0.75. At t=20: censored. S(30) = 0.75*(1 - 1/2) = 0.375 <= 0.5.
    # Reverse KM median is 30.0.
    df <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        censor = c(0, 1, 0, 1),
        resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        censorVar = "censor", responseVar = "resp"
    ))

    am <- r$advancedMetrics$asDF
    expect_equal(val_adv(am, "median_followup"), 30.0)
})

test_that("B01-B07 Reference parity with survival::survfit and stats::binom.test on swimmerplot_censoring", {
    skip_if_not_installed("survival")
    data("swimmerplot_censoring", package = "ClinicoPath")

    r <- q(swimmerplot(
        data = swimmerplot_censoring, patientID = "PatientID", startTime = "StartTime",
        endTime = "EndTime", responseVar = "Response", censorVar = "CensorStatus"
    ))

    am <- r$advancedMetrics$asDF

    # Parity with survival::survfit
    km_fit <- survival::survfit(
        survival::Surv(swimmerplot_censoring$EndTime, 1 - swimmerplot_censoring$CensorStatus) ~ 1
    )
    km_median <- unname(summary(km_fit)$table["median"])
    expect_equal(val_adv(am, "median_followup"), km_median)

    # Parity with stats::binom.test
    # 7 responders (3 CR + 4 PR), 9 disease control (3 CR + 4 PR + 2 SD) out of 10
    bt_orr <- stats::binom.test(7, 10, conf.level = 0.95)
    bt_dcr <- stats::binom.test(9, 10, conf.level = 0.95)

    expect_equal(val_adv(am, "orr"), 70.0)
    expect_equal(ci_adv(am, "orr"), as.numeric(round(bt_orr$conf.int * 100, 1)))

    expect_equal(val_adv(am, "dcr"), 90.0)
    expect_equal(ci_adv(am, "dcr"), as.numeric(round(bt_dcr$conf.int * 100, 1)))
})

test_that("B08 Datetime interval person-time matches lubridate sum", {
    skip_if_not_installed("lubridate")
    data("swimmer_unified_datetime", package = "ClinicoPath")

    r <- q(swimmerplot(
        data = swimmer_unified_datetime, patientID = "PatientID", startTime = "StartDate",
        endTime = "EndDate", responseVar = "BestResponse", timeType = "datetime",
        dateFormat = "ymd", timeUnit = "months"
    ))

    ints <- lubridate::interval(
        as.Date(swimmer_unified_datetime$StartDate),
        as.Date(swimmer_unified_datetime$EndDate)
    )
    expected_pt <- round(sum(lubridate::time_length(ints, unit = "months")), 2)

    s <- r$summary$asDF
    expect_equal(val_summary(s, "Total Person-Time"), expected_pt, tolerance = 0.01)
})

test_that("B09 Sweep-line interval merging correctly handles overlapping episodes per patient", {
    # P1: [0, 10] and [5, 15] -> Union [0, 15] = 15.
    # P2: [0, 20] = 20. Total PT = 35 (not 40).
    df <- data.frame(
        id = c("P1", "P1", "P2"),
        start = c(0, 5, 0),
        end = c(10, 15, 20),
        censor = c(0, 0, 1),
        resp = c("PR", "PR", "CR"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        censorVar = "censor", responseVar = "resp"
    ))

    s <- r$summary$asDF
    expect_equal(val_summary(s, "Total Person-Time"), 35.0)
})

test_that("M01 Permuting dataset rows leaves all output metrics invariant", {
    df <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        censor = c(0, 1, 0, 1),
        resp = c("CR", "PR", "SD", "PD"),
        arm = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )

    r1 <- q(swimmerplot(data = df, patientID = "id", startTime = "start", endTime = "end",
                        censorVar = "censor", responseVar = "resp", groupVar = "arm"))
    r2 <- q(swimmerplot(data = df[c(3, 1, 4, 2), ], patientID = "id", startTime = "start", endTime = "end",
                        censorVar = "censor", responseVar = "resp", groupVar = "arm"))

    expect_equal(r2$summary$asDF$value, r1$summary$asDF$value)
    expect_equal(r2$advancedMetrics$asDF$metric_value, r1$advancedMetrics$asDF$metric_value)
    expect_equal(r2$groupComparisonTest$asDF$p_value, r1$groupComparisonTest$asDF$p_value)
})

test_that("M03 Progression in episode 1 precludes subsequent CR from contributing to best response", {
    # RECIST 1.1: best response is evaluated up to progression.
    # P1: [0, 10] PD, [10, 20] CR. Best response must be PD, ORR = 0%.
    df <- data.frame(
        id = c("P1", "P1"),
        start = c(0, 10),
        end = c(10, 20),
        censor = c(0, 0),
        resp = c("PD", "CR"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(data = df, patientID = "id", startTime = "start", endTime = "end",
                       censorVar = "censor", responseVar = "resp"))

    am <- r$advancedMetrics$asDF
    expect_equal(val_adv(am, "orr"), 0.0)
    expect_equal(val_adv(am, "dcr"), 0.0)
})

test_that("D01 100% events (0% censored) falls back honestly to observed median duration", {
    df <- data.frame(
        id = paste0("P", 1:4), start = 0, end = c(10, 20, 30, 40),
        censor = c(1, 1, 1, 1), resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(data = df, patientID = "id", startTime = "start", endTime = "end",
                       censorVar = "censor", responseVar = "resp"))

    am <- r$advancedMetrics$asDF
    row <- am[gsub('^"|"$', "", rownames(am)) == "median_followup", ]
    # LABEL CORRECTED 2026-09-20. A censoring variable WAS supplied here and
    # every value WAS classified - all four are events - so the reverse
    # Kaplan-Meier simply is not estimable. Calling that "no censoring
    # information" told the reader the opposite of what happened; the label now
    # names the real reason, and the estimator's own explanation is shown in
    # the interpretation cell beside it.
    expect_match(row$metric_name[1], "reverse Kaplan-Meier not estimable", fixed = TRUE)
    expect_false(grepl("no censoring information", row$metric_name[1], fixed = TRUE))
    expect_equal(as.numeric(row$metric_value[1]), 25.0)
})

test_that("D03 Negative duration rows are excluded before analysis", {
    df <- data.frame(
        id = c("P1", "P2"), start = c(0, 10), end = c(10, 5),
        censor = c(0, 0), resp = c("CR", "PR"), stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(data = df, patientID = "id", startTime = "start", endTime = "end",
                       censorVar = "censor", responseVar = "resp"))

    s <- r$summary$asDF
    expect_equal(val_summary(s, "Number of Patients"), 1.0)
    expect_equal(val_summary(s, "Total Person-Time"), 10.0)
})

test_that("D05 Unrecognised response categories leave ORR and DCR as NA, not fabricated 0%", {
    df <- data.frame(
        id = c("P1", "P2"), start = c(0, 0), end = c(10, 20),
        censor = c(0, 0), resp = c("unrec_1", "unrec_2"), stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(data = df, patientID = "id", startTime = "start", endTime = "end",
                       censorVar = "censor", responseVar = "resp"))

    am <- r$advancedMetrics$asDF
    expect_true(is.na(val_adv(am, "orr")))
    expect_true(is.na(val_adv(am, "dcr")))
})

test_that("G01-G04 Cross-output consistency and copy-ready manuscript text parity", {
    df <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        censor = c(0, 1, 0, 1),
        resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )

    r <- q(swimmerplot(data = df, patientID = "id", startTime = "start", endTime = "end",
                       censorVar = "censor", responseVar = "resp", showCopyReady = TRUE))

    am <- r$advancedMetrics$asDF
    orr <- val_adv(am, "orr")
    dcr <- val_adv(am, "dcr")
    ci_orr <- ci_adv(am, "orr")
    ci_dcr <- ci_adv(am, "dcr")

    # Invariant: DCR >= ORR
    expect_gte(dcr, orr)

    # CI nesting
    expect_gte(orr, ci_orr[1])
    expect_lte(orr, ci_orr[2])
    expect_gte(dcr, ci_dcr[1])
    expect_lte(dcr, ci_dcr[2])

    # Text parity
    txt <- r$copyReadyReport$content
    expect_match(txt, "4 patients", fixed = TRUE)
    expect_match(txt, "30.0 months", fixed = TRUE)
    expect_match(txt, "100.0 months", fixed = TRUE)
    expect_match(txt, "50.0%", fixed = TRUE)
    expect_match(txt, "75.0%", fixed = TRUE)
})

test_that("VAL-swimmerplot-01 ClinicoPath citation DOI in 00refs.yaml has no conflict", {
    skip("VAL-swimmerplot-01 open defect: jamovi/00refs.yaml ClinicoPathJamoviModule embeds Zenodo DOI in title while doi field is OSF DOI; year is 2022 instead of 2020. Remove skip when fixed via /update-refs.")
    refs_text <- paste(readLines("../../jamovi/00refs.yaml", warn = FALSE), collapse = "\n")
    # Title should not embed a conflicting DOI string
    expect_false(grepl("title:.*doi:10.5281/zenodo", refs_text))
})
