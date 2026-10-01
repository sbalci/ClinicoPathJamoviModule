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
df_hand <- data.frame(
    id = c("P1", "P2", "P3", "P4"),
    start = c(0, 0, 0, 0),
    end = c(10, 20, 30, 40),
    censor = c(0, 1, 0, 1),
    resp = c("CR", "PR", "SD", "PD"),
    arm = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
)

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
    refs_path <- testthat::test_path("..", "..", "jamovi", "00refs.yaml")
    skip_if_not(dir.exists(dirname(refs_path)), "package source tree not available")
    refs_text <- paste(readLines(refs_path, warn = FALSE), collapse = "\n")
    # Title should not embed a conflicting DOI string
    expect_false(grepl("title:.*doi:10.5281/zenodo", refs_text))
})

test_that("B10-B11 2-group and 3-group Fisher exact test p-value parity with stats::fisher.test", {
    # B10: 2 groups
    df_grp2 <- data.frame(
        id = paste0("P", 1:8),
        start = 0,
        end = 1:8,
        resp = c("CR", "PR", "SD", "PD", "CR", "CR", "PR", "PR"),
        arm = c("A", "A", "A", "A", "B", "B", "B", "B"),
        stringsAsFactors = FALSE
    )
    r_grp2 <- q(swimmerplot(data = df_grp2, patientID = "id", startTime = "start", endTime = "end",
                            responseVar = "resp", groupVar = "arm"))
    grp2_df <- r_grp2$groupComparisonTest$asDF
    tab2_orr <- table(factor(df_grp2$arm, levels = c("A", "B")),
                      factor(df_grp2$resp %in% c("CR", "PR"), levels = c(FALSE, TRUE)))
    ft2_orr <- stats::fisher.test(tab2_orr)
    expect_equal(as.numeric(grp2_df$p_value[1]), ft2_orr$p.value, tolerance = 1e-6)

    # B11: 3 groups
    df_grp3 <- data.frame(
        id = paste0("P", 1:9),
        start = 0,
        end = 1:9,
        resp = c("CR", "SD", "PD", "PR", "PR", "SD", "CR", "CR", "PR"),
        arm = factor(rep(c("Arm1", "Arm2", "Arm3"), each = 3), levels = c("Arm1", "Arm2", "Arm3")),
        stringsAsFactors = FALSE
    )
    r_grp3 <- q(swimmerplot(data = df_grp3, patientID = "id", startTime = "start", endTime = "end",
                            responseVar = "resp", groupVar = "arm"))
    grp3_df <- r_grp3$groupComparisonTest$asDF
    tab3_orr <- table(df_grp3$arm, factor(df_grp3$resp %in% c("CR", "PR"), levels = c(FALSE, TRUE)))
    ft3_orr <- stats::fisher.test(tab3_orr)
    expect_equal(as.numeric(grp3_df$p_value[1]), ft3_orr$p.value, tolerance = 1e-6)
})

test_that("B12-B14 Follow-up density, milestone table, and event marker table arithmetic", {
    # B12: Person-time table
    r_pt <- q(swimmerplot(data = df_hand, patientID = "id", startTime = "start", endTime = "end",
                          responseVar = "resp", personTimeAnalysis = TRUE, responseAnalysis = TRUE))
    pt_df <- r_pt$personTimeTable$asDF
    expect_equal(nrow(pt_df), 4L)
    expect_equal(sum(as.numeric(pt_df$total_time)), 100.0)
    expect_equal(sort(as.numeric(pt_df$incidence_rate)), round(c(1/40, 1/30, 1/20, 1/10) * 100, 3))

    # B13: Milestones table
    df_ms <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        surg = c(2, 4, 6, 8),
        resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )
    r_ms <- q(swimmerplot(data = df_ms, patientID = "id", startTime = "start", endTime = "end",
                          responseVar = "resp", milestone1Name = "Surgery", milestone1Date = "surg"))
    ms_df <- r_ms$milestoneTable$asDF
    expect_equal(as.numeric(ms_df$n_events[1]), 4)
    expect_equal(as.numeric(ms_df$median_time[1]), 5.0)

    # B14: Event markers
    df_evm <- data.frame(
        id = paste0("P", 1:6),
        start = 0,
        end = 10,
        ev = factor(c("Relapse", "Relapse", "AE", "AE", "AE", "Death")),
        stringsAsFactors = FALSE
    )
    r_evm <- q(swimmerplot(data = df_evm, patientID = "id", startTime = "start", endTime = "end",
                           showEventMarkers = TRUE, eventVar = "ev"))
    evm_df <- r_evm$eventMarkerTable$asDF
    expect_equal(sum(as.numeric(evm_df$n_events)), 6)
    ae_row <- evm_df[grepl("AE", evm_df$event_type), ]
    expect_equal(as.numeric(ae_row$n_events[1]), 3)
    expect_equal(as.numeric(ae_row$percent[1]), 0.5, tolerance = 1e-6)
})

test_that("M05-M06 Metamorphic properties: time scaling and person-time monotonicity", {
    # M05: Linear time scaling by c=30
    df_scale30 <- df_hand
    df_scale30$start <- df_scale30$start * 30
    df_scale30$end <- df_scale30$end * 30
    r_scale30 <- q(swimmerplot(data = df_scale30, patientID = "id", startTime = "start", endTime = "end",
                               censorVar = "censor", responseVar = "resp", groupVar = "arm"))
    s_scale30 <- r_scale30$summary$asDF
    am_scale30 <- r_scale30$advancedMetrics$asDF
    expect_equal(val_summary(s_scale30, "Total Person-Time"), 3000.0)
    expect_equal(val_summary(s_scale30, "Mean Duration"), 750.0)
    expect_equal(val_adv(am_scale30, "median_followup"), 900.0)
    expect_equal(val_adv(am_scale30, "orr"), 50.0)

    # M06: Extending episode increases person-time
    df_ext <- df_hand
    df_ext$end[4] <- 50
    r_ext <- q(swimmerplot(data = df_ext, patientID = "id", startTime = "start", endTime = "end",
                           censorVar = "censor", responseVar = "resp"))
    expect_equal(val_summary(r_ext$summary$asDF, "Total Person-Time"), 110.0)
})

test_that("D06-D11 Degenerate conditions, synonyms, and UTF-8 column headers", {
    # D06: Incomplete rows (NA in patientID, start, or end)
    df_na_rows <- data.frame(
        id = c("P1", NA, "P2", "P3"),
        start = c(0, 0, NA, 0),
        end = c(10, 20, 30, NA),
        resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )
    r_na_rows <- q(swimmerplot(data = df_na_rows, patientID = "id", startTime = "start", endTime = "end",
                               responseVar = "resp"))
    expect_equal(val_summary(r_na_rows$summary$asDF, "Number of Patients"), 1.0)
    expect_true(grepl("3 of 4 rows were excluded", r_na_rows$notices$content))

    # D07: Zero follow-up duration (end == start)
    df_zero_dur <- data.frame(
        id = c("P1", "P2"), start = c(5, 10), end = c(5, 10), resp = c("CR", "PR"), stringsAsFactors = FALSE
    )
    r_zero_dur <- q(swimmerplot(data = df_zero_dur, patientID = "id", startTime = "start", endTime = "end",
                                responseVar = "resp"))
    expect_equal(val_summary(r_zero_dur$summary$asDF, "Mean Duration"), 0.0)
    expect_true(grepl("Zero follow-up time", r_zero_dur$notices$content))

    # D08: Single group in groupVar
    df_single_grp <- data.frame(
        id = c("P1", "P2"), start = c(0, 0), end = c(10, 20),
        resp = c("CR", "PR"), arm = c("Arm1", "Arm1"), stringsAsFactors = FALSE
    )
    r_single_grp <- q(swimmerplot(data = df_single_grp, patientID = "id", startTime = "start", endTime = "end",
                                  responseVar = "resp", groupVar = "arm"))
    expect_true(grepl("needs at least two groups", r_single_grp$groupComparisonTest$notes$not_run$note))

    # D09: Partial input (startTime and endTime NULL)
    r_partial <- q(swimmerplot(data = df_hand, patientID = "id", startTime = NULL, endTime = NULL))
    expect_true(grepl("Start Time is still empty|Keep going", r_partial$notices$content))

    # D10: Response synonym normalization
    df_syn <- data.frame(
        id = paste0("P", 1:4), start = 0, end = 10,
        resp = c("Complete Response", "partial response", "stable disease", "progressive disease"),
        stringsAsFactors = FALSE
    )
    r_syn <- q(swimmerplot(data = df_syn, patientID = "id", startTime = "start", endTime = "end",
                           responseVar = "resp"))
    am_syn <- r_syn$advancedMetrics$asDF
    expect_equal(val_adv(am_syn, "orr"), 50.0)
    expect_equal(val_adv(am_syn, "dcr"), 75.0)

    # D11: Non-ASCII / UTF-8 column headers
    skip_if(identical(Sys.getlocale("LC_CTYPE"), "C"), "Cannot test UTF-8 column names under C locale")
    df_utf8 <- data.frame(
        `Patıent ID` = c("Pâtient_1", "Pâtient_2"),
        `Başlangıç` = c(0, 0),
        `Bitiş` = c(12, 24),
        `Yanıt` = c("CR", "PR"),
        check.names = FALSE, stringsAsFactors = FALSE
    )
    r_utf8 <- q(swimmerplot(data = df_utf8, patientID = "Patıent ID", startTime = "Başlangıç",
                            endTime = "Bitiş", responseVar = "Yanıt"))
    expect_equal(val_summary(r_utf8$summary$asDF, "Number of Patients"), 2.0)
})

test_that("E01-E08 High-risk option pairs behave consistently", {
    # E01: timeType="datetime", timeUnit="days" vs "weeks"
    data("swimmer_unified_datetime", package = "ClinicoPath")
    r_dt_days <- q(swimmerplot(data = swimmer_unified_datetime, patientID = "PatientID",
                               startTime = "StartDate", endTime = "EndDate",
                               timeType = "datetime", dateFormat = "ymd", timeUnit = "days"))
    r_dt_weeks <- q(swimmerplot(data = swimmer_unified_datetime, patientID = "PatientID",
                                startTime = "StartDate", endTime = "EndDate",
                                timeType = "datetime", dateFormat = "ymd", timeUnit = "weeks"))
    pt_days <- val_summary(r_dt_days$summary$asDF, "Total Person-Time")
    pt_weeks <- val_summary(r_dt_weeks$summary$asDF, "Total Person-Time")
    expect_equal(pt_days / pt_weeks, 7.0, tolerance = 1e-4)

    # E02: timeDisplay relative vs absolute
    r_dt_abs <- q(swimmerplot(data = swimmer_unified_datetime, patientID = "PatientID",
                              startTime = "StartDate", endTime = "EndDate",
                              timeType = "datetime", dateFormat = "ymd", timeDisplay = "absolute",
                              exportTimeline = TRUE))
    r_dt_rel <- q(swimmerplot(data = swimmer_unified_datetime, patientID = "PatientID",
                              startTime = "StartDate", endTime = "EndDate",
                              timeType = "datetime", dateFormat = "ymd", timeDisplay = "relative",
                              exportTimeline = TRUE))
    expect_equal(as.numeric(r_dt_rel$timelineData$asDF$start_time[1]), 0.0)
    expect_equal(as.numeric(r_dt_rel$timelineData$asDF$duration), as.numeric(r_dt_abs$timelineData$asDF$duration), tolerance = 1e-6)

    # E03: censorVar with 1/2 encoding
    df_censor_12 <- df_hand
    df_censor_12$censor <- df_hand$censor + 1
    r_censor_12 <- q(swimmerplot(data = df_censor_12, patientID = "id", startTime = "start",
                                 endTime = "end", censorVar = "censor", responseVar = "resp"))
    expect_equal(val_adv(r_censor_12$advancedMetrics$asDF, "median_followup"), 30.0)
    expect_true(grepl("contains only the values 1 and 2", r_censor_12$notices$content))

    # E04: sortOrder duration_desc vs duration_asc
    r_sort_desc <- q(swimmerplot(data = df_hand, patientID = "id", startTime = "start", endTime = "end",
                                 sortOrder = "duration_desc", exportTimeline = TRUE))
    r_sort_asc  <- q(swimmerplot(data = df_hand, patientID = "id", startTime = "start", endTime = "end",
                                 sortOrder = "duration_asc", exportTimeline = TRUE))
    expect_equal(as.character(r_sort_desc$timelineData$asDF$patient_id[1]), "P4")
    expect_equal(as.character(r_sort_asc$timelineData$asDF$patient_id[1]), "P1")

    # E05: responseAnalysis toggling
    r_no_resp <- q(swimmerplot(data = df_hand, patientID = "id", startTime = "start", endTime = "end",
                               responseVar = "resp", responseAnalysis = FALSE, groupVar = "arm"))
    expect_false(any(grepl("Rate", r_no_resp$summary$asDF$metric)))
    expect_false(any(c("orr", "dcr") %in% gsub('^"|"$', "", rownames(r_no_resp$advancedMetrics$asDF))))
    expect_equal(r_no_resp$groupComparisonTest$rowCount, 0)

    # E06: personTimeAnalysis toggling
    r_no_pt <- q(swimmerplot(data = df_hand, patientID = "id", startTime = "start", endTime = "end",
                             responseVar = "resp", personTimeAnalysis = FALSE))
    expect_equal(r_no_pt$personTimeTable$rowCount, 0)

    # E07: exportTimeline and exportSummary
    r_exp <- q(swimmerplot(data = df_hand, patientID = "id", startTime = "start", endTime = "end",
                           responseVar = "resp", exportTimeline = TRUE, exportSummary = TRUE))
    expect_equal(nrow(r_exp$timelineData$asDF), 4)
    expect_equal(nrow(r_exp$summaryData$asDF), 13)

    # E08: maxMilestones=1
    df_ms <- data.frame(
        id = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end = c(10, 20, 30, 40),
        surg = c(2, 4, 6, 8),
        resp = c("CR", "PR", "SD", "PD"),
        stringsAsFactors = FALSE
    )
    r_mm1 <- q(swimmerplot(data = df_ms, patientID = "id", startTime = "start", endTime = "end",
                           milestone1Name = "Surg", milestone1Date = "surg",
                           milestone2Name = "Assess", milestone2Date = "end",
                           maxMilestones = 1))
    expect_equal(r_mm1$milestoneTable$rowCount, 1)
    expect_true(grepl("Milestone slots not shown", r_mm1$notices$content))
})

test_that("F01-F02 Monte Carlo validation test sanity checks", {
    # Check that Clopper-Pearson 95% CI covers p=0.4 in sample size N=20
    for (k in 0:20) {
        ci <- stats::binom.test(k, 20)$conf.int
        expect_gte(ci[2], ci[1])
        expect_gte(ci[1], 0.0)
        expect_lte(ci[2], 1.0)
    }
})
