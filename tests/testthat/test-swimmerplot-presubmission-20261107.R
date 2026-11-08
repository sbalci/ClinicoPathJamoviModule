# Regression tests for the swimmerplot findings of the 2026-11-07 pre-submission review
# (OncoPath 1.1.3). Each test fails on the code before the fix.

sp7 <- function(...) suppressWarnings(ClinicoPath::swimmerplot(...))
sp7_txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))
sp7_note <- function(tbl, key) {
    n <- tbl$notes[[key]]
    if (is.null(n)) "" else n$note
}

test_that("SW-H1: a patient-level response repeated on every row is not called derived or unconfirmed", {
    d <- data.frame(id = rep(sprintf("P%02d", 1:10), each = 2),
                    s = rep(c(0, 6), 10), e = rep(c(6, 12), 10) + rep(0:9, each = 2),
                    BOR = rep(c("CR", "PR", "PR", "SD", "SD", "SD", "PD", "PD", "NE", "PR"), each = 2))
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", responseVar = "BOR",
             showCopyReady = TRUE)
    expect_false(grepl("Unconfirmed best response", sp7_txt(r$notices$content), fixed = TRUE))
    cr <- sp7_txt(r$copyReadyReport$content)
    expect_true(nzchar(cr))
    expect_false(grepl("not confirmed", cr, fixed = TRUE))
    expect_false(grepl("unconfirmed", sp7_note(r$personTimeTable, "guarantee_time"), fixed = TRUE))
    # the numbers are unchanged: 4/10 responders, 7/10 disease control
    am <- r$advancedMetrics$asDF
    expect_equal(am$metric_value[am$metric_name == "Objective Response Rate (ORR)"], 40)
    expect_equal(am$metric_value[am$metric_name == "Disease Control Rate (DCR)"], 70)
})

test_that("SW-H1: different responses across rows are still disclosed", {
    d <- data.frame(id = c("A", "A", "B", "B", "C"), s = c(0, 6, 0, 4, 0), e = c(6, 12, 4, 10, 8),
                    r = c("SD", "PR", "PR", "PR", "PD"))
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", responseVar = "r",
             showCopyReady = TRUE)
    expect_match(sp7_txt(r$notices$content), "1 of 3 patients have more than one different response",
                 fixed = TRUE)
    expect_match(sp7_txt(r$copyReadyReport$content), "were not confirmed", fixed = TRUE)
    expect_match(sp7_note(r$personTimeTable, "guarantee_time"), "unconfirmed", fixed = TRUE)
})

test_that("SW-M1: responses and censoring values on excluded rows are named in the exclusion notice", {
    d <- data.frame(id = c("P01", "P01", "P02", "P02", "P03", "P03", "P04", "P05", "P06"),
                    s = c(0, 6, 0, 4, 0, 5, 0, 0, 0), e = c(6, NA, 4, NA, 5, NA, 10, 8, 12),
                    resp = c("SD", "PR", "SD", "PR", "SD", "CR", "PD", "PR", "SD"),
                    status = c(0, 0, 0, 0, 0, 0, 1, 1, 1))
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", responseVar = "resp",
             censorVar = "status")
    txt <- sp7_txt(r$notices$content)
    expect_match(txt, "3 excluded rows had a recorded response that is not used: P01 (PR), P02 (PR), P03 (CR)",
                 fixed = TRUE)
    expect_match(txt, "3 excluded rows had a censoring/event value that is not used", fixed = TRUE)
})

test_that("SW-L10: a group that varies within a patient skips only the comparison", {
    g <- data.frame(id = c("A", "A", "B", "C"), s = c(0, 5, 0, 0), e = c(5, 9, 7, 8),
                    r = c("PR", "SD", "CR", "PD"), arm = c("X", "Y", "X", "Y"))
    r <- sp7(data = g, patientID = "id", startTime = "s", endTime = "e", responseVar = "r",
             groupVar = "arm")
    expect_equal(r$summary$asDF$value[1], 3)
    expect_equal(r$groupComparisonTest$rowCount, 0)
    expect_match(sp7_note(r$groupComparisonTest, "not_run"), "different group values", fixed = TRUE)
    expect_match(sp7_txt(r$notices$content), "Group comparison skipped", fixed = TRUE)
})

test_that("SW-L2/L4: text 'NA' is a missing response and the response rows follow clinical order", {
    d <- data.frame(id = sprintf("P%d", 1:6), s = 0, e = 5:10,
                    r = c("SD", "PD", "NA", "CR", "PR", "NE"))
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", responseVar = "r")
    m <- r$summary$asDF$metric
    expect_equal(m[6:11], c("CR Rate (1/6)", "PR Rate (1/6)", "SD Rate (1/6)", "PD Rate (1/6)",
                            "NE Rate (1/6)", "No recorded response (1/6)"))
})

test_that("SW-L3: Non-CR/Non-PD is not called a non-RECIST category", {
    d <- data.frame(id = sprintf("P%d", 1:4), s = 0, e = 5:8, r = c("CR", "PR", "Non-CR/Non-PD", "PD"))
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", responseVar = "r")
    txt <- sp7_txt(r$notices$content)
    expect_false(grepl("not RECIST categories", txt, fixed = TRUE))
    expect_match(txt, "Non-CR/Non-PD is the RECIST 1.1 response", fixed = TRUE)
})

test_that("SW-L7: Sort by variable uses each patient's first non-missing value", {
    d <- data.frame(id = c("A", "B", "B", "C"), s = 0, e = c(5, 6, 7, 8), age = c(70, NA, 40, 60))
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", sortVariable = "age")
    expect_equal(rev(levels(r$plot$state$patient_data$patient_id)), c("B", "C", "A"))
})

test_that("SW-L5: an unestimable follow-up bound is not printed as NE", {
    a <- ClinicoPath:::swimmerplotClass$new(
        options = ClinicoPath:::swimmerplotOptions$new(),
        data = data.frame(x = 1))
    txt <- a$.__enclos_env__$private$.followUpCIText(NA_real_, 25)
    expect_false(grepl("^NE", txt))
    expect_match(txt, "25.00", fixed = TRUE)
})

test_that("SW-R1: relative mode classifies end < start on a later row when first row has start = NA", {
    d <- data.frame(
        id = c("P1", "P1", "P2", "P3"),
        s  = c(NA, 10, 0, 0),
        e  = c(5,   4, 8, 12)
    )
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e", timeDisplay = "relative")
    txt <- sp7_txt(r$notices$content)
    expect_match(txt, "1 had an end time before the start time", fixed = TRUE)
    expect_false(grepl("not a number", txt, fixed = TRUE))
})

test_that("SW-R2: an all-missing responseVar with groupVar does not claim group response rates or 0.0% footnotes", {
    d <- data.frame(
        id  = sprintf("P%d", 1:6),
        s   = rep(0, 6),
        e   = c(4, 6, 8, 10, 12, 14),
        rsp = rep(NA_character_, 6),
        grp = rep(c("Arm A", "Arm B"), 3)
    )
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e",
             responseVar = "rsp", groupVar = "grp")
    txt <- sp7_txt(r$notices$content)
    expect_false(grepl("Group response rates in the Group Comparison table", txt, fixed = TRUE))
    expect_equal(sp7_note(r$groupComparisonTest, "response_comparison"), "")
})

test_that("SW-R3: excluded rows do not leak milestones, event markers, or sort values into valid episodes", {
    d <- data.frame(
        id     = c("P1", "P1", "P2", "P3"),
        s      = c(0, 5, 0, 0),
        e      = c(6, NA, 8, 10),
        m1     = c(NA, 4, 3, 2),
        ev     = c(NA, "Tox", "Response", NA),
        evtime = c(NA, 4, 3, NA),
        sortv  = c(100, 1, 50, 10)
    )
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e",
             milestone1Date = "m1", milestone1Name = "M1",
             eventVar = "ev", eventTimeVar = "evtime",
             sortVariable = "sortv")
    st <- r$plot$state
    expect_false("P1" %in% as.character(st$milestone_data$patient_id))
    expect_false("P1" %in% as.character(st$event_markers$patient_id))
    expect_equal(rev(levels(st$patient_data$patient_id)), c("P3", "P2", "P1"))
})

test_that("SW-R4: sortOrder = response groups non-RECIST responses ahead of missing responses", {
    d <- data.frame(
        id  = c("P1", "P2", "P3", "P4"),
        s   = c(0, 0, 0, 0),
        e   = c(5, 6, 7, 8),
        rsp = c(NA, "MR", "PR", "MR")
    )
    r <- sp7(data = d, patientID = "id", startTime = "s", endTime = "e",
             responseVar = "rsp", sortOrder = "response")
    ord <- rev(levels(r$plot$state$patient_data$patient_id))
    expect_equal(ord[1], "P3")
    expect_true(all(ord[2:3] %in% c("P2", "P4")))
    expect_equal(ord[4], "P1")
})
