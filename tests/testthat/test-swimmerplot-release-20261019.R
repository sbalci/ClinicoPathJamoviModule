# Regression tests for the swimmerplot findings of the 2026-10-19 pre-release critical review
# (OncoPath docs/audit/release-review-20261019/review_swimmerplot.md).

sr2 <- function(...) suppressWarnings(ClinicoPath::swimmerplot(...))
sr2_txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))

test_that("SW-1: reference lines on an absolute axis with a common non-zero start are not drawn at bare durations", {
    d <- data.frame(id = paste0("P", 1:5), start = 100, end = c(104, 108, 113, 120, 130))
    r <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end",
             timeDisplay = "absolute", referenceLines = "median")
    a <- r$plot$analysis
    expect_false(a$.__enclos_env__$private$.isDurationAxis(r$plot$state$patient_data))
})

test_that("SW-2: a reverse-KM median that could not be estimated is not called follow-up", {
    d <- data.frame(id = paste0("P", 1:8), start = 0, end = c(2, 3, 4, 5, 20, 22, 24, 26),
                    st = c(0, 0, 1, 1, 1, 1, 1, 1))
    r <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "st",
             showCopyReady = TRUE, personTimeAnalysis = TRUE)
    cr <- sr2_txt(r$copyReadyReport$content)
    expect_match(cr, "could not be estimated, so this is not the length of follow-up", fixed = TRUE)
})

test_that("SW-3: a 0/1 column whose 1 means censored can be read that way, and its name raises a warning", {
    d <- data.frame(id = paste0("P", 1:8), start = 0, end = c(3, 5, 8, 10, 12, 15, 18, 24),
                    Censored = c(0, 0, 0, 1, 0, 1, 1, 1))
    auto <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "Censored")
    expect_match(sr2_txt(auto$notices$content), "Check the censoring coding", fixed = TRUE)
    skip_if_not_installed("survival")
    r <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "Censored",
             censorCoding = "one_censored", personTimeAnalysis = TRUE)
    km <- survival::survfit(survival::Surv(d$end, d$Censored) ~ 1)   # reverse KM: censored = "event"
    expect_equal(r$advancedMetrics$asDF$metric_value[1], unname(summary(km)$table[["median"]]))
    expect_setequal(as.character(r$plot$state$arrow_data$patient_id), d$id[d$Censored == 1])
})

test_that("SW-4: response sorting ranks a patient by best response, not by a post-progression row", {
    d <- data.frame(id = c("A", "A", "B", "C"), start = c(0, 6, 0, 0), end = c(6, 20, 10, 12),
                    resp = c("PD", "CR", "PR", "SD"))
    r <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp",
             sortOrder = "response", exportTimeline = TRUE)
    expect_equal(as.character(r$timelineData$asDF$patient_id), c("B", "C", "A"))
})

test_that("SW-5: duplicated episodes do not raise a false long-follow-up warning", {
    d <- data.frame(id = c("A", "A", "A", "B"), start = 0, end = c(50, 50, 50, 20))
    nt <- sr2_txt(sr2(data = d, patientID = "id", startTime = "start", endTime = "end")$notices$content)
    expect_false(grepl("exceeds 10 years", nt, fixed = TRUE))
})

test_that("SW-7: a single-valued status column waits for a coding", {
    d <- data.frame(id = paste0("P", 1:4), start = 0, end = 3:6, st = 1)
    r <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "st")
    expect_match(sr2_txt(r$notices$content), "Choose the censoring coding", fixed = TRUE)
    # an explicit 0/1 = event choice resolves it, so an all-censored column can be read too
    r0 <- sr2(data = transform(d, st = 0), patientID = "id", startTime = "start", endTime = "end",
              censorVar = "st", censorCoding = "one_event_01")
    expect_equal(NROW(r0$plot$state$arrow_data), 4)
    # 0/1/2 fits no single coding: recode, not choose
    d3 <- data.frame(id = paste0("P", 1:6), start = 0, end = 3:8, st = c(0, 1, 2, 0, 1, 2))
    nt3 <- sr2_txt(sr2(data = d3, patientID = "id", startTime = "start", endTime = "end",
                       censorVar = "st")$notices$content)
    expect_match(nt3, "Censoring variable not readable", fixed = TRUE)
})

test_that("SW-8/SW-9: one-patient caveat and arrow caption wording", {
    d <- data.frame(id = "P1", start = 0, end = 5, st = 0)
    r <- sr2(data = d, patientID = "id", startTime = "start", endTime = "end", showCopyReady = TRUE)
    expect_match(sr2_txt(r$copyReadyReport$content), "With only 1 patient these", fixed = TRUE)
    src <- readLines(test_path("..", "..", "R", "swimmerplot.b.R"), warn = FALSE)
    expect_false(any(grepl("patient still at risk (censored / alive) at the data cutoff", src, fixed = TRUE)))
})
