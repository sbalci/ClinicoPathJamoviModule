# Regression tests for the 2026-09-18 OncoPath release check (report and repro scripts:
# quality-reports/oncopath-release-2026-09-18/). Finding IDs (W00, GW03, ...) refer to that
# report. Expected values are computed by hand from the data in each test, not from the module.

wf_run <- function(d, ...) ClinicoPath::waterfall(data = d, ...)
wf_notices <- function(res) paste(res$notices$content, collapse = " ")
wf_counts <- function(res) {
  s <- res$summaryTable$asDF
  stats::setNames(s$n, s$category)
}
wf_metric <- function(res, pattern) {
  cm <- res$clinicalMetrics$asDF
  cm$value[grepl(pattern, cm$metric)][1]
}

# Four patients, baseline at time 0. Best change after baseline:
#   P1 +20% (50 -> 60 -> 70)  PD     P2 -60% (50 -> 30 -> 20)  PR
#   P3 +10% (40 -> 44 -> 52)  SD     P4 -10% (50 -> 45 -> 48)  SD
raw4 <- data.frame(pid = rep(c("P1", "P2", "P3", "P4"), each = 3), time = rep(c(0, 6, 12), 4),
                   size = c(50, 60, 70, 50, 30, 20, 40, 44, 52, 50, 45, 48))

# ---- Slice A: best response is taken over post-baseline assessments only -----------------

test_that("C1 (W00-W02): a patient who only grows is PD with raw input, and DCR excludes them", {
  res <- wf_run(raw4, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  n <- wf_counts(res)
  expect_equal(unname(n[c("CR", "PR", "SD", "PD")]), c(0, 1, 2, 1))
  expect_match(wf_metric(res, "Disease Control"), "^75\\.0%")
  w <- res$waterfallplot$state$data$waterfall
  expect_equal(w$response[w$pid == "P1"], 20)       # the bar is +20%, not 0%
  expect_equal(w$response[w$pid == "P3"], 10)
})

test_that("C1 (GW00): percentage input with a 0% time-0 row does not hide progression", {
  pct <- data.frame(pid = rep(c("P1", "P2", "P3", "P4"), each = 3), time = rep(c(0, 6, 12), 4),
                    chg = c(0, 20, 40, 0, -40, -60, 0, 10, 30, 0, -10, -4))
  res <- wf_run(pct, patientID = "pid", responseVar = "chg", timeVar = "time")
  expect_equal(unname(wf_counts(res)[c("CR", "PR", "SD", "PD")]), c(0, 1, 2, 1))
})

test_that("C1 on a cohort above 100 rows: progressors are PD there too", {
  set.seed(11)
  big <- do.call(rbind, lapply(1:40, function(i) {
    b <- round(runif(1, 30, 80))
    grow <- i <= 20                                    # 20 pure progressors (+25% then +40%)
    data.frame(pid = sprintf("P%02d", i), time = c(0, 2, 4),
               size = if (grow) b * c(1, 1.25, 1.40) else b * c(1, 0.6, 0.5))
  }))
  res <- wf_run(big, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  n <- wf_counts(res)
  expect_equal(unname(n["PD"]), 20)
  expect_equal(unname(n["PR"]), 20)
})

test_that("results do not depend on cohort size (one processing path)", {
  # The same per-patient data repeated: 30 patients (90 rows) vs 60 patients (180 rows).
  mk <- function(np) do.call(rbind, lapply(seq_len(np), function(i)
    data.frame(pid = sprintf("P%03d", i), time = c(0, 2, 4),
               size = c(100, 100, 100) * c(1, c(0.5, 1.3, 1.1)[(i %% 3) + 1], 1))))
  cats <- lapply(c(30, 60), function(np) {
    res <- wf_run(mk(np), patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
    w <- res$waterfallplot$state$data$waterfall
    w <- w[order(w$pid), ]
    as.character(w$recist_category[seq_len(30)])
  })
  expect_identical(cats[[1]], cats[[2]])
})

test_that("C3 (W03): a group that changes within a patient does not duplicate the patient", {
  set.seed(3)
  d <- do.call(rbind, lapply(1:40, function(i) {
    b <- runif(1, 30, 80)
    data.frame(pid = sprintf("P%02d", i), time = c(0, 2, 4),
               size = b * c(1, runif(1, 0.5, 1.3), runif(1, 0.5, 1.3)),
               arm = if (i %% 2) "A" else "B")
  }))
  d$arm[2] <- "B"                                      # P01's week-2 row recorded in the other arm
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", groupVar = "arm")
  expect_equal(sum(wf_counts(res)[c("CR", "PR", "SD", "PD")]), 40)
  expect_equal(sum(res$groupComparisonTable$asDF$n_patients), 40)
  expect_match(wf_notices(res), "P01")                 # the conflict is reported by patient
})

test_that("W14: a numeric group variable on a large cohort still draws the waterfall", {
  set.seed(5)
  d <- do.call(rbind, lapply(1:60, function(i)
    data.frame(pid = sprintf("P%02d", i), chg = round(runif(2, -80, 40)), arm = i %% 3)))
  res <- wf_run(d, patientID = "pid", responseVar = "chg", groupVar = "arm", colorBy = "group")
  img <- res$waterfallplot
  expect_true(is.factor(img$state$data$waterfall$patient_group))
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(isTRUE(img$analysis$.__enclos_env__$private$.waterfallplot(
    img, ggtheme = ggplot2::theme_grey(), theme = list())))
})

test_that("W06: rows without a patient ID are dropped and reported, not merged into one patient", {
  d <- data.frame(pid = c(paste0("P", 1:9), NA, NA, NA),
                  chg = c(-50, -40, -35, -10, 0, 10, 25, 30, -60, -90, -80, -70))
  res <- wf_run(d, patientID = "pid", responseVar = "chg")
  expect_equal(sum(wf_counts(res)[c("CR", "PR", "SD", "PD")]), 9)
  expect_match(wf_notices(res), "3 row\\(s\\) with no patient ID")
})

test_that("W15/W16: a follow-up row with a missing measurement does not make the patient SD at 0%", {
  d <- rbind(raw4, data.frame(pid = c("P5", "P5"), time = c(0, 6), size = c(50, NA)))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  w <- res$waterfallplot$state$data$waterfall
  expect_equal(as.character(w$recist_category[w$pid == "P5"]), "Unknown")
  expect_equal(wf_metric(res, "Evaluable"), "4")
  expect_match(wf_notices(res), "NOT RESPONSE-EVALUABLE")
})

test_that("W17: several rows per patient without a time variable are reported", {
  d <- data.frame(pid = rep(paste0("P", 1:6), each = 2), chg = c(-40, -10, 5, 30, -35, -50,
                                                                 0, 10, 25, 22, -20, -5))
  res <- wf_run(d, patientID = "pid", responseVar = "chg")
  expect_match(wf_notices(res), "more than one row")
})

test_that("W11: a patient-level override on a later row is used, not only the first row", {
  d <- raw4
  d$override <- NA_character_
  d$override[d$pid == "P2" & d$time == 12] <- "PD"     # new lesion recorded at week 12
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", responseCategoryVar = "override")
  w <- res$waterfallplot$state$data$waterfall
  expect_equal(as.character(w$recist_category[w$pid == "P2"]), "PD")
})

test_that("W33: a cohort with no evaluable patient gets one clear notice, no n = 0 sample-size notices", {
  d <- data.frame(pid = rep(paste0("P", 1:4), each = 2), time = rep(c(0, 6), 4),
                  size = c(50, NA, 40, NA, 30, NA, 20, NA))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", showConfidenceIntervals = TRUE)
  txt <- wf_notices(res)
  expect_match(txt, "NO EVALUABLE PATIENTS")
  expect_false(grepl("SMALL SAMPLE", txt))
  expect_false(grepl("VERY WIDE CONFIDENCE INTERVAL", txt))
})

test_that("W34/W35: with percentage input the exclusion reason is the missing value, not the baseline", {
  d <- data.frame(pid = paste0("P", 1:6), chg = c(-40, -10, 5, 30, NA, -50))
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "chg"))
  expect_match(txt, "PATIENTS EXCLUDED")
  expect_match(txt, "no non-missing response value")
  expect_false(grepl("baseline could not be established", txt))
})

# ---- Slice B: time to event and person-time -------------------------------------------------

wf_rdt <- function(res) res$responseDurationTable$asDF
wf_notes <- function(tbl) paste(vapply(tbl$notes, function(n) n$note, ""), collapse = " ")

test_that("W05/GW03: reappearance after a complete response is progression", {
  p <- ClinicoPath:::waterfallClass$new(
    options = ClinicoPath:::waterfallOptions$new(patientID = "id", responseVar = "r"),
    data = data.frame(id = "x", r = 1))$.__enclos_env__$private
  # CR at t = 2 (-100%), lesion back at t = 6 (-60%): RECIST PD at t = 6
  expect_equal(p$.progressionTimes(c(0, 2, 4, 6), c(0, -100, -100, -60), 2), 6)

  # Three of four responders relapse after CR -> KM median is estimable (hand: 5)
  d <- data.frame(pid = rep(c("C1", "C2", "C3", "C4"), each = 5), time = rep(0:4 * 2, 4),
                  size = c(100, 0, 0, 40, 40,     # CR t2, back t6  -> DoR 4, event
                           100, 0, 0, 0, 25,      # CR t2, back t8  -> DoR 6, event
                           100, 0, 30, 30, 30,    # CR t2, back t4  -> DoR 2, event
                           100, 0, 0, 0, 0))      # CR t2, sustained -> DoR 6, censored
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", showResponseDuration = TRUE)
  km <- wf_rdt(res)
  km <- km[grepl("Kaplan-Meier", km$metric), ]
  expect_equal(km$value, 4)                       # survfit: events at 2,4,6(+cens 6) -> median 4
  expect_match(km$detail, "3 progression events")
})

test_that("W07: duration of response is censored at the last MEASURED visit", {
  d <- data.frame(pid = rep(c("A", "B"), each = 3), time = rep(c(0, 2, 4), 2),
                  size = c(100, 60, NA,           # PR at t2, t4 scan missing -> censored at 2
                           100, 50, 45))
  p <- ClinicoPath:::waterfallClass$new(
    options = ClinicoPath:::waterfallOptions$new(patientID = "pid", responseVar = "size",
                                                timeVar = "time", inputType = "raw"),
    data = d)$.__enclos_env__$private
  proc <- p$.processData(d, "pid", "raw", "size", "time", NULL)
  tte <- p$.calculateTimeToEventMetrics(proc$spider, "pid", "time", "response")
  by_pt <- as.data.frame(tte$by_patient)
  expect_equal(by_pt$duration_of_response[by_pt$pid == "A"], 0)
  expect_equal(by_pt$duration_of_response[by_pt$pid == "B"], 2)
})

test_that("W09/W08/GW06: overrides and demotions reach TTR/DoR and the person-time table", {
  d <- raw4
  d$override <- NA_character_
  d$override[d$pid == "P2"] <- "PD"               # the only shrinking patient is overridden to PD
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                responseCategoryVar = "override", showResponseDuration = TRUE)
  expect_equal(unname(wf_counts(res)[c("PR", "PD")]), c(0, 2))
  # no responder remains, so there is no TTR/DoR to report and the table says why
  expect_false(any(grepl("n=1 responders", wf_rdt(res)$detail)))
  expect_match(wf_notes(res$responseDurationTable), "No patient")
  # the person-time table uses the same categories as the summary table
  pt <- res$personTimeTable$asDF
  pt <- pt[pt$category %in% c("CR", "PR", "SD", "PD"), ]
  expect_equal(pt$patients[match(c("PR", "SD", "PD"), pt$category)], c(0, 2, 2))
})

test_that("W10/GW07/W20: the headline DoR is the Kaplan-Meier median with its CI", {
  # 8 responders, 2 progress -> KM median not reached; the crude median would be a number
  set.seed(4)
  d <- do.call(rbind, lapply(1:8, function(i) data.frame(
    pid = sprintf("R%d", i), time = c(0, 2, 4, 6, 8),
    size = if (i <= 2) c(100, 50, 45, 60, 70) else c(100, 60, 50, 45, 40))))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", showResponseDuration = TRUE)
  cm <- res$clinicalMetrics$asDF
  row <- cm[grepl("Duration of Response", cm$metric), ]
  expect_equal(nrow(row), 1)
  expect_match(row$metric, "Kaplan-Meier")
  expect_match(row$value, "not reached")
  km <- wf_rdt(res)
  expect_match(km$detail[grepl("Kaplan-Meier", km$metric)], "95% CI")
})

test_that("GW04/GW05: person-time 'time in response' is the duration of response", {
  # PR at t2, nadir 45 at t4, 60 at t6 is +33% over the nadir -> PD at t6 although
  # still -40% from baseline. DoR = 4; the old first-to-last responding-scan span was 6.
  d <- data.frame(pid = rep(c("A", "B"), each = 5), time = rep(0:4 * 2, 2),
                  size = c(100, 55, 45, 60, 62,
                           100, 90, 95, 100, 105))
  p <- ClinicoPath:::waterfallClass$new(
    options = ClinicoPath:::waterfallOptions$new(patientID = "pid", responseVar = "size",
                                                timeVar = "time", inputType = "raw"),
    data = d)$.__enclos_env__$private
  proc <- p$.processData(d, "pid", "raw", "size", "time", NULL)
  cats <- data.frame(pid = proc$waterfall$pid, category = as.character(proc$waterfall$recist_category))
  tte <- p$.calculateTimeToEventMetrics(proc$spider, "pid", "time", "response", cats)
  ptm <- p$.calculatePersonTimeMetrics(proc$spider, "pid", "time", "size", cats, tte)
  bp <- as.data.frame(ptm$by_patient)
  expect_equal(bp$time_in_response[bp$pid == "A"], 4)
  expect_equal(bp$time_in_response[bp$pid == "B"], 0)
  # time in response / follow-up = 4 / (8 + 8) * 100
  expect_equal(ptm$summary$response_rate_per_100, 25)
})

test_that("W21/GW10: no responders -> the DoR table explains itself; empty categories print blank", {
  res <- wf_run(raw4[raw4$pid != "P2", ], patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", showResponseDuration = TRUE)
  expect_match(wf_notes(res$responseDurationTable), "No patient")
  pt <- res$personTimeTable$asDF
  expect_false(any(pt$median_time == "NA" | pt$median_duration == "NA", na.rm = TRUE))
})
