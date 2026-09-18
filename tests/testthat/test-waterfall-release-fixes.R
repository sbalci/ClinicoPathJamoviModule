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
  # Reference: survfit on the hand-derived DoRs (4, 6, 2 events; 6 censored). S(t) is
  # exactly 0.5 on [4, 6), so survfit reports the midpoint 5, with 95% CI 2 to not reached.
  ref <- summary(survival::survfit(survival::Surv(c(4, 6, 2, 6), c(1, 1, 1, 0)) ~ 1))$table
  expect_equal(km$value, unname(ref["median"]))
  expect_match(km$detail, "3 progression events")
  expect_match(km$detail, sprintf("95%% CI %.1f to not estimable", ref["0.95LCL"]))
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
  expect_match(km$detail[grepl("Kaplan-Meier", km$metric)], "not reached \\(only 2 of 8")
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

# ---- Slice C: input-scale diagnostics -------------------------------------------------------

test_that("GW02: percent change entered as proportions is flagged", {
  d <- data.frame(pid = paste0("P", 1:10), chg = c(-0.35, -0.5, -0.1, 0.05, 0.2, -0.8, -1, 0.3, -0.45, 0))
  expect_match(wf_notices(wf_run(d, patientID = "pid", responseVar = "chg")), "PROPORTIONS")
})

test_that("GW02: partly proportion-scaled entry is flagged", {
  d <- data.frame(pid = paste0("P", 1:10), chg = c(-35, -0.5, -10, 0.2, 20, -0.8, -60, 0.3, -45, 5))
  expect_match(wf_notices(wf_run(d, patientID = "pid", responseVar = "chg")), "PROPORTIONS")
})

test_that("W04: raw measurements under the default percentage input are flagged", {
  d <- raw4                                            # sizes 40-70 mm, time 0 rows are not 0%
  expect_match(wf_notices(wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time")),
               "RAW MEASUREMENTS")
})

test_that("GW08: percent change multiplied by 100 is flagged", {
  d <- data.frame(pid = paste0("P", 1:8), chg = c(-3500, -4500, 1000, -9000, 2500, -500, -3000, 800))
  expect_match(wf_notices(wf_run(d, patientID = "pid", responseVar = "chg")), "MULTIPLIED BY 100")
})

test_that("GW09: both validation warnings survive when shrinkage and growth outliers occur together", {
  d <- data.frame(pid = paste0("P", 1:10), chg = c(-150, 250, -30, 20, -10, 5, -45, 60, -70, 15))
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "chg"))
  expect_match(txt, "Invalid Tumor Shrinkage Values Detected")
  expect_match(txt, "Unusually Large Growth Values Detected")
})

test_that("well-scaled percent change raises no scale warning", {
  d <- data.frame(pid = paste0("P", 1:8), chg = c(-35, -45, 10, -90, 25, -5, -30, 8))
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "chg"))
  expect_false(grepl("PROPORTIONS|MULTIPLIED BY 100|RAW MEASUREMENTS", txt))
})

# ---- Slice D: group comparison ---------------------------------------------------------------

grp_data <- function() {
  # Drug 7/10 responders, Placebo 1/10 (percentage input, one row per patient)
  data.frame(pid = paste0("P", 1:20), arm = rep(c("Drug", "Placebo"), each = 10),
             chg = c(-40, -50, -35, -60, -45, -31, -70, 10, 5, -10,
                     -40, 10, 5, -10, 15, 0, 20, -5, 8, -20))
}

test_that("W12: the Fisher odds ratio names its direction and carries a CI", {
  res <- wf_run(grp_data(), patientID = "pid", responseVar = "chg", groupVar = "arm")
  gt <- res$groupComparisonTest$asDF
  orr <- gt$test_statistic[grepl("Objective", gt$comparison)]
  # Reference: fisher.test on the 2x2 table, rows Drug/Placebo, columns non-responder/responder
  ref <- stats::fisher.test(matrix(c(3, 7, 9, 1), nrow = 2, byrow = TRUE))
  expect_match(orr, "Placebo vs Drug")
  expect_match(orr, sprintf("= %.2f", ref$estimate))
  expect_match(orr, sprintf("95%% CI %.2f-%.2f", ref$conf.int[1], ref$conf.int[2]))
})

test_that("W27: a very small p-value is kept as a number, not rounded to 0", {
  d <- data.frame(pid = paste0("P", 1:40), arm = rep(c("A", "B"), each = 20),
                  chg = c(rep(-50, 18), 10, 10, rep(10, 20)))
  gt <- wf_run(d, patientID = "pid", responseVar = "chg", groupVar = "arm")$groupComparisonTest$asDF
  p <- gt$p_value[grepl("Objective", gt$comparison)]
  expect_gt(p, 0)
  expect_equal(p, stats::fisher.test(matrix(c(2, 18, 20, 0), 2, byrow = TRUE))$p.value, tolerance = 1e-12)
})

test_that("W25/W26: patients with a missing group are excluded from the comparison, with a note", {
  d <- grp_data()
  d$arm[c(3, 15)] <- NA
  res <- wf_run(d, patientID = "pid", responseVar = "chg", groupVar = "arm")
  tab <- res$groupComparisonTable$asDF
  expect_false(any(is.na(tab$group) | tab$group %in% c("NA", "<NA>")))
  expect_equal(sum(tab$n_patients), 18)
  expect_match(wf_notes(res$groupComparisonTable), "2 patient\\(s\\) with a missing group")
})

# ---- Slice E: wording and presentation -----------------------------------------------------

wf_render <- function(res, item, fun) {
  img <- res[[item]]
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  img$analysis$.__enclos_env__$private[[fun]](img, ggtheme = ggplot2::theme_grey(), theme = list())
  ggplot2::last_plot()
}
verdict_words <- "Promising|Excellent|Moderate activity|Limited activity|Good disease control|Limited disease control"

test_that("W13: rates are reported with their CI, without efficacy verdicts", {
  res <- wf_run(grp_data(), patientID = "pid", responseVar = "chg", showConfidenceIntervals = TRUE)
  expect_false(any(grepl(verdict_words, res$clinicalMetrics$asDF$value)))
  ref <- stats::binom.test(8, 20)$conf.int * 100                  # 8 responders of 20
  expect_match(wf_metric(res, "Objective Response"), sprintf("95%% CI %.1f-%.1f", ref[1], ref[2]))
  ecm <- res$enhancedClinicalMetrics$asDF
  expect_false(any(grepl(verdict_words, ecm$interpretation)))
  expect_match(ecm$interpretation[1], "compatible with these data")
})

test_that("W18: one factual disclaimer, no regulatory claims", {
  txt <- wf_notices(wf_run(grp_data(), patientID = "pid", responseVar = "chg"))
  expect_match(txt, "EXPLORATORY RESPONSE CATEGORIES")
  expect_false(grepl("REGULATORY USE PROHIBITED|RECIST COMPLIANCE LIMITATION|CONFIRMATION NOT REQUIRED", txt))
  expect_false(grepl("FDA|EMA|certified", txt))
})

test_that("W24/W30: copy-ready text and summary report CIs, the evaluable count and one decimal", {
  # rows 1-9 of grp_data(): -40 -50 -35 -60 -45 -31 -70 +10 and a missing 9th value
  d <- grp_data()[1:9, ]
  d$chg[9] <- NA                                    # 8 evaluable: 7 PR, 1 SD
  res <- wf_run(d, patientID = "pid", responseVar = "chg", generateCopyReadyReport = TRUE,
                showExplanations = TRUE)
  cr <- res$copyReadyReport$content
  expect_match(cr, "unconfirmed")
  expect_match(cr, "Of 9 patients, 8 were evaluable")
  oref <- stats::binom.test(7, 8)$conf.int * 100                  # ORR 7/8 = 87.5%
  dref <- stats::binom.test(8, 8)$conf.int * 100                  # DCR 8/8 = 100%
  expect_match(cr, sprintf("87\\.5%% \\(95%% CI %.1f-%.1f", oref[1], oref[2]))
  expect_match(cr, sprintf("100\\.0%% \\(95%% CI %.1f-%.1f", dref[1], dref[2]))
  expect_match(res$naturalLanguageSummary$content, "87\\.5%")    # one decimal, not "88%"
})

test_that("GT07: the welcome text does not name another product", {
  res <- ClinicoPath::waterfall(data = grp_data(), patientID = "pid")   # response not chosen yet
  expect_false(grepl("ClinicoPath", res$todo$content))
})

test_that("W22: the median-CI notice counts evaluable patients, as the plot does", {
  d <- data.frame(pid = paste0("P", 1:12), chg = c(-40, -10, 5, 30, -35, -50, 20, -5, 15,
                                                   NA, NA, NA))
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "chg", showCI = TRUE))
  expect_match(txt, "MEDIAN CI NOT DRAWN")
})

test_that("W32: the reused-marker-shapes notice reaches the notices panel", {
  d <- grp_data(); d$conf <- rep(letters[1:7], length.out = 20)
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "chg", confirmationVar = "conf"))
  expect_match(txt, "MARKER SHAPES REUSED")
})

test_that("W29: plot states carry small plot-ready frames only", {
  res <- wf_run(raw4, patientID = "pid", responseVar = "size", timeVar = "time",
                inputType = "raw", showSpiderPlot = TRUE)
  ws <- res$waterfallplot$state
  expect_null(ws$data$spider)
  expect_null(ws$metrics)
  ss <- res$spiderplot$state
  expect_null(ss$data$waterfall)
  expect_true(all(names(ss$data$spider) %in% c("pid", "time", "response", "patient_group")))
})

test_that("W23/W28: spider thresholds follow showThresholds; waterfall axis names the best change", {
  res <- wf_run(raw4, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                showSpiderPlot = TRUE, showThresholds = FALSE)
  sp <- wf_render(res, "spiderplot", ".spiderplot")
  expect_false(any(vapply(sp$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))))
  wp <- wf_render(res, "waterfallplot", ".waterfallplot")
  expect_match(wp$labels$y, "Best change from baseline")
})

test_that("GT02: spider points keep responder colours under a non-English locale", {
  res <- wf_run(raw4, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                showSpiderPlot = TRUE)
  priv <- res$spiderplot$analysis$.__enclos_env__$private
  key <- priv$.spiderResponderKey(c(-50, -10, 30))
  expect_identical(as.character(key), c("Responder", "Non-responder", "Non-responder"))
})

test_that("W28: a zero raw measurement (complete disappearance) raises no data warning", {
  d <- raw4; d$size[d$pid == "P2" & d$time == 12] <- 0
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw"))
  expect_false(grepl("zero or negative", txt))
})

# ---- Round 2: issues raised by the independent review of the fix ----------------------------

test_that("R2-float: progression at exactly +20% over the nadir is detected with raw input", {
  p <- ClinicoPath:::waterfallClass$new(
    options = ClinicoPath:::waterfallOptions$new(patientID = "id", responseVar = "r"),
    data = data.frame(id = "x", r = 1))$.__enclos_env__$private
  miss <- 0; tot <- 0
  for (b in 30:120) for (n in 25:(b - 1)) {
    l <- n * 1.2; if (l != round(l)) next
    v <- ((c(b, n, l) - b) / b) * 100                  # module's percent values from raw mm
    tot <- tot + 1
    if (!identical(p$.progressionTimes(c(0, 2, 4), v, 2), 4)) miss <- miss + 1
  }
  expect_gt(tot, 500)
  expect_equal(miss, 0)
  # category boundaries from raw input: exactly -30% is PR, exactly +20% is PD
  expect_equal(as.character(p$.categorizeRECIST(((c(21, 36) - c(30, 30)) / c(30, 30)) * 100)), c("PR", "PD"))
})

test_that("R2-notime: several rows per patient without a time variable get a strong warning", {
  pct <- data.frame(pid = rep(c("P1", "P2", "P3", "P4"), each = 3),
                    chg = c(0, 20, 40, 0, -40, -60, 0, 10, 30, 0, -10, -4))
  txt <- wf_notices(wf_run(pct, patientID = "pid", responseVar = "chg"))
  expect_match(txt, "SEVERAL ROWS PER PATIENT")
  expect_match(txt, "a 0% baseline row")
})

test_that("R2-counts: one cohort count across notices, summaries and copy-ready text", {
  d <- rbind(raw4,
             data.frame(pid = c("P5", "P5"), time = c(0, 6), size = c(50, NA)),   # Unknown (no measured follow-up)
             data.frame(pid = c("P6", "P6"), time = c(0, 6), size = c(0, 10)))    # excluded (zero baseline)
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                generateCopyReadyReport = TRUE, showExplanations = TRUE)
  txt <- wf_notices(res)
  expect_match(txt, "computed over the 4 evaluable patients")
  expect_match(res$clinicalSummary$content, "6 patients \\(4 evaluable\\)")
  expect_match(res$naturalLanguageSummary$content, "6 patients \\(4 evaluable\\)")
  expect_match(res$copyReadyReport$content, "Of 6 patients, 4 were evaluable")
})

test_that("R2-KM: missing CI limits are spelled out; a not-reached median keeps its lower limit", {
  # 20 responders (PR at t = 2). Eight progress (+40% over the nadir) after DoR 2,2,4,4,6,6,8,8;
  # twelve stay in response to t = 12 (censored at DoR 10). survfit: median NR, 95% CI 8 to NA.
  dor <- c(2, 2, 4, 4, 6, 6, 8, 8, rep(NA, 12))
  d <- do.call(rbind, lapply(seq_along(dor), function(i) {
    tt <- seq(0, 12, 2)
    size <- ifelse(tt == 0, 100, 50)
    if (!is.na(dor[i])) size[tt >= 2 + dor[i]] <- 70
    data.frame(pid = sprintf("R%02d", i), time = tt, size = size)
  }))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  ref <- summary(survival::survfit(survival::Surv(ifelse(is.na(dor), 10, dor), !is.na(dor)) ~ 1))$table
  row <- wf_metric(res, "Duration of Response")
  expect_true(is.na(ref["median"]))
  expect_match(row, "not reached")
  expect_match(row, sprintf("%.1f", ref["0.95LCL"]))           # the finite lower limit is kept
  expect_false(grepl("\\bNR\\b", row))                         # no undefined abbreviation
})

test_that("R2-units: time-to-event values carry the chosen time unit", {
  res <- wf_run(raw4, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                timeUnitLabel = "months")
  expect_match(wf_metric(res, "Time to First Response"), "months")
  expect_false(grepl("time units", wf_metric(res, "Time to First Response")))
})

test_that("R2-benchmarks: the clinical significance panel has no unsourced ORR bands", {
  res <- wf_run(grp_data(), patientID = "pid", responseVar = "chg", showClinicalSignificance = TRUE)
  expect_false(grepl("conventionally cited|15-30%|ORR <15%", res$clinicalSignificance$content))
})

test_that("R2-zero: with nobody evaluable the category percentages are empty, not 0%", {
  d <- data.frame(pid = rep(paste0("P", 1:4), each = 2), time = rep(c(0, 6), 4),
                  size = c(50, NA, 40, NA, 30, NA, 20, NA))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  expect_true(all(is.na(res$summaryTable$asDF$percent[1:4])))
})

test_that("R2-person-time: override-only and single responders do not get a DoR median", {
  d <- raw4
  d$override <- NA_character_
  d$override[d$pid == "P3"] <- "PR"                    # responder by override, never measured <= -30%
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                responseCategoryVar = "override")
  pt <- res$personTimeTable$asDF
  expect_false(any(pt$median_duration == "not reached", na.rm = TRUE))   # no DoR data, or n = 1
})

test_that("R2-notices: processing errors appear once; plot-only notices follow the plot option", {
  d <- data.frame(pid = paste0("P", 1:6), time = 0, size = c(50, 40, 30, 20, 10, 60))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  expect_false(isTRUE(res$todo2$visible) && grepl("No patients", res$todo2$content))
  d2 <- grp_data(); d2$conf <- rep(letters[1:7], length.out = 20)
  txt <- wf_notices(wf_run(d2, patientID = "pid", responseVar = "chg", confirmationVar = "conf",
                           showWaterfallPlot = FALSE, showCI = TRUE))
  expect_false(grepl("MARKER SHAPES|Marker shapes|BOOTSTRAP CI|MEDIAN CI", txt))
})

test_that("R2-scale: raw sizes with a 0 mm value are still recognised; progression-heavy data are not 'x100'", {
  sizes <- data.frame(pid = paste0("P", 1:8), size = c(45, 0, 30, 60, 25, 52, 18, 40))
  expect_match(wf_notices(wf_run(sizes, patientID = "pid", responseVar = "size")), "NO NEGATIVE CHANGES")
  prog <- data.frame(pid = paste0("P", 1:8), chg = c(110, 150, 240, 130, 180, 95, 200, 160))
  expect_false(grepl("MULTIPLIED BY 100", wf_notices(wf_run(prog, patientID = "pid", responseVar = "chg"))))
})

# ---- Round 3: best response until progression, ITT rate, remaining review items -------------

test_that("R3-BOR: scans after a documented progression do not improve the best response", {
  # RECIST v1.1: any progression precludes a later CR, PR or SD (Seymour 2017, iRECIST).
  d <- data.frame(pid = rep(c("G1", "G2", "G3"), each = 4), time = rep(c(0, 2, 4, 6), 3),
                  size = c(100, 125, 60, 60,     # PD at t2 (+25%), shrinks later       -> PD, bar +25
                           100, 50, 65, 0,       # PR t2, +30% over nadir at t4 (PD), later 0 -> PR, bar -50
                           100, 90, 60, 55))     # SD then PR, no progression           -> PR, bar -45
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  w <- res$waterfallplot$state$data$waterfall
  w <- w[order(w$pid), ]
  expect_equal(as.character(w$recist_category), c("PD", "PR", "PR"))
  expect_equal(w$response, c(25, -50, -45))
})

test_that("R3-BOR: percentage input without a baseline row still sees progression at the first scan", {
  d <- data.frame(pid = rep(c("A", "B"), each = 2), time = rep(c(2, 4), 2),
                  chg = c(25, -40,     # +25% at the first scan is PD against the baseline
                          -35, -50))
  w <- wf_run(d, patientID = "pid", responseVar = "chg", timeVar = "time")$waterfallplot$state$data$waterfall
  expect_equal(as.character(w$recist_category[order(w$pid)]), c("PD", "PR"))
})

test_that("R3-ITT: an all-patients ORR counts not-evaluable patients as non-responders", {
  d <- rbind(raw4,
             data.frame(pid = c("P5", "P5"), time = c(0, 6), size = c(50, NA)),
             data.frame(pid = c("P6", "P6"), time = c(0, 6), size = c(0, 10)))
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  ci <- stats::binom.test(1, 6)$conf.int * 100                    # 1 responder of 6 supplied
  expect_match(wf_metric(res, "all 6 patients"), sprintf("^16\\.7%% \\(95%% CI %.1f-%.1f%%\\)", ci[1], ci[2]))
})

test_that("R3-no-follow-up: no assessment after the first response is not 'not reached'", {
  d <- data.frame(pid = rep(paste0("R", 1:5), each = 2), time = rep(c(0, 2), 5),
                  size = rep(c(100, 50), 5))                       # every responder's last scan
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  row <- wf_metric(res, "Duration of Response")
  expect_false(isTRUE(grepl("not reached", row)))
})

test_that("R3-baseline-time: 0% rows at the earliest visit that is not time 0 are flagged", {
  d <- data.frame(pid = rep(paste0("P", 1:4), each = 3), time = rep(c(1, 43, 85), 4),
                  chg = c(0, 20, 40, 0, -40, -60, 0, 10, 30, 0, -10, -4))
  expect_match(wf_notices(wf_run(d, patientID = "pid", responseVar = "chg", timeVar = "time")),
               "BASELINE NOT AT TIME 0")
})

test_that("R3-fields: override conflicts ignore case/space; ongoing uses the latest status", {
  d <- raw4
  d$override <- NA_character_
  d$override[d$pid == "P2"] <- c("PD", "pd ", NA)
  d$ongoing <- rep(c("Yes", "Yes", "No"), 4)                       # stopped treatment by the last visit
  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw",
                responseCategoryVar = "override", ongoingVar = "ongoing")
  expect_false(grepl("CONFLICTING CATEGORY OVERRIDES", wf_notices(res)))
  w <- res$waterfallplot$state$data$waterfall
  expect_false(any(w$ongoing_flag))
})

# ---- Differential guard: best response, category and DoR against an independent reference ----

test_that("best response, category and DoR agree with an independent RECIST reference on random data", {
  # Reference written from the RECIST v1.1 rules in base R, sharing no code with the module:
  # PD when a sum is >= 1.2 x the smallest sum so far (baseline included) or > 0 after a zero
  # nadir; best response = smallest post-baseline change up to the first PD; DoR from the
  # first response to the next PD, else censored at the last measured scan.
  is_pd <- function(x, nadir) (nadir > 0 && x >= 1.2 * nadir - 1e-9) || (nadir == 0 && x > 0)
  ref <- function(t, x) {
    o <- order(t); t <- t[o]; x <- x[o]; b <- x[t == 0]
    nadir <- b; upto <- length(t)
    for (i in which(t > 0)) { if (is_pd(x[i], nadir)) { upto <- i; break }; nadir <- min(nadir, x[i]) }
    pct <- (x - b) / b * 100
    post <- which(t > 0 & seq_along(t) <= upto)
    best <- min(pct[post])
    cat <- if (best <= -100 + 1e-9) "CR" else if (best <= -30 + 1e-9) "PR" else if (best < 20 - 1e-9) "SD" else "PD"
    resp <- post[pct[post] <= -30 + 1e-9]
    dor <- NA_real_; ev <- NA_real_
    if (length(resp)) {
      first <- t[resp[1]]; nad <- b; ev <- 0; dor <- max(t) - first
      for (i in seq_along(t)) { if (t[i] > first && is_pd(x[i], nad)) { ev <- 1; dor <- t[i] - first; break }
                                nad <- min(nad, x[i]) }
    }
    list(best = best, cat = cat, dor = dor, ev = ev)
  }
  set.seed(20260918)
  d <- do.call(rbind, lapply(seq_len(150), function(i) {
    k <- sample(3:7, 1); b <- sample(20:120, 1)
    x <- round(b * cumprod(c(1, runif(k - 1, 0.45, 1.45))))
    if (runif(1) < 0.1) x[sample(2:k, 1)] <- 0
    data.frame(pid = sprintf("P%03d", i), time = (seq_len(k) - 1) * 2, size = x)
  }))
  r <- lapply(split(d, d$pid), function(p) ref(p$time, p$size))

  res <- wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw")
  w <- as.data.frame(res$waterfallplot$state$data$waterfall)
  w <- w[match(names(r), w$pid), ]
  expect_equal(as.character(w$recist_category), unname(vapply(r, `[[`, "", "cat")))
  expect_equal(w$response, unname(vapply(r, `[[`, 0, "best")), tolerance = 1e-9)

  an <- ClinicoPath:::waterfallClass$new(options = ClinicoPath:::waterfallOptions$new(
    patientID = "pid", responseVar = "size", timeVar = "time", inputType = "raw"), data = d)
  p <- an$.__enclos_env__$private
  proc <- p$.processData(d, "pid", "raw", "size", "time", NULL)
  tte <- p$.calculateTimeToEventMetrics(proc$spider, "pid", "time", "response",
    data.frame(pid = proc$waterfall$pid, category = as.character(proc$waterfall$recist_category)))
  bp <- as.data.frame(tte$by_patient)
  resp_ids <- names(r)[!is.na(vapply(r, `[[`, 0, "dor"))]
  expect_setequal(bp$pid, resp_ids)
  bp <- bp[match(resp_ids, bp$pid), ]
  expect_equal(bp$duration_of_response, unname(vapply(r[resp_ids], `[[`, 0, "dor")))
  expect_equal(bp$duration_censored, unname(vapply(r[resp_ids], `[[`, 0, "ev")))
})

test_that("R3-scale: integer +/-1% changes are not taken for proportions", {
  d <- data.frame(pid = paste0("P", 1:10), chg = c(-1, 1, -1, -45, 12, -60, 1, 25, -33, 8))
  expect_false(grepl("PROPORTIONS", wf_notices(wf_run(d, patientID = "pid", responseVar = "chg"))))
})

test_that("R3-annotation: conflicting per-visit annotation values are reported", {
  d <- raw4
  d$kras <- rep(c("WT", "WT", "MUT"), 4)                            # recorded differently at the last visit
  txt <- wf_notices(wf_run(d, patientID = "pid", responseVar = "size", timeVar = "time",
                           inputType = "raw", annotationVars = "kras"))
  expect_match(txt, "CONFLICTING ANNOTATION VALUES")
})
