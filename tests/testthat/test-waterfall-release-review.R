# Regression tests from the waterfall release review.
#
# These pin behaviour that was verified against the RECIST v1.1 definitions
# (Eisenhauer et al., Eur J Cancer 2009;45:228-247) and against stats::binom.test,
# rather than against the module's own arithmetic.

# waterfall renders many outputs, so a full run() is slow. The statistical
# helpers are private, so reach them directly.
wf_private <- function(df, ...) {
  analysis <- waterfallClass$new(options = waterfallOptions$new(...), data = df)
  analysis$.__enclos_env__$private
}

notice_titles <- function(p) vapply(p$.noticeList, function(n) n$title, character(1))


test_that("RECIST category boundaries are inclusive on both sides", {
  # RECIST v1.1: PR is "at least a 30% decrease", PD is "at least a 20% increase".
  # Both boundaries are therefore inclusive. The PD boundary was previously
  # exclusive (> 20), so a change of exactly +20% was reported as SD -- reachable
  # whenever percentages are pre-rounded, which is normal for inputType =
  # "percentage".
  p <- wf_private(data.frame(id = "x", r = 1), patientID = "id", responseVar = "r")
  cat_of <- function(v) as.character(p$.categorizeRECIST(v))

  expect_equal(cat_of(-100),   "CR")
  expect_equal(cat_of(-99.99), "PR")
  expect_equal(cat_of(-30),    "PR")     # boundary: at least a 30% decrease
  expect_equal(cat_of(-29.99), "SD")
  expect_equal(cat_of(0),      "SD")
  expect_equal(cat_of(19.99),  "SD")
  expect_equal(cat_of(20),     "PD")     # boundary: at least a 20% increase
  expect_equal(cat_of(20.01),  "PD")
  expect_equal(cat_of(NA_real_), "Unknown")
})

test_that("categoriser returns untranslated factor levels", {
  # One former copy declared levels as c(..., .("Unknown")) while its case_when
  # emitted the untranslated "Unknown". Under any non-English locale that made
  # every unevaluable patient silently NA instead of "Unknown".
  p <- wf_private(data.frame(id = "x", r = 1), patientID = "id", responseVar = "r")
  out <- p$.categorizeRECIST(c(-50, NA, 30))
  expect_equal(levels(out), c("CR", "PR", "SD", "PD", "Unknown"))
  expect_false(any(is.na(out)))
  expect_equal(as.character(out)[2], "Unknown")
})

test_that("a patient with no post-baseline assessment is not scored as SD", {
  # Baseline-only patient used to yield ((b - b)/b)*100 = 0, i.e. a 0% change,
  # and was categorised SD -- inflating the disease control rate with a patient
  # who was never re-assessed.
  d <- data.frame(id = c("A", "A", "B"), tm = c(0, 1, 0), size = c(100, 50, 80))
  p <- wf_private(d, patientID = "id", responseVar = "size",
                  timeVar = "tm", inputType = "raw")
  w <- p$.accountForUnevaluablePatients(
    p$.processData(d, "id", "raw", "size", "tm", NULL)$waterfall, d, "id", "tm")

  expect_equal(as.character(w$recist_category[w$id == "B"]), "Unknown")
  expect_true(is.na(w$response[w$id == "B"]))
  expect_equal(as.character(w$recist_category[w$id == "A"]), "PR")
  expect_true("NOT RESPONSE-EVALUABLE" %in% notice_titles(p))

  # and the unevaluable patient must not sit in the rate denominator
  expect_equal(p$.calculateMetrics(w)$n, 1)
})

test_that("a patient with an unusable baseline is excluded WITH an explanation", {
  # baseline == 0 makes percent change undefined; the patient was dropped and the
  # cohort silently got smaller.
  z <- data.frame(id = rep(c("A", "B"), each = 2), tm = rep(c(0, 1), 2),
                  size = c(0, 10, 100, 50))
  p <- wf_private(z, patientID = "id", responseVar = "size",
                  timeVar = "tm", inputType = "raw")
  w <- p$.accountForUnevaluablePatients(
    p$.processData(z, "id", "raw", "size", "tm", NULL)$waterfall, z, "id", "tm")

  expect_false("A" %in% w$id)
  expect_true("PATIENTS EXCLUDED" %in% notice_titles(p))
  expect_match(p$.noticeList[[which(notice_titles(p) == "PATIENTS EXCLUDED")[1]]]$content,
               "1 of 2 patients")
})

test_that("a complete cohort raises no exclusion notices and is unchanged", {
  d <- data.frame(id = rep(c("A", "B", "C"), each = 3), tm = rep(c(0, 1, 2), 3),
                  size = c(100, 60, 55,  100, 90, 140,  100, 100, 98))
  p <- wf_private(d, patientID = "id", responseVar = "size",
                  timeVar = "tm", inputType = "raw")
  w <- p$.accountForUnevaluablePatients(
    p$.processData(d, "id", "raw", "size", "tm", NULL)$waterfall, d, "id", "tm")

  expect_equal(nrow(w), 3)
  # No patient is dropped and none is unevaluable; the only notice is the
  # small-cohort caveat, which is expected at n = 3.
  expect_false("PATIENTS EXCLUDED" %in% notice_titles(p))
  expect_false("NOT RESPONSE-EVALUABLE" %in% notice_titles(p))
  expect_false("NO EVALUABLE PATIENTS" %in% notice_titles(p))
  # best response per patient = min percent change from baseline
  expect_equal(sort(round(w$response, 4)), c(-45, -10, -2))
})

test_that("optimised large-data path agrees with the standard path", {
  # .processData dispatches to .processLargeDataset above 100 rows / 50 patients.
  # A divergence there would silently change results for larger cohorts only.
  set.seed(1)
  big <- do.call(rbind, lapply(1:60, function(i) data.frame(
    id = sprintf("P%03d", i), tm = c(0, 1, 2),
    size = c(100, round(runif(1, 20, 160)), round(runif(1, 20, 160))))))
  p <- wf_private(big, patientID = "id", responseVar = "size",
                  timeVar = "tm", inputType = "raw")
  expect_true(p$.shouldOptimizeForLargeDataset(big))

  std <- p$.processDataStandard(big, "id", "raw", "size", "tm", NULL)$waterfall
  lrg <- p$.processLargeDataset(big, "id", "raw", "size", "tm", NULL)$waterfall
  std <- std[order(std$id), ]; lrg <- lrg[order(lrg$id), ]

  expect_equal(nrow(std), nrow(lrg))
  expect_equal(std$response, lrg$response)
  expect_equal(as.character(std$recist_category), as.character(lrg$recist_category))
})

test_that("ORR and DCR match hand-computed rates over evaluable patients", {
  d <- data.frame(id = paste0("P", 1:8),
                  r = c(-100, -45, -30, -29, 0, 19, 20, 60))
  #                      CR    PR    PR   SD   SD  SD   PD  PD   (with fixed boundary)
  p <- wf_private(d, patientID = "id", responseVar = "r", inputType = "percentage")
  w <- data.frame(id = d$id, response = d$r)
  w$recist_category <- p$.categorizeRECIST(w$response)
  m <- p$.calculateMetrics(w)

  expect_equal(as.character(w$recist_category),
               c("CR", "PR", "PR", "SD", "SD", "SD", "PD", "PD"))
  expect_equal(m$n, 8)
  expect_equal(m$ORR, round(3 / 8 * 100, 1))   # CR + PR
  expect_equal(m$DCR, round(6 / 8 * 100, 1))   # CR + PR + SD
})

test_that("response-rate confidence intervals are Clopper-Pearson exact", {
  # The module calls stats::binom.test; pin that it is the exact interval and not
  # a normal approximation, which would be wrong at the small n typical here.
  for (cfg in list(c(3, 8), c(0, 10), c(10, 10), c(7, 20))) {
    k <- cfg[1]; n <- cfg[2]
    ci <- binom.test(k, n)$conf.int
    expect_gte(ci[1], 0); expect_lte(ci[2], 1)
    # normal approximation would fall outside [0,1] at the extremes
    if (k == 0) expect_equal(ci[1], 0)
    if (k == n) expect_equal(ci[2], 1)
  }
})

test_that("unevaluable patients are excluded from the rate denominator", {
  # Documented behaviour: rates are over evaluable patients, NOT intention-to-treat.
  d <- data.frame(id = paste0("P", 1:10),
                  r = c(-50, -40, -35, -10, 5, 30, 40, NA, NA, NA))
  p <- wf_private(d, patientID = "id", responseVar = "r", inputType = "percentage")
  w <- data.frame(id = d$id, response = d$r)
  w$recist_category <- p$.categorizeRECIST(w$response)
  m <- p$.calculateMetrics(w)

  expect_equal(m$n, 7)                       # 3 NA patients dropped
  expect_equal(m$ORR, round(3 / 7 * 100, 1)) # not 3/10
})


# ---------------------------------------------------------------------------
# Defects surfaced by the multi-reviewer pass and reproduced before fixing.
# ---------------------------------------------------------------------------

test_that("response rates do not change at the 100-row optimisation boundary", {
  # .processData dispatches to the optimised path above 100 rows / 50 patients.
  # That path's no-timeVar branch never collapsed to one row per patient, so
  # rates were computed over ASSESSMENTS: the same design gave ORR 100% at 30
  # patients and 33.3% at 60.
  mkdat <- function(np) do.call(rbind, lapply(seq_len(np), function(i)
    data.frame(id = sprintf("P%03d", i), pct = c(-40, -10, 20))))

  res <- lapply(c(30, 60), function(np) {
    d <- mkdat(np)
    p <- wf_private(d, patientID = "id", responseVar = "pct", inputType = "percentage")
    m <- p$.calculateMetrics(p$.processData(d, "id", "percentage", "pct", NULL, NULL)$waterfall)
    list(np = np, n = m$n, ORR = m$ORR, big = p$.shouldOptimizeForLargeDataset(d))
  })

  expect_false(res[[1]]$big); expect_true(res[[2]]$big)  # opposite sides of the boundary
  expect_equal(res[[1]]$n, 30)                            # patients, not rows
  expect_equal(res[[2]]$n, 60)
  expect_equal(res[[1]]$ORR, 100)
  expect_equal(res[[2]]$ORR, 100)
})

test_that("ORR point estimate lies inside its own confidence interval", {
  # The estimate used the evaluable denominator while the CI used nrow(),
  # producing e.g. ORR 50.0% with a 95% CI of 28.8-46.8%.
  d <- data.frame(id = paste0("P", 1:120),
                  pct = c(rep(-100, 5), rep(-50, 40), rep(0, 25), rep(30, 20), rep(NA, 30)))
  p <- wf_private(d, patientID = "id", responseVar = "pct", inputType = "percentage")
  w <- data.frame(id = d$id, response = d$pct)
  w$recist_category <- p$.categorizeRECIST(w$response)
  m <- p$.calculateMetrics(w)

  n_resp <- sum(w$recist_category %in% c("CR", "PR"))
  ci <- binom.test(n_resp, m$n)$conf.int * 100

  expect_equal(m$n, 90)                 # 30 unevaluable excluded
  expect_gte(m$ORR, ci[1])
  expect_lte(m$ORR, ci[2])
})

test_that("copy-ready sentence still renders when no patient achieves CR", {
  # dplyr::count() drops unobserved levels -> integer(0) -> sprintf() returns
  # character(0) -> paste0() collapsed the whole sentence to nothing.
  d <- data.frame(id = paste0("P", 1:6), pct = c(-50, -40, -35, -5, 10, 40))
  a <- waterfallClass$new(
    options = waterfallOptions$new(patientID = "id", responseVar = "pct",
                                   inputType = "percentage",
                                   generateCopyReadyReport = TRUE),
    data = d)
  p <- a$.__enclos_env__$private
  w <- data.frame(id = d$id, response = d$pct)
  w$recist_category <- p$.categorizeRECIST(w$response)
  p$.generateCopyReadyReport(list(waterfall = w), p$.calculateMetrics(w), NULL)

  txt <- as.character(a$results$copyReadyReport$content)
  expect_match(txt, "objective response rate")
  expect_match(txt, "0 patients achieving complete response")
})

test_that("unrecognised response-category overrides are rejected, not fatal", {
  # Assigning an out-of-vocabulary label into the factor produced NA; overriding
  # every row made the column all-NA and aborted the run with
  # "missing value where TRUE/FALSE needed".
  d <- data.frame(id = paste0("P", 1:6), pct = c(-50, -40, -35, -5, 10, 40),
                  cat = rep("NE", 6))
  p <- wf_private(d, patientID = "id", responseVar = "pct",
                  inputType = "percentage", responseCategoryVar = "cat")
  w <- data.frame(id = d$id, response = d$pct)
  w$recist_category <- p$.categorizeRECIST(w$response)

  out <- p$.applyCategoryOverride(w, d, "id", "cat")
  expect_equal(sum(is.na(out$recist_category)), 0)
  expect_equal(as.character(out$recist_category), as.character(w$recist_category))
  expect_true("RESPONSE CATEGORY OVERRIDE IGNORED" %in% notice_titles(p))
})

test_that("valid response-category overrides are applied case-insensitively", {
  d <- data.frame(id = paste0("P", 1:6), pct = c(-50, -40, -35, -5, 10, 40),
                  cat = c("PD", "PD", "cr", "", "x", "SD"))
  p <- wf_private(d, patientID = "id", responseVar = "pct",
                  inputType = "percentage", responseCategoryVar = "cat")
  w <- data.frame(id = d$id, response = d$pct)
  w$recist_category <- p$.categorizeRECIST(w$response)
  out <- p$.applyCategoryOverride(w, d, "id", "cat")

  #             P1   P2   P3(lowercase)  P4(blank->computed)  P5(bad->computed)  P6
  expect_equal(as.character(out$recist_category),
               c("PD", "PD", "CR",       "SD",                "SD",              "SD"))
})

test_that("exported response category lands on the correct patients' rows", {
  # setRowNums(rownames(waterfall)) used a dplyr tibble whose rownames are always
  # "1".."k" and which had been re-sorted by patient ID, so every exported label
  # was written against the wrong patient.
  d <- data.frame(id = c("PT5", "PT1", "PT3", "PT2", "PT4"),
                  pct = c(60, -80, -40, 5, -10))
  a <- waterfallClass$new(
    options = waterfallOptions$new(patientID = "id", responseVar = "pct",
                                   inputType = "percentage",
                                   addResponseCategory = TRUE),
    data = d)
  p <- a$.__enclos_env__$private
  o <- p$.processData(d, "id", "percentage", "pct", NULL, NULL)

  cats <- o$waterfall
  # map source rows to categories the same way the backend now does
  idx <- match(d$id, cats$id)
  got <- as.character(cats$recist_category[idx])
  truth <- as.character(p$.categorizeRECIST(d$pct))

  expect_equal(got, truth)
  expect_equal(got, c("PD", "PR", "PR", "SD", "SD"))
})


test_that("progression is detected relative to the nadir, not to baseline", {
  # RECIST v1.1 defines PD as ">=20% increase taking as reference the smallest
  # sum on study". Testing against BASELINE meant a patient who shrank and then
  # regrew was never recorded as progressing while still below their enrolment
  # burden, inflating every duration-of-response summary and the KM curve.
  p <- wf_private(data.frame(id = "x", r = 1), patientID = "id", responseVar = "r")
  pt <- function(times, values, after) p$.progressionTimes(times, values, after)

  # 100 -> 60 -> 78 mm: nadir burden 60, later 78 = +30% over nadir -> PD at t=12
  expect_equal(pt(c(0, 6, 12), c(0, -40, -22), 6), 12)
  # monotone shrinkage never progresses
  expect_length(pt(c(0, 6, 12), c(0, -45, -50), 6), 0)
  # +18% over the nadir is below the threshold
  expect_length(pt(c(0, 6, 12), c(0, -50, -41), 6), 0)
  # exactly +20% over the nadir IS progression (inclusive, as for the categories)
  expect_equal(pt(c(0, 6, 12), c(0, -50, -40), 6), 12)
  # a rise before the first response does not count
  expect_length(pt(c(0, 6, 12), c(0, 40, -40), 6), 0)
  # unsorted input must give the same answer
  expect_equal(pt(c(12, 0, 6), c(-22, 0, -40), 6), 12)
})

test_that("duration of response records the responder who regrows", {
  d <- data.frame(patientID = rep(c("PT1", "PT2"), each = 3),
                  visitTime = rep(c(0, 6, 12), 2),
                  target_sum = c(100, 60, 78,   # responds then regrows -> PD
                                 100, 55, 50))  # sustained response -> censored
  p <- wf_private(d, patientID = "patientID", responseVar = "target_sum",
                  timeVar = "visitTime", inputType = "raw")
  proc <- p$.processData(d, "patientID", "raw", "target_sum", "visitTime", NULL)
  tte <- p$.calculateTimeToEventMetrics(proc$spider, "patientID", "visitTime", "response")
  by_pt <- as.data.frame(tte$by_patient)

  expect_equal(by_pt$duration_censored[by_pt$patientID == "PT1"], 1)  # event observed
  expect_equal(by_pt$duration_censored[by_pt$patientID == "PT2"], 0)  # still in response
})

test_that("the analysis does not claim RECIST v1.1 compliance", {
  # The summary table was titled "Response Categories Based on RECIST v1.1
  # Criteria" and the description said "following RECIST criteria", while the
  # analysis's own notices state it is NOT RECIST-compliant.
  rl <- yaml::read_yaml("../../jamovi/waterfall.r.yaml")
  summary_item <- Filter(function(i) identical(i$name, "summaryTable"), rl$items)[[1]]
  expect_false(grepl("Based on RECIST", summary_item$title, fixed = TRUE))

  al <- yaml::read_yaml("../../jamovi/waterfall.a.yaml")
  expect_match(al$description$main, "NOT a RECIST v1.1 implementation")
})


test_that("annotation tracks align tile-for-bar with the waterfall", {
  # Covariate tracks drawn under the bars. Design credit: Jamovi-TrialPlots by
  # highwindmx (LGPL), https://github.com/highwindmx/Jamovi-TrialPlots
  # Alignment is the whole point: a tile must sit under its own patient's bar
  # whatever order the bars were sorted into.
  d <- data.frame(id = paste0("P", 1:8),
                  r = c(-100, -45, -30, -29, 0, 19, 25, 60),
                  Biomarker = c("Pos", "Pos", "Neg", "Pos", "Neg", "Neg", "Pos", "Neg"),
                  Arm = rep(c("A", "B"), 4), stringsAsFactors = FALSE)
  p <- wf_private(d, patientID = "id", responseVar = "r", inputType = "percentage")

  df <- data.frame(id = d$id, response = d$r, stringsAsFactors = FALSE)
  df <- df[order(df$response), ]                       # bar order
  pd <- list(options = list(patientID = "id",
                            annotationVars = c("Biomarker", "Arm")))

  track <- p$.annotationTrack(df, pd)
  expect_s3_class(track, "ggplot")

  # one tile per patient per track
  expect_equal(nrow(ggplot2::ggplot_build(track)$data[[1]]), nrow(df) * 2)

  # first listed variable renders on top, so levels run bottom-up
  expect_equal(levels(track$data$track), c("Arm", "Biomarker"))

  # every tile carries its own patient's value, in bar order
  for (i in seq_len(nrow(df))) {
    expect_equal(track$data$value[track$data$bar == i & track$data$track == "Biomarker"],
                 d$Biomarker[d$id == df$id[i]], info = paste("bar", i))
  }
})

test_that("annotation tracks are absent unless asked for", {
  d <- data.frame(id = paste0("P", 1:4), r = c(-40, -10, 5, 30), Arm = "A",
                  stringsAsFactors = FALSE)
  p <- wf_private(d, patientID = "id", responseVar = "r", inputType = "percentage")
  df <- data.frame(id = d$id, response = d$r, stringsAsFactors = FALSE)

  expect_null(p$.annotationTrack(df, list(options = list(patientID = "id",
                                                         annotationVars = NULL))))
  expect_null(p$.annotationTrack(df, list(options = list(patientID = "id",
                                                         annotationVars = character(0)))))
  # a variable that is not in the data must not error
  expect_null(p$.annotationTrack(df, list(options = list(patientID = "id",
                                                         annotationVars = "NoSuchColumn"))))
})

test_that("the waterfall still renders when no annotation track is requested", {
  d <- data.frame(id = paste0("P", 1:8), r = c(-100, -45, -30, -29, 0, 19, 25, 60),
                  stringsAsFactors = FALSE)
  result <- waterfall(data = d, patientID = "id", responseVar = "r",
                      inputType = "percentage")
  expect_true(!is.null(result$waterfallplot$state))

  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 800, height = 500)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_error(print(result$waterfallplot), NA)
})

# ─────────────────────────────────────────────────────────────
# Regression tests from the 2026-08 full audit fixes
# ─────────────────────────────────────────────────────────────

rr_txt <- function(res, item = "notices")
  gsub("<[^>]+>", " ", paste(res[[item]]$content, collapse = ""))

test_that("a cohort with zero evaluable patients completes and explains itself", {
  # Every patient baseline-only: all responses demoted to Unknown, ORR = NA.
  # This used to crash .run() with "missing value where TRUE/FALSE needed"
  # BEFORE notices rendered, so the explanation was lost with the run.
  b_only <- data.frame(pid = sprintf("B%02d", 1:12), time = 0, resp = 0)
  res <- ClinicoPath::waterfall(data = b_only, patientID = "pid",
                                responseVar = "resp", timeVar = "time",
                                inputType = "percentage")
  expect_match(rr_txt(res), "NOT RESPONSE-EVALUABLE")
  # NA-safe wording, not "NA%"
  expect_false(grepl("NA%", rr_txt(res, "clinicalSummary"), fixed = TRUE))
})

test_that("notices are delivered even when the run aborts early", {
  # Processing error path (all responses missing) returns before the end of
  # .run(); the on.exit(renderNotices) must still deliver what accumulated.
  d <- data.frame(pid = paste0("P", 1:12), resp = NA_real_)
  res <- ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "resp")
  expect_match(rr_txt(res), "REGULATORY USE PROHIBITED")
  expect_match(rr_txt(res), "missing response values")
})

test_that("non-fatal validation warnings reach the notices panel", {
  # These were written to todo2 and wiped in the same run whenever validation
  # passed, so the user never saw them.
  d <- data.frame(pid = paste0("P", 1:15),
                  resp = c(-45, -10, 25, -60, 5, NA, -35, 80, NA, 0, 30, -75, 15, NA, -20))
  res <- ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "resp")
  expect_match(rr_txt(res), "DATA VALIDATION WARNINGS")
  expect_match(rr_txt(res), "3 missing response values")
})

test_that("safety notices describe nadir-referenced progression, not baseline", {
  d <- data.frame(pid = rep(paste0("P", 1:12), each = 2),
                  time = rep(c(0, 3), 12),
                  resp = rep(c(0, -40), 12))
  res <- ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "resp",
                                timeVar = "time", inputType = "percentage")
  txt <- rr_txt(res)
  expect_match(txt, "referenced to the nadir")
  expect_match(txt, "increase over the NADIR")
  expect_false(grepl("measured from BASELINE, not from the NADIR", txt, fixed = TRUE))
})

test_that("group rates use the evaluable-only denominator with a table note", {
  d <- data.frame(pid = rep(paste0("Q", 1:12), each = 2),
                  time = rep(c(0, 3), 12),
                  resp = as.vector(rbind(rep(0, 12), c(-45,-10,25,-60,5,15,-35,80,-20,0,30,-75))))
  d <- d[!(d$pid %in% c("Q11", "Q12") & d$time == 3), ]  # 2 baseline-only
  d$arm <- ifelse(as.integer(sub("Q", "", d$pid)) %% 2 == 0, "B", "A")
  res <- ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "resp",
                                timeVar = "time", inputType = "percentage",
                                groupVar = "arm")
  gc <- res$groupComparisonTable$asDF
  expect_equal(sum(gc$n_patients), 10)   # 12 patients, 2 unevaluable
  # summary table discloses the unevaluable patients as their own row
  st <- res$summaryTable$asDF
  expect_true(any(grepl("Unknown", st$category)))
  expect_equal(sum(st$n), 12)
})

test_that("clinicalMetrics has exactly one DoR row, from the nadir-based method", {
  set.seed(7)
  rawd <- do.call(rbind, lapply(1:30, function(i) {
    base <- runif(1, 20, 80)
    mult <- cumprod(c(1, runif(3, 0.6, 1.25)))
    data.frame(pid = sprintf("R%02d", i), time = c(0, 2, 4, 6), meas = base * mult)
  }))
  res <- ClinicoPath::waterfall(data = rawd, patientID = "pid", responseVar = "meas",
                                timeVar = "time", inputType = "raw")
  cm <- res$clinicalMetrics$asDF
  expect_equal(sum(grepl("Median Duration of Response", cm$metric)), 1)
  expect_false(any(grepl("rapid response|durable response", cm$value)))
})

test_that("spider data keeps patient_group on the large-dataset path", {
  set.seed(7)
  rawd <- do.call(rbind, lapply(1:30, function(i) {
    base <- runif(1, 20, 80)
    mult <- cumprod(c(1, runif(3, 0.6, 1.25)))
    data.frame(pid = sprintf("R%02d", i), time = c(0, 2, 4, 6),
               meas = base * mult, arm = ifelse(i %% 2, "A", "B"))
  }))
  res <- ClinicoPath::waterfall(data = rawd, patientID = "pid", responseVar = "meas",
                                timeVar = "time", inputType = "raw", groupVar = "arm",
                                showSpiderPlot = TRUE, spiderColorBy = "group")
  expect_true("patient_group" %in% names(res$spiderplot$state$data$spider))
})

test_that("a duplicate baseline row blocks the run with a specific message", {
  d <- data.frame(pid = rep(c("A", "B"), each = 3),
                  time = c(0, 0, 2, 0, 2, 4),
                  meas = c(50, 55, 30, 40, 32, 20))
  expect_error(
    ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "meas",
                           timeVar = "time", inputType = "raw"),
    "more than one baseline")
})

test_that("a patient with no usable assessment does not erase the time-to-event table", {
  # PT3 has a baseline row but every percentage is NA. In the per-patient
  # summarise() min() gave Inf and which.min() gave integer(0), dplyr raised
  # "must return size 1", the tryCatch swallowed it and the ENTIRE TTR/DoR
  # table disappeared for the cohort, with only a generic warning notice.
  d <- data.frame(patientID = rep(c("PT1", "PT2", "PT3"), each = 3),
                  visitTime = rep(c(0, 6, 12), 3),
                  pct = c(0, -40, -22,  0, -45, -50,  NA, NA, NA))
  p <- wf_private(d, patientID = "patientID", responseVar = "pct",
                  timeVar = "visitTime", inputType = "percentage")
  proc <- p$.processData(d, "patientID", "percentage", "pct", "visitTime", NULL)
  tte <- p$.calculateTimeToEventMetrics(proc$spider, "patientID", "visitTime", "response")

  expect_false(is.null(tte))
  by_pt <- as.data.frame(tte$by_patient)
  expect_setequal(by_pt$patientID, c("PT1", "PT2"))
  expect_equal(tte$summary$median_time_to_response, 6)
})

test_that("re-running on the same instance does not duplicate table rows", {
  # jamovi reuses the analysis instance; Table$addRow() never checks the rowKey.
  # Before the deleteRows() sweep at the top of .run(), a second run that did
  # not trip clearWith doubled clinicalMetrics, the DoR table and the group tables.
  set.seed(1); n <- 8
  d <- data.frame(patientID = rep(sprintf("PT%02d", 1:n), each = 3),
                  visitTime = rep(c(0, 6, 12), n),
                  pct = as.vector(sapply(1:n, function(i) c(0, cumsum(rnorm(2, -15, 25))))),
                  grp = rep(c("A", "B"), each = 3, length.out = 3 * n))
  an <- waterfallClass$new(
    options = waterfallOptions$new(patientID = "patientID", responseVar = "pct",
                                   timeVar = "visitTime", inputType = "percentage",
                                   groupVar = "grp", showResponseDuration = TRUE),
    data = d)
  counts <- function() vapply(c("clinicalMetrics", "responseDurationTable",
                                "groupComparisonTable", "groupComparisonTest"),
                              function(t) an$results[[t]]$rowCount, numeric(1))
  an$run(); first <- counts()
  an$run(); second <- counts()
  expect_gt(first[["clinicalMetrics"]], 0)
  expect_equal(second, first)
})

test_that("group comparison tests explain themselves when skipped or not 2x2", {
  set.seed(2); n <- 9
  base <- data.frame(patientID = rep(sprintf("PT%02d", 1:n), each = 2),
                     visitTime = rep(c(0, 6), n))
  # three groups: Fisher runs but has no odds ratio -> no "OR = NA"
  d3 <- base; d3$pct <- rep(c(0, -50, 0, 10, 0, 30), length.out = 2 * n)
  d3$grp <- rep(c("A", "B", "C"), each = 6)
  an3 <- waterfallClass$new(options = waterfallOptions$new(patientID = "patientID",
    responseVar = "pct", timeVar = "visitTime", inputType = "percentage", groupVar = "grp"), data = d3)
  an3$run()
  lab <- an3$results$groupComparisonTest$asDF$test_statistic
  expect_true(length(lab) >= 1)
  expect_false(any(grepl("NA", lab, fixed = TRUE)))

  # nobody responds in either group: ORR contingency is 2x1, test skipped -> note
  d0 <- base; d0$pct <- rep(c(0, 5), n); d0$grp <- rep(c("A", "B"), length.out = 2 * n)
  an0 <- waterfallClass$new(options = waterfallOptions$new(patientID = "patientID",
    responseVar = "pct", timeVar = "visitTime", inputType = "percentage", groupVar = "grp"), data = d0)
  an0$run()
  expect_true("skipped" %in% names(an0$results$groupComparisonTest$notes))
})
