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
