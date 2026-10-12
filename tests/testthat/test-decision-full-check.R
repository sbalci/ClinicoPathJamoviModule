# Regression tests for /check-function-full decision, 2026-09-27: notice severity order, a
# margin emptied by the other variable's excluded level, raw tables written only when shown,
# and the Fagan renderer calling nomogrammer() directly. Expected values are hand arithmetic.

fc_run <- function(d, ...)
  suppressWarnings(decision(data = d, gold = "g", goldPositive = "P", goldNegative = "N",
                            newtest = "t", testPositive = "P", testNegative = "N", ...))
fc_mk <- function(TP, FP, FN, TN) data.frame(
  t = factor(rep(c("P", "P", "N", "N"), c(TP, FP, FN, TN)), levels = c("P", "N")),
  g = factor(rep(c("P", "N", "P", "N"), c(TP, FP, FN, TN)), levels = c("P", "N")))
# Severity words of the rendered notices, in display order
fc_severities <- function(html) {
  heads <- regmatches(html, gregexpr("<strong>[^<:]*:", html))[[1]]
  sub(":$", "", sub("^<strong>", "", heads))
}

test_that("an ERROR raised late in .run() is rendered above earlier warnings and notes", {
  # sens 3/21, spec 12/40: Youden -0.557 raises the worse-than-chance ERROR in
  # .validateDiscrimination(), after the missing-data WARNING, the n < 100 INFO and the
  # small-cell WARNING were raised.
  d <- fc_mk(6, 28, 18, 12)
  d$t[1:3] <- NA
  sev <- fc_severities(fc_run(d)$notices$content)
  expect_true(nzchar(paste(sev, collapse = "")))
  expect_true(length(sev) >= 3)
  expect_identical(sev[1], "Error")
  # most severe first throughout, and same-severity notices keep their order
  rank <- match(sev, c("Error", "Important warning", "Warning", "Note"))
  expect_false(anyNA(rank))
  expect_identical(rank, sort(rank))
})

test_that("a margin emptied by the other variable's excluded level stops with the 2x2 error", {
  # Excluding test level E removes the only disease-present case (a), the only
  # disease-free case (b), or excluding gold level X removes every positive test (c).
  # The recoded levels used to be fixed before that joint filter, so the run went on with
  # a zero row or column: blank sensitivity beside a specificity, or LR+ = LR- = 1.00.
  cases <- list(
    a = data.frame(g = factor(c("N", "N", "N", "N", "P", "N", "N")),
                   t = factor(c("P", "N", "P", "N", "E", "P", "N"))),
    b = data.frame(g = factor(c("P", "P", "P", "P", "N", "P", "P")),
                   t = factor(c("P", "N", "P", "N", "E", "P", "N"))),
    c = data.frame(g = factor(c("P", "N", "P", "N", "X", "P", "N")),
                   t = factor(c("N", "N", "N", "N", "P", "N", "N"))))
  for (nm in names(cases)) {
    r <- fc_run(cases[[nm]], pp = TRUE)
    expect_match(r$notices$content, "No analysed cases at one of the positive or negative levels",
                 fixed = TRUE, info = nm)
    expect_identical(fc_severities(r$notices$content)[1], "Error", info = nm)
    # the ERROR now renders above the exclusion warnings it refers to, so its text must not
    # point "above" (review of this fix, 2026-09-27)
    expect_no_match(r$notices$content, "reported above", fixed = TRUE)
    expect_match(r$notices$content, "any exclusions reported in the warnings", fixed = TRUE, info = nm)
    rt <- r$ratioTable$asDF
    expect_true(all(is.na(unlist(rt[, c("Sens", "Spec", "PPV", "NPV", "LRP", "LRN")]))), info = nm)
    expect_false(any(is.nan(unlist(rt[, -1]))), info = nm)
  }
})

test_that("the raw data tables are written only when Raw data tables is ticked", {
  d <- fc_mk(30, 6, 5, 40)
  off <- fc_run(d)
  expect_equal(nrow(off$rawCounts$asDF), 0)
  expect_true(all(is.na(off$rawContingency$asDF$gold_pos)))
  expect_false(nzchar(paste(off$missingDataSummary$content, collapse = "")))
  on <- fc_run(d, od = TRUE)
  expect_equal(on$rawContingency$asDF$gold_pos, c(30, 5, 35))
  expect_equal(on$rawContingency$asDF$gold_neg, c(6, 40, 46))
  expect_equal(sum(on$rawCounts$asDF$count), 81)
  # the 2x2 note still names the inferred negative levels when the raw tables are hidden
  d2 <- d
  r <- suppressWarnings(decision(data = d2, gold = "g", goldPositive = "P", goldNegative = NULL,
                                 newtest = "t", testPositive = "P", testNegative = NULL))
  expect_match(r$cTable$notes$levels$note, 'Reference Negative is "N"', fixed = TRUE)
  expect_match(r$cTable$notes$levels$note, 'Test Negative is "N"', fixed = TRUE)
})

test_that("the Fagan nomogram renders through nomogrammer()", {
  r <- fc_run(fc_mk(30, 6, 5, 40), fagan = TRUE)
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  suppressWarnings(r$plot1$saveAs(f))
  expect_true(file.exists(f))
  # saveAs() writes a blank canvas (about 5 KB) even when the renderer draws nothing;
  # the nomogram is about 70 KB.
  expect_gt(file.size(f), 20000)
})

test_that("a chance-level test is named with its own word, not a bare 'no' shared with jjdotplotstats", {
  # sens 10/20 = spec 10/20: Youden 0, equivalent AUC 0.5
  r <- fc_run(fc_mk(10, 10, 10, 10), showNaturalLanguage = TRUE)
  expect_match(gsub("<[^>]+>", "", r$naturalLanguageSummary$content),
               "the test shows chance-level discrimination (equivalent AUC 0.500)", fixed = TRUE)
})

test_that("notice severity follows the Youden interval, not the sign of the point estimate", {
  # TP 50, FP 51, FN 50, TN 50: Youden -0.005, Agresti-Caffo interval -0.142 to 0.132.
  # TP 50, FP 50, FN 50, TN 51: Youden +0.005. A coin flip either way: same warning, no ERROR.
  for (d in list(fc_mk(50, 51, 50, 50), fc_mk(50, 50, 50, 51))) {
    h <- fc_run(d)$notices$content
    expect_match(h, "No evidence that this test discriminates in this sample", fixed = TRUE)
    expect_false("Error" %in% fc_severities(h))
  }
  expect_match(fc_run(fc_mk(50, 51, 50, 50))$notices$content, "95% CI -0.142 to 0.132", fixed = TRUE)
  # sens 3/21, spec 12/40: Youden -0.557, interval wholly below 0 -> the inverted-levels ERROR
  d <- fc_mk(6, 28, 18, 12); d$t[1:3] <- NA
  h <- fc_run(d)$notices$content
  expect_identical(fc_severities(h)[1], "Error")
  expect_match(h, "lies entirely below 0, so this is not sampling noise", fixed = TRUE)
})

test_that("the copy-ready paragraph accounts for every excluded case (STARD 2015)", {
  # 80 rows: 3 missing test results, 10 at the unselected gold level "Eq", 67 analysed
  d <- data.frame(g = factor(rep(c("P", "N", "Eq"), c(30, 40, 10))),
                  t = factor(c(rep("P", 25), rep("N", 5), rep("P", 4), rep("N", 36), rep("P", 5), rep("N", 5))))
  d$t[1:3] <- NA
  txt <- function(r) gsub("\\s+", " ", gsub("<[^>]+>", " ", r$reportTemplate$content))
  both <- txt(fc_run(d, showReportTemplate = TRUE))
  expect_match(both, "in 67 cases", fixed = TRUE)
  expect_match(both, "13 of the 80 cases were excluded before analysis: 3 with a missing test or reference-standard result, and 10 with a test or reference-standard level other than the two selected.", fixed = TRUE)
  # the sentence sits inside the copied box, before its closing instruction
  expect_lt(regexpr("13 of the 80 cases", both), regexpr("Copy the text above", both))
  expect_match(txt(fc_run(d[!is.na(d$t), ], showReportTemplate = TRUE)),
               "10 of the 77 cases were excluded before analysis because their test or reference-standard level was neither of the two selected", fixed = TRUE)
  expect_match(txt(fc_run(d[d$g != "Eq", ], showReportTemplate = TRUE)),
               "3 of the 70 cases were excluded before analysis because the test or reference-standard result was missing.", fixed = TRUE)
  expect_no_match(txt(fc_run(fc_mk(30, 6, 5, 40), showReportTemplate = TRUE)), "excluded before analysis", fixed = TRUE)
})

test_that("predictive values at a supplied prior follow Bayes' theorem", {
  # sens 30/35, spec 40/46, prior 0.10:
  # PPV = .1 * 30/35 / (.1 * 30/35 + .9 * 6/46); NPV = .9 * 40/46 / (.9 * 40/46 + .1 * 5/35)
  rt <- fc_run(fc_mk(30, 6, 5, 40), pp = TRUE, pprob = 0.10)$ratioTable$asDF
  se <- 30 / 35; sp <- 40 / 46
  expect_equal(rt$PPV, 0.1 * se / (0.1 * se + 0.9 * (1 - sp)), tolerance = 1e-12)
  expect_equal(rt$NPV, 0.9 * sp / (0.9 * sp + 0.1 * (1 - se)), tolerance = 1e-12)
  expect_equal(rt$PrevalenceD, 0.10)
})
