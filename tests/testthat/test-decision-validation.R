# Regression tests from /validate-function decision depth=exhaustive (2026-09-25).
# Report: development-ideas/decision-validation-2026-09-25.md
# Evidence script: development-scripts/validate_decision.R (313 checks, simulations included).
#
# Live tests lock in behaviour verified against an independent oracle. A test guarded by
# skip("VAL-decision-NN open defect: ...") already holds the CORRECT expectation; remove the skip
# when the fix lands.

skip_if_not_installed("epiR")

# Positive levels deliberately listed SECOND, so "first level = positive" would fail.
val_mk <- function(TP, FP, FN, TN) data.frame(
  gold = factor(rep(c("Present", "Absent", "Present", "Absent"), c(TP, FP, FN, TN)), levels = c("Absent", "Present")),
  test = factor(rep(c("Pos", "Pos", "Neg", "Neg"), c(TP, FP, FN, TN)), levels = c("Neg", "Pos")))
val_run <- function(d, gp = "Present", tp = "Pos", gn = NULL, tn = NULL, ...)
  suppressWarnings(ClinicoPath::decision(data = d, gold = "gold", goldPositive = gp, goldNegative = gn,
                                         newtest = "test", testPositive = tp, testNegative = tn, ...))
val_ratio <- function(r) r$ratioTable$asDF[1, ]
val_key <- function(df, key) df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
val_strip <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(x, collapse = " "))))
val_notices <- function(r) val_strip(r$notices$content)

# ---------------------------------------------------------------------------------- live --

test_that("C11-C18 known answer: TP 12, FP 3, FN 5, TN 30", {
  # Oracle: hand fractions (Altman & Bland 1994 definitions)
  rt <- val_ratio(val_run(val_mk(12, 3, 5, 30)))
  expect_equal(unlist(rt[, c("Sens", "Spec", "AccurT", "PrevalenceD", "PPV", "NPV", "LRP", "LRN")]),
               c(12 / 17, 30 / 33, 42 / 50, 17 / 50, 12 / 15, 30 / 35, 396 / 51, 165 / 510),
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("C17/C29/C18/C19 reproduce Deeks & Altman 2004 BMJ 329:168 Table 1 (>=40 pack years)", {
  # Oracle: published LR+ 20.4 (5.04 to 82.8), LR- 0.73, post-test 0.69 at pre-test 0.1
  r <- val_run(val_mk(42, 2, 106, 142), ci = TRUE)
  lp <- val_key(r$epirTable_number$asDF, "lr.pos")
  # Absolute half-unit in the last printed digit (testthat 3e's tolerance is relative, so not used here)
  expect_lt(abs(val_ratio(r)$LRP - 20.4), 0.05)
  expect_lt(abs(lp$lower - 5.04), 0.005)
  expect_lt(abs(lp$upper - 82.8), 0.05)
  expect_lt(abs(val_ratio(r)$LRN - 0.73), 0.005)
  expect_lt(abs(val_ratio(val_run(val_mk(42, 2, 106, 142), pp = TRUE, pprob = 0.1))$PPV - 0.69), 0.005)
})

test_that("C26 Se/Sp/PPV/NPV intervals are Clopper-Pearson (stats::binom.test)", {
  r <- val_run(val_mk(88, 31, 22, 159), ci = TRUE)
  df <- r$epirTable_ratio$asDF
  cp <- function(x, n) as.numeric(stats::binom.test(x, n)$conf.int)
  expect_equal(unlist(val_key(df, "se")[, c("lower", "upper")]), cp(88, 110), tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(unlist(val_key(df, "sp")[, c("lower", "upper")]), cp(159, 190), tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(unlist(val_key(df, "pv.pos")[, c("lower", "upper")]), cp(88, 119), tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(unlist(val_key(df, "pv.neg")[, c("lower", "upper")]), cp(159, 181), tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("C29/C30/C28 LR intervals follow Simel 1991, DOR follows Woolf", {
  # Oracle: closed-form log-scale intervals typed from the estimand
  a <- 88; b <- 31; c <- 22; d <- 159; z <- stats::qnorm(0.975)
  lrp <- (a / (a + c)) / (b / (b + d)); slp <- sqrt(1 / a - 1 / (a + c) + 1 / b - 1 / (b + d))
  lrn <- (c / (a + c)) / (d / (b + d)); sln <- sqrt(1 / c - 1 / (a + c) + 1 / d - 1 / (b + d))
  dor <- a * d / (b * c); sd <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  df <- val_run(val_mk(a, b, c, d), ci = TRUE)$epirTable_number$asDF
  row <- function(k) unlist(val_key(df, k)[, c("est", "lower", "upper")])
  expect_equal(row("lr.pos"), lrp * exp(c(0, -z, z) * slp), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(row("lr.neg"), lrn * exp(c(0, -z, z) * sln), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(row("diag.or"), dor * exp(c(0, -z, z) * sd), tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("C11 sens/spec/PPV/NPV agree with caret::confusionMatrix", {
  skip_if_not_installed("caret")
  tab <- as.table(matrix(c(88, 22, 31, 159), 2, dimnames = list(pred = c("P", "N"), ref = c("P", "N"))))
  cm <- caret::confusionMatrix(tab, positive = "P")$byClass
  rt <- val_ratio(val_run(val_mk(88, 31, 22, 159)))
  expect_equal(unlist(rt[, c("Sens", "Spec", "PPV", "NPV")]),
               unname(cm[c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value")]),
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("C19/C20 a population prior gives Bayes PPV/NPV and leaves Se/Sp/LR alone", {
  se <- 88 / 110; sp <- 159 / 190; p <- 0.1
  r0 <- val_run(val_mk(88, 31, 22, 159)); r1 <- val_run(val_mk(88, 31, 22, 159), pp = TRUE, pprob = p)
  expect_equal(val_ratio(r1)$PPV, p * se / (p * se + (1 - p) * (1 - sp)), tolerance = 1e-10)
  expect_equal(val_ratio(r1)$NPV, (1 - p) * sp / ((1 - p) * sp + p * (1 - se)), tolerance = 1e-10)
  expect_equal(val_ratio(r1)$PrevalenceD, p)
  expect_equal(unlist(val_ratio(r1)[, c("Sens", "Spec", "AccurT", "LRP", "LRN")]),
               unlist(val_ratio(r0)[, c("Sens", "Spec", "AccurT", "LRP", "LRN")]))
})

test_that("C11 metamorphic: swapping the disease level maps sens -> 1 - spec and LR+ -> 1/LR+", {
  a <- 88; b <- 31; c <- 22; d <- 159
  r <- val_run(val_mk(a, b, c, d), gp = "Absent")
  expect_equal(val_ratio(r)$Sens, 1 - d / (b + d), tolerance = 1e-10)
  expect_equal(val_ratio(r)$Spec, 1 - a / (a + c), tolerance = 1e-10)
  expect_equal(val_ratio(r)$LRP, 1 / ((a / (a + c)) / (b / (b + d))), tolerance = 1e-10)
})

test_that("C11 metamorphic: swapping both positive levels swaps sens/spec and PPV/NPV", {
  r0 <- val_ratio(val_run(val_mk(88, 31, 22, 159)))
  r1 <- val_ratio(val_run(val_mk(88, 31, 22, 159), gp = "Absent", tp = "Neg"))
  expect_equal(c(r1$Sens, r1$Spec, r1$PPV, r1$NPV), c(r0$Spec, r0$Sens, r0$NPV, r0$PPV), tolerance = 1e-10)
})

test_that("C17/C22 a zero cell: only the ratio whose formula has the zero is corrected, and both tables say so", {
  # FN = 0 enters LR- and the DOR, not LR+ = (20/20)/(5/30) = 6 (observed, defined).
  r <- val_run(val_mk(20, 5, 0, 25), ci = TRUE, fagan = TRUE)
  expect_equal(val_ratio(r)$Sens, 1)
  expect_equal(val_ratio(r)$LRP, 6, tolerance = 1e-10)
  expect_equal(val_ratio(r)$LRN, (0.5 / 21) / (25.5 / 31), tolerance = 1e-10)
  expect_equal(val_key(r$epirTable_number$asDF, "lr.pos")$est, val_ratio(r)$LRP, tolerance = 1e-10)
  expect_equal(val_key(r$epirTable_number$asDF, "lr.neg")$est, val_ratio(r)$LRN, tolerance = 1e-10)
  expect_false(is.null(r$ratioTable$notes[["continuity"]]))
  expect_false(is.null(r$epirTable_number$notes[["continuity"]]))
  # the nomogram's proportions reproduce the table's ratios exactly
  st <- r$plot1$state
  expect_equal(c(st$Sens / (1 - st$Spec), (1 - st$Sens) / st$Spec), c(val_ratio(r)$LRP, val_ratio(r)$LRN), tolerance = 1e-10)
})

test_that("C11/C72 an unselected gold level is excluded, not pooled into disease-absent", {
  d <- rbind(val_mk(30, 6, 5, 40), data.frame(gold = "Equivocal", test = c(rep("Pos", 4), rep("Neg", 5))))
  d$gold <- factor(d$gold, levels = c("Absent", "Equivocal", "Present")); d$test <- factor(d$test, levels = c("Neg", "Pos"))
  r <- val_run(d, gn = "Absent")
  expect_equal(c(val_ratio(r)$Sens, val_ratio(r)$Spec), c(30 / 35, 40 / 46), tolerance = 1e-10)
  expect_equal(r$nTable$asDF$TotalPop, 81)
  expect_match(val_notices(val_run(d)), "Choose which gold-standard level means disease absent", fixed = TRUE)
})

test_that("C73 an explicit-NA level is excluded and disclosed, never counted as negative", {
  base <- val_mk(40, 10, 10, 40)
  d <- rbind(base, data.frame(gold = c(NA, NA), test = c("Pos", "Neg")))
  d$gold <- addNA(factor(d$gold, levels = c("Absent", "Present"))); d$test <- factor(d$test, levels = c("Neg", "Pos"))
  r <- val_run(d)
  expect_equal(r$nTable$asDF$TotalPop, 100)
  expect_equal(val_ratio(r)$Spec, 40 / 50, tolerance = 1e-10)
  expect_match(val_notices(r), "Removed 2 case(s) with an explicit missing level", fixed = TRUE)
})

test_that("C65-C68 guard thresholds fire on the stated side of the boundary", {
  expect_match(val_notices(val_run(val_mk(6, 10, 3, 81))), "Few cases in one arm: 9 disease-present", fixed = TRUE)
  expect_no_match(val_notices(val_run(val_mk(6, 10, 4, 80))), "Few cases in one arm", fixed = TRUE)
  expect_match(val_notices(val_run(val_mk(40, 95, 9, 856))), "Very low disease prevalence observed in this sample (4.9%)", fixed = TRUE)
  expect_no_match(val_notices(val_run(val_mk(40, 95, 10, 855))), "Very low disease prevalence", fixed = TRUE)
  expect_match(val_notices(val_run(val_mk(15, 16, 5, 4))), "This test performs worse than chance", fixed = TRUE)
  expect_match(val_notices(val_run(val_mk(10, 10, 20, 20))), "This test is uninformative", fixed = TRUE)
})

test_that("C49 the nomogram's posterior endpoints equal PPV and 1 - NPV", {
  r <- val_run(val_mk(12, 3, 5, 30), fagan = TRUE)
  st <- r$plot1$state
  p <- suppressWarnings(ClinicoPath:::nomogrammer(Prevalence = st$Prevalence, Sens = st$Sens, Spec = st$Spec))
  b <- ggplot2::ggplot_build(p)$data[[1]]
  inv <- function(y) 10^y / (1 + 10^y)
  expect_equal(inv(b$y[b$colour == "red" & b$x == 1]), val_ratio(r)$PPV, tolerance = 1e-8)
  expect_equal(inv(b$y[b$colour == "blue" & b$x == 1]), 1 - val_ratio(r)$NPV, tolerance = 1e-8)
})

test_that("C41 under a row filter the FP/FN 'Row' column is the spreadsheet row", {
  d <- as.data.frame(val_mk(40, 25, 10, 60))[-(1:5), ]
  r <- val_run(d, showMisclassified = TRUE)
  expect_equal(r$falseNegativeTable$asDF$case_id, 66:75)
})

# ------------------------------------------------- fixed 2026-09-25 (were open defects) --
# Each block below held the correct expectation behind a skip() while the defect was open.
# Where the fix introduced a sourced convention (Hosmer-Lemeshow-Sturdivant AUC bands for the
# summary word, Newcombe's interval for Youden's index), the expected values come from that
# source, not from the old vocabulary.

# Agresti & Caffo (2000) interval for Youden = TP/n1 - FP/n0, typed from the method's definition.
val_ac <- function(x1, n1, x2, n2) {
  z <- stats::qnorm(0.975); q1 <- (x1 + 1) / (n1 + 2); q2 <- (x2 + 1) / (n2 + 2)
  s <- sqrt(q1 * (1 - q1) / (n1 + 2) + q2 * (1 - q2) / (n2 + 2))
  c(x1 / n1 - x2 / n2, max(-1, q1 - q2 - z * s), min(1, q1 - q2 + z * s))
}

test_that("C32 VAL-decision-01: the NNDx interval contains its estimate, and NNDx is never negative", {
  # se .60, sp .55: Youden 0.15 with an interval crossing 0 -> NNDx 6.67, lower 1/U, upper
  # unbounded (blank) and explained by a note. Was 6.67 (-3.08 to 1.73).
  r <- val_run(val_mk(12, 9, 8, 11), ci = TRUE)
  row <- val_key(r$epirTable_number$asDF, "nndx")
  expect_equal(row$est, 1 / 0.15, tolerance = 1e-10)
  expect_equal(row$lower, 1 / val_ac(12, 20, 9, 20)[3], tolerance = 1e-10)
  expect_true(is.na(row$upper))
  expect_false(is.null(r$epirTable_number$notes[["nndx_unbounded"]]))
  # Youden -0.05: no point estimate (was -20); the valid set (1/U, Inf) is still shown, and
  # the table says why the estimate is blank.
  ri <- val_run(val_mk(15, 16, 5, 4), ci = TRUE)
  inv <- val_key(ri$epirTable_number$asDF, "nndx")
  expect_true(is.na(inv$est))
  expect_equal(inv$lower, 1 / val_ac(15, 20, 16, 20)[3], tolerance = 1e-10)
  expect_true(is.na(inv$upper))
  expect_false(is.null(ri$epirTable_number$notes[["nndx_undefined"]]))
})

test_that("C31 Youden's interval is the Agresti-Caffo interval (formula and DescTools)", {
  # Oracles: the formula above (Agresti & Caffo 2000) and DescTools::BinomDiffCI(method = "ac").
  # Tables include zero counts and x = n, where the interval must stay inside [-1, 1].
  for (e in list(c(12, 3, 5, 30), c(20, 5, 0, 25), c(15, 0, 0, 20), c(0, 1, 5, 19), c(88, 31, 22, 159))) {
    y <- val_key(val_run(val_mk(e[1], e[2], e[3], e[4]), ci = TRUE)$epirTable_number$asDF, "youden")
    ref <- val_ac(e[1], e[1] + e[3], e[2], e[2] + e[4])
    expect_equal(c(y$est, y$lower, y$upper), ref, tolerance = 1e-10)
    if (requireNamespace("DescTools", quietly = TRUE))
      expect_equal(c(y$lower, y$upper),
                   as.numeric(DescTools::BinomDiffCI(e[1], e[1] + e[3], e[2], e[2] + e[4], method = "ac")[1, 2:3]),
                   tolerance = 1e-8)
  }
})

test_that("C54 VAL-decision-02: the summary never calls a poor or worse-than-chance test 'moderate'", {
  # Expected words: Hosmer, Lemeshow & Sturdivant (2013) AUC bands with AUC = (1 + J)/2.
  word <- function(r) sub(".*the test shows ([[:alpha:]-]+) discrimination \\(equivalent AUC.*", "\\1",
                          val_strip(r$naturalLanguageSummary$content))
  expect_equal(word(val_run(val_mk(15, 16, 5, 4), showNaturalLanguage = TRUE)), "worse-than-chance")  # J = -0.05
  expect_equal(word(val_run(val_mk(95, 70, 5, 30), showNaturalLanguage = TRUE)), "poor")               # AUC 0.625
  expect_equal(word(val_run(val_mk(35, 15, 15, 35), showNaturalLanguage = TRUE)), "acceptable")         # AUC 0.70 exactly
  expect_equal(word(val_run(val_mk(95, 5, 5, 95), showNaturalLanguage = TRUE)), "outstanding")          # AUC 0.95
  # banded at the printed 3 decimals: J = .6996 + .7000 - 1 = .3996 prints 0.400 -> acceptable
  expect_equal(word(val_run(val_mk(1749, 750, 751, 1750), showNaturalLanguage = TRUE)), "acceptable")
  expect_match(val_strip(val_run(val_mk(35, 15, 15, 35), showNaturalLanguage = TRUE)$naturalLanguageSummary$content),
               "rule of thumb for the area under the ROC curve (Hosmer, Lemeshow and Sturdivant 2013), the test shows acceptable discrimination (equivalent AUC 0.700)",
               fixed = TRUE)
})

test_that("C55 LR bands are decided at the printed two decimals, in every panel", {
  # LR+ = (26/29)/(7/39) = 4.995 prints 5.00: Jaeschke's 5-10 band, "moderate"
  r <- val_run(val_mk(26, 7, 3, 32), showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE,
               showReportTemplate = TRUE)
  expect_match(val_strip(r$naturalLanguageSummary$content), "Positive LR: 5.00 ( Moderate evidence for disease )", fixed = TRUE)
  expect_match(val_strip(r$clinicalInterpretation$content), "Positive LR (5.00): Moderate increase", fixed = TRUE)
  expect_match(val_strip(r$reportTemplate$content), "provides moderate evidence for disease when positive", fixed = TRUE)
  # LR- = (9/100)/(90/100) = 0.1 exactly: "0.1 to 0.2" -> moderate
  r9 <- val_run(val_mk(91, 10, 9, 90), showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE)
  expect_match(val_strip(r9$naturalLanguageSummary$content), "Negative LR: 0.10 ( Moderate evidence against disease )", fixed = TRUE)
  expect_match(val_strip(r9$clinicalInterpretation$content), "Negative LR (0.10): Moderate decrease", fixed = TRUE)
})

test_that("C55 a corrected LR on the wrong side of 1 from the observed data has no stated direction", {
  # TP 0, FP 1, FN 5, TN 19: sensitivity 0, Youden < 0; the zero-cell corrected LR+ is 1.17 > 1.
  r <- val_run(val_mk(0, 1, 5, 19), showNaturalLanguage = TRUE, showReportTemplate = TRUE)
  rp <- val_strip(r$reportTemplate$content)
  expect_match(rp, "no reliable evidence either way", fixed = TRUE)
  expect_no_match(rp, "minimal evidence for disease", fixed = TRUE)
  expect_match(val_strip(r$naturalLanguageSummary$content), "Unreliable: rests on a zero count", fixed = TRUE)
})

test_that("C55 VAL-decision-03: a likelihood ratio of exactly 1 in rationals reads as uninformative", {
  r <- val_run(val_mk(10, 10, 20, 20), showClinicalInterpretation = TRUE, showReportTemplate = TRUE,
               showNaturalLanguage = TRUE)
  expect_match(val_strip(r$clinicalInterpretation$content), "Uninformative: a positive result leaves", fixed = TRUE)
  expect_match(val_strip(r$naturalLanguageSummary$content), "Uninformative: a negative result does not change", fixed = TRUE)
  expect_no_match(val_strip(r$reportTemplate$content), "inverted", fixed = TRUE)
})

test_that("C55 VAL-decision-04: every panel puts LR+ = 10 in the same band (Jaeschke: 5-10 moderate)", {
  r <- val_run(val_mk(40, 4, 24, 60), showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE,
               showReportTemplate = TRUE)
  expect_equal(val_ratio(r)$LRP, 10)
  expect_match(val_strip(r$naturalLanguageSummary$content), "Moderate evidence for disease", fixed = TRUE)
  expect_match(val_strip(r$clinicalInterpretation$content), "Moderate increase in probability of disease", fixed = TRUE)
  expect_match(val_strip(r$reportTemplate$content), "moderate evidence for disease when positive", fixed = TRUE)
})

test_that("C63 VAL-decision-05: the report's printed LR and its band agree", {
  txt <- val_strip(val_run(val_mk(49, 25, 51, 75), showReportTemplate = TRUE)$reportTemplate$content)
  expect_no_match(txt, "likelihood ratio of 2.0 provides minimal", fixed = TRUE)
  expect_match(txt, "likelihood ratio of 1.96", fixed = TRUE)
})

test_that("C62 VAL-decision-06: the copy-ready report states N, both arms and an interval per estimate", {
  # Oracles: stats::binom.test (Clopper-Pearson) for PPV/NPV; Simel 1991 log interval for LR+.
  txt <- val_strip(val_run(val_mk(12, 3, 5, 30), showReportTemplate = TRUE)$reportTemplate$content)
  expect_match(txt, "in 50 cases (17 with and 33 without the target condition)", fixed = TRUE)
  cpv <- stats::binom.test(12, 15)$conf.int; cnv <- stats::binom.test(30, 35)$conf.int
  expect_match(txt, sprintf("positive predictive value was 80.0%% (95%% CI %.1f-%.1f%%)", 100 * cpv[1], 100 * cpv[2]), fixed = TRUE)
  expect_match(txt, sprintf("negative predictive value was 85.7%% (95%% CI %.1f-%.1f%%)", 100 * cnv[1], 100 * cnv[2]), fixed = TRUE)
  lr <- (12 / 17) / (3 / 33); se <- sqrt(1 / 12 - 1 / 17 + 1 / 3 - 1 / 33)
  ci <- lr * exp(c(-1, 1) * stats::qnorm(0.975) * se)
  expect_match(txt, sprintf("likelihood ratio of %.2f (95%% CI %.2f-%.2f)", lr, ci[1], ci[2]), fixed = TRUE)
  # With a population prior the report says, INSIDE the copied text, that its predictive
  # values carry no interval
  txp <- val_strip(val_run(val_mk(12, 3, 5, 30), showReportTemplate = TRUE, pp = TRUE, pprob = 0.1)$reportTemplate$content)
  expect_match(txp, "so no confidence interval is given for them", fixed = TRUE)
  expect_lt(regexpr("so no confidence interval is given for them", txp, fixed = TRUE),
            regexpr("Copy the text above", txp, fixed = TRUE))
  expect_no_match(txp, "positive predictive value was [0-9.]+% \\(95% CI")
})

test_that("C50 VAL-decision-07: the nomogram draws both pathways for a strong rule-out test", {
  st <- val_run(val_mk(19, 19, 1, 361), fagan = TRUE)$plot1$state          # prevalence 5%, se = sp = .95
  p <- suppressWarnings(ClinicoPath:::nomogrammer(Prevalence = st$Prevalence, Sens = st$Sens, Spec = st$Spec))
  b <- suppressWarnings(ggplot2::ggplot_build(p)$data[[1]])
  expect_equal(sum(is.finite(b$y[b$colour == "blue"])), 2)
  # and a nomogram whose endpoints already fitted keeps its old frame (other callers unchanged)
  st1 <- val_run(val_mk(12, 3, 5, 30), fagan = TRUE)$plot1$state
  p1 <- ClinicoPath:::nomogrammer(Prevalence = st1$Prevalence, Sens = st1$Sens, Spec = st1$Spec)
  t <- log10(c(0.001, 0.99) / (1 - c(0.001, 0.99))); old <- t + abs(t[1]) - diff(t) / 2
  b1 <- ggplot2::ggplot_build(p1)
  expect_equal(b1$layout$panel_params[[1]]$y.range, old, tolerance = 1e-10)
  lab <- b1$layout$panel_params[[1]]$y.sec$get_labels()
  expect_equal(as.numeric(lab[!is.na(lab)]), c(0.5, 1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99))
  # the prior at the UI minimum (0.001) keeps both pathways
  s0 <- val_run(val_mk(12, 3, 5, 30), fagan = TRUE, pp = TRUE, pprob = 0.001)$plot1$state
  b0 <- suppressWarnings(ggplot2::ggplot_build(ClinicoPath:::nomogrammer(Prevalence = s0$Prevalence, Sens = s0$Sens, Spec = s0$Spec))$data[[1]])
  expect_equal(c(sum(is.finite(b0$y[b0$colour == "red"])), sum(is.finite(b0$y[b0$colour == "blue"]))), c(2, 2))
})

test_that("C91 VAL-decision-09: the DOR footnote gives the odds-ratio definition (Glas 2003)", {
  r <- val_run(val_mk(88, 31, 22, 159), ci = TRUE, fnote = TRUE)
  note <- paste(r$epirTable_number$getCell(rowKey = "diag.or", col = "statsnames")$footnotes, collapse = " ")
  expect_match(note, "odds of a positive test in patients with the disease divided by the odds of a positive test in patients without it", fixed = TRUE)
  expect_no_match(note, "correct diagnosis than an incorrect diagnosis", fixed = TRUE)
})

test_that("C71 VAL-decision-10: a one-level gold standard is refused with its own message", {
  d <- rbind(val_mk(88, 0, 12, 0), data.frame(gold = c("Present", "Absent"), test = c(NA, NA)))
  d$gold <- factor(d$gold, levels = c("Absent", "Present")); d$test <- addNA(factor(d$test, levels = c("Neg", "Pos")))
  n <- val_notices(val_run(d))
  expect_match(n, "The gold standard has only one level among the analysed cases", fixed = TRUE)
  expect_no_match(n, "has more than one level besides", fixed = TRUE)
})

test_that("C71 VAL-decision-10: a one-level test is not described as having 'more than one level'", {
  d <- rbind(val_mk(88, 12, 0, 0), data.frame(gold = c(NA, NA), test = c("Pos", "Neg")))
  d$gold <- addNA(factor(d$gold, levels = c("Absent", "Present"))); d$test <- factor(d$test, levels = c("Neg", "Pos"))
  n <- val_notices(val_run(d))
  expect_no_match(n, "has more than one level besides", fixed = TRUE)
  expect_match(n, "The test has only one level among the analysed cases", fixed = TRUE)
})

test_that("C69 fewer than 4 cases left after exclusions is refused, not tabulated", {
  # 6 rows pass the raw-row guard; 3 are Equivocal and excluded -> n = 3 analysed.
  d <- data.frame(gold = factor(c("Present", "Absent", "Absent", "Equivocal", "Equivocal", "Equivocal"),
                                levels = c("Absent", "Equivocal", "Present")),
                  test = factor(c("Pos", "Neg", "Pos", "Pos", "Neg", "Pos"), levels = c("Neg", "Pos")))
  r <- val_run(d, gn = "Absent")
  expect_match(val_notices(r), "Insufficient data after exclusions: 3 cases analysed", fixed = TRUE)
  expect_true(is.na(val_ratio(r)$Sens))
})

test_that("C71 a level emptied by missing-value removal is dropped, not inferred as the negative level", {
  # Review finding RT-04: every test-negative row has a missing gold standard. The emptied "Neg"
  # level used to stay in levels(), get inferred, and the run was refused as "not dichotomous".
  d <- val_mk(20, 5, 4, 15); d$gold[d$test == "Neg"] <- NA
  n <- val_notices(val_run(d))
  expect_match(n, "The test has only one level among the analysed cases", fixed = TRUE)
  expect_no_match(n, "must have exactly 2 levels each", fixed = TRUE)
  # a declared but never-used third level no longer blocks inference of the negative level
  d3 <- val_mk(88, 31, 22, 159); d3$gold <- factor(as.character(d3$gold), levels = c("Absent", "Present", "Unknown"))
  expect_equal(val_ratio(val_run(d3))$Sens, 88 / 110, tolerance = 1e-10)
})
