# Regression tests from the 2026-10-12 validation of decisioncurve
# (development-ideas/decisioncurve-validation-2026-10-12.md; evidence script
# development-scripts/validate_decisioncurve.R). Expected values are hand counts on an 11-patient
# fixture, independently reproduced with exact fractions, or dcurves 0.5.1 output. The VAL-* tests
# were written skip-guarded against the defects found that day; all were fixed on 2026-10-13.

val_dc <- function(data, outcome, pos, models, ..., rulePos = NULL)
  suppressWarnings(suppressMessages(do.call(ClinicoPath::decisioncurve,
    c(list(data = data, outcome = outcome, outcomePositive = pos, models = models,
           decisionRulePositive = rulePos), list(...)))))
val_one <- function(df, sel) { r <- df[sel, , drop = FALSE]; stopifnot(nrow(r) == 1); r }
val_tab_nb <- function(res, title, t) {
  cols <- res$resultsTable$columns
  nm <- vapply(cols, function(c) c$name, ""); ti <- vapply(cols, function(c) c$title, "")
  df <- res$resultsTable$asDF
  val_one(df, abs(df$threshold - t) < 1e-9)[[nm[ti == title]]]
}
val_curve_nb <- function(res, model, t) {
  pd <- res$dcaPlot$state$plotData
  val_one(pd, pd$model == model & abs(pd$threshold - t) < 1e-9)$net_benefit
}
val_nb <- function(pred, y, t) { pos <- pred >= t; (sum(pos & y) - sum(pos & !y) * t / (1 - t)) / length(y) }

# 12 rows, row 12 missing Model A -> n = 11, 4 events. Ties sit exactly on every threshold and the
# positive level is the FIRST factor level.
val_tiny <- function() data.frame(
  o = factor(c(rep("malignant", 4), rep("benign", 7), "malignant"), levels = c("malignant", "benign")),
  a = c(.80, .45, .30, .20, .30, .20, .10, .05, .10, .02, .15, NA),
  b = c(.60, .30, .25, .05, .10, .40, .20, .15, .02, .50, .08, .70),
  r = factor(c("treat", "treat", "observe", "treat", "treat", "observe", "observe", "observe",
               "observe", "treat", "observe", "treat"), levels = c("treat", "observe")))
val_tiny_run <- function(...) {
  args <- list(thresholdRange = "custom", thresholdMin = 0.10, thresholdMax = 0.30, thresholdStep = 0.05,
               selectedThresholds = "0.10, 0.15, 0.20, 0.25, 0.30",
               clinicalDecisionRule = TRUE, decisionRuleVar = "r", rulePos = "treat",
               decisionRuleLabel = "Rule X", showBenefitRange = TRUE, weightedAUC = TRUE,
               calculateClinicalImpact = TRUE, costBenefitAnalysis = TRUE,
               showDecisionConsequences = TRUE, resourceUtilization = TRUE)
  do.call(val_dc, c(list(val_tiny(), "o", "malignant", c("a", "b")), utils::modifyList(args, list(...))))
}
VAL_TH <- c(.10, .15, .20, .25, .30)

test_that("C01/C02 net benefit table equals hand counts at the exact thresholds", {
  # Oracle: hand counts of prediction >= t (TP/n - FP/n * t/(1-t)); Python Fraction cross-check.
  res <- val_tiny_run()
  expect_equal(vapply(VAL_TH, function(t) val_tab_nb(res, "a", t), 0), c(31/99, 59/187, 7/22, 8/33, 18/77), tolerance = 1e-10)
  expect_equal(vapply(VAL_TH, function(t) val_tab_nb(res, "b", t), 0), c(2/9, 39/187, 9/44, 7/33, 8/77), tolerance = 1e-10)
  expect_equal(vapply(VAL_TH, function(t) val_tab_nb(res, "Rule X", t), 0), c(25/99, 45/187, 5/22, 7/33, 15/77), tolerance = 1e-10)
  expect_equal(vapply(VAL_TH, function(t) val_tab_nb(res, "Treat All", t), 0), c(29/99, 47/187, 9/44, 5/33, 1/11), tolerance = 1e-10)
})

test_that("C10 range of benefit is strict against max(treat all, 0), including an exact NB tie", {
  # Oracle: hand. Model b equals treat-all exactly at t = .20 (9/44), so its range starts at .25.
  br <- val_tiny_run()$benefitRangeTable$asDF
  r <- function(m) unlist(val_one(br, br$model == m)[c("range_start", "range_end", "range_width")])
  expect_equal(unname(r("a")), c(.10, .30, .20), tolerance = 1e-10)
  expect_equal(unname(r("b")), c(.25, .30, .05), tolerance = 1e-10)
  expect_equal(unname(r("Rule X")), c(.20, .30, .10), tolerance = 1e-10)
})

test_that("C28-C30 decision consequences at exact thresholds", {
  # Oracle: hand counts. Model a at .15: TP 4 FP 3 TN 4 FN 0; at .30: 3 1 6 1. Rule: 3 2 5 1.
  dct <- val_tiny_run()$decisionConsequencesTable$asDF
  g <- function(m, t) val_one(dct, dct$model == m & abs(dct$threshold - t) < 1e-9)
  r <- g("a", .15)
  expect_equal(c(r$true_positive, r$false_positive, r$true_negative, r$false_negative), c(4, 3, 4, 0))
  expect_equal(c(r$sensitivity, r$specificity, r$ppv, r$npv), c(1, 4/7, 4/7, 1), tolerance = 1e-10)
  r <- g("a", .30)
  expect_equal(c(r$sensitivity, r$specificity, r$ppv, r$npv), c(3/4, 6/7, 3/4, 6/7), tolerance = 1e-10)
  r <- g("Rule X", .15)
  expect_equal(c(r$true_positive, r$false_positive, r$true_negative, r$false_negative), c(3, 2, 5, 1))
})

test_that("C23-C27 clinical impact projections to a population of 1000", {
  # Oracle: hand. Net interventions avoided = (NB - NB_all)/odds * N = (TN - FN*(1-t)/t)/n * N.
  cit <- val_tiny_run()$clinicalImpactTable$asDF
  g <- function(m, t) unlist(val_one(cit, cit$model == m & abs(cit$threshold - t) < 1e-9)[
    c("interventions_per_100", "true_positives_per_100", "false_positives_per_100",
      "interventions_avoided", "number_needed_to_screen")])
  expect_equal(unname(g("a", .15)), c(7000/11, 4000/11, 3000/11, 4000/11, 11/4), tolerance = 1e-10)
  expect_equal(unname(g("a", .30)), c(4000/11, 3000/11, 1000/11, 1000/3, 11/3), tolerance = 1e-10)
  expect_equal(unname(g("Rule X", .15)), c(5000/11, 3000/11, 2000/11, -2000/33, 11/3), tolerance = 1e-10)
})

test_that("C31-C34 monetary payoff and resource utilisation", {
  # Oracle: hand, default costs (test 100, treatment 1000, TP value 10000, FP harm 500), N = 1000.
  res <- val_tiny_run()
  cb <- res$costBenefitTable$asDF
  r <- val_one(cb, cb$model == "a" & abs(cb$threshold - .30) < 1e-9)
  expect_equal(c(r$total_cost, r$total_benefit, r$net_monetary_benefit, r$incremental_cost,
                 r$incremental_benefit, r$incremental_net_payoff),
               c(5100000/11, 29500000/11, 24400000/11, -5900000/11, -7000000/11, -100000), tolerance = 1e-10)
  ru <- res$resourceUtilizationTable$asDF
  r <- val_one(ru, ru$model == "a" & abs(ru$threshold - .30) < 1e-9)
  # "Projected Tests" was dropped: it always equalled populationSize (CF-decisioncurve-10).
  expect_equal(c(r$treatments_per_1000, r$unnecessary_treatments, r$missed_cases,
                 r$reduction_vs_treat_all), c(4000/11, 1000/11, 1000/11, 7/11), tolerance = 1e-10)
})

test_that("C05/C35/C37 curves, net interventions avoided and standardized NB match dcurves", {
  skip_if_not_installed("dcurves")
  # Oracle: dcurves 0.5.1 at the module's own grid values (continuous marker, no ties at t).
  d0 <- as.data.frame(dcurves::df_binary)
  d <- data.frame(y = factor(ifelse(d0$cancer, "TRUE", "FALSE"), levels = c("FALSE", "TRUE")),
                  m = d0$cancerpredmarker)
  res <- val_dc(d, "y", "TRUE", "m", thresholdRange = "auto", showInterventionAvoided = TRUE)
  pd <- res$dcaPlot$state$plotData; pm <- pd[pd$model == "m", ]; pm <- pm[order(pm$threshold), ]
  ref <- dcurves::dca(cancer ~ cancerpredmarker, data = d0, thresholds = pm$threshold)
  tb <- as.data.frame(dplyr::as_tibble(ref))
  expect_equal(pm$net_benefit, tb[tb$variable == "cancerpredmarker", "net_benefit"], tolerance = 1e-10)
  nia <- as.data.frame(dplyr::as_tibble(dcurves::net_intervention_avoided(ref, nper = 100)))
  ggplot2::set_last_plot(NULL)
  suppressWarnings(res$interventionsAvoidedPlot$saveAs(tempfile(fileext = ".png")))
  pi <- ggplot2::last_plot()$data; pi <- pi[order(pi$threshold), ]
  expect_equal(pi$interventions_avoided, nia[nia$variable == "cancerpredmarker", "net_intervention_avoided"], tolerance = 1e-10)
})

test_that("C07/C08/C17 bootstrap intervals and p reproduce from the documented seed substreams", {
  skip_if_not_installed("dcurves")
  # Oracle: own resampling code; streams seed + 1e5 + model index (CI) and seed + 3e5 + pair (p).
  d0 <- as.data.frame(dcurves::df_binary)
  d <- data.frame(y = factor(ifelse(d0$cancer, "yes", "no"), levels = c("no", "yes")),
                  m = d0$cancerpredmarker, f = d0$famhistory)
  B <- 150; L <- 0.9; seed <- 777
  res <- val_dc(d, "y", "yes", c("m", "f"), thresholdRange = "custom", thresholdMin = .05, thresholdMax = .35,
                thresholdStep = .05, confidenceIntervals = TRUE, compareModels = TRUE, bootReps = B,
                ciLevel = L, seed = seed)
  pd <- res$dcaPlot$state$plotData; pm <- pd[pd$model == "m", ]; pm <- pm[order(pm$threshold), ]
  thr <- pm$threshold; y <- d$y == "yes"; n <- nrow(d)
  bt <- withr::with_seed(seed + 100001L, t(vapply(seq_len(B), function(b) {
    i <- sample(n, n, replace = TRUE); vapply(thr, function(t) val_nb(d$m[i], y[i], t), 0) }, numeric(length(thr)))))
  expect_equal(pm$ci_lower, apply(bt, 2, quantile, (1 - L) / 2, names = FALSE), tolerance = 1e-10)
  expect_equal(pm$ci_upper, apply(bt, 2, quantile, 1 - (1 - L) / 2, names = FALSE), tolerance = 1e-10)
  nbh <- vapply(thr, function(t) val_nb(d$m, y, t), 0); se <- apply(bt, 2, sd)
  q <- quantile(apply(abs(sweep(bt, 2, nbh)) / matrix(se, B, length(thr), byrow = TRUE), 1, max), L, names = FALSE)
  expect_equal(pm$sim_lower, nbh - q * se, tolerance = 1e-10)
  trap <- function(v) sum(diff(thr) * (head(v, -1) + tail(v, -1)) / 2) / (max(thr) - min(thr))
  bc <- withr::with_seed(seed + 300001L, vapply(seq_len(B), function(b) {
    i <- sample(n, n, replace = TRUE)
    trap(vapply(thr, function(t) val_nb(d$m[i], y[i], t), 0)) - trap(vapply(thr, function(t) val_nb(d$f[i], y[i], t), 0)) }, 0))
  p <- min(1, 2 * min((sum(bc >= 0) + 1) / (B + 1), (sum(bc <= 0) + 1) / (B + 1)))
  expect_equal(res$comparisonTable$asDF$p_value, p, tolerance = 1e-12)
})

test_that("C06/C7/C11 metamorphic relations hold", {
  skip_if_not_installed("dcurves")
  d0 <- as.data.frame(dcurves::df_binary)
  d <- data.frame(y = factor(ifelse(d0$cancer, "ca", "no"), levels = c("no", "ca")),
                  p = d0$cancerpredmarker, q = 1 - d0$cancerpredmarker, f = d0$famhistory)
  # Swapping the two models negates the difference, mirrors the CI and keeps p.
  a <- val_dc(d, "y", "ca", c("p", "f"), compareModels = TRUE, bootReps = 100)$comparisonTable$asDF
  b <- val_dc(d, "y", "ca", c("f", "p"), compareModels = TRUE, bootReps = 100)$comparisonTable$asDF
  expect_equal(c(b$weighted_auc_diff, b$ci_lower, b$ci_upper, b$p_value),
               c(-a$weighted_auc_diff, -a$ci_upper, -a$ci_lower, a$p_value), tolerance = 1e-12)
  # Net interventions avoided at t equals NB at 1 - t for the complementary event with risk 1 - p.
  ra <- val_dc(d, "y", "ca", "p", selectedThresholds = "0.10, 0.20, 0.30", calculateClinicalImpact = TRUE)
  rb <- val_dc(d, "y", "no", "q", thresholdRange = "auto", selectedThresholds = "0.70, 0.80, 0.90")
  nia <- ra$clinicalImpactTable$asDF; nia <- nia[order(nia$threshold), "interventions_avoided"] / 1000
  expect_equal(nia, vapply(c(.9, .8, .7), function(t) val_tab_nb(rb, "q", t), 0), tolerance = 1e-10)
  # Identical models: difference 0, CI [0, 0], p = 1.
  ct <- val_dc(transform(d, p2 = p), "y", "ca", c("p", "p2"), compareModels = TRUE, bootReps = 100)$comparisonTable$asDF
  expect_equal(c(ct$weighted_auc_diff, ct$ci_lower, ct$ci_upper, ct$p_value), c(0, 0, 0, 1))
})

test_that("C10/C11 a superior set with a gap is disclosed in the interpretation", {
  # Oracle: hand design (see validation script H3); superior on .05-.20 and .35-.55.
  pr <- c(rep(.03, 20), rep(.22, 11), rep(.62, 40))
  y <- factor(c(rep("no", 20), rep("yes", 10), "no", rep("yes", 30), rep("no", 10)), levels = c("no", "yes"))
  res <- val_dc(data.frame(y = y, p = pr), "y", "yes", "p", thresholdRange = "custom", thresholdMin = .05,
                thresholdMax = .55, thresholdStep = .05, showBenefitRange = TRUE, showInterpretation = TRUE)
  br <- res$benefitRangeTable$asDF
  expect_equal(c(br$range_start, br$range_end), c(.05, .55), tolerance = 1e-10)
  expect_match(res$summaryText$content, "contains a gap", fixed = TRUE)
})

test_that("C46/C47/C49 guards fire at their stated thresholds", {
  mk <- function(n, events) { set.seed(1); data.frame(
    y = factor(c(rep("yes", events), rep("no", n - events)), levels = c("no", "yes")),
    p = round(runif(n, .02, .98), 4)) }
  # A fatal input problem is jamovi's error state (jmvcore::reject), not a banner (guide §25).
  expect_error(val_dc(mk(9, 4), "y", "yes", "p"), "Insufficient complete cases", fixed = TRUE)
  expect_gt(nrow(val_dc(mk(10, 4), "y", "yes", "p")$resultsTable$asDF), 0)
  expect_match(val_dc(mk(200, 24), "y", "yes", "p")$notices$content, "Few Outcome Events", fixed = TRUE)
  expect_no_match(val_dc(mk(200, 25), "y", "yes", "p")$notices$content, "Few Outcome Events", fixed = TRUE)
  expect_match(val_dc(mk(200, 9), "y", "yes", "p")$notices$content, "Extreme Prevalence", fixed = TRUE)
  expect_no_match(val_dc(mk(200, 10), "y", "yes", "p")$notices$content, "Extreme Prevalence", fixed = TRUE)
  expect_match(val_dc(mk(200, 191), "y", "yes", "p")$notices$content, "Extreme Prevalence", fixed = TRUE)
})

# ---- regressions for the defects found on 2026-10-12 (fixed 2026-10-13) --------------------------

test_that("VAL-decisioncurve-01 the curve point labelled t equals the table row at t", {
  # Oracle: hand counts. Model a has a non-event at exactly .15; the table counts it, the curve does not.
  res <- val_tiny_run()
  for (m in c("a", "b")) for (t in VAL_TH)
    expect_equal(val_curve_nb(res, m, t), val_tab_nb(res, m, t), tolerance = 1e-12)
  expect_equal(val_curve_nb(res, "a", .15), 59/187, tolerance = 1e-12)
})

test_that("VAL-decisioncurve-01 average NB and gain use the labelled thresholds", {
  # Oracle: exact fractions (trapezoid / width) at decimal thresholds; b's gain is NEGATIVE.
  wa <- val_tiny_run()$weightedAUCTable$asDF
  expect_equal(val_one(wa, wa$model == "a")$weighted_auc, 13543/47124, tolerance = 1e-10)
  expect_equal(val_one(wa, wa$model == "b")$benefit_gain, -65/23562, tolerance = 1e-10)
})

test_that("VAL-decisioncurve-01 bundled 3-decimal predictions: curve equals dcurves at the labelled thresholds", {
  skip_if_not_installed("dcurves")
  d <- ClinicoPath::dca_test_data
  d$cardiac_event <- factor(d$cardiac_event, levels = c("No", "Yes"))
  res <- val_dc(d, "cardiac_event", "Yes", "basic_model")
  pd <- res$dcaPlot$state$plotData; pm <- pd[pd$model == "basic_model", ]; pm <- pm[order(pm$threshold), ]
  ref <- as.data.frame(dplyr::as_tibble(dcurves::dca(cardiac_event ~ basic_model,
    data = transform(d, cardiac_event = cardiac_event == "Yes"), thresholds = round(pm$threshold, 2))))
  expect_equal(pm$net_benefit, ref[ref$variable == "basic_model", "net_benefit"], tolerance = 1e-10)
})

test_that("VAL-decisioncurve-02 the two comparison tables do not report different 'average' differences", {
  skip_if_not_installed("dcurves")
  d0 <- as.data.frame(dcurves::df_binary)
  d <- data.frame(y = factor(ifelse(d0$cancer, "yes", "no"), levels = c("no", "yes")),
                  m = d0$cancerpredmarker, f = d0$famhistory)
  res <- val_dc(d, "y", "yes", c("m", "f"), compareModels = TRUE, multiModelComparison = TRUE, bootReps = 100)
  a <- res$comparisonTable$asDF$weighted_auc_diff; b <- res$modelComparisonEnhanced$asDF$nb_difference_mean
  ttl <- vapply(res$modelComparisonEnhanced$columns, function(c) c$title, "")
  # Either one estimand in both tables, or a title that says it is a different one.
  expect_true(isTRUE(all.equal(a, b)) || any(grepl("grid|arithmetic|unweighted", ttl, ignore.case = TRUE)))
})

test_that("VAL-decisioncurve-03 few NON-events is warned like few events (24 warns, 25 does not)", {
  set.seed(1)
  d <- data.frame(y = factor(c(rep("yes", 180), rep("no", 20)), levels = c("no", "yes")), p = round(runif(200, .02, .98), 4))
  expect_match(val_dc(d, "y", "yes", "p")$notices$content, "Few Patients Without the Outcome", fixed = TRUE)
  set.seed(1)
  d25 <- data.frame(y = factor(c(rep("yes", 175), rep("no", 25)), levels = c("no", "yes")), p = round(runif(200, .02, .98), 4))
  expect_no_match(val_dc(d25, "y", "yes", "p")$notices$content, "Few Patients Without the Outcome", fixed = TRUE)
})

test_that("VAL-decisioncurve-04 strict-inequality warnings do not fire exactly on their boundary", {
  set.seed(1)
  d <- data.frame(y = factor(c(rep("yes", 50), rep("no", 150)), levels = c("no", "yes")), p = round(runif(200, .02, .98), 4))
  wide <- val_dc(d, "y", "yes", "p", thresholdRange = "custom", thresholdMin = .10, thresholdMax = .80)
  expect_no_match(wide$notices$content, "Wide Threshold Range", fixed = TRUE)
  narrow <- val_dc(d, "y", "yes", "p", thresholdRange = "custom", thresholdMin = .20, thresholdMax = .25)
  expect_no_match(narrow$notices$content, "Narrow Range", fixed = TRUE)
  flat <- data.frame(y = factor(rep(c("yes", "no"), 30), levels = c("no", "yes")), p = rep(c(.30, .35), 30))
  expect_no_match(val_dc(flat, "y", "yes", "p")$notices$content, "Narrow Probability Range", fixed = TRUE)
})

test_that("VAL-decisioncurve-05 the simultaneous band is not attributed to a different method", {
  skip_if_not_installed("dcurves")
  d0 <- as.data.frame(dcurves::df_binary)
  d <- data.frame(y = factor(ifelse(d0$cancer, "yes", "no"), levels = c("no", "yes")), m = d0$cancerpredmarker)
  res <- val_dc(d, "y", "yes", "m", confidenceIntervals = TRUE, ciBand = "simultaneous", bootReps = 100,
                showInterpretation = TRUE)
  expect_no_match(res$summaryText$content, "Mandel", fixed = TRUE)
  expect_match(res$summaryText$content, "Montiel Olea and Plagborg-Moller 2019", fixed = TRUE)
})

test_that("VAL-decisioncurve-06 sparse thresholds are flagged when intervals are requested", {
  # 1000 patients, but only 5 have a predicted risk above .30: intervals there collapse.
  set.seed(2)
  p <- c(rep(.40, 5), runif(995, .01, .29)); y <- factor(ifelse(runif(1000) < p, "yes", "no"), levels = c("no", "yes"))
  d <- data.frame(y = y, p = p)
  ci <- val_dc(d, "y", "yes", "p", confidenceIntervals = TRUE, bootReps = 100)
  expect_match(ci$notices$content, "Few Positive Classifications: p", fixed = TRUE)
  # Oracle: first default-grid threshold (5%..50% by 1%) at which fewer than 10 risks are >= t.
  grid <- round(seq(.05, .50, by = .01), 2)
  first <- grid[which(vapply(grid, function(t) sum(p >= t), 0) < 10)[1]]
  expect_match(ci$notices$content, sprintf("From the %.1f%% threshold upwards", 100 * first), fixed = TRUE)
  expect_no_match(val_dc(d, "y", "yes", "p")$notices$content, "Few Positive Classifications", fixed = TRUE)
})

test_that("VAL-decisioncurve-07 exactly tied models are both named, each with its own range", {
  set.seed(4); p <- runif(200); y <- factor(ifelse(runif(200) < p, "yes", "no"), levels = c("no", "yes"))
  st <- val_dc(data.frame(y = y, a = p, b = p), "y", "yes", c("a", "b"), showInterpretation = TRUE)$summaryText$content
  expect_match(st, "Selected Range:</strong> a, b (tied)", fixed = TRUE)
  expect_match(st, "Range of Benefit (a):", fixed = TRUE)
  expect_match(st, "Range of Benefit (b):", fixed = TRUE)
})

test_that("VAL-decisioncurve-08 the relative utility plot states its definition", {
  res <- val_tiny_run(showRelativeUtility = TRUE)
  ggplot2::set_last_plot(NULL)
  suppressWarnings(res$relativeUtilityPlot$saveAs(tempfile(fileext = ".png")))
  cap <- ggplot2::get_labs(ggplot2::last_plot())$caption
  expect_match(cap, "RU = (NB - D) / (prevalence - D)", fixed = TRUE)
})
