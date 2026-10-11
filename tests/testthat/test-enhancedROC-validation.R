# Validation regression tests for enhancedROC (/validate-function, 2026-10-09).
# Report: development-ideas/enhancedROC-validation-2026-10-09.md
# Evidence script: development-scripts/validate_enhancedROC.R
#
# Live tests lock in claims that passed against an independent oracle. The VAL-enhancedROC-NN tests
# were written skip-guarded, holding the CORRECT expectation for a confirmed defect; every defect is
# now fixed and the tests run. Three were rewritten because the fix changed the expectation by design:
# VAL-03 (the positive class is chosen, never pre-filled), VAL-04 (a degenerate interval is left
# blank and explained) and VAL-14 (the tie option was retired for an automatic note with bounds).

# ---- fixtures and oracles ------------------------------------------------------------------------
# D1, hand-checkable: 10 Benign (controls), 10 Malignant (cases), cross-class ties at 3, 5, 6 and 8.
erv_d1 <- function() data.frame(
    status = factor(c(rep("Benign", 10), rep("Malignant", 10)), levels = c("Benign", "Malignant")),
    marker = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 10,   3, 5, 6, 8, 9, 11, 12, 13, 14, 15))
erv_cont <- function() {                       # continuous, no ties, 40 controls / 30 cases
    set.seed(20261008)
    x0 <- round(rnorm(40, 0, 1), 6); x1 <- round(rnorm(30, 1.2, 1.3), 6)
    data.frame(y = factor(c(rep("neg", 40), rep("pos", 30)), levels = c("neg", "pos")), m1 = c(x0, x1))
}
erv_two <- function() {                        # two markers on the same patients
    set.seed(20261009)
    lat <- c(rnorm(70, 0, 1), rnorm(50, 1, 1))
    data.frame(y = factor(c(rep("neg", 70), rep("pos", 50)), levels = c("neg", "pos")),
               A = round(lat + rnorm(120, 0, 0.6), 6), B = round(0.5 * lat + rnorm(120, 0, 1), 6))
}
erv_prob <- function() {                       # supplied probabilities, true logit = 0.3 + 1.5 logit(p)
    set.seed(20261010)
    p <- round(rbeta(400, 2, 5), 6)
    data.frame(y = factor(ifelse(runif(400) < plogis(0.3 + 1.5 * qlogis(p)), "event", "none"),
                          levels = c("none", "event")), p = p)
}
erv_perfect <- function() data.frame(y = factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos")),
                                     x = c(1:15, 16:30))
# do.call so that a variable holding a column name is passed as a value, not read as a bare symbol.
erv_run <- function(data, outcome, predictors, positiveClass, ...)
    suppressWarnings(suppressMessages(do.call(ClinicoPath::enhancedROC,
        c(list(data = data, outcome = outcome, predictors = predictors, positiveClass = positiveClass), list(...)))))
erv_cell <- function(tab, key, col) {
    df <- tab$asDF
    rownames(df) <- gsub('^"|"$', "", rownames(df))
    r <- df[rownames(df) == key, , drop = FALSE]
    if (nrow(r) != 1L) return(NA)
    r[[col]][1]
}
erv_notices <- function(res) as.character(res$results$notices$content)
erv_psi <- function(cs, ct) outer(cs, ct, ">") + 0.5 * outer(cs, ct, "==")
erv_delong <- function(cs, ct) {               # DeLong, DeLong & Clarke-Pearson (1988) placement values
    psi <- erv_psi(cs, ct)
    v <- stats::var(rowMeans(psi)) / length(cs) + stats::var(colMeans(psi)) / length(ct)
    list(auc = mean(psi), se = sqrt(v), v10 = rowMeans(psi), v01 = colMeans(psi))
}
erv_curve <- function(cs, ct) {                # empirical ROC vertices in path order
    u <- sort(unique(c(cs, ct))); th <- c(-Inf, (head(u, -1) + tail(u, -1)) / 2, Inf)
    p <- data.frame(fpr = 1 - vapply(th, function(t) mean(ct < t), 0), tpr = vapply(th, function(t) mean(cs >= t), 0))
    unique(p[order(p$fpr, p$tpr), ])
}
erv_croc <- function(cv, alpha) {              # Swamidass et al. (2010): integral of TPR d f(FPR), exact
    f <- function(x) (1 - exp(-alpha * x)) / (1 - exp(-alpha))
    Fi <- function(x0, x1) ((x1 - x0) + (exp(-alpha * x1) - exp(-alpha * x0)) / alpha) / (1 - exp(-alpha))
    s <- 0
    for (i in seq_len(nrow(cv) - 1)) {
        x0 <- cv$fpr[i]; x1 <- cv$fpr[i + 1]; y0 <- cv$tpr[i]; y1 <- cv$tpr[i + 1]
        if (x1 > x0) s <- s + y1 * f(x1) - y0 * f(x0) - (y1 - y0) / (x1 - x0) * Fi(x0, x1)
    }
    s
}
erv_logit <- function(y, x = NULL, offset = 0) {   # Newton-Raphson maximum likelihood, independent of glm()
    X <- if (is.null(x)) matrix(1, length(y), 1) else cbind(1, x); b <- rep(0, ncol(X))
    for (it in 1:200) {
        mu <- plogis(drop(X %*% b) + offset)
        st <- drop(solve(crossprod(X, X * (mu * (1 - mu))), crossprod(X, y - mu))); b <- b + st
        if (max(abs(st)) < 1e-13) break
    }
    b
}

# ---- live tests (claims that passed against an independent oracle) -------------------------------

test_that("C01-C03 AUC, DeLong SE and 95% CI equal DeLong (1988) by hand on D1", {
    # Oracle: case placements .3 .55 .65 .85 .9 1 1 1 1 1 -> AUC 82.5/100; Var = (0.54125/9 + 0.25625/9)/10.
    res <- erv_run(erv_d1(), "status", "marker", "Malignant")
    se <- sqrt((0.54125 / 9 + 0.25625 / 9) / 10)
    expect_equal(erv_cell(res$results$aucSummary, "marker", "auc"), 0.825, tolerance = 1e-12)
    expect_equal(erv_cell(res$results$aucSummary, "marker", "std_error"), se, tolerance = 1e-10)
    expect_equal(c(erv_cell(res$results$aucSummary, "marker", "auc_lower"), erv_cell(res$results$aucSummary, "marker", "auc_upper")),
                 pmin(1, 0.825 + c(-1, 1) * qnorm(0.975) * se), tolerance = 1e-10)
})

test_that("C02 DeLong interval follows the Confidence level option (ratio of z quantiles)", {
    d <- erv_cont()
    hw <- function(lvl) { r <- erv_run(d, "y", "m1", "pos", confidenceLevel = lvl)
        (erv_cell(r$results$aucSummary, "m1", "auc_upper") - erv_cell(r$results$aucSummary, "m1", "auc_lower")) / 2 }
    expect_equal(hw(99) / hw(95), qnorm(0.995) / qnorm(0.975), tolerance = 1e-8)
})

test_that("C08-C11 Youden optimum on D1: cutoff 7.5, Se .7, Sp .8, J .5, accuracy .75", {
    # Oracle: J = .5 at thresholds 7.5, 8.5, 10.5 (exact ties); by the module's convention the tie
    # closest to the top-left corner is reported, 7.5 (distance .13 against .17 and .25).
    tb <- erv_run(erv_d1(), "status", "marker", "Malignant")$results$optimalCutoffSummary
    expect_equal(c(erv_cell(tb, "marker", "optimal_cutoff"), erv_cell(tb, "marker", "sensitivity"), erv_cell(tb, "marker", "specificity"),
                   erv_cell(tb, "marker", "youden_index"), erv_cell(tb, "marker", "accuracy")), c(7.5, 0.7, 0.8, 0.5, 0.75), tolerance = 1e-12)
})

test_that("C10 Youden optimum agrees with cutpointr on continuous data", {
    skip_if_not_installed("cutpointr")
    d <- erv_cont()
    cp <- cutpointr::cutpointr(data.frame(x = d$m1, cl = as.character(d$y)), x, cl, pos_class = "pos", neg_class = "neg", direction = ">=",
                               method = cutpointr::maximize_metric, metric = cutpointr::youden, break_ties = c, silent = TRUE)
    tb <- erv_run(d, "y", "m1", "pos")$results$optimalCutoffSummary
    se <- erv_cell(tb, "m1", "sensitivity"); sp <- erv_cell(tb, "m1", "specificity")
    expect_true(any(abs(unlist(cp$sensitivity) - se) < 1e-12 & abs(unlist(cp$specificity) - sp) < 1e-12))
    expect_equal(erv_cell(tb, "m1", "youden_index"), max(unlist(cp$sensitivity) + unlist(cp$specificity) - 1), tolerance = 1e-12)
})

test_that("C13 closest-to-top-left criterion when Youden optimisation is off", {
    # Oracle: argmin (1-Se)^2 + (1-Sp)^2 over the D1 thresholds is t = 7.5 (Se .7, Sp .8; d = .13).
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", youdenOptimization = FALSE, cutoffTable = TRUE)
    expect_equal(c(erv_cell(r$results$optimalCutoffSummary, "marker", "optimal_cutoff"),
                   erv_cell(r$results$optimalCutoffSummary, "marker", "sensitivity")), c(7.5, 0.7))
    expect_equal(erv_cell(r$results$cutoffAnalysis, "marker_optimal", "cutoff_type"), "Optimal (closest to top-left)")
})

test_that("C14 a minimum-sensitivity constraint gives the constrained Youden optimum and says so", {
    d <- erv_cont(); cs <- d$m1[d$y == "pos"]; ct <- d$m1[d$y == "neg"]
    u <- sort(unique(d$m1)); th <- c(-Inf, (head(u, -1) + tail(u, -1)) / 2, Inf)
    se <- vapply(th, function(t) mean(cs >= t), 0); sp <- vapply(th, function(t) mean(ct < t), 0)
    ok <- se >= 0.95; jm <- max((se + sp - 1)[ok])
    r <- erv_run(d, "y", "m1", "pos", sensitivityThreshold = 0.95)
    expect_equal(erv_cell(r$results$optimalCutoffSummary, "m1", "youden_index"), jm, tolerance = 1e-12)
    expect_true(erv_cell(r$results$optimalCutoffSummary, "m1", "sensitivity") >= 0.95)
    expect_match(erv_notices(r), "Cutoff Constrained by Your Thresholds", fixed = TRUE)
})

test_that("C16/C18 sensitivity and specificity CIs are Clopper-Pearson at the chosen level", {
    # Oracle: beta quantiles (Clopper & Pearson 1934) for 7/10 and 8/10.
    for (lvl in c(95, 90)) {
        a <- (1 - lvl / 100) / 2
        r <- erv_run(erv_d1(), "status", "marker", "Malignant", confidenceLevel = lvl)
        tb <- r$results$diagnosticPerformance
        expect_equal(erv_cell(tb, "marker", "sensitivity_ci"), sprintf("(%s, %s)", round(qbeta(a, 7, 4), 3), round(qbeta(1 - a, 8, 3), 3)))
        expect_equal(erv_cell(tb, "marker", "specificity_ci"), sprintf("(%s, %s)", round(qbeta(a, 8, 3), 3), round(qbeta(1 - a, 9, 2), 3)))
        expect_equal(tb$getColumn("sensitivity_ci")$title, sprintf("Sensitivity %s%% CI", lvl))
    }
})

test_that("C19-C22 PPV/NPV by Bayes, LR+, LR- and DOR from the D1 2x2 (TP 7, FP 2, FN 3, TN 8)", {
    tb <- erv_run(erv_d1(), "status", "marker", "Malignant", clinicalMetrics = TRUE)$results$clinicalApplicationMetrics
    expect_equal(c(erv_cell(tb, "marker", "ppv"), erv_cell(tb, "marker", "npv")), c(7 / 9, 8 / 11), tolerance = 1e-12)
    expect_equal(c(erv_cell(tb, "marker", "lr_positive"), erv_cell(tb, "marker", "lr_negative"), erv_cell(tb, "marker", "diagnostic_odds_ratio")),
                 c(3.5, 0.375, 56 / 6), tolerance = 1e-12)
    tb2 <- erv_run(erv_d1(), "status", "marker", "Malignant", clinicalMetrics = TRUE, useObservedPrevalence = FALSE, prevalence = 0.1)$results$clinicalApplicationMetrics
    expect_equal(c(erv_cell(tb2, "marker", "ppv"), erv_cell(tb2, "marker", "npv")), c(0.07 / 0.25, 0.72 / 0.75), tolerance = 1e-12)
})

test_that("C24 an infinite LR+ and DOR are blank with the reason, LR- is 0", {
    tb <- erv_run(erv_perfect(), "y", "x", "pos", clinicalMetrics = TRUE)$results$clinicalApplicationMetrics
    expect_true(is.na(erv_cell(tb, "x", "lr_positive")))
    expect_equal(erv_cell(tb, "x", "lr_negative"), 0)
    expect_match(erv_cell(tb, "x", "clinical_interpretation"), "LR+ is infinite", fixed = TRUE)
})

test_that("C26 paired DeLong comparison of two markers on the same patients", {
    # Oracle: DeLong (1988) covariance of the placement values, z = dAUC/sqrt(Var(A-B)).
    d <- erv_two(); pos <- d$y == "pos"
    a <- erv_delong(d$A[pos], d$A[!pos]); b <- erv_delong(d$B[pos], d$B[!pos])
    v <- (var(a$v10) + var(b$v10) - 2 * cov(a$v10, b$v10)) / sum(pos) + (var(a$v01) + var(b$v01) - 2 * cov(a$v01, b$v01)) / sum(!pos)
    z <- (a$auc - b$auc) / sqrt(v)
    tb <- erv_run(d, "y", c("A", "B"), "pos", analysisType = "comparative", pairwiseComparisons = TRUE)$results$rocComparisons
    expect_equal(erv_cell(tb, "A_vs_B", "test_statistic"), z, tolerance = 1e-7)
    expect_equal(erv_cell(tb, "A_vs_B", "p_value"), 2 * pnorm(-abs(z)), tolerance = 1e-7)
})

test_that("C35-C37 partial AUC (specificity and sensitivity focus) and McClish (1989) standardisation", {
    # Oracle (D1 by hand): spec[.8,1] = .05 + .065 = .115; sens[.8,1] = .055 + .03 = .085; min = .02, max = .2.
    r1 <- erv_run(erv_d1(), "status", "marker", "Malignant", partialAuc = TRUE)
    r2 <- erv_run(erv_d1(), "status", "marker", "Malignant", partialAuc = TRUE, partialAucType = "sensitivity")
    expect_equal(c(erv_cell(r1$results$partialAucAnalysis, "marker", "partial_auc"), erv_cell(r1$results$partialAucAnalysis, "marker", "normalized_pauc")),
                 c(0.115, 0.5 * (1 + 0.095 / 0.18)), tolerance = 1e-10)
    expect_equal(c(erv_cell(r2$results$partialAucAnalysis, "marker", "partial_auc"), erv_cell(r2$results$partialAucAnalysis, "marker", "normalized_pauc")),
                 c(0.085, 0.5 * (1 + 0.065 / 0.18)), tolerance = 1e-10)
})

test_that("C41 ROC convex hull area (upper hull of the empirical points)", {
    # Oracle (D1 by hand): hull (0,0),(0,.5),(.2,.7),(.5,.9),(.8,1),(1,1) -> .12 + .24 + .285 + .2 = .845.
    tb <- erv_run(erv_d1(), "status", "marker", "Malignant", convexHull = TRUE)$results$convexHullTable
    expect_equal(erv_cell(tb, "marker", "hull_auc"), 0.845, tolerance = 1e-12)
})

test_that("C46-C48 precision-recall: average precision .85 and max F1 .75 on D1", {
    # Oracle: AP = .5 + .1*(6/7 + 7/9 + 8/12 + 9/14 + 10/18); max F1 at P 9/14, R .9.
    tb <- erv_run(erv_d1(), "status", "marker", "Malignant", detectImbalance = TRUE)$results$precisionRecallTable
    expect_equal(erv_cell(tb, "marker", "average_precision"), 0.85, tolerance = 1e-12)
    expect_equal(c(erv_cell(tb, "marker", "f1_score"), erv_cell(tb, "marker", "precision"), erv_cell(tb, "marker", "recall")), c(0.75, 9 / 14, 0.9), tolerance = 1e-12)
})

test_that("C58-C63 calibration of supplied probabilities: Brier, scaled Brier, slope, intercept, CITL, E/O", {
    # Supplied probabilities are DECLARED (Prediction scale = Probability): values in [0, 1] are no
    # longer read as risks on sight, and the default Score scale fits a logistic model instead.
    d <- erv_prob(); y <- as.integer(d$y == "event"); p <- d$p
    tb <- erv_run(d, "y", "p", "event", calibrationAnalysis = TRUE, brierScore = TRUE, calibrationMetrics = TRUE, eoRatio = TRUE,
                  predictionScale = "probability")$results$calibrationSummary
    b <- unname(erv_logit(y, qlogis(p)))
    expect_equal(erv_cell(tb, "p", "brier_score"), mean((p - y)^2), tolerance = 1e-12)
    expect_equal(erv_cell(tb, "p", "scaled_brier"), 1 - mean((p - y)^2) / (mean(y) * (1 - mean(y))), tolerance = 1e-12)
    expect_equal(c(erv_cell(tb, "p", "calibration_intercept"), erv_cell(tb, "p", "calibration_slope")), b, tolerance = 1e-6)
    expect_equal(erv_cell(tb, "p", "calibration_in_large"), erv_logit(y, NULL, offset = qlogis(p)), tolerance = 1e-6)
    expect_equal(erv_cell(tb, "p", "eo_ratio"), sum(p) / sum(y), tolerance = 1e-12)
})

test_that("C65-C67 Hosmer-Lemeshow statistic, and df = g for externally supplied probabilities", {
    skip_if_not_installed("ResourceSelection")
    d <- erv_prob(); y <- as.integer(d$y == "event"); p <- d$p
    tb <- erv_run(d, "y", "p", "event", calibrationAnalysis = TRUE, hosmerLemeshow = TRUE,
                  predictionScale = "probability")$results$hosmerLemeshowTable
    H <- unname(ResourceSelection::hoslem.test(y, p, g = 10)$statistic)
    expect_equal(erv_cell(tb, "p", "chi_square"), H, tolerance = 1e-10)
    expect_equal(erv_cell(tb, "p", "df"), 10)
    expect_equal(erv_cell(tb, "p", "p_value"), pchisq(H, 10, lower.tail = FALSE), tolerance = 1e-10)
})

test_that("C69-C71 multi-class one-vs-rest and one-vs-one AUCs and their averages", {
    set.seed(20261011)
    cls <- factor(rep(c("Low", "Mid", "High"), c(30, 25, 20)), levels = c("Low", "Mid", "High"))
    d <- data.frame(cls = cls, m = round(c(rnorm(30, 0), rnorm(25, 0.8), rnorm(20, 1.5)), 6))
    auc <- function(a, b) mean(erv_psi(a, b))
    ovr <- vapply(levels(cls), function(l) auc(d$m[cls == l], d$m[cls != l]), 0)
    r <- erv_run(d, "cls", "m", NULL, multiClassROC = TRUE)
    # No positive class: only the multi-class tables run, and the notice says how to get the binary ones.
    expect_equal(r$results$aucSummary$rowCount, 0)
    expect_match(erv_notices(r), "Binary ROC tables need a positive class", fixed = TRUE)
    expect_equal(c(erv_cell(r$results$multiClassAUC, "m_Low", "auc"), erv_cell(r$results$multiClassAUC, "m_Mid", "auc"),
                   erv_cell(r$results$multiClassAUC, "m_High", "auc")), unname(ovr), tolerance = 1e-12)
    expect_equal(c(erv_cell(r$results$multiClassAverage, "m", "macro_auc"), erv_cell(r$results$multiClassAverage, "m", "weighted_auc")),
                 c(mean(ovr), sum(c(30, 25, 20) * ovr) / 75), tolerance = 1e-12)
    ovo <- c(auc(d$m[cls == "Mid"], d$m[cls == "Low"]), auc(d$m[cls == "High"], d$m[cls == "Low"]), auc(d$m[cls == "High"], d$m[cls == "Mid"]))
    r2 <- erv_run(d, "cls", "m", NULL, multiClassROC = TRUE, multiClassStrategy = "ovo")
    expect_equal(erv_cell(r2$results$multiClassAverage, "m", "macro_auc"), mean(ovo), tolerance = 1e-12)
})

test_that("C74-C80 clinical impact at a fixed score cutoff and a projected population", {
    # Oracle: at cutoff 7.5 TP 7, FP 2 of N 20; NB = (7 - 2*.1/.9)/20. At 2% in 1000: TP 14, FP 196.
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", clinicalImpact = TRUE, nntCalculation = TRUE, decisionImpactTable = TRUE,
                 impactCutoffs = "7.5", prevalenceScenarios = "0.02")
    nb <- (7 - 2 * 0.1 / 0.9) / 20
    expect_equal(erv_cell(r$results$clinicalImpactTable, "marker_7.5", "net_benefit_per_100"), 100 * nb, tolerance = 1e-12)
    expect_equal(erv_cell(r$results$clinicalImpactTable, "marker_7.5", "nnt"), 1 / nb, tolerance = 1e-12)
    expect_equal(erv_cell(r$results$clinicalImpactTable, "marker_7.5", "nnd"), 2, tolerance = 1e-12)
    wl <- r$results$screeningWorkload$asDF
    pr <- wl[wl$source == "Projected" & abs(wl$prevalence - 0.02) < 1e-12, ]
    expect_equal(c(pr$tp, pr$fp, pr$ppv, pr$net_benefit), c(14, 196, 14 / 210, (14 - 196 * 0.1 / 0.9) / 1000), tolerance = 1e-12)
})

test_that("C82 frozen-model metrics at a fixed cutoff (no refitting, no optimisation)", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", validationPanel = TRUE, validationNegative = "Benign",
                 modelVersion = "v1", impactCutoffs = "7.5")
    fm <- r$results$frozenMetrics$asDF
    est <- function(m) fm$estimate[fm$metric == m & fm$model == "marker" & fm$site == "All"]
    expect_equal(c(est("auc"), est("sensitivity"), est("specificity"), est("ppv"), est("npv"), est("f1")),
                 c(0.825, 0.7, 0.8, 7 / 9, 8 / 11, 14 / 19), tolerance = 1e-12)
})

test_that("C83 patient-cluster bootstrap resamples whole patients", {
    # Two identical slides per patient resampled by patient must give exactly the intervals of the
    # de-duplicated data resampled by row (same seed, same patient order).
    set.seed(20261012)
    d1 <- data.frame(y = factor(rep(c("neg", "pos"), c(35, 25)), levels = c("neg", "pos")),
                     s = round(c(rnorm(35), rnorm(25, 1)), 6), pid = as.character(1:60))
    d2 <- d1[rep(1:60, each = 2), ]
    fb <- function(d, ...) {
        f <- erv_run(d, "y", "s", "pos", validationPanel = TRUE, validationNegative = "neg", modelVersion = "v1", impactCutoffs = "0.5",
                     validationBootstrap = TRUE, validationDraws = 200, seed = 11, ...)$results$frozenMetrics$asDF
        unlist(f[f$metric == "auc" & f$model == "s" & f$site == "All", c("lower", "upper")])
    }
    expect_equal(fb(d2, patientID = "pid"), fb(d1))
})

test_that("C88/C93 guards: fewer than 10 per class is an ERROR banner; fewer than 20 rows is refused", {
    set.seed(1)
    d9 <- data.frame(y = factor(c(rep("neg", 30), rep("pos", 9)), levels = c("neg", "pos")), x = c(rnorm(30), rnorm(9, 1.5)))
    expect_match(erv_notices(erv_run(d9, "y", "x", "pos")), "Low Per-Class Count", fixed = TRUE)
    d19 <- data.frame(y = factor(c(rep("neg", 10), rep("pos", 9)), levels = c("neg", "pos")), x = c(rnorm(10), rnorm(9, 1.5)))
    r19 <- erv_run(d19, "y", "x", "pos")
    expect_equal(nrow(r19$results$aucSummary$asDF), 0)
    expect_match(erv_notices(r19), "Data Validation Failed", fixed = TRUE)
})

test_that("C04 bootstrap AUC interval is boot.ci on the same resamples (percentile)", {
    skip_if_not_installed("boot")
    d <- erv_cont()
    r <- erv_run(d, "y", "m1", "pos", useBootstrap = TRUE, bootstrapSamples = 300, bootstrapMethod = "percentile", seed = 42)
    set.seed(42)
    rr <- pROC::roc(d$y, d$m1, levels = c("neg", "pos"), direction = "<", quiet = TRUE)
    bd <- data.frame(resp = rr$response, pred = rr$predictor)
    st <- function(z, i) as.numeric(pROC::auc(pROC::roc(z$resp[i], z$pred[i], levels = c("neg", "pos"), direction = "<", quiet = TRUE)))
    b <- boot::boot(bd, st, R = 300, strata = rep(1L, nrow(bd)))
    expect_equal(c(erv_cell(r$results$aucSummary, "m1", "auc_lower"), erv_cell(r$results$aucSummary, "m1", "auc_upper")),
                 boot::boot.ci(b, conf = 0.95, type = "perc")$percent[4:5], tolerance = 1e-12)
})

test_that("C117 same seed reproduces a bootstrap interval and the caller's RNG stream is restored", {
    d <- erv_cont()
    lim <- function(s) { r <- erv_run(d, "y", "m1", "pos", useBootstrap = TRUE, bootstrapSamples = 200, seed = s)
        c(erv_cell(r$results$aucSummary, "m1", "auc_lower"), erv_cell(r$results$aucSummary, "m1", "auc_upper")) }
    expect_identical(lim(7), lim(7))
    set.seed(123); u1 <- runif(1); invisible(lim(1)); u2 <- runif(1)
    set.seed(123); v <- runif(2)
    expect_identical(c(u1, u2), v)
})

# ---- fixed defects (VAL-enhancedROC-NN): the correct expectation, now live -------------------------

test_that("C39 CROC AUC is the exact integral of TPR d f(FPR) (VAL-enhancedROC-01)", {
    r <- erv_run(erv_perfect(), "y", "x", "pos", crocAnalysis = TRUE)
    expect_equal(erv_cell(r$results$crocAnalysisTable, "x", "croc_auc"), 1, tolerance = 1e-10)
    d <- erv_cont(); cv <- erv_curve(d$m1[d$y == "pos"], d$m1[d$y == "neg"])
    r2 <- erv_run(d, "y", "m1", "pos", crocAnalysis = TRUE)
    expect_equal(erv_cell(r2$results$crocAnalysisTable, "m1", "croc_auc"), erv_croc(cv, 7), tolerance = 1e-8)
})

test_that("C97 the ROC curve is drawn as a monotone path (VAL-enhancedROC-02)", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    r <- erv_run(erv_cont(), "y", "m1", "pos")
    r$results$rocCurvePlot$saveAs(tempfile(fileext = ".png"))
    ld <- ggplot2::layer_data(ggplot2::last_plot(), 1)
    expect_equal(sum(vapply(split(ld, ld$group), function(g) sum(diff(g$y) < -1e-9), 0)), 0)
})

test_that("C112 the positive class is chosen deliberately, not pre-filled with the first level (VAL-enhancedROC-03)", {
    a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "enhancedROC.a.yaml"))
    opt <- Filter(function(o) identical(o$name, "positiveClass"), a$options)[[1]]
    expect_true(isTRUE(opt$allowNone))
    # Until a level is chosen nothing is computed, and that is the normal first step, not an error: an
    # INFO notice asks for the choice and the Outcome coding table lists every level as not assigned.
    # NULL is what jamovi sends for None; "" is what an R caller's empty Level argument sends.
    for (pc in list(NULL, "")) {
        r <- erv_run(erv_d1(), "status", "marker", pc)
        expect_equal(r$results$aucSummary$rowCount, 0)
        expect_match(erv_notices(r), "Choose the positive class", fixed = TRUE)
        expect_false(grepl("rgba(220, 38, 38, 0.10)", erv_notices(r), fixed = TRUE))   # no ERROR panel
        oc <- r$results$outcomeCoding
        expect_equal(c(erv_cell(oc, "lvl:Benign", "n"), erv_cell(oc, "lvl:Malignant", "n")), c(10, 10))
        expect_equal(oc$asDF$role, rep("Not assigned yet", 2))
        expect_match(oc$notes[["unset"]]$note, "levels are never chosen from their order", fixed = TRUE)
    }
})

test_that("C02 no zero-width AUC interval is presented as an estimate (VAL-enhancedROC-04)", {
    # Perfect separation: AUC = 1 and DeLong's SE is 0. The limits and the SE are left blank ("", never
    # a [1, 1] interval and never NaN), each with a footnote, and a notice says why.
    r <- erv_run(erv_perfect(), "y", "x", "pos")
    tb <- r$results$aucSummary
    expect_equal(erv_cell(tb, "x", "auc"), 1)
    for (col in c("auc_lower", "auc_upper", "std_error")) {
        cell <- tb$getCell(col, rowKey = "x")
        expect_identical(cell$value, "", label = col)
        expect_gte(length(cell$footnotes), 1)
    }
    expect_match(erv_notices(r), "No AUC Interval: x", fixed = TRUE)
    expect_match(erv_notices(r), "AUC = 1", fixed = TRUE)
})

test_that("C27/C32 one bootstrap comparison gives one p-value (VAL-enhancedROC-05)", {
    r <- erv_run(erv_two(), "y", c("A", "B"), "pos", analysisType = "comparative", pairwiseComparisons = TRUE, statisticalComparison = TRUE,
                 comparisonMethod = "bootstrap", bootstrapSamples = 200, seed = 9)
    expect_equal(erv_cell(r$results$rocComparisons, "A_vs_B", "p_value"), erv_cell(r$results$statisticalSummary, "A_B_stat", "p_value"))
})

test_that("C56 the AUC definition sentence holds for Direction = lower (VAL-enhancedROC-06)", {
    d <- erv_d1(); d$marker <- -d$marker
    r <- erv_run(d, "status", "marker", "Malignant", direction = "lower")
    expect_false(grepl("scores above a randomly chosen non-case", r$results$clinicalReport$content, fixed = TRUE))
    # the old phrase is gone everywhere, so also pin the sentence that replaced it: P(case LOWER) = the AUC 0.825
    expect_match(r$results$clinicalReport$content, "randomly chosen case has a LOWER value than a randomly chosen non-case", fixed = TRUE)
    expect_false(grepl("has a HIGHER value", r$results$clinicalReport$content, fixed = TRUE))
})

test_that("C05 interpretation bands include their lower edge (VAL-enhancedROC-07)", {
    band <- function(k) { hi <- k %/% 10; rem <- k %% 10
        data.frame(y = factor(rep(c("n", "p"), each = 10), levels = c("n", "p")),
                   x = c(1:10, rep(11, hi), if (rem > 0) rem + 0.5, rep(0, 10 - hi - (rem > 0)))) }
    expect_equal(erv_cell(erv_run(band(80), "y", "x", "p")$results$aucSummary, "x", "auc_interpretation"), "Good")
    expect_equal(erv_cell(erv_run(band(70), "y", "x", "p")$results$aucSummary, "x", "auc_interpretation"), "Fair")
})

test_that("C08 tied Youden maxima are disclosed (VAL-enhancedROC-08)", {
    set.seed(21)
    d <- data.frame(y = factor(rep(c("neg", "pos"), c(30, 25)), levels = c("neg", "pos")),
                    s = c(sample(1:5, 30, TRUE, prob = c(.35, .3, .2, .1, .05)), sample(1:5, 25, TRUE, prob = c(.05, .15, .2, .3, .3))))
    expect_match(erv_notices(erv_run(d, "y", "s", "pos")), "Tied", fixed = TRUE)
})

test_that("C68 a non-significant Hosmer-Lemeshow test is not reported as 'Good fit' (VAL-enhancedROC-09)", {
    tb <- erv_run(erv_prob(), "y", "p", "event", calibrationAnalysis = TRUE, hosmerLemeshow = TRUE)$results$hosmerLemeshowTable
    expect_false(identical(erv_cell(tb, "p", "conclusion"), "Good fit"))
    expect_match(erv_cell(tb, "p", "conclusion"), "No evidence of lack of fit", fixed = TRUE)   # absence of evidence, said so
})

test_that("C50 the Methods text names the cut-point criterion actually used (VAL-enhancedROC-10)", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", comprehensive_output = TRUE, youdenOptimization = FALSE)
    expect_false(grepl("maximizing the Youden Index", r$results$methodsExplanation$content, fixed = TRUE))
    # and it names the criterion that ran
    expect_match(r$results$methodsExplanation$content, "top-left corner", fixed = TRUE)
    expect_false(grepl("maximise the Youden index", r$results$methodsExplanation$content, fixed = TRUE))
})

test_that("C107/C108 the impact plots draw something at the default single cutoff (VAL-enhancedROC-11)", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", clinicalImpact = TRUE, clinicalUtilityCurve = TRUE, impactCutoffs = "7.5")
    r$results$clinicalUtilityPlot$saveAs(tempfile(fileext = ".png"))
    ld <- ggplot2::layer_data(ggplot2::last_plot(), 1)
    expect_true(all(table(ld$group) >= 2))
})

test_that("C115 a [0,1] score declared as a score is not graded as a probability (VAL-enhancedROC-12)", {
    d <- erv_d1(); d$marker <- d$marker / 20
    tb <- erv_run(d, "status", "marker", "Malignant", calibrationAnalysis = TRUE, eoRatio = TRUE, predictionScale = "score")$results$calibrationSummary
    expect_true(is.na(erv_cell(tb, "marker", "eo_ratio")))
})

test_that("C86 optimism correction starts from the fitted model's apparent AUC (VAL-enhancedROC-13)", {
    d <- erv_cont()
    r <- erv_run(d, "y", "m1", "pos", internalValidation = TRUE, bootstrapSamples = 200, seed = 5, direction = "lower")
    corr <- as.numeric(sub(".*Corrected AUC = ?(-?[0-9.]+).*", "\\1", r$results$analysisSummary$content))
    expect_gt(corr, 0.5)
})

test_that("C113 ties count one half and a note gives the bounds for how they are resolved (VAL-enhancedROC-14)", {
    # The Upper / Lower tie options only raised a notice and left the AUC unchanged, so they were retired.
    # The AUC keeps the usual half credit and an automatic note gives the bounds instead. D1 has 5 tied
    # case-control pairs of 100: AUC 0.825; every tie to the cases 0.850, every tie to the non-cases 0.800.
    expect_false("tiedScoreHandling" %in% names(formals(ClinicoPath::enhancedROC)))
    tb <- erv_run(erv_d1(), "status", "marker", "Malignant")$results$aucSummary
    expect_equal(erv_cell(tb, "marker", "auc"), 0.825, tolerance = 1e-12)
    note <- tb$notes[["ties"]]$note
    expect_match(note, "marker: 5% of case-control pairs are tied", fixed = TRUE)
    expect_match(note, "the AUC would be 0.850", fixed = TRUE)
    expect_match(note, "favoured the non-cases 0.800", fixed = TRUE)
    expect_match(note, "not a confidence interval", fixed = TRUE)
    # Direction lower reads the same ties from the other side: AUC 0.175, bounds 0.200 and 0.150.
    lo <- erv_run(erv_d1(), "status", "marker", "Malignant", direction = "lower")$results$aucSummary
    expect_equal(erv_cell(lo, "marker", "auc"), 0.175, tolerance = 1e-12)
    expect_match(lo$notes[["ties"]]$note, "the AUC would be 0.200, if every tie favoured the non-cases 0.150", fixed = TRUE)
    # no tied pair, no note
    expect_null(erv_run(erv_cont(), "y", "m1", "pos")$results$aucSummary$notes[["ties"]])
})

test_that("C93 an explicit NA outcome level is excluded, not counted negative (VAL-enhancedROC-15)", {
    # Retain 20 complete rows under the analysis minimum; add two explicit NA levels.
    d <- rbind(erv_d1(), erv_d1()[19:20, ])
    d$status <- addNA(factor(c(as.character(d$status[1:20]), NA, NA), levels = c("Benign", "Malignant")))
    r <- erv_run(d, "status", "marker", "Malignant")
    auc <- mean(erv_psi(d$marker[1:20][d$status[1:20] == "Malignant"], d$marker[1:20][d$status[1:20] == "Benign"]))
    expect_equal(erv_cell(r$results$aucSummary, "marker", "auc"), auc)
})

test_that("C79 the decision-impact cutoff column is not a percentage 'Risk Threshold' (VAL-enhancedROC-16)", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", clinicalImpact = TRUE, decisionImpactTable = TRUE, impactCutoffs = "7.5")
    col <- r$results$decisionImpactSummary$getColumn("threshold")
    expect_false(identical(col$format, "pc") && erv_cell(r$results$decisionImpactSummary, "marker_7.5", "threshold") > 1)
})

test_that("C42 hull point count includes the (0,0) vertex (VAL-enhancedROC-17)", {
    tb <- erv_run(erv_d1(), "status", "marker", "Malignant", convexHull = TRUE)$results$convexHullTable
    expect_equal(erv_cell(tb, "marker", "n_hull_points"), 6)
})

test_that("C29 both comparison tables use one sign convention for an AUC difference (VAL-enhancedROC-18)", {
    r <- erv_run(erv_two(), "y", c("A", "B"), "pos", analysisType = "comparative", pairwiseComparisons = TRUE, showMetricsDiff = TRUE)
    dc <- r$results$detailedComparison$asDF
    expect_equal(dc$difference[dc$metric == "AUC ( A vs B )"], erv_cell(r$results$rocComparisons, "A_vs_B", "auc_difference"))
})

test_that("C45 imbalance severity and recommendation agree (VAL-enhancedROC-19)", {
    set.seed(1)
    d <- data.frame(y = factor(c(rep("neg", 55), rep("pos", 10)), levels = c("neg", "pos")), x = c(rnorm(55), rnorm(10, 1.5)))
    tb <- erv_run(d, "y", "x", "pos", detectImbalance = TRUE, imbalanceThreshold = 6)$results$imbalanceMetrics
    expect_false(identical(erv_cell(tb, "1", "imbalance_severity"), "High imbalance") &&
                 identical(erv_cell(tb, "1", "recommendation"), "ROC analysis is appropriate"))
})

test_that("C73 empty multi-class CI columns are explained (VAL-enhancedROC-20)", {
    set.seed(20261011)
    d <- data.frame(cls = factor(rep(c("Low", "Mid", "High"), c(30, 25, 20)), levels = c("Low", "Mid", "High")),
                    m = c(rnorm(30, 0), rnorm(25, 0.8), rnorm(20, 1.5)))
    tb <- erv_run(d, "cls", "m", NULL, multiClassROC = TRUE)$results$multiClassAUC
    expect_true(any(!is.na(tb$asDF$auc_lower)) || any(grepl("interval|bootstrap", unlist(tb$notes), ignore.case = TRUE)))
})

test_that("C40 the CROC early-retrieval reading can say something other than 'Limited' (VAL-enhancedROC-21)", {
    r <- erv_run(erv_perfect(), "y", "x", "pos", crocAnalysis = TRUE)
    expect_false(identical(erv_cell(r$results$crocAnalysisTable, "x", "interpretation"), "Limited early retrieval advantage"))
    # the gain is now measured from a random ranking (CROC AUC 1/7 - 1/(e^7 - 1)) to a perfect one
    expect_equal(erv_cell(r$results$crocAnalysisTable, "x", "random_croc"), 1 / 7 - 1 / (exp(7) - 1), tolerance = 1e-10)
    expect_identical(erv_cell(r$results$crocAnalysisTable, "x", "interpretation"), "100% of the way from a random ranking to a perfect one (alpha 7)")
})

test_that("C32 Venkatraman's whole-curve test is not labelled an AUC comparison (VAL-enhancedROC-22)", {
    skip_if_not_installed("pROC")
    r <- erv_run(pROC::aSAH, "outcome", c("s100b", "ndka"), "Poor", analysisType = "comparative", statisticalComparison = TRUE,
                 comparisonMethod = "venkatraman", bootstrapSamples = 200, seed = 3)
    expect_false(grepl("AUC Comparison", erv_cell(r$results$statisticalSummary, "s100b_ndka_stat", "test_name"), fixed = TRUE))
})

test_that("C31 a relative change alone is not called 'Clinically significant' (VAL-enhancedROC-23)", {
    skip_if_not_installed("pROC")
    r <- erv_run(pROC::aSAH, "outcome", c("s100b", "ndka"), "Poor", analysisType = "comparative", showMetricsDiff = TRUE)
    expect_false(any(r$results$detailedComparison$asDF$interpretation == "Clinically significant difference"))
})

test_that("C06 'AUC in Context' quotes no unsourced reference level (VAL-enhancedROC-24)", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", clinicalContext = "screening")
    expect_false(grepl("conventionally quoted for screening", erv_cell(r$results$aucSummary, "marker", "clinical_utility"), fixed = TRUE))
})

# ---- Outcome coding table: which level is positive, and why ----------------------------------------
# The positive class is never taken from level order. Only a NUMERIC outcome coded 0/1 defaults (1 is
# positive); any other outcome waits for a choice. The table shows every level, its N and its role.
erv_roles <- function(res) { df <- res$results$outcomeCoding$asDF; setNames(df$role, df$level) }
erv_tnotes <- function(tab) vapply(tab$notes, function(z) z$note, "")
# A note with a leftover {placeholder} was never substituted; a " [" makes the translator drop the rest.
erv_clean_notes <- function(notes, label) {
    expect_false(any(grepl("{", notes, fixed = TRUE)), label = paste(label, "has a {placeholder}"))
    expect_false(any(grepl(" [", notes, fixed = TRUE)), label = paste(label, "has a ' ['"))
}
erv_all_notes <- function(item) {
    if (inherits(item, "Table")) return(erv_tnotes(item))
    if (inherits(item, "Array")) return(unlist(lapply(item$items, erv_all_notes)))
    if (inherits(item, "Group")) return(unlist(lapply(item$itemNames, function(nm) erv_all_notes(item$get(nm)))))
    character(0)
}

test_that("Outcome coding: a binary outcome names its positive and its negative level", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant")
    oc <- r$results$outcomeCoding
    expect_equal(erv_roles(r), c(Benign = "Negative", Malignant = "Positive (condition present)"))
    expect_equal(c(erv_cell(oc, "lvl:Benign", "n"), erv_cell(oc, "lvl:Malignant", "n")), c(10, 10))
    n <- erv_tnotes(oc)
    expect_match(n[["positive"]], "Positive class: Malignant.", fixed = TRUE)
    expect_match(n[["positive"]], "specificity and NPV refer to Benign", fixed = TRUE)
    expect_identical(n[["source"]], "Chosen in Positive class.")
    expect_match(n[["n"]], "(20 of 20 rows)", fixed = TRUE)
    expect_match(n[["reading"]], "HIGHER values of marker were taken to indicate Malignant", fixed = TRUE)
    erv_clean_notes(n, "outcomeCoding")
})

test_that("Outcome coding: with three levels the others are pooled and the AUC is positive-vs-rest", {
    d <- erv_d1()
    d$status <- factor(c(rep("Benign", 7), rep("Atypia", 3), rep("Malignant", 10)), levels = c("Benign", "Atypia", "Malignant"))
    r <- erv_run(d, "status", "marker", "Malignant")
    pooled <- "Negative (pooled with the other non-positive levels)"
    expect_equal(erv_roles(r), c(Benign = pooled, Atypia = pooled, Malignant = "Positive (condition present)"))
    expect_equal(c(erv_cell(r$results$outcomeCoding, "lvl:Benign", "n"), erv_cell(r$results$outcomeCoding, "lvl:Atypia", "n")), c(7, 3))
    n <- erv_tnotes(r$results$outcomeCoding)
    expect_match(n[["positive"]], "refer to the other levels combined (Benign, Atypia)", fixed = TRUE)
    expect_equal(erv_cell(r$results$aucSummary, "marker", "auc"),
                 mean(erv_psi(d$marker[d$status == "Malignant"], d$marker[d$status != "Malignant"])), tolerance = 1e-12)
    erv_clean_notes(n, "outcomeCoding")
})

test_that("Outcome coding: a level literally named 'Other' can be the positive class", {
    # The pooled negative side used to be labelled 'Other', so choosing a level called Other produced
    # two levels with one name. It is now 'Not Other' in that case.
    d <- erv_d1()
    d$status <- factor(c(rep("Benign", 7), rep("Other", 3), rep("Malignant", 10)), levels = c("Benign", "Other", "Malignant"))
    for (pos in c("Other", "Malignant")) {
        r <- erv_run(d, "status", "marker", pos)
        expect_equal(erv_cell(r$results$aucSummary, "marker", "auc"),
                     mean(erv_psi(d$marker[d$status == pos], d$marker[d$status != pos])), tolerance = 1e-12, label = pos)
        expect_identical(unname(erv_roles(r)[pos]), "Positive (condition present)")
        expect_equal(sum(erv_roles(r) == "Negative (pooled with the other non-positive levels)"), 2)
    }
})

test_that("Outcome coding: an explicit NA level is a '(missing)' row, not a category", {
    d <- erv_d1(); d$status <- addNA(factor(c(as.character(d$status[1:18]), NA, NA), levels = c("Benign", "Malignant")))
    r <- erv_run(d, "status", "marker", "Malignant")
    oc <- r$results$outcomeCoding
    expect_equal(c(erv_cell(oc, "lvl:Benign", "n"), erv_cell(oc, "lvl:Malignant", "n"), erv_cell(oc, "missing", "n")), c(10, 8, 2))
    expect_identical(erv_cell(oc, "missing", "role"), "Excluded: missing outcome")
    expect_match(erv_tnotes(oc)[["n"]], "(18 of 20 rows)", fixed = TRUE)
    expect_match(erv_notices(r), "2 rows with a missing outcome were excluded", fixed = TRUE)
    erv_clean_notes(erv_tnotes(oc), "outcomeCoding")
})

test_that("Outcome coding: a numeric 0/1 outcome defaults to 1 and the source note says so", {
    d <- erv_d1(); d$status <- as.numeric(d$status == "Malignant")
    r <- erv_run(d, "status", "marker", NULL)
    expect_equal(erv_cell(r$results$aucSummary, "marker", "auc"), 0.825, tolerance = 1e-12)
    expect_equal(erv_roles(r), c(`0` = "Negative", `1` = "Positive (condition present)"))
    n <- erv_tnotes(r$results$outcomeCoding)
    expect_match(n[["source"]], "coded 0/1, so 1 is read as the positive class", fixed = TRUE)
    erv_clean_notes(n, "outcomeCoding")
})

test_that("Outcome coding: a numeric outcome coded other than 0/1 waits for a choice", {
    d <- erv_d1(); d$status <- as.numeric(d$status == "Malignant") + 1
    r <- erv_run(d, "status", "marker", NULL)
    expect_equal(r$results$aucSummary$rowCount, 0)
    expect_equal(unname(erv_roles(r)), rep("Not assigned yet", 2))
    n <- erv_tnotes(r$results$outcomeCoding)
    expect_match(n[["unset"]], "numeric with values 1, 2", fixed = TRUE)
    expect_match(n[["unset"]], "Data > Setup", fixed = TRUE)
    expect_match(erv_notices(r), "Choose the positive class", fixed = TRUE)
    expect_false(grepl("rgba(220, 38, 38, 0.10)", erv_notices(r), fixed = TRUE))   # INFO, not an ERROR
    erv_clean_notes(n, "outcomeCoding")
    # once the level is chosen it is the ordinary analysis
    expect_equal(erv_cell(erv_run(d, "status", "marker", "2")$results$aucSummary, "marker", "auc"), 0.825, tolerance = 1e-12)
})

test_that("no table note carries a {placeholder} or a ' [', even for level names with brackets", {
    d <- erv_d1(); levels(d$status) <- c("Benign [b]", "Mal <x> [m]")
    rb <- erv_run(d, "status", "marker", "Mal <x> [m]")
    expect_equal(erv_cell(rb$results$aucSummary, "marker", "auc"), 0.825, tolerance = 1e-12)
    expect_match(erv_tnotes(rb$results$outcomeCoding)[["positive"]], "Positive class: Mal < x> (m)", fixed = TRUE)
    rich <- erv_run(erv_d1(), "status", "marker", "Malignant", cutoffTable = TRUE, clinicalMetrics = TRUE, partialAuc = TRUE,
                    crocAnalysis = TRUE, convexHull = TRUE, detectImbalance = TRUE, clinicalImpact = TRUE, decisionImpactTable = TRUE,
                    impactCutoffs = "7.5", calibrationAnalysis = TRUE, hosmerLemeshow = TRUE, brierScore = TRUE, calibrationMetrics = TRUE)
    for (res in list(rb, rich)) {
        n <- erv_all_notes(res$results)
        expect_gt(length(n), 5)
        erv_clean_notes(n, "a table note")
    }
})

# ---- other behaviour the fixes changed by design ------------------------------------------------------
erv_binorm <- function(n1, n0, seed) {          # binormal marker, separation 1.2
    set.seed(seed)
    data.frame(y = factor(c(rep("neg", n0), rep("pos", n1)), levels = c("neg", "pos")), x = round(c(rnorm(n0), rnorm(n1, 1.2)), 6))
}

test_that("VAL-26 a DeLong interval shown for a small outcome group carries a small-sample warning", {
    guard <- "DeLong confidence intervals may be too narrow"
    small <- erv_notices(erv_run(erv_binorm(15, 15, 1), "y", "x", "pos"))
    expect_match(small, guard, fixed = TRUE)
    expect_match(small, "The smaller outcome group has 15 patients (15 positive, 15 negative)", fixed = TRUE)
    expect_false(grepl(guard, erv_notices(erv_run(erv_binorm(40, 40, 2), "y", "x", "pos")), fixed = TRUE))
    # no DeLong interval on screen, no DeLong warning
    expect_false(grepl(guard, erv_notices(erv_run(erv_binorm(15, 15, 1), "y", "x", "pos", useBootstrap = TRUE, bootstrapSamples = 200)), fixed = TRUE))
    expect_false(grepl(guard, erv_notices(erv_run(erv_perfect(), "y", "x", "pos")), fixed = TRUE))
})

test_that("a degenerate AUC interval is never quoted: blank bootstrap limits, 'interval not estimable' in the report", {
    # Perfect separation under the bootstrap: every resample has AUC 1, so boot.ci forms no interval.
    invisible(utils::capture.output(r <- erv_run(erv_perfect(), "y", "x", "pos", useBootstrap = TRUE, bootstrapSamples = 200, seed = 1)))
    for (col in c("auc_lower", "auc_upper")) {
        cell <- r$results$aucSummary$getCell(col, rowKey = "x")
        expect_identical(cell$value, "", label = col)
        expect_match(paste(unlist(cell$footnotes), collapse = " "), "Every bootstrap resample gave the same AUC", fixed = TRUE)
    }
    report <- erv_run(erv_perfect(), "y", "x", "pos")$results$clinicalReport$content
    expect_match(report, "(interval not estimable)", fixed = TRUE)
    expect_false(grepl("1--1", report, fixed = TRUE))
    # A constant marker: AUC 0.5 by construction, blank limits, and the J <= 0 cut-point is explained.
    d <- erv_d1(); d$marker <- 5
    rk <- erv_run(d, "status", "marker", "Malignant")
    expect_equal(erv_cell(rk$results$aucSummary, "marker", "auc"), 0.5)
    expect_true(is.na(erv_cell(rk$results$aucSummary, "marker", "auc_lower")))
    chance <- rk$results$optimalCutoffSummary$notes[["chance"]]$note
    expect_match(chance, "J <= 0", fixed = TRUE)
    expect_match(chance, "No threshold separates the groups better than chance", fixed = TRUE)
    expect_match(chance, "everyone is classified the same way", fixed = TRUE)
})

test_that("every Detailed Model Comparison row is value 1 - value 2, relative to value 2", {
    skip_if_not_installed("pROC")
    dc <- erv_run(pROC::aSAH, "outcome", c("s100b", "ndka"), "Poor", analysisType = "comparative", showMetricsDiff = TRUE)$results$detailedComparison$asDF
    expect_equal(nrow(dc), 4)
    expect_equal(dc$difference, dc$model1_value - dc$model2_value, tolerance = 1e-12)
    expect_equal(dc$percent_change, 100 * (dc$model1_value - dc$model2_value) / dc$model2_value, tolerance = 1e-10)
})

test_that("McNemar is computed when one marker calls every case positive (the 2 x 2 keeps its empty cells)", {
    # P separates perfectly (cut-point 30.5: all 30 cases positive); Q calls 15 of the 30 cases positive.
    # Discordant cases: b = 15 (P+, Q-), c = 0, so McNemar's chi-square (b - c)^2 / (b + c) = 15 on 1 df.
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 30), levels = c("neg", "pos")),
                    P = c(1:30, 31:60), Q = c(1:30, rep(c(10, 40), each = 15)))
    dc <- erv_run(d, "y", c("P", "Q"), "pos", analysisType = "comparative", showMetricsDiff = TRUE)$results$detailedComparison
    expect_equal(erv_cell(dc, "P_Q_Sensitivity", "p_value"), stats::pchisq(15, 1, lower.tail = FALSE), tolerance = 1e-12)
})

# ---- second-round validation fixes: one regression test per fix ------------------------------------
# Each test below fails on R/enhancedROC.b.R as it was before these fixes (checked against a saved copy).
erv_private <- function(data, ...) {               # the analysis object, for the helpers checked directly
    o <- do.call(ClinicoPath:::enhancedROCOptions$new, list(...))
    a <- ClinicoPath:::enhancedROCClass$new(options = o, data = data)
    list(a = a, p = a$.__enclos_env__$private)
}
erv_footnotes <- function(tab, col, key) paste(unlist(tab$getCell(col, rowKey = key)$footnotes), collapse = " ")

test_that("VAL-26 the DeLong warning also covers a high AUC in a larger group, and names the predictor", {
    # 40 vs 40 at AUC 0.96: each group has more than 30 patients, but n_min * (1 - AUC) = 1.6, far below
    # the product of about 6 at which the module's simulation found 93% coverage.
    guard <- "DeLong confidence intervals may be too narrow"
    set.seed(5)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 40), levels = c("neg", "pos")), m = c(rnorm(40), rnorm(40, 2.33)))
    auc <- mean(erv_psi(d$m[d$y == "pos"], d$m[d$y == "neg"]))
    expect_lt(40 * (1 - auc), 6)
    txt <- erv_notices(erv_run(d, "y", "m", "pos"))
    expect_match(txt, guard, fixed = TRUE)
    expect_match(txt, sprintf("affected: m (AUC %.3f)", auc), fixed = TRUE)
    # two predictors on 50 vs 50: only the one at risk is named
    set.seed(13)
    d2 <- data.frame(y = factor(rep(c("neg", "pos"), each = 50), levels = c("neg", "pos")),
                     hi = c(rnorm(50), rnorm(50, 2.6)), lo = c(rnorm(50), rnorm(50, 1.0)))
    txt2 <- erv_notices(erv_run(d2, "y", c("hi", "lo"), "pos"))
    expect_match(txt2, "affected: hi (AUC", fixed = TRUE)
    expect_false(grepl("lo (AUC", txt2, fixed = TRUE))
})

test_that("Venkatraman's test compares two curves as they are read, also when their directions differ", {
    # B is A reflected plus noise: read 'lower' it is nearly A's curve. pROC's paired statistic ranks the raw
    # values as if higher meant case, so it compared A with B's mirror image (p = 0). Read as the module reads
    # them, the p-value equals the one for B negated (the same curve read 'higher'), and is not significant.
    d <- erv_two()
    set.seed(3)
    d$B <- -d$A + rnorm(120, 0, 0.05)
    p_vk <- function(dd) erv_cell(erv_run(dd, "y", c("A", "B"), "pos", direction = "auto", analysisType = "comparative",
                                          pairwiseComparisons = TRUE, comparisonMethod = "venkatraman",
                                          bootstrapSamples = 200, seed = 3)$results$rocComparisons, "A_vs_B", "p_value")
    p_mixed <- p_vk(d)
    d2 <- d
    d2$B <- -d$B
    expect_equal(p_mixed, p_vk(d2))
    expect_gt(p_mixed, 0.05)
})

test_that("calibration slope: no reading under (quasi-)separation, and a slope of 1.100 is inside the band", {
    cal <- function(p, y) {
        d <- data.frame(y = factor(ifelse(y == 1, "event", "none"), levels = c("none", "event")), p = p)
        erv_run(d, "y", "p", "event", predictionScale = "probability", calibrationAnalysis = TRUE,
                calibrationMetrics = TRUE)$results$calibrationSummary
    }
    # risk 0.1 for every non-event and 0.9 for every event: the outcomes do not overlap on the fitted logits,
    # so the slope MLE is infinite, although glm converges to a finite slope (12.09)
    sep <- cal(rep(c(0.1, 0.9), each = 30), rep(c(0, 1), each = 30))
    expect_match(sep$getCell("interpretation", rowKey = "p")$value, "^No reading")
    expect_identical(sep$getCell("calibration_slope", rowKey = "p")$value, "")
    # quasi-complete separation: events and non-events share only the 0.5 level
    quasi <- cal(c(rep(0.2, 29), 0.5, 0.5, rep(0.8, 29)), c(rep(0, 29), 0, 1, rep(1, 29)))
    expect_match(quasi$getCell("interpretation", rowKey = "p")$value, "^No reading")
    # overlapping outcomes keep their slope: 0/1 risks sit at logits -10 / 10 with 2/30 and 28/30 events
    ov <- cal(c(rep(0, 30), rep(1, 30)), c(rep(0, 28), rep(1, 2), rep(0, 2), rep(1, 28)))
    expect_equal(unname(ov$getCell("calibration_slope", rowKey = "p")$value), qlogis(28 / 30) / 10, tolerance = 1e-6)
    # a slope of 1.100143 is 1.1 at 3 dp, inside [0.9, 1.1] (abs(1.1 - 1) <= 0.1 is FALSE in floating point)
    set.seed(1552)
    p <- round(rbeta(400, 2, 3), 4)
    y <- rbinom(400, 1, plogis(1.1 * qlogis(p)))
    edge <- cal(p, y)
    expect_equal(unname(round(edge$getCell("calibration_slope", rowKey = "p")$value, 3)), 1.1)
    expect_match(edge$getCell("interpretation", rowKey = "p")$value, "^Close to 1")
})

test_that("LR+ is not estimable (0/0), not infinite, at a cutoff that calls nobody positive", {
    # m = 1..30 read 'lower' while the cases are HIGHER: the best cutoff calls every patient negative
    # (TP = FP = 0), so LR+ = 0/0 and LR- = (1 - 0) / 1 = 1.
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos")), m = 1:30)
    tb <- erv_run(d, "y", "m", "pos", clinicalMetrics = TRUE, direction = "lower")$results$clinicalApplicationMetrics
    txt <- erv_cell(tb, "m", "clinical_interpretation")
    expect_match(txt, "LR+ not estimable.", fixed = TRUE)
    expect_false(grepl("infinite", txt, fixed = TRUE))
    expect_identical(tb$getCell("lr_positive", rowKey = "m")$value, "")
    expect_match(erv_footnotes(tb, "lr_positive", "m"), "Not estimable (0/0)", fixed = TRUE)
    expect_equal(erv_cell(tb, "m", "lr_negative"), 1)
})

test_that("likelihood-ratio bands: exactly 10 and 0.1 are moderate (Jaeschke 1994), read at 3 significant figures", {
    lr <- function(neg, pos) erv_run(data.frame(y = factor(rep(c("neg", "pos"), c(length(neg), length(pos))), levels = c("neg", "pos")),
                                                m = c(neg, pos)), "y", "m", "pos", clinicalMetrics = TRUE)$results$clinicalApplicationMetrics$asDF
    t10 <- lr(c(1:19, 30), c(25:34, -10:-1))                              # Se 0.5, Sp 0.95: LR+ = 10
    expect_equal(t10$lr_positive, 10, tolerance = 1e-9)
    expect_match(t10$clinical_interpretation, "LR+ 5 to 10: moderate", fixed = TRUE)
    t01 <- lr(c(1:10, seq(13, 31, 2)), c(-5, 11, 12, seq(14, 30, 2), 32:39))   # Se 0.95, Sp 0.5: LR- = 0.1
    expect_equal(t01$lr_negative, 0.1, tolerance = 1e-9)
    expect_match(t01$clinical_interpretation, "LR- 0.1 to 0.2: moderate", fixed = TRUE)
    # 0.104 is shown as 0.104: moderate, where round(x, 2) = 0.10 called it large
    p <- erv_private(erv_d1(), outcome = "status", positiveClass = "Malignant", predictors = "marker")$p
    expect_match(p$.interpretClinicalMetrics(0.5, 0.9, 2, 0.104, "general"), "LR- 0.1 to 0.2: moderate", fixed = TRUE)
})

test_that("'AUC in Context' and the summary say that ties count one half", {
    # D1 has 5 tied case-control pairs in 100: a case is strictly higher 80% of the time; 82.5% with ties as one half.
    r <- erv_run(erv_d1(), "status", "marker", "Malignant")
    expect_identical(erv_cell(r$results$aucSummary, "marker", "clinical_utility"),
                     "A random case has a higher value than a random non-case 82% of the time (ties count one half)")
    expect_match(r$results$analysisSummary$content, "of the time (ties count one half)", fixed = TRUE)
})

test_that("the CROC plot draws the random-ranking reference in CROC coordinates", {
    # A random ranking is the curve (f(s), s) after the magnifier, below y = x everywhere: against the diagonal
    # alone a CROC curve well above random was drawn mostly under 'chance'. It is the last layer.
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    r <- erv_run(erv_cont(), "y", "m1", "pos", crocAnalysis = TRUE)
    r$results$crocCurvePlot$saveAs(tempfile(fileext = ".png"))
    p <- ggplot2::last_plot()
    ref <- ggplot2::layer_data(p, length(p$layers))
    s <- seq(0, 1, length.out = 201)
    expect_equal(ref$x, (1 - exp(-7 * s)) / (1 - exp(-7)), tolerance = 1e-12)   # crocAlpha 7
    expect_equal(ref$y, s, tolerance = 1e-12)
    expect_match(p$labels$caption, sprintf("CROC area %.3f", 1 / 7 - 1 / (exp(7) - 1)), fixed = TRUE)
})

test_that("a multi-class AUC with a one-patient class says why its interval is blank", {
    # DeLong's variance needs two patients in each group; this is not a zero-variance (separated) case.
    set.seed(11)
    d <- data.frame(g = factor(c(rep("A", 30), rep("B", 25), "C"), levels = c("A", "B", "C")), m = c(rnorm(30), rnorm(25, 1), 0.3))
    tb <- erv_run(d, "g", "m", NULL, multiClassROC = TRUE)$results$multiClassAUC
    expect_identical(tb$getCell("auc_lower", rowKey = "m_C")$value, "")
    fn <- erv_footnotes(tb, "auc_lower", "m_C")
    expect_match(fn, "One of the two groups has a single patient", fixed = TRUE)
    expect_false(grepl("perfectly separated", fn, fixed = TRUE))
})

test_that("partial-AUC bootstrap limits that cannot be formed are blank with a footnote", {
    # perfect separation: every resample has the same partial AUC, so boot.ci forms no interval
    invisible(utils::capture.output(r <- erv_run(erv_perfect(), "y", "x", "pos", partialAuc = TRUE, bootstrapPartialAUC = TRUE,
                                                 partialRange = "0.8,1.0", bootstrapSamples = 200, seed = 1)))
    tb <- r$results$partialAucAnalysis
    for (col in c("partial_auc_lower", "partial_auc_upper")) {
        expect_identical(tb$getCell(col, rowKey = "x")$value, "", label = col)
        expect_match(erv_footnotes(tb, col, "x"), "No bootstrap interval could be formed for this partial AUC", fixed = TRUE)
    }
    # no bootstrap requested: the hidden columns carry no footnote
    off <- erv_run(erv_perfect(), "y", "x", "pos", partialAuc = TRUE, partialRange = "0.8,1.0")$results$partialAucAnalysis
    expect_identical(erv_footnotes(off, "partial_auc_lower", "x"), "")
})

test_that("a marker read backwards: check Positive class first; Direction re-estimates, nothing is 'swapped'", {
    # Healthy is named as the positive class while the marker is higher in Disease: AUC 0.139, CI below 0.5.
    set.seed(1)
    d <- data.frame(y = factor(rep(c("Healthy", "Disease"), each = 30), levels = c("Healthy", "Disease")), m = c(rnorm(30), rnorm(30, 1.2)))
    r <- erv_run(d, "y", "m", "Healthy", direction = "higher")
    txt <- erv_notices(r)
    expect_match(txt, "Marker Reads Backwards", fixed = TRUE)
    expect_match(txt, "First check Positive class: is Healthy the level that means the condition is present?", fixed = TRUE)
    expect_lt(regexpr("First check Positive class", txt, fixed = TRUE), regexpr("setting Direction to", txt, fixed = TRUE))
    expect_match(txt, "re-estimates the cut-point, sensitivity and specificity", fixed = TRUE)
    expect_false(grepl("swapped", txt, fixed = TRUE))
    expect_match(txt, "Check Positive class (is Healthy the level that means the condition is present?), then Direction", fixed = TRUE)
    expect_match(erv_tnotes(r$results$outcomeCoding)[["reading"]], "first check Positive class: is Healthy the level", fixed = TRUE)
    # below 0.5 with a CI that crosses it: Positive class, then Direction
    set.seed(2)
    d2 <- data.frame(y = factor(rep(c("neg", "pos"), each = 20)), m = rnorm(40))
    d2$m[d2$y == "pos"] <- d2$m[d2$y == "pos"] - 0.25
    bc <- erv_notices(erv_run(d2, "y", "m", "pos", direction = "higher"))
    expect_match(bc, "AUC Below Chance", fixed = TRUE)
    expect_match(bc, "first check Positive class (is pos the level that means the condition is present?), then Direction", fixed = TRUE)
    # at 0.5 <= AUC < 0.7, switching Direction would give 1 - AUC, below chance: no longer suggested
    set.seed(4)
    d3 <- data.frame(y = factor(rep(c("neg", "pos"), each = 40), levels = c("neg", "pos")), m = c(rnorm(40), rnorm(40, 0.5)))
    lim <- erv_notices(erv_run(d3, "y", "m", "pos"))
    expect_match(lim, "Limited Diagnostic Performance", fixed = TRUE)
    expect_false(grepl("Checking if direction setting is appropriate", lim, fixed = TRUE))
})

test_that("listwise deletion that removes a whole outcome level is named as such", {
    # Every Disease row lacks m2, so the complete cases are all Healthy. That was reported as an outcome
    # with only one value, with advice to choose another outcome.
    set.seed(4)
    d <- data.frame(y = factor(rep(c("Healthy", "Disease"), each = 30), levels = c("Healthy", "Disease")), m = rnorm(60), m2 = rnorm(60))
    d$m2[d$y == "Disease"] <- NA
    r <- erv_run(d, "y", c("m", "m2"), "Disease")
    txt <- erv_notices(r)
    expect_match(txt, "Outcome Level Lost to Missing Predictor Values", fixed = TRUE)
    expect_match(txt, "Outcome level Disease (30 rows)", fixed = TRUE)
    expect_match(txt, "m2 (30/30)", fixed = TRUE)
    expect_false(grepl("has only one value", txt, fixed = TRUE))
    expect_equal(r$results$aucSummary$rowCount, 0)
    expect_identical(erv_roles(r)[["Disease"]], "Positive (condition present)")   # the whole-column preview stays
})

test_that("the copy-ready report names the positive and the negative class", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant")
    expect_match(r$results$clinicalReport$content,
                 "The positive class (cases) was Malignant and the negative class (non-cases) was Benign.", fixed = TRUE)
    d <- erv_d1()
    d$status <- factor(c(rep("Benign", 7), rep("Atypia", 3), rep("Malignant", 10)), levels = c("Benign", "Atypia", "Malignant"))
    expect_match(erv_run(d, "status", "marker", "Malignant")$results$clinicalReport$content,
                 "The positive class (cases) was Malignant; the other levels (Benign, Atypia) were combined as the negative class (non-cases).",
                 fixed = TRUE)
})

test_that("'Direction Auto-Detected' names the positive level instead of calling it 'disease'", {
    txt <- erv_notices(erv_run(erv_d1(), "status", "marker", "Malignant", direction = "auto"))
    expect_match(txt, "higher values of marker were taken to indicate Malignant (the positive class).", fixed = TRUE)
    expect_false(grepl("(disease)", txt, fixed = TRUE))
})

test_that("a numeric outcome with more than two values gets one message and no roles; a measurement is summarised", {
    set.seed(8)
    d <- data.frame(y = sample(0:2, 60, TRUE), m = rnorm(60))
    for (pc in list(NULL, "1")) {
        r <- erv_run(d, "y", "m", pc, multiClassROC = TRUE)
        n <- erv_tnotes(r$results$outcomeCoding)
        expect_match(n[["unset"]], "numeric with values 0, 1, 2", fixed = TRUE)
        expect_false("positive" %in% names(n))
        expect_equal(unname(erv_roles(r)), rep("Not assigned yet", 3))
        # the ERROR gives the table note's advice, not a second one
        expect_match(erv_notices(r), "only a 0/1 outcome is read without a choice (1 = positive)", fixed = TRUE)
    }
    # a measurement chosen as the outcome: one summary row, not one row per value
    set.seed(3)
    dc <- data.frame(y = round(runif(60, 25, 85)), m = rnorm(60))
    oc <- erv_run(dc, "y", "m", NULL)$results$outcomeCoding
    expect_equal(oc$rowCount, 1)
    expect_identical(oc$asDF$level, sprintf("%d distinct values (%s to %s)", length(unique(dc$y)), min(dc$y), max(dc$y)))
    expect_match(erv_tnotes(oc)[["unset"]], "looks like a measurement", fixed = TRUE)
})

test_that("Outcome coding cells show a level name as it is; only notes map [] to ()", {
    # Cells are not re-translated, so they keep the real name; mapping them made 'A (1)' and 'A [1]' look alike.
    set.seed(9)
    d <- data.frame(y = factor(rep(c("A (1)", "A [1]"), each = 25), levels = c("A (1)", "A [1]")), m = c(rnorm(25), rnorm(25, 1)))
    r <- erv_run(d, "y", "m", "A [1]")
    expect_identical(erv_roles(r), c(`A (1)` = "Negative", `A [1]` = "Positive (condition present)"))
})

test_that("the narrative panels are emptied and hidden when a run stops before writing them", {
    # jamovi restores an Html item's previous content whatever its clearWith says. After the positive class
    # is unset, the previous report must not stay on screen under 'Choose the positive class'.
    d <- erv_d1()
    old <- erv_run(d, "status", "marker", "Malignant")$results$clinicalReport$content
    expect_true(nzchar(old))
    pr <- erv_private(d, outcome = "status", positiveClass = NULL, predictors = "marker")
    pr$a$init()
    pr$a$results$results$clinicalReport$fromProtoBuf(
        list(stale = FALSE, state = raw(), html = list(content = old, scripts = character(), stylesheets = character())),
        oChanges = "positiveClass", vChanges = character())
    suppressWarnings(pr$a$run())
    expect_identical(pr$a$results$results$clinicalReport$content, "")
    expect_false(isTRUE(pr$a$results$results$clinicalReport$visible))
    expect_match(as.character(pr$a$results$results$notices$content), "Choose the positive class", fixed = TRUE)
})

test_that("the AUC definition is one whole sentence, not a phrase spliced into 'The AUC is %s.'", {
    p <- erv_private(erv_d1(), outcome = "status", positiveClass = "Malignant", predictors = "marker")$p
    expect_identical(p$.aucMeaning(">"),
                     "The AUC is the probability that a randomly chosen case has a LOWER value than a randomly chosen non-case (ties count one half).")
    expect_identical(p$.aucMeaning("<", "A"),
                     "A: the AUC is the probability that a randomly chosen case has a HIGHER value than a randomly chosen non-case (ties count one half).")
    expect_match(erv_run(erv_d1(), "status", "marker", "Malignant")$results$clinicalReport$content,
                 "(ties count one half). It is computed over all cutpoints", fixed = TRUE)
})

test_that("tied Youden optima are found from exact counts (pROC rebuilds the counts with float noise)", {
    set.seed(20261011)
    cls <- factor(rep(c("Low", "Mid", "High"), c(30, 25, 20)), levels = c("Low", "Mid", "High"))
    d <- data.frame(cls = cls, m = round(c(rnorm(30, 0), rnorm(25, 0.8), rnorm(20, 1.5)), 6))
    # oracle in integers: J * n1 * n0 = TP * n0 + TN * n1 - n1 * n0 over the midpoint thresholds (x >= t positive)
    cs <- d$m[cls == "Mid"]; ct <- d$m[cls != "Mid"]
    u <- sort(unique(d$m)); th <- c(-Inf, (head(u, -1) + tail(u, -1)) / 2, Inf)
    Jn <- vapply(th, function(t) sum(cs >= t) * length(ct) + sum(ct < t) * length(cs) - length(cs) * length(ct), 0)
    n_tied <- sum(Jn == max(Jn))
    expect_equal(n_tied, 3)
    r <- erv_run(d, "cls", "m", "Mid")
    expect_match(erv_notices(r), sprintf("%d cutoffs for m reach the same highest Youden index (%.2f)", n_tied,
                                         max(Jn) / (length(cs) * length(ct))), fixed = TRUE)
})

test_that("the clinical guide and the instructions list the AUC bands with the edges the table uses", {
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", clinical_interpretation = TRUE)
    g <- r$results$clinicalInterpretationGuide$content
    ins <- r$results$instructions$content
    expect_match(g, "AUC from 0.80 to below 0.90", fixed = TRUE)
    expect_match(ins, "0.80 to below 0.90 = Good", fixed = TRUE)
    for (txt in c(g, ins)) expect_false(grepl("0.80-0.89", txt, fixed = TRUE))
})

# ---- third-round fixes (wording, citations, blank outcomes): one regression test per fix ----------------
# Each test below fails on R/enhancedROC.b.R and R/utils.R as they were before these fixes (checked against
# saved copies). The lines marked "guard" pass on both: they pin what must NOT be cited when nothing cites it.
erv_methods <- function(res) res$results$methodsExplanation$content

test_that("the Youden index weighs sensitivity and specificity equally, not a missed case and a false alarm", {
    # Maximising J minimises FNR + FPR (error RATES): per patient a missed case counts as (1 - p)/p false alarms.
    r <- erv_run(erv_d1(), "status", "marker", "Malignant", clinical_interpretation = TRUE)
    for (txt in c(r$results$clinicalInterpretationGuide$content, r$results$instructions$content)) {
        expect_match(txt, "weighs sensitivity and specificity equally; at prevalence p that counts one missed case as (1 - p)/p false alarms", fixed = TRUE)
        expect_false(grepl("equally costly", txt, fixed = TRUE))
    }
})

test_that("Methods: ties count one half after Hanley & McNeil (1982); Mann & Whitney (1947) give the U identity", {
    # Mann & Whitney assume continuous data (no ties); the half credit is Hanley & McNeil's (1982, p. 31).
    me <- erv_methods(erv_run(erv_d1(), "status", "marker", "Malignant", comprehensive_output = TRUE))
    expect_match(me, "with ties counted as one half (Hanley & McNeil, 1982); it equals the Mann-Whitney U statistic divided by the number of case/non-case pairs (Mann & Whitney, 1947).", fixed = TRUE)
    expect_match(me, "Hanley JA, McNeil BJ (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve.", fixed = TRUE)
})

test_that("the DeLong warning credits Feng et al. with diverging interval methods only, and Methods lists both papers", {
    small <- erv_run(erv_binorm(15, 15, 1), "y", "x", "pos", comprehensive_output = TRUE)
    expect_match(erv_notices(small), "Baumgartner (2017) found that interval methods for the AUC disagree most when the AUC is high and the sample small.", fixed = TRUE)
    me <- erv_methods(small)
    expect_match(me, "Obuchowski NA, Lieber ML (1998)", fixed = TRUE)
    expect_match(me, "Feng D, Cortese G, Baumgartner R (2017)", fixed = TRUE)
    # guard: no warning at 40 vs 40 (VAL-26 above), so neither paper is listed
    big <- erv_methods(erv_run(erv_binorm(40, 40, 2), "y", "x", "pos", comprehensive_output = TRUE))
    expect_false(grepl("Obuchowski", big, fixed = TRUE) || grepl("Feng D", big, fixed = TRUE))
})

test_that("a calibration slope above 1 cites Austin & Steyerberg for under-fitting, and Methods lists them", {
    # erv_prob(): true logit = 0.3 + 1.5 logit(p), so the supplied risks are too moderate (slope above 1).
    cal <- function(d) erv_run(d, "y", "p", "event", predictionScale = "probability", comprehensive_output = TRUE,
                               calibrationAnalysis = TRUE, calibrationMetrics = TRUE)
    r <- cal(erv_prob())
    expect_identical(erv_cell(r$results$calibrationSummary, "p", "interpretation"),
                     "Above 1: predictions too moderate (Van Calster et al. 2019), suggesting under-fitting (Austin & Steyerberg 2019)")
    expect_match(erv_methods(r), "Austin PC, Steyerberg EW (2019)", fixed = TRUE)   # cited by that cell; no spline curve
    # guard: risks made too extreme (slope below 1) - nothing cites Austin & Steyerberg, so they are not listed
    sharp <- erv_prob()
    sharp$p <- stats::plogis(3 * stats::qlogis(sharp$p))
    rs <- cal(sharp)
    expect_match(erv_cell(rs$results$calibrationSummary, "p", "interpretation"), "^Below 1")
    expect_false(grepl("Austin PC", erv_methods(rs), fixed = TRUE))
})

test_that("Methods attributes the calibration intercept and the spline curve as their sources define them", {
    # Van Calster's 'calibration intercept' IS calibration-in-the-large; Austin & Steyerberg fitted loess and
    # restricted cubic splines on the probability scale, not a natural spline of logit(p).
    me <- erv_methods(erv_run(erv_prob(), "y", "p", "event", predictionScale = "probability", comprehensive_output = TRUE,
                              calibrationAnalysis = TRUE, calibrationMetrics = TRUE, splineCalibration = TRUE, splineKnots = 4))
    expect_match(me, "the Intercept (slope model) column is that model's intercept; calibration-in-the-large is the intercept with the slope fixed at 1, which Van Calster et al. (2019) call the calibration intercept.", fixed = TRUE)
    expect_match(me, "gives ICI, E50, E90 and Emax as defined by Austin & Steyerberg (2019); it is fitted here as a natural cubic spline of the logit of the predicted risk with 4 knots, a variant of their loess and restricted cubic spline curves on the probability scale.", fixed = TRUE)
})

test_that("the convex hull subtitle says 'randomly mixing two cut-offs' and 'in-sample', not 'optimal'", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    r <- erv_run(erv_cont(), "y", "m1", "pos", convexHull = TRUE)
    r$results$convexHullPlot$saveAs(tempfile(fileext = ".png"))
    expect_identical(gsub("\n", " ", ggplot2::last_plot()$labels$subtitle, fixed = TRUE),
                     "Convex hull: operating points reachable by randomly mixing two cut-offs (Provost & Fawcett 2001); in-sample, so optimistic")
})

test_that("Methods lists Prentice & Pyke (1979) exactly when the decision curves shift the intercept", {
    dc <- function(...) erv_methods(erv_run(erv_cont(), "y", "m1", "pos", comprehensive_output = TRUE, clinicalImpact = TRUE,
                                            clinicalUtilityCurve = TRUE, ...))
    expect_match(dc(useObservedPrevalence = FALSE, prevalence = 0.1), "Prentice RL, Pyke R (1979)", fixed = TRUE)   # sample: 30 of 70
    expect_false(grepl("Prentice", dc(useObservedPrevalence = TRUE), fixed = TRUE))                                # guard
})

test_that("the direction note is one whole sentence per reading, emphasised with <i>, not <b>", {
    # The client's table-note renderer drops <b>; "HIGHER values of %s" used to be spliced in per marker.
    set.seed(3)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 30), levels = c("neg", "pos")),
                    up = c(rnorm(30), rnorm(30, 1.5)), down = c(rnorm(30), rnorm(30, -1.5)), up2 = c(rnorm(30), rnorm(30, 1)))
    r <- erv_run(d, "y", c("up", "down", "up2"), "pos", direction = "auto")
    n <- erv_tnotes(r$results$aucSummary)[["direction_used"]]
    expect_match(n, "Reading of the test values: <i>HIGHER values of up, up2 and LOWER values of down were taken to indicate pos</i>.", fixed = TRUE)
    expect_false(grepl("<b>", n, fixed = TRUE))
    expect_identical(erv_tnotes(r$results$outcomeCoding)[["reading"]], n)
})

test_that("a Positive class that no row has is explained neutrally, naming the observed levels", {
    txt <- erv_notices(erv_run(erv_cont(), "y", "m1", "Malignant"))
    expect_match(txt, "Positive class is set to Malignant, but no row of y has that level (its observed levels are: neg, pos).", fixed = TRUE)
    expect_match(txt, "Choose one of the observed levels in Positive class.", fixed = TRUE)
})

test_that("an AUC of 0 is a result: it is named best predictor, and the report is withheld, not 'No Valid ROC Results'", {
    # every case lies below every non-case and Direction is 'higher': AUC 0, a marker read backwards
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 20), levels = c("neg", "pos")), m = c(11:30, seq(1, 10, length.out = 20)))
    r <- erv_run(d, "y", "m", "pos", direction = "higher", comprehensive_output = TRUE)
    expect_equal(erv_cell(r$results$aucSummary, "m", "auc"), 0)
    txt <- erv_notices(r)
    expect_match(txt, "1 predictor (m, AUC=0)", fixed = TRUE)
    expect_match(txt, "Clinical Report Withheld", fixed = TRUE)
    expect_false(grepl("No Valid ROC Results", txt, fixed = TRUE))
    cs <- r$results$comprehensiveAnalysisSummary$asDF
    expect_match(cs$value[cs$measure == "Best Predictor"], "^m \\(AUC")
    expect_match(r$results$analysisSummary$content, "with an AUC of 0.000 (", fixed = TRUE)
})

test_that("a blank outcome value is missing, as an NA is, in the table, the notices and the analysis", {
    # R path only (jamovi stores an empty cell as missing). A blank used to be a level pooled with the negatives.
    set.seed(9)
    d <- data.frame(y = c(rep("neg", 30), rep("pos", 30), rep("", 6)), m = c(rnorm(30), rnorm(30, 1.3), rnorm(6, 5)),
                    m2 = rnorm(66), stringsAsFactors = FALSE)
    d$m2[c(1, 31)] <- NA
    dn <- d
    dn$y[dn$y == ""] <- NA
    r <- erv_run(d, "y", c("m", "m2"), "pos")
    expect_equal(r$results$aucSummary$asDF, erv_run(dn, "y", c("m", "m2"), "pos")$results$aucSummary$asDF)
    expect_equal(erv_cell(r$results$outcomeCoding, "missing", "n"), 6)
    txt <- erv_notices(r)
    expect_match(txt, "6 rows with a missing outcome were excluded (a blank value or an explicit missing-value level is not a category).", fixed = TRUE)
    expect_match(txt, "a predictor analysed on its own may use more rows (largest: 60)", fixed = TRUE)   # blanks have no outcome
})

test_that("the missing-outcome notice counts the rows the (missing) row counts, also one that lacks a predictor", {
    d <- erv_d1()
    d$status <- as.character(d$status)
    d$status[c(9, 19, 20)] <- ""
    d$m2 <- d$marker
    d$m2[19] <- NA          # row 19: blank outcome AND a missing predictor, so naOmit() drops it first
    r <- erv_run(d, "status", c("marker", "m2"), "Malignant")
    expect_equal(erv_cell(r$results$outcomeCoding, "missing", "n"), 3)
    expect_match(erv_notices(r), "3 rows with a missing outcome were excluded", fixed = TRUE)
})
