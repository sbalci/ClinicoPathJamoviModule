# Regression cover for the seven defects the /review-function pass confirmed. Each test names
# the wrong number the user used to be shown, so a future refactor that reintroduces it fails
# here rather than in a clinic.

library(testthat)

er_run <- function(...) suppressWarnings(do.call(ClinicoPath::enhancedROC, list(...)))
er_strip <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", if (is.null(x)) "" else x)))

# ── C-3  Precision-recall must not depend on spreadsheet row order ────────────

test_that("PR metrics are invariant to row order when the predictor has ties", {
    # order() is a stable sort, so accumulating TP/FP one ROW at a time inside a group of tied
    # scores walked the curve through operating points no threshold can produce. On this data
    # AUPRC used to read 0.478 / 0.841 / 0.626 for the same 200 patients in three orders.
    set.seed(5)
    base <- data.frame(
        y = factor(rep(c("P", "N"), each = 100), levels = c("N", "P")),
        m = c(sample(0:2, 100, TRUE, c(.2, .3, .5)), sample(0:2, 100, TRUE, c(.5, .3, .2)))
    )
    got <- lapply(
        list(order(base$y), order(base$y, decreasing = TRUE), sample(nrow(base))),
        function(o) {
            pr <- as.data.frame(er_run(data = base[o, ], outcome = "y", predictors = "m",
                                       positiveClass = "P", detectImbalance = TRUE)$results$precisionRecallTable)
            c(as.numeric(pr$auc_pr[1]), as.numeric(pr$average_precision[1]), as.numeric(pr$f1_score[1]))
        }
    )
    expect_equal(got[[1]], got[[2]], tolerance = 1e-12)
    expect_equal(got[[1]], got[[3]], tolerance = 1e-12)
})

test_that("tie-pooled PR values match a hand computation", {
    set.seed(5)
    d <- data.frame(
        y = factor(rep(c("P", "N"), each = 100), levels = c("N", "P")),
        m = c(sample(0:2, 100, TRUE, c(.2, .3, .5)), sample(0:2, 100, TRUE, c(.5, .3, .2)))
    )
    yy <- as.integer(d$y == "P")
    u <- sort(unique(d$m), decreasing = TRUE)
    tp <- cumsum(vapply(u, function(v) sum(yy[d$m == v]), numeric(1)))
    fp <- cumsum(vapply(u, function(v) sum(1 - yy[d$m == v]), numeric(1)))
    rec <- c(0, tp / sum(yy))
    pre <- c(1, tp / (tp + fp))

    pr <- as.data.frame(er_run(data = d, outcome = "y", predictors = "m",
                               positiveClass = "P", detectImbalance = TRUE)$results$precisionRecallTable)
    expect_equal(as.numeric(pr$average_precision[1]), sum(diff(rec) * pre[-1]), tolerance = 1e-9)
    expect_equal(as.numeric(pr$auc_pr[1]),
                 sum(diff(rec) * (pre[-1] + pre[-length(pre)]) / 2), tolerance = 1e-9)
})

# ── C-1  A [0,1] predictor must respect ROC direction ─────────────────────────

mirrored_data <- function(seed = 4, n = 300) {
    set.seed(seed)
    risk <- runif(n)
    y <- rbinom(n, 1, risk)
    data.frame(y = factor(ifelse(y == 1, "P", "N"), levels = c("N", "P")),
               hi = risk, lo = 1 - risk)
}

test_that("a mirrored [0,1] marker gives mirrored-identical clinical impact", {
    # `probs <- pred_vals` never consulted roc_obj$direction, so the same marker read the other
    # way round reported net benefit -25.47 per 100 against +23.61 - a useful marker declared
    # worse than treating nobody, with an identical AUC two panels above.
    d <- mirrored_data()
    grab <- function(v) {
        r <- er_run(data = d, outcome = "y", predictors = v, positiveClass = "P",
                    clinicalImpact = TRUE, useObservedPrevalence = TRUE)
        ci <- as.data.frame(r$results$clinicalImpactTable)
        c(auc = as.numeric(as.data.frame(r$results$aucSummary)$auc[1]),
          tpr = as.numeric(ci$true_positive_rate[1]),
          fpr = as.numeric(ci$false_positive_rate[1]),
          nb  = as.numeric(ci$net_benefit_per_100[1]))
    }
    hi <- grab("hi"); lo <- grab("lo")
    expect_equal(unname(hi["auc"]), unname(lo["auc"]), tolerance = 1e-9)
    expect_equal(unname(hi["nb"]),  unname(lo["nb"]),  tolerance = 1e-9)
    expect_equal(unname(hi["tpr"]), unname(lo["tpr"]), tolerance = 1e-9)
    expect_gt(hi["nb"], 0)   # a marker with AUC 0.86 is not worse than treating nobody
})

test_that("calibration also respects direction for a [0,1] marker", {
    d <- mirrored_data()
    brier <- function(v) as.numeric(as.data.frame(er_run(
        data = d, outcome = "y", predictors = v, positiveClass = "P",
        calibrationAnalysis = TRUE, calibrationMetrics = TRUE,
        brierScore = TRUE)$results$calibrationSummary)$brier_score[1])
    expect_equal(brier("hi"), brier("lo"), tolerance = 1e-9)
})

test_that("taking raw values as risks is announced, not assumed silently", {
    d <- mirrored_data()
    r <- er_run(data = d, outcome = "y", predictors = "hi", positiveClass = "P", clinicalImpact = TRUE)
    expect_match(er_strip(r$results$notices$content), "Values Read as Risks")
})

# ── C-2  Multi-class output must be attributable to a predictor ───────────────

mc_data <- function(seed = 8, n = 400) {
    set.seed(seed)
    cls <- factor(sample(c("A", "B", "C"), n, TRUE))
    data.frame(cls = cls, strong = as.numeric(cls) * 1.8 + rnorm(n), weak = rnorm(n))
}

test_that("multi-class average AUC does not depend on predictor order", {
    # setRow(rowNo = 1) inside the predictor loop meant the panel showed whichever marker was
    # last in the box: macro AUC 0.4938 or 0.7975 on identical data.
    d <- mc_data()
    grab <- function(ord) {
        av <- as.data.frame(er_run(data = d, outcome = "cls", predictors = ord, positiveClass = "A",
                                   multiClassROC = TRUE, multiClassStrategy = "ovr")$results$multiClassAverage)
        setNames(as.numeric(av$macro_auc), sub(":.*$", "", av$averaging_method))
    }
    a <- grab(c("strong", "weak"))
    b <- grab(c("weak", "strong"))
    expect_equal(length(a), 2L)                       # one row per predictor, not one row total
    expect_equal(a[order(names(a))], b[order(names(b))], tolerance = 1e-9)
    expect_gt(a[["strong"]], a[["weak"]])             # and they are the right way round
})

test_that("multi-class rows name their predictor when there is more than one", {
    d <- mc_data()
    r <- er_run(data = d, outcome = "cls", predictors = c("strong", "weak"), positiveClass = "A",
                multiClassROC = TRUE, multiClassStrategy = "ovr")
    cls <- as.data.frame(r$results$multiClassAUC)$class
    expect_true(all(grepl("^(strong|weak): ", cls)))
    expect_equal(anyDuplicated(cls), 0L)
})

# ── C-4  No copy-ready report for a below-chance marker ───────────────────────

test_that("the clinical report is withheld when the marker is below chance", {
    # It used to emit, verbatim: "At the optimal cutoff of Inf, the test achieved high
    # sensitivity (100.0%)" for a marker with AUC 0.219 - under a heading telling the reader to
    # paste it into a publication.
    set.seed(3)
    g <- rbinom(200, 1, 0.5)
    d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")), bm = g * 1.5 + rnorm(200))
    r <- er_run(data = d, outcome = "y", predictors = "bm", positiveClass = "P", direction = "lower")
    expect_lt(as.numeric(as.data.frame(r$results$aucSummary)$auc[1]), 0.5)

    rep <- er_strip(r$results$clinicalReport$content)
    expect_match(rep, "Not Generated")
    expect_false(grepl("high sensitivity", rep, fixed = TRUE))
    expect_false(grepl("cutoff of Inf", rep, fixed = TRUE))
    expect_match(er_strip(r$results$notices$content), "Clinical Report Withheld")
})

test_that("a sound marker still gets its report", {
    set.seed(3)
    g <- rbinom(200, 1, 0.5)
    d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")), bm = g * 1.5 + rnorm(200))
    r <- er_run(data = d, outcome = "y", predictors = "bm", positiveClass = "P")
    expect_match(er_strip(r$results$clinicalReport$content), "Results Section")
})

# ── H-1  Tied best cutoffs must not abort the analysis ────────────────────────

tie_data <- function(seed) {
    set.seed(seed)
    g <- rbinom(80, 1, 0.5)
    data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")),
               m = pmin(4, pmax(0, round(g * 1.5 + rnorm(80)))))
}

test_that("youdenOptimization = FALSE survives a tied best cutoff", {
    # pROC::coords("best") returns one row per tied optimum; the scalars downstream then hit
    # `if (youden >= 0.6)` as a length-2 logical and killed the whole analysis - every table,
    # since .populateOptimalCutoffs() runs from .run() with no tryCatch.
    for (s in c(34, 140)) {
        d <- tie_data(s)
        r <- pROC::roc(d$y, d$m, quiet = TRUE, levels = c("N", "P"))
        skip_if_not(NROW(pROC::coords(r, "best", ret = c("threshold", "sensitivity", "specificity"))) > 1,
                    paste("seed", s, "no longer ties in this pROC version"))
        res <- expect_no_error(
            er_run(data = d, outcome = "y", predictors = "m", positiveClass = "P",
                   youdenOptimization = FALSE))
        oc <- as.data.frame(res$results$optimalCutoffSummary)
        expect_equal(nrow(oc), 1L)
        expect_true(is.finite(as.numeric(oc$optimal_cutoff[1])))
        # Since 2026-09-03 youdenOptimization = FALSE means closest-to-top-left (the default
        # best.method of coords("best") was still youden, so the toggle changed nothing). The
        # tie notice therefore fires only when THAT criterion ties.
        n_tl <- NROW(pROC::coords(r, "best", best.method = "closest.topleft",
                                  ret = c("threshold", "sensitivity", "specificity")))
        if (n_tl > 1) expect_match(er_strip(res$results$notices$content), "Tied Best Cutoff")
        ca <- as.data.frame(res$results$cutoffAnalysis)
        if (nrow(ca) > 0) expect_true(any(grepl("closest to top-left", ca$cutoff_type, fixed = TRUE)))
    }
})

test_that("no dataset in a 120-seed sweep aborts with youdenOptimization = FALSE", {
    for (s in 1:120) {
        d <- tie_data(s)
        if (length(unique(d$y)) < 2) next
        expect_no_error(er_run(data = d, outcome = "y", predictors = "m",
                               positiveClass = "P", youdenOptimization = FALSE))
    }
})

# ── H-2  A constrained cutoff must say it is constrained ──────────────────────

test_that("the constraint notice fires when the thresholds displace the Youden optimum", {
    # Silent before: the row still read "Optimal (Youden)" while holding a smaller, constrained
    # value - measured up to 0.09 below the true maximum.
    fired <- FALSE
    for (s in 1:400) {
        set.seed(s)
        g <- rbinom(200, 1, 0.5)
        m <- ifelse(g == 1, rnorm(200, 2.0, 0.7), rnorm(200, 0, 2.4))
        d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")), m = m)
        roc <- pROC::roc(d$y, d$m, quiet = TRUE, levels = c("N", "P"))
        co <- pROC::coords(roc, "all", ret = c("threshold", "sensitivity", "specificity"))
        J <- co$sensitivity + co$specificity - 1
        fin <- is.finite(J); J <- J[fin]; co <- co[fin, ]
        valid <- co$sensitivity >= 0.8 & co$specificity >= 0.8
        if (!any(valid) || valid[which.max(J)]) next

        res <- er_run(data = d, outcome = "y", predictors = "m", positiveClass = "P",
                      sensitivityThreshold = 0.8, specificityThreshold = 0.8)
        txt <- er_strip(res$results$notices$content)
        expect_match(txt, "Cutoff Constrained by Your Thresholds")
        expect_match(txt, "Unconstrained optimum")
        fired <- TRUE
        break
    }
    expect_true(fired, info = "no binding-constraint dataset found to exercise the notice")
})

# ── H-3  Calibration slope must not be reported when it is a tautology ────────

test_that("calibration slope is withheld for a predictor we had to model ourselves", {
    # Regressing y on qlogis(fitted(glm(y ~ x))) re-parameterises the same fit, so the MLE
    # returns slope 1 / intercept 0 for ANY data - and it was graded "Good calibration slope".
    set.seed(42)
    x <- rnorm(400)
    y <- rbinom(400, 1, plogis(-1 + 1.5 * x))
    d <- data.frame(y = factor(ifelse(y == 1, "P", "N"), levels = c("N", "P")), x = x)
    cs <- as.data.frame(er_run(data = d, outcome = "y", predictors = "x", positiveClass = "P",
                               calibrationAnalysis = TRUE, calibrationMetrics = TRUE,
                               brierScore = TRUE)$results$calibrationSummary)
    expect_true(is.na(as.numeric(cs$calibration_slope[1])))
    expect_true(is.na(as.numeric(cs$calibration_intercept[1])))
    expect_match(cs$interpretation[1], "Not estimable")
    expect_false(grepl("Good calibration", cs$interpretation[1], fixed = TRUE))
    # the genuinely computed metrics on the same panel must survive
    expect_true(is.finite(as.numeric(cs$brier_score[1])))
})

test_that("a real probability predictor still gets a real calibration slope", {
    set.seed(9)
    p <- runif(300)
    y <- rbinom(300, 1, p)
    d <- data.frame(y = factor(ifelse(y == 1, "P", "N"), levels = c("N", "P")), risk = p)
    cs <- as.data.frame(er_run(data = d, outcome = "y", predictors = "risk", positiveClass = "P",
                               calibrationAnalysis = TRUE, calibrationMetrics = TRUE,
                               brierScore = TRUE)$results$calibrationSummary)
    slope <- as.numeric(cs$calibration_slope[1])
    expect_true(is.finite(slope))
    expect_gt(slope, 0.5)
    expect_lt(slope, 1.5)
})
