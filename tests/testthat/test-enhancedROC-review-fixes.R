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
    #
    # The two readings are only the SAME marker once the direction is declared. Since 2026-09-21
    # `direction` defaults to "higher" (a stated hypothesis, not an orientation read off the same
    # data that supply the AUC), so `lo` must be declared "lower" - or left to "auto" - for the
    # mirror identity to hold. Under the default `lo` is a genuinely wrong-way marker; that case
    # is pinned in the next test.
    d <- mirrored_data()
    grab <- function(v, dir) {
        r <- er_run(data = d, outcome = "y", predictors = v, positiveClass = "P", direction = dir,
                    clinicalImpact = TRUE, useObservedPrevalence = TRUE)
        ci <- as.data.frame(r$results$clinicalImpactTable)
        c(auc = as.numeric(as.data.frame(r$results$aucSummary)$auc[1]),
          tpr = as.numeric(ci$true_positive_rate[1]),
          fpr = as.numeric(ci$false_positive_rate[1]),
          nb  = as.numeric(ci$net_benefit_per_100[1]))
    }
    hi <- grab("hi", "higher"); lo <- grab("lo", "lower")
    expect_equal(unname(hi["auc"]), unname(lo["auc"]), tolerance = 1e-9)
    expect_equal(unname(hi["nb"]),  unname(lo["nb"]),  tolerance = 1e-9)
    expect_equal(unname(hi["tpr"]), unname(lo["tpr"]), tolerance = 1e-9)
    expect_equal(unname(hi["fpr"]), unname(lo["fpr"]), tolerance = 1e-9)
    expect_gt(hi["nb"], 0)   # a marker with AUC 0.86 is not worse than treating nobody
    # an auto-detected ">" must feed the clinical panel exactly as an explicit "lower" does
    expect_equal(grab("lo", "auto"), lo, tolerance = 1e-9)
})

test_that("under the default direction a wrong-way marker stays wrong-way in every panel", {
    # The complement of the test above, and the reason it is not a bug that `lo` scores 0.145
    # by default: the panels must AGREE. Auto-orienting inside the clinical code would have made
    # net benefit positive while the AUC two panels up still said 0.145. If this ever flips to
    # AUC 0.855, someone has reintroduced direction-fitting behind the user's back.
    d <- mirrored_data()
    r <- er_run(data = d, outcome = "y", predictors = "lo", positiveClass = "P",
                clinicalImpact = TRUE, useObservedPrevalence = TRUE)
    expect_lt(as.numeric(as.data.frame(r$results$aucSummary)$auc[1]), 0.5)
    expect_lt(as.numeric(as.data.frame(r$results$clinicalImpactTable)$net_benefit_per_100[1]), 0)
    expect_match(er_strip(r$results$notices$content), "opposite direction")
})

test_that("declaring the right direction leaves no backwards-marker error on screen", {
    # The same marker declared the right way round must not be accused of reading backwards.
    # (The stale-panel exposure this sat next to was not the empty notice list - every early
    # return adds a notice first - but a throw raised after the last .addNotice(), which used to
    # skip the tail .renderNotices() altogether. .run() now registers it on an on.exit().)
    d <- mirrored_data()
    r <- er_run(data = d, outcome = "y", predictors = "lo", positiveClass = "P",
                direction = "lower", clinicalImpact = TRUE, useObservedPrevalence = TRUE)
    expect_gt(as.numeric(as.data.frame(r$results$aucSummary)$auc[1]), 0.5)
    expect_false(grepl("opposite direction", er_strip(r$results$notices$content), fixed = TRUE))
})

test_that("calibration also respects direction for a [0,1] marker", {
    d <- mirrored_data()
    brier <- function(v, dir) as.numeric(as.data.frame(er_run(
        data = d, outcome = "y", predictors = v, positiveClass = "P", direction = dir,
        calibrationAnalysis = TRUE, calibrationMetrics = TRUE,
        brierScore = TRUE)$results$calibrationSummary)$brier_score[1])
    expect_equal(brier("hi", "higher"), brier("lo", "lower"), tolerance = 1e-9)
    expect_equal(brier("lo", "auto"),   brier("lo", "lower"), tolerance = 1e-9)
    # Brier is a calibration loss, not a rank statistic, so it is NOT mirror-invariant by itself:
    # read `lo` the way it was declared and it really is a bad risk (0.519 against 0.157). That
    # is the right answer, not a defect - the flip belongs to the declared direction, not to Brier.
    expect_gt(brier("lo", "higher"), 0.5)
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

test_that("the one-vs-one pairwise AUC honours Direction and is not floored at 0.5", {
    # The multi-class panels used to take their pairwise AUC from pROC::multiclass.roc(), which
    # orients each class pair by comparing the two groups' MEDIANS on the same data it scores
    # (pROC:::roc.default), silently ignoring Direction. That is upward-biased but NOT floored at
    # 0.5. Every pair is now read in the one declared direction, which is why the statistic is no
    # longer labelled "Hand-Till" anywhere the user can see.
    d <- mc_data()
    pairs <- combn(levels(d$cls), 2)
    # Mann-Whitney AUC of each pair read as "higher value indicates the LATER level of the pair",
    # averaged unweighted over pairs - the definition the analysis now implements.
    hi <- mean(apply(pairs, 2, function(p) {
        a <- d$strong[d$cls == p[1]]
        b <- d$strong[d$cls == p[2]]
        mean(outer(b, a, ">")) + 0.5 * mean(outer(b, a, "=="))
    }))
    got <- function(dir, strategy = "ovo") as.data.frame(er_run(
        data = d, outcome = "cls", predictors = "strong", positiveClass = "A", direction = dir,
        multiClassROC = TRUE, multiClassStrategy = strategy)$results$multiClassAverage)

    expect_equal(as.numeric(got("higher")$macro_auc[1]), hi, tolerance = 1e-9)
    # Reading the same marker the other way is the exact complement, NOT the same number back:
    # a per-pair "auto" orientation would return `hi` here too and hide the declaration.
    expect_equal(as.numeric(got("lower")$macro_auc[1]), 1 - hi, tolerance = 1e-9)
    expect_lt(as.numeric(got("lower")$macro_auc[1]), 0.5)
    # and the figure the One-vs-Rest panel quotes in prose is the same computation
    expect_match(got("higher", "ovr")$interpretation[1], sprintf("%.3f", hi), fixed = TRUE)
    expect_false(any(grepl("Hand-Till", got("higher")$averaging_method, fixed = TRUE)))
})

test_that("a non-monotone marker's pairwise collapse is disclosed by the printed range", {
    # One declared direction is the honest orientation, but it makes the unweighted pairwise mean
    # a bad summary when the classes are not ordered monotonically by the marker: B sits far above
    # both A and C, so A-vs-B reads ~0.95 and B-vs-C reads ~0.00 the same way round, and the mean
    # lands near 0.5 - "Below chance" for a marker that separates two of the three pairs almost
    # perfectly. The mean alone would hide that; the range beside it must not.
    set.seed(7)
    d <- data.frame(
        cls = factor(rep(c("A", "B", "C"), each = 20)),
        m = c(rnorm(20, 0), rnorm(20, 3), rnorm(20, 0.1))
    )
    r <- er_run(data = d, outcome = "cls", predictors = "m", positiveClass = "A",
                direction = "higher", multiClassROC = TRUE, multiClassStrategy = "ovo")
    av <- as.data.frame(r$results$multiClassAverage)
    pa <- as.numeric(as.data.frame(r$results$multiClassAUC)$auc)
    expect_gt(max(pa), 0.9)                                   # one pair separates almost perfectly
    expect_lt(min(pa), 0.1)                                   # another does, the other way round
    expect_equal(as.numeric(av$macro_auc[1]), mean(pa), tolerance = 1e-9)
    expect_lt(as.numeric(av$macro_auc[1]), 0.55)              # and the mean collapses toward 0.5
    # the collapse is visible without leaving the summary row
    expect_match(av$interpretation[1], sprintf("%.3f", min(pa)), fixed = TRUE)
    expect_match(av$interpretation[1], sprintf("%.3f", max(pa)), fixed = TRUE)
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

# ── 2026-09-22 convergence: the multi-class direction note was written per predictor ─────────

test_that("the multi-class direction note describes every marker, not just the last one", {
    # setNote() was called INSIDE the per-predictor loop, so with Direction = auto the last
    # marker's orientation overwrote the first's while the note still claimed it held for
    # "each marker": two markers pointing opposite ways were both described as "higher".
    set.seed(11)
    cls <- factor(rep(c("A", "B", "C"), each = 40))
    d <- data.frame(cls = cls,
                    up = as.numeric(cls) * 1.8 + rnorm(120),     # auto resolves to "<"
                    down = -as.numeric(cls) * 1.8 + rnorm(120),  # auto resolves to ">"
                    up2 = as.numeric(cls) * 1.2 + rnorm(120))    # auto resolves to "<" as well
    for (strategy in c("ovr", "ovo")) {
        tbl <- er_run(data = d, outcome = "cls", predictors = c("up", "down"),
                      positiveClass = "A", direction = "auto", multiClassROC = TRUE,
                      multiClassStrategy = strategy)$results$multiClassAUC
        note <- paste(unlist(lapply(tbl$notes, function(z) z$note)), collapse = " ")
        expect_true(nzchar(note))
        # both markers are named and both readings appear - no single claim for the set
        expect_match(note, "up", fixed = TRUE)
        expect_match(note, "down", fixed = TRUE)
        expect_match(note, "higher", fixed = TRUE)
        expect_match(note, "lower", fixed = TRUE)
    }
    # when the markers agree, the note states one direction for all of them and names neither
    tbl <- er_run(data = d, outcome = "cls", predictors = c("up", "up2"), positiveClass = "A",
                  direction = "higher", multiClassROC = TRUE,
                  multiClassStrategy = "ovr")$results$multiClassAUC
    note <- paste(unlist(lapply(tbl$notes, function(z) z$note)), collapse = " ")
    expect_match(note, "higher values of every marker", fixed = TRUE)
    expect_false(grepl("lower", note, fixed = TRUE))
})
