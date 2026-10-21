# Release fix pass 2026-10-11 for the enhancedROC independent review
# (development-ideas/enhancedROC-independent-review-2026-10-09.md). One test_that per finding,
# each written to FAIL on the code before its fix.

rel11_run <- function(data, ...) suppressWarnings(do.call(enhancedROC,
    utils::modifyList(list(data = data, showTableNotes = TRUE, showAnalysisSummary = TRUE,
                          showClinicalReport = TRUE), list(...), keep.null = TRUE)))

# Builds and runs the analysis object, to read the typed notice list behind the Html panel.
rel11_obj <- function(data, ..., .fatal = FALSE) {
    options <- do.call(enhancedROCOptions$new, utils::modifyList(list(showTableNotes = TRUE, showAnalysisSummary = TRUE,
        showClinicalReport = TRUE), list(...), keep.null = TRUE))
    analysis <- enhancedROCClass$new(options = options, data = data)
    suppressWarnings(analysis$run(noThrow = .fatal))
    if (.fatal) {
        expect_identical(analysis$status, "error")
        expect_true(analysis$errored)
        expect_false(analysis$complete)
    }
    analysis
}
rel11_notices <- function(analysis) analysis$.__enclos_env__$private$.noticeList
rel11_note <- function(table, key) {
    note <- table$.__enclos_env__$private$.notes[[key]]
    if (is.null(note)) "" else note$note
}
rel11_cell <- function(table, col, key) table$getCell(col, rowKey = key)
rel11_footnotes <- function(table, col, key) paste(unlist(rel11_cell(table, col, key)$footnotes), collapse = " ")

test_that("F-02: repeated patient IDs raise a WARNING naming the independence assumption", {
    # 15 case and 15 control patients, 4 slides each, ICC 0.5 (the review's F9 design).
    set.seed(20261010)
    np <- 15; m <- 4
    case <- rep(rep(c(0, 1), each = np), each = m)
    d <- data.frame(pid = rep(sprintf("P%02d", 1:(2 * np)), each = m),
                    y = factor(ifelse(case == 1, "pos", "neg"), levels = c("neg", "pos")),
                    m = 1.19023216289999 * case + rep(rnorm(2 * np, 0, sqrt(.5)), each = m) +
                        rnorm(2 * np * m, 0, sqrt(.5)))
    a <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "m", patientID = "pid")
    w <- Filter(function(n) identical(n$title, "Several Rows per Patient"), rel11_notices(a))
    expect_length(w, 1L)
    expect_identical(w[[1]]$type, "WARNING")
    expect_match(w[[1]]$content, "The 120 analysed rows that carry a patient identifier (pid) come from 30 patients", fixed = TRUE)
    expect_match(w[[1]]$content, "assumes that each row is an independent patient", fixed = TRUE)
    expect_match(w[[1]]$content, "too narrow", fixed = TRUE)
    expect_match(w[[1]]$content, "one row per patient", fixed = TRUE)

    # One row per patient: no warning; the INFO about the inactive cluster option stays.
    u <- rel11_obj(d[!duplicated(d$pid), ], outcome = "y", positiveClass = "pos", predictors = "m", patientID = "pid")
    titles <- vapply(rel11_notices(u), `[[`, "", "title")
    expect_false("Several Rows per Patient" %in% titles)
    expect_true("Patient clustering is inactive" %in% titles)
})

test_that("F-03: the one-direction one-vs-rest average carries no AUC band", {
    bands <- "^(Excellent|Good|Fair|Poor|Minimal|Below chance)"
    # Perfect A < B < C marker, 15/15/15: OVR AUCs 0, 0.5, 1, so macro = weighted = 0.5 by identity.
    d <- data.frame(y3 = factor(rep(c("A", "B", "C"), each = 15)),
                    m = c(seq(0, 1, length.out = 15), seq(2, 3, length.out = 15), seq(4, 5, length.out = 15)))
    av <- rel11_run(d, outcome = "y3", positiveClass = NULL, predictors = "m", multiClassROC = TRUE)$results$multiClassAverage
    df <- av$asDF
    expect_equal(c(df$macro_auc, df$weighted_auc), c(0.5, 0.5), tolerance = 1e-12)
    expect_false(grepl(bands, df$interpretation))
    expect_match(df$interpretation, "pairwise AUC, unweighted mean over class pairs: 1.000; pairs range 1.000 to 1.000", fixed = TRUE)
    expect_match(rel11_note(av, "ovr_average"), "not a summary of discrimination", fixed = TRUE)

    # Unequal classes 20/25/30: the same perfect marker averaged 0.467 and read 'Below chance'.
    d2 <- data.frame(y3 = factor(rep(c("A", "B", "C"), c(20, 25, 30))), m = seq_len(75))
    df2 <- rel11_run(d2, outcome = "y3", positiveClass = NULL, predictors = "m", multiClassROC = TRUE)$results$multiClassAverage$asDF
    expect_equal(df2$macro_auc, 1.4 / 3, tolerance = 1e-12)
    expect_false(grepl(bands, df2$interpretation))

    # One-vs-One keeps its band on the pairwise mean, and the OVR note goes.
    ovo <- rel11_run(d, outcome = "y3", positiveClass = NULL, predictors = "m", multiClassROC = TRUE,
                     multiClassStrategy = "ovo")$results$multiClassAverage
    expect_match(ovo$asDF$interpretation, "^Excellent")
    expect_identical(rel11_note(ovo, "ovr_average"), "")
})

test_that("F-04: no bootstrap AUC interval when one class has a single member", {
    single <- "One of the two groups has a single patient, so every bootstrap resample repeats that patient"
    d1 <- data.frame(y = factor(c("Malignant", rep("Benign", 99)), levels = c("Benign", "Malignant")),
                     m = c(50.5, 1:99))
    for (args in list(list(stratifiedBootstrap = TRUE, bootstrapMethod = "percentile"),
                      list(stratifiedBootstrap = FALSE, bootstrapMethod = "bca"),
                      list(stratifiedBootstrap = TRUE, bootstrapMethod = "bca"))) {
        r <- do.call(rel11_run, c(list(d1, outcome = "y", positiveClass = "Malignant", predictors = "m",
                                       useBootstrap = TRUE, bootstrapSamples = 200, seed = 3), args))$results
        for (col in c("auc_lower", "auc_upper")) {
            expect_identical(rel11_cell(r$aucSummary, col, "m")$value, "", label = paste(col, args$bootstrapMethod))
            expect_match(rel11_footnotes(r$aucSummary, col, "m"), single, fixed = TRUE)
        }
        expect_equal(r$aucSummary$asDF$auc, 50 / 99, tolerance = 1e-12)
        expect_match(r$notices$content, "No AUC Interval: m", fixed = TRUE)
        # no BCa fallback is reported for an interval that is not shown
        expect_false(grepl("Bias-Corrected Interval Not Available", r$notices$content, fixed = TRUE))
    }

    # Multi-class: the one-member class 'C' gets no bootstrap interval; the others keep theirs.
    set.seed(11)
    d <- data.frame(g = factor(c(rep("A", 30), rep("B", 25), "C"), levels = c("A", "B", "C")),
                    m = c(rnorm(30), rnorm(25, 1), 0.3))
    tb <- rel11_run(d, outcome = "g", positiveClass = NULL, predictors = "m", multiClassROC = TRUE,
                    useBootstrap = TRUE, bootstrapSamples = 200, seed = 3)$results$multiClassAUC
    expect_identical(rel11_cell(tb, "auc_lower", "m_C")$value, "")
    expect_match(rel11_footnotes(tb, "auc_lower", "m_C"), single, fixed = TRUE)
    expect_true(is.numeric(rel11_cell(tb, "auc_lower", "m_A")$value))
})

test_that("F-21: spline calibration measures say they have no interval and need about 200 per class", {
    set.seed(2026)
    p <- runif(50, 0.05, 0.95)
    yy <- rbinom(50, 1, p)
    d <- data.frame(y = factor(ifelse(yy == 1, "pos", "neg"), levels = c("neg", "pos")), p = p)
    cal <- rel11_run(d, outcome = "y", positiveClass = "pos", predictors = "p", calibrationAnalysis = TRUE,
                     splineCalibration = TRUE, predictionScale = "probability")$results$calibrationSummary
    expect_true(is.finite(cal$asDF$ici))
    note <- rel11_note(cal, "spline_precision")
    expect_match(note, "point estimates with no confidence interval", fixed = TRUE)
    expect_match(note, "200 patients with and 200 without the event (Van Calster et al. 2019)", fixed = TRUE)
    expect_match(note, sprintf("this sample has %d with and %d without", sum(yy), 50L - sum(yy)), fixed = TRUE)
    expect_match(note, "a well-calibrated model can look miscalibrated", fixed = TRUE)

    off <- rel11_run(d, outcome = "y", positiveClass = "pos", predictors = "p", calibrationAnalysis = TRUE,
                     predictionScale = "probability")$results$calibrationSummary
    expect_identical(rel11_note(off, "spline_precision"), "")
})

test_that("F-05: AUC-PR is interpolated in ROC space, not inflated by tied scores", {
    pr_of <- function(d) rel11_run(d, outcome = "y", positiveClass = "Malignant", predictors = "m",
                                   detectImbalance = TRUE)$results$precisionRecallTable$asDF
    y <- factor(rep(c("Benign", "Malignant"), each = 20))
    # Constant marker: no information, so AUC-PR equals the prevalence (it read 0.75).
    const <- pr_of(data.frame(y = y, m = 5))
    expect_equal(c(const$auc_pr, const$average_precision), c(0.5, 0.5), tolerance = 1e-12)
    # Dichotomous marker: closed form of the continuous ROC-space interpolation (PRROC's
    # auc.integral, Keilwagen et al. 2014); the anchored trapezoid read 0.7934.
    dich <- pr_of(data.frame(y = y, m = c(rep(0, 15), rep(1, 5), rep(0, 6), rep(1, 14))))
    expect_equal(dich$auc_pr, 0.7 * 14 / 19 + 0.3 * (2 / 7 + (60 / 147) * log(40 / 19)), tolerance = 1e-12)
    expect_equal(dich$average_precision, 0.7 * 14 / 19 + 0.3 * 0.5, tolerance = 1e-12)
    # Useless ordinal marker, prevalence 0.2, same score distribution in both classes: 0.2 (it read 0.24).
    y2 <- factor(rep(c("Malignant", "Benign"), c(8, 32)))
    useless <- pr_of(data.frame(y = y2, m = c(rep(1:4, each = 2), rep(1:4, each = 8))))
    expect_equal(useless$auc_pr, 0.2, tolerance = 1e-12)
})

test_that("F-23: an integer-coded Nominal/Ordinal predictor is analysed by its values", {
    codes <- c(1, 1, 1, 1, 1, 1, 2, 2, 4, 4, 1, 3, 3, 3, 4, 4, 4, 4, 4, 4)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 10)),
                    gf = factor(codes, levels = 1:4, labels = c("10", "20", "40", "80")))
    attr(d$gf, "values") <- c(10, 20, 40, 80)   # how jamovi delivers an integer column typed Nominal/Ordinal
    a <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "gf")
    r <- a$results$results
    v <- c(10, 20, 40, 80)[codes]
    oracle <- mean(outer(v[11:20], v[1:10], ">")) + 0.5 * mean(outer(v[11:20], v[1:10], "=="))
    expect_equal(r$aucSummary$asDF$auc, oracle, tolerance = 1e-12)
    expect_equal(oracle, 0.81)
    # the cut-point is on the values' scale, not the level codes 1-4
    expect_gte(r$optimalCutoffSummary$asDF$optimal_cutoff, 10)
    info <- Filter(function(n) identical(n$title, "Integer Codes Read as Numbers"), rel11_notices(a))
    expect_length(info, 1L)
    expect_identical(info[[1]]$type, "INFO")

    # A text factor (no 'values' attribute) is still refused, by jmvcore's `permitted: numeric` check.
    d$txt <- factor(ifelse(codes > 2, "high", "low"))
    expect_error(rel11_run(d, outcome = "y", positiveClass = "pos", predictors = "txt"), "requires a numeric variable")
})

# ---- FIX-B (F-09, F-10, F-12, F-24 to F-31, F-34) ----

# 20 + 20 marker on its own scale (analysed values 1.7 to 9.1).
rel11_scale_data <- function() {
    set.seed(1)
    data.frame(y = factor(rep(c("Benign", "Malignant"), each = 20), levels = c("Benign", "Malignant")),
               m = round(c(runif(20, 1.4, 6), runif(20, 3, 9.5)), 1))
}
rel11_titled <- function(analysis, title) Filter(function(n) identical(n$title, title), rel11_notices(analysis))
rel11_iv <- function(analysis) analysis$results$results$analysisSummary$content

test_that("F-09: a fixed cutoff outside the marker's range raises a WARNING naming it and the range", {
    d <- rel11_scale_data()
    # the default '0.5' on a 1.7-9.1 marker flags everyone
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalImpact = TRUE)
    expect_equal(a$results$results$clinicalImpactTable$asDF$tested_positive, 1)
    w <- rel11_titled(a, "Fixed Cutoff Flags Every Patient")
    expect_length(w, 1L)
    expect_identical(w[[1]]$type, "WARNING")
    expect_match(w[[1]]$content, "m at cutoff 0.5 (analysed values 1.7 to 9.1)", fixed = TRUE)
    expect_match(w[[1]]$content, "the default 0.5 suits predicted probabilities only", fixed = TRUE)

    # above the range flags no one; a cutoff inside the range is silent
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalImpact = TRUE, impactCutoffs = "5, 20")
    w <- rel11_titled(a, "Fixed Cutoff Flags No Patient")
    expect_length(w, 1L)
    expect_match(w[[1]]$content, "m at cutoff 20 (analysed values 1.7 to 9.1)", fixed = TRUE)
    expect_false(grepl("at cutoff 5 ", w[[1]]$content, fixed = TRUE))
    expect_length(rel11_titled(a, "Fixed Cutoff Flags Every Patient"), 0L)

    # Direction lower: a score at or below the cutoff is positive, so 20 now flags everyone
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalImpact = TRUE,
                   impactCutoffs = "20", direction = "lower")
    expect_length(rel11_titled(a, "Fixed Cutoff Flags Every Patient"), 1L)

    # the frozen-model panel evaluates the same cutoffs
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", validationPanel = TRUE,
                   validationNegative = "Benign", direction = "higher", modelVersion = "v1")
    expect_gt(a$results$results$frozenMetrics$rowCount, 0)
    w <- rel11_titled(a, "Fixed Cutoff Flags Every Patient")
    expect_length(w, 1L)
    expect_match(w[[1]]$content, "m at cutoff 0.5 (analysed values 1.7 to 9.1)", fixed = TRUE)
})

test_that("F-10: minimum sensitivity/specificity with Youden off are announced, not silently ignored", {
    d <- rel11_scale_data()
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m",
                   youdenOptimization = FALSE, sensitivityThreshold = 0.9)
    w <- rel11_titled(a, "Minimum Sensitivity and Specificity Not Applied")
    expect_length(w, 1L)
    expect_identical(w[[1]]$type, "WARNING")
    expect_match(w[[1]]$content, "The minimum sensitivity (0.9) and minimum specificity (0)", fixed = TRUE)
    expect_match(w[[1]]$content, "Turn Youden index optimization on", fixed = TRUE)

    # The documented criterion is unchanged: closest to (0, 1) over all thresholds (independent
    # brute force over the midpoints, smallest cutoff among ties).
    x1 <- d$m[d$y == "Malignant"]; x0 <- d$m[d$y == "Benign"]
    u <- sort(unique(d$m)); cand <- (u[-1] + u[-length(u)]) / 2
    dist <- vapply(cand, function(c) (1 - mean(x1 >= c))^2 + (1 - mean(x0 < c))^2, 0)
    oracle <- min(cand[abs(dist - min(dist)) < 1e-12])
    cut <- a$results$results$optimalCutoffSummary$asDF
    expect_equal(cut$optimal_cutoff, oracle, tolerance = 1e-12)
    expect_lt(cut$sensitivity, 0.9)

    # no notice when Youden is on or the minimums are 0
    on <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", sensitivityThreshold = 0.9)
    expect_length(rel11_titled(on, "Minimum Sensitivity and Specificity Not Applied"), 0L)
    off0 <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", youdenOptimization = FALSE)
    expect_length(rel11_titled(off0, "Minimum Sensitivity and Specificity Not Applied"), 0L)
})

test_that("F-12: the copy-ready Results sentence prints the cut-point to 4 significant digits", {
    d <- rel11_scale_data()
    d$m <- d$m * 1e-12
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalReport = TRUE)
    cut <- a$results$results$optimalCutoffSummary$asDF$optimal_cutoff
    # independent Youden brute force over the midpoints
    x1 <- d$m[d$y == "Malignant"]; x0 <- d$m[d$y == "Benign"]
    u <- sort(unique(d$m)); cand <- (u[-1] + u[-length(u)]) / 2
    j <- vapply(cand, function(c) mean(x1 >= c) + mean(x0 < c) - 1, 0)
    expect_true(any(abs(cand[abs(j - max(j)) < 1e-12] - cut) < 1e-24))
    report <- a$results$results$clinicalReport$content
    expect_match(report, sprintf("At the cutoff that maximised the Youden index, %s, sensitivity", signif(cut, 4)), fixed = TRUE)
    expect_false(grepl("Youden index, 0.000,", report, fixed = TRUE))
})

test_that("F-24: internal validation keeps the bootstrap result when CV cannot run as asked", {
    # The review's D9: 15 negative, 9 positive.
    set.seed(9)
    d9 <- data.frame(y = factor(rep(c("neg", "pos"), c(15, 9)), levels = c("neg", "pos")),
                     x = round(c(rnorm(15), rnorm(9, 1)), 2))
    boot_line <- function(html) regmatches(html, regexpr("x \\(Bootstrap\\):[^<]*", html))
    run <- function(method) rel11_obj(d9, outcome = "y", positiveClass = "pos", predictors = "x",
                                      internalValidation = TRUE, validationMethod = method, seed = 1,
                                      comprehensive_output = TRUE)
    b <- run("bootstrap"); both <- run("both"); cv <- run("cv")
    expect_match(both$results$results$methodsExplanation$content, "Stratified 9-fold cross-validation", fixed = TRUE)
    expect_match(rel11_iv(b), "Optimism = 0.0228 (SD across resamples 0.1057), Corrected AUC = 0.740", fixed = TRUE)
    expect_identical(boot_line(rel11_iv(both)), boot_line(rel11_iv(b)))
    for (html in c(rel11_iv(both), rel11_iv(cv))) {
        expect_false(grepl("must have two levels", html, fixed = TRUE))
        expect_match(html, "x (9-fold CV): Mean AUC = ", fixed = TRUE)
    }
    info <- rel11_titled(both, "Fewer Cross-Validation Folds")
    expect_length(info, 1L)
    expect_match(info[[1]]$content, "has 9 patients, so cross-validation used 9 folds instead of 10", fixed = TRUE)
    # the closing paragraph about the corrected AUC's interval only beside a bootstrap line
    closing <- "No interval is given for the corrected AUC."
    expect_match(rel11_iv(both), closing, fixed = TRUE)
    expect_false(grepl(closing, rel11_iv(cv), fixed = TRUE))

    # 3 events in 24: single-class resamples are dropped and counted (independent replay of the stream)
    set.seed(3)
    d3 <- data.frame(y = factor(rep(c("neg", "pos"), c(21, 3)), levels = c("neg", "pos")),
                     x = round(c(rnorm(21), rnorm(3, 1)), 2))
    a3 <- rel11_obj(d3, outcome = "y", positiveClass = "pos", predictors = "x",
                    internalValidation = TRUE, validationMethod = "bootstrap", seed = 1)
    y3 <- as.integer(d3$y == "pos")
    single <- withr::with_seed(1, sum(vapply(1:200, function(i) length(unique(y3[sample(1:24, replace = TRUE)])) < 2, TRUE)))
    expect_gt(single, 0)
    expect_match(rel11_iv(a3), sprintf("%d of 200 resamples held only one outcome group and were dropped; the optimism is the mean over the other %d.", single, 200L - single), fixed = TRUE)
    expect_match(rel11_iv(a3), "x (Bootstrap): Apparent AUC = ", fixed = TRUE)

    # one positive: CV cannot put both groups in every fold, so it is not run, with a WARNING
    d1 <- data.frame(y = factor(c("pos", rep("neg", 23)), levels = c("neg", "pos")), x = c(2, rnorm(23)))
    a1 <- rel11_obj(d1, outcome = "y", positiveClass = "pos", predictors = "x",
                    internalValidation = TRUE, validationMethod = "cv", seed = 1)
    w <- rel11_titled(a1, "Cross-Validation Not Run")
    expect_length(w, 1L)
    expect_identical(w[[1]]$type, "WARNING")
    expect_false(grepl("Validation failed", rel11_iv(a1), fixed = TRUE))
})

test_that("F-25 / F-26: optimism uses the exact slope sign; the CV SD is labelled and has 3 decimals", {
    # The review's AB D1 (12 Benign, 10 Malignant), seed 123, B capped at 200.
    d <- data.frame(y = factor(rep(c("Benign", "Malignant"), c(12, 10)), levels = c("Benign", "Malignant")),
                    m = c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8, 9, 12, 3, 5, 6, 7, 8, 9, 10, 11, 12, 14))
    html <- rel11_iv(rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m",
                               internalValidation = TRUE, validationMethod = "both", seed = 123, bootstrapSamples = 1000))
    # Independent replay: Mann-Whitney AUC (not pROC), each model oriented by the sign of its
    # class-mean difference (exact for integer data), then the stratified folds.
    y <- as.integer(d$y == "Malignant"); x <- d$m; n <- length(y)
    mw <- function(y, s) { r <- rank(s); n1 <- sum(y == 1); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * sum(y == 0)) }
    sgn <- function(y, x) sign(mean(x[y == 1]) - mean(x[y == 0]))
    # Each resampling step starts from the seed (production pass 2026-10-11, P-02): the folds no
    # longer continue the bootstrap's stream, so the CV result does not depend on "both" vs "cv".
    opt <- withr::with_seed(123, vapply(1:200, function(i) {
        idx <- sample(1:n, replace = TRUE)
        if (length(unique(y[idx])) < 2) return(NA_real_)
        s <- sgn(y[idx], x[idx]); mw(y[idx], s * x[idx]) - mw(y, s * x)
    }, 0))
    oracle <- withr::with_seed(123, {
        folds <- integer(n)
        folds[y == 1] <- sample(rep(1:10, length.out = sum(y == 1)))
        folds[y == 0] <- sample(rep(1:10, length.out = sum(y == 0)))
        cv <- vapply(1:10, function(k) mw(y[folds == k], sgn(y[folds != k], x[folds != k]) * x[folds == k]), 0)
        list(opt = opt, cv = cv)
    })
    apparent <- mw(y, sgn(y, x) * x)
    expect_match(html, sprintf("Optimism = %.4f (SD across resamples %.4f), Corrected AUC = %.3f",
                               mean(oracle$opt, na.rm = TRUE), sd(oracle$opt, na.rm = TRUE),
                               apparent - mean(oracle$opt, na.rm = TRUE)), fixed = TRUE)
    expect_match(html, "Optimism = 0.0039 (SD across resamples 0.1040), Corrected AUC = 0.754", fixed = TRUE)
    expect_match(html, sprintf("m (10-fold CV): Mean AUC = %.3f (SD across folds %.3f, not a standard error)",
                               mean(oracle$cv), sd(oracle$cv)), fixed = TRUE)
    expect_match(html, "Mean AUC = 0.700 (SD across folds 0.422, not a standard error)", fixed = TRUE)   # folds seeded on their own (P-02)
})

test_that("F-27: the comparative-plot annotation subtracts unrounded AUCs", {
    plot_file <- withr::local_tempfile(fileext = ".png")
    grDevices::png(plot_file)
    withr::defer(grDevices::dev.off())
    set.seed(2)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos")))
    d$m1 <- round(c(rnorm(15), rnorm(15, 1)), 1)
    d$m2 <- round(c(rnorm(15), rnorm(15, 0.5)), 1)
    y <- d$y == "pos"
    auc <- vapply(c("m1", "m2"), function(m) mean(outer(d[[m]][y], d[[m]][!y], ">")) +
                      0.5 * mean(outer(d[[m]][y], d[[m]][!y], "==")), 0)
    diff_true <- round(max(auc) - min(auc), 3)
    expect_false(isTRUE(all.equal(diff_true, round(round(max(auc), 3) - round(min(auc), 3), 3))))   # the case that showed it
    a <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = c("m1", "m2"), analysisType = "comparative", rocCurve = TRUE)
    expect_true(a$.__enclos_env__$private$.plotComparativeROC(a$results$results$comparativeROCPlot,
        ggtheme = ggplot2::theme_minimal(), theme = list(palette = "jmv")))
    label <- unlist(lapply(ggplot2::last_plot()$layers, function(l) l$aes_params$label))
    expect_match(label, paste0("Difference: +", diff_true), fixed = TRUE)
})

test_that("F-28: an undefined McClish-normalised pAUC is blank with a footnote, not NaN", {
    d <- rel11_scale_data()
    d$m <- -d$m   # read 'higher' this marker lies below the diagonal: raw pAUC 0 over specificity 0.8-1
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", partialAuc = TRUE)
    tb <- a$results$results$partialAucAnalysis
    expect_equal(tb$asDF$partial_auc, 0)
    expect_identical(rel11_cell(tb, "normalized_pauc", "m")$value, "")
    expect_match(rel11_footnotes(tb, "normalized_pauc", "m"), "lies above the chance diagonal over the focus range, and this curve lies below it", fixed = TRUE)
    # above the diagonal the value stays (McClish 1989: (1 + (pAUC - min) / (max - min)) / 2)
    up <- rel11_obj(rel11_scale_data(), outcome = "y", positiveClass = "Malignant", predictors = "m", partialAuc = TRUE)
    expect_true(is.numeric(rel11_cell(up$results$results$partialAucAnalysis, "normalized_pauc", "m")$value))
})

test_that("F-29: non-numeric Custom cutoffs entries are named in a WARNING", {
    d <- rel11_scale_data()
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", customCutoffs = "2.9, abc, 5, 0.5%")
    w <- rel11_titled(a, "Custom Cutoffs Not Read")
    expect_length(w, 1L)
    expect_identical(w[[1]]$type, "WARNING")
    expect_match(w[[1]]$content, "were ignored: 'abc', '0.5%'.", fixed = TRUE)
    expect_equal(a$.__enclos_env__$private$.rocResults$m$custom_cutoffs$cutoff, c(2.9, 5))
    # every entry a number ('3,5' is the two cutoffs 3 and 5): no notice
    ok <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", customCutoffs = "3,5")
    expect_length(rel11_titled(ok, "Custom Cutoffs Not Read"), 0L)
})

test_that("F-30: an all-missing outcome is refused as having no values, not 'only one value: NA'", {
    d <- data.frame(y = factor(rep(NA, 20), levels = c("Benign", "Malignant")), m = 1:20)
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", .fatal = TRUE)
    e <- Filter(function(n) identical(n$type, "ERROR"), rel11_notices(a))
    expect_length(e, 1L)
    expect_match(e[[1]]$content, "Outcome variable 'y' has no non-missing values.", fixed = TRUE)
    expect_false(grepl("only one value", e[[1]]$content, fixed = TRUE))
})

test_that("F-31: Stratified bootstrap reaches roc.test() and the ROC confidence bands", {
    set.seed(2)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos")))
    d$m1 <- round(c(rnorm(15), rnorm(15, 1)), 1)
    d$m2 <- round(c(rnorm(15), rnorm(15, 0.5)), 1)
    r1 <- pROC::roc(d$y, d$m1, levels = c("neg", "pos"), direction = "<", quiet = TRUE)
    r2 <- pROC::roc(d$y, d$m2, levels = c("neg", "pos"), direction = "<", quiet = TRUE)
    p <- band <- list()
    for (st in c(TRUE, FALSE)) {
        k <- as.character(st)
        a <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = c("m1", "m2"), analysisType = "comparative",
                       pairwiseComparisons = TRUE, comparisonMethod = "bootstrap", bootstrapSamples = 200, seed = 5,
                       stratifiedBootstrap = st)
        p[[k]] <- a$results$results$rocComparisons$asDF$p_value
        expect_equal(p[[k]], withr::with_seed(5, pROC::roc.test(r1, r2, method = "bootstrap", boot.n = 200,
                                                                 boot.stratified = st)$p.value), tolerance = 1e-12)
        b <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "m1", rocCurve = TRUE,
                       showConfidenceBands = TRUE, bootstrapSamples = 200, seed = 5, stratifiedBootstrap = st)
        band[[k]] <- b$results$results$rocCurvePlot$state$roc$m1$bands$TPR_lower
        oracle <- withr::with_seed(5, pROC::ci.coords(r1, x = seq(0.1, 0.9, by = 0.1), input = "specificity",
            ret = "sensitivity", boot.n = 200, boot.stratified = st, conf.level = 0.95))$sensitivity[, 1]
        expect_equal(band[[k]], unname(oracle), tolerance = 1e-12)
    }
    expect_false(isTRUE(all.equal(p[["TRUE"]], p[["FALSE"]])))
    expect_false(isTRUE(all.equal(band[["TRUE"]], band[["FALSE"]])))
})

test_that("F-34: asSource() writes non-syntactic variable names so the call parses and round-trips", {
    set.seed(2)
    d <- data.frame(`Tumour status` = factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos")),
                    `Ki 67 (%)` = round(c(rnorm(15), rnorm(15, 1)), 1), m2 = round(rnorm(30), 1), check.names = FALSE)
    options <- enhancedROCOptions$new(outcome = "Tumour status", positiveClass = "pos", predictors = c("Ki 67 (%)", "m2"))
    src <- enhancedROCClass$new(options = options, data = d)$asSource()
    expect_match(src, "outcome = \"Tumour status\"", fixed = TRUE)
    expect_match(src, "predictors = c(\"Ki 67 (%)\", \"m2\")", fixed = TRUE)
    call <- str2lang(src)
    call[[1]] <- quote(enhancedROC)
    res <- suppressWarnings(eval(call, envir = list2env(list(data = d), parent = environment())))
    direct <- rel11_run(d, outcome = "Tumour status", positiveClass = "pos", predictors = c("Ki 67 (%)", "m2"))
    expect_equal(res$results$aucSummary$asDF$auc, direct$results$aucSummary$asDF$auc, tolerance = 1e-12)
    # a syntactic name keeps jmvcore's bare form
    plain <- enhancedROCOptions$new(outcome = "y", positiveClass = "pos", predictors = "m2")
    expect_match(enhancedROCClass$new(options = plain, data = data.frame(y = d[[1]], m2 = d$m2))$asSource(),
                 "outcome = y,", fixed = TRUE)
})

# ---- FIX-C (wording, notices, copy-ready text, citations) ----

# Review fixtures: K (10 + 10, AUC 0.88), INC (20 + 20, AUC 0.6175, CI crossing 0.5),
# SEP (10 + 10, perfect separation).
rel11_k <- function() data.frame(y = factor(rep(c("Benign", "Malignant"), each = 10), levels = c("Benign", "Malignant")),
                                 m = c(1:8, 13, 14, 4.5, 10:12, 15:20))
rel11_inc <- function() data.frame(y = factor(rep(c("Benign", "Malignant"), each = 20), levels = c("Benign", "Malignant")),
                                   m = c(1:20, (1:20) + 2.5))
rel11_text <- function(analysis, title) paste(vapply(rel11_titled(analysis, title), `[[`, "", "content"), collapse = " ")
rel11_all_notices <- function(analysis) paste(vapply(rel11_notices(analysis), function(n) paste(n$title, n$content), ""), collapse = " ")
rel11_frozen <- function(...) {
    set.seed(4)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 20), levels = c("neg", "pos")), s = c(runif(20, 0, .6), runif(20, .3, 1)))
    rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "s", validationPanel = TRUE, validationNegative = "neg",
              modelVersion = "v1", impactCutoffs = "0.5", ...)
}
# Mann-Whitney AUC (ties one half) and the DeLong interval from placement values, base R only.
rel11_delong <- function(cases, controls, level = 0.95) {
    v10 <- vapply(cases, function(a) mean((a > controls) + 0.5 * (a == controls)), 0)
    v01 <- vapply(controls, function(b) mean((cases > b) + 0.5 * (cases == b)), 0)
    auc <- mean(v10)
    se <- sqrt(var(v10) / length(cases) + var(v01) / length(controls))
    c(auc = auc, lo = auc - qnorm(1 - (1 - level) / 2) * se, hi = auc + qnorm(1 - (1 - level) / 2) * se)
}

test_that("F-00: the Methods texts and the instructions say each row is an independent patient", {
    r <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m", comprehensive_output = TRUE)$results$results
    for (txt in c(r$clinicalReport$content, r$methodsExplanation$content))
        expect_match(txt, "Each row was analysed as a separate, independent patient; the confidence intervals and tests assume this.", fixed = TRUE)
    expect_match(r$instructions$content, "Each row must be a different patient", fixed = TRUE)
})

test_that("F-06: the copy-ready Results state the AUC and interval with no band verdict, and say when the interval includes 0.5", {
    d <- rel11_inc()
    oracle <- rel11_delong(d$m[d$y == "Malignant"], d$m[d$y == "Benign"])
    expect_equal(unname(oracle["auc"]), 0.6175, tolerance = 1e-12)
    rep_inc <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m")$results$results$clinicalReport$content
    expect_match(rep_inc, sprintf("The AUC of m was 0.618 (95%% CI: %s--%s); the interval includes 0.5, so these data are compatible with no discrimination.",
                                  round(oracle[["lo"]], 3), round(oracle[["hi"]], 3)), fixed = TRUE)
    rep_k <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m")$results$results$clinicalReport$content
    expect_match(rep_k, "The AUC of m was 0.880 (95% CI: 0.727--1).", fixed = TRUE)
    expect_false(grepl("includes 0.5", rep_k, fixed = TRUE))
    sep <- data.frame(y = factor(rep(c("Benign", "Malignant"), each = 10), levels = c("Benign", "Malignant")), m = 1:20)
    rep_sep <- rel11_obj(sep, outcome = "y", positiveClass = "Malignant", predictors = "m")$results$results$clinicalReport$content
    expect_match(rep_sep, "The AUC of m was 1.000 (interval not estimable). The marker separates the two groups perfectly in these data (AUC = 1); with 10 positive and 10 negative cases the true AUC could still be well below 1.", fixed = TRUE)
    for (txt in c(rep_inc, rep_k, rep_sep)) {
        expect_false(grepl("demonstrated", txt, fixed = TRUE))
        expect_false(grepl("discrimination:", txt, fixed = TRUE))   # 'showed good to excellent discrimination: the AUC was'
        expect_false(grepl("good to excellent", txt, fixed = TRUE))
    }
})

test_that("F-07: with fewer than 10 in a class there is no 'completed successfully' and the report names the counts", {
    set.seed(91)
    d <- data.frame(y = factor(rep(c("neg", "pos"), c(21, 9)), levels = c("neg", "pos")), m = c(rnorm(21), rnorm(9, 1)))
    a <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "m")
    expect_length(rel11_titled(a, "Low Per-Class Count"), 1L)   # once per run since the production pass
    expect_false(grepl("completed successfully", rel11_all_notices(a), fixed = TRUE))
    expect_length(rel11_titled(a, "Analysis Finished with Errors"), 1L)
    expect_match(a$results$results$clinicalReport$content,
                 "With only 9 positive and 21 negative patients (fewer than 10 in a class), the estimates below are unreliable.", fixed = TRUE)
    # 10 + 10: no ERROR, the usual completion notice, no caveat
    k <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m")
    expect_length(rel11_titled(k, "Analysis Complete"), 1L)
    expect_false(grepl("fewer than 10 in a class", k$results$results$clinicalReport$content, fixed = TRUE))
})

test_that("F-08: PPV/NPV at the observed prevalence carry a note on the sampling design", {
    tb <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalMetrics = TRUE)$results$results$clinicalApplicationMetrics
    note <- rel11_note(tb, "prevalence")
    expect_match(note, "PPV and NPV use the prevalence observed in these data (0.500)", fixed = TRUE)
    expect_match(note, "only if the sample is consecutive or cross-sectional; in a case-control or enriched sample", fixed = TRUE)
    # Bayes oracle: at the sample prevalence 0.5, PPV = 9/11 (Se 0.9, Sp 0.8)
    expect_equal(tb$asDF$ppv, 0.9 * 0.5 / (0.9 * 0.5 + 0.2 * 0.5), tolerance = 1e-12)
    typed <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalMetrics = TRUE,
                       useObservedPrevalence = FALSE, prevalence = 0.1)$results$results$clinicalApplicationMetrics
    expect_identical(rel11_note(typed, "prevalence"), "")
})

test_that("F-14: both Methods texts say how the direction was set", {
    auto_sentence <- "was chosen from these same data by comparing the two groups' medians, which biases the AUC upward."
    for (dir in c("auto", "higher", "lower")) {
        d <- rel11_k()
        if (dir == "lower") d$m <- -d$m   # read 'lower' the marker keeps AUC 0.88, so the report is written
        r <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", direction = dir,
                       comprehensive_output = TRUE)$results$results
        want <- switch(dir, auto = auto_sentence,
                       higher = "Higher values of each marker were taken to indicate the positive class, as specified in the analysis options",
                       lower = "Lower values of each marker were taken to indicate the positive class, as specified in the analysis options")
        for (txt in c(r$clinicalReport$content, r$methodsExplanation$content)) {
            expect_match(txt, want, fixed = TRUE)
            if (dir != "auto") expect_false(grepl(auto_sentence, txt, fixed = TRUE))
        }
    }
})

test_that("F-15 / F-37: the null-marker figures quoted under Direction = auto match the median rule", {
    # Independent oracle: pROC's auto rule ('<' iff median(controls) <= median(cases)) and a rank AUC, 10 + 10 null.
    set.seed(15)
    sim <- replicate(4000, {
        x <- rnorm(20); cases <- x[1:10]; controls <- x[11:20]
        auc <- (sum(rank(x)[1:10]) - 55) / 100
        if (median(controls) <= median(cases)) auc else 1 - auc
    })
    expect_lt(abs(mean(sim < 0.5) - 1 / 9), 3 * sqrt((1 / 9) * (8 / 9) / 4000) + 0.01)   # 'about 1 in 9'
    expect_lt(mean(sim), 0.61)                                                             # 'on average ... up to about 0.61'
    expect_equal(pwilcox(49, 10, 10), 0.485256229838, tolerance = 1e-9)                    # fixed direction: 'about half'
    # the module: a marker below 0.5 under auto (median rule picks '<', AUC 0.30)
    d <- data.frame(y = factor(rep(c("Benign", "Malignant"), each = 10), levels = c("Benign", "Malignant")),
                    m = c(1:5, 100:104, 0.5, 0.6, 0.7, 0.8, 53:58))
    a <- rel11_obj(d, outcome = "y", positiveClass = "Malignant", predictors = "m", direction = "auto", clinical_interpretation = TRUE)
    below <- rel11_text(a, "AUC Below Chance: m")
    expect_match(below, "about 1 in 9 at 10 positive and 10 negative patients, rising toward 1 in 6 in large samples", fixed = TRUE)
    expect_false(grepl("About half", below, fixed = TRUE))
    guide <- a$results$results$clinicalInterpretationGuide$content
    expect_match(guide, "lands here about 1 in 9 times at 10 positive and 10 negative patients", fixed = TRUE)
    expect_false(grepl("about half of markers", guide, fixed = TRUE))
    expect_match(rel11_text(a, "Direction Auto-Detected: m"), "would still be reported, on average, at an AUC of up to about 0.61", fixed = TRUE)
    # fixed direction keeps 'about half'
    set.seed(2)
    d2 <- data.frame(y = factor(rep(c("neg", "pos"), each = 20)), m = rnorm(40))
    d2$m[d2$y == "pos"] <- d2$m[d2$y == "pos"] - 0.25
    expect_match(rel11_text(rel11_obj(d2, outcome = "y", positiveClass = "pos", predictors = "m"), "AUC Below Chance: m"),
                 "About half of markers carrying no information at all fall below 0.5 by chance", fixed = TRUE)
})

test_that("F-16: the internal-validation INFO no longer calls the optimism of a single marker minimal", {
    # Oracle: a null marker's out-of-sample AUC is 0.5, so its true optimism is E(apparent AUC) - 0.5, where the
    # apparent AUC orients the marker by the fitted slope's sign (the class-mean difference). 20 + 20.
    set.seed(16)
    opt <- mean(replicate(4000, {
        x <- rnorm(40); y <- rep(0:1, each = 20)
        s <- if (mean(x[y == 1]) >= mean(x[y == 0])) x else -x
        (sum(rank(s)[y == 1]) - 210) / 400
    })) - 0.5
    expect_lt(abs(opt - 0.07), 0.01)
    a <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m", internalValidation = TRUE,
                   validationMethod = "bootstrap", seed = 1)
    info <- rel11_text(a, "Internal Validation Note")
    expect_false(grepl("is minimal", info, fixed = TRUE))
    expect_match(info, "about 0.07 for a marker carrying no information with 20 positive and 20 negative patients", fixed = TRUE)
})

test_that("F-17: imbalance texts no longer call PRC 'more reliable' or blame specificity on the majority class", {
    set.seed(2)
    d <- data.frame(y = factor(rep(c("neg", "pos"), c(10, 40)), levels = c("neg", "pos")), m = c(rnorm(10), rnorm(40, .8)))
    a <- rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "m", showImbalanceWarning = TRUE, recommendPRC = TRUE, detectImbalance = TRUE)
    txt <- rel11_text(a, "Class Imbalance Detected")
    for (bad in c("instead of ROC", "more reliable", "dominated by the majority class", "may be optimistic"))
        expect_false(grepl(bad, txt, fixed = TRUE), label = bad)
    expect_match(txt, "do not depend on the class proportions", fixed = TRUE)
    expect_false(grepl("instead of ROC", a$results$results$imbalanceMetrics$asDF$recommendation, fixed = TRUE))
    # Oracle for 'do not depend': five copies of every negative leave the AUC unchanged.
    d5 <- rbind(d, d[rep(which(d$y == "neg"), 4), ])
    auc <- function(x) rel11_run(x, outcome = "y", positiveClass = "pos", predictors = "m")$results$aucSummary$asDF$auc
    expect_equal(auc(d5), auc(d), tolerance = 1e-12)
    # Extreme prevalence: PPV 'low', not 'unreliable'; ROC not 'misleading'
    set.seed(3)
    ep <- data.frame(y = factor(rep(c("neg", "pos"), c(194, 6)), levels = c("neg", "pos")), m = c(rnorm(194), rnorm(6, 1.5)))
    ext <- rel11_text(rel11_obj(ep, outcome = "y", positiveClass = "pos", predictors = "m"), "Extreme Prevalence")
    expect_match(ext, "the PPV will be low unless specificity is very high", fixed = TRUE)
    expect_false(grepl("unreliable", ext, fixed = TRUE))
    expect_false(grepl("may be misleading", ext, fixed = TRUE))
})

test_that("F-18: the Performance Summary restates the Youden index without unsourced band words", {
    tb <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m")$results$results$optimalCutoffSummary
    expect_identical(tb$asDF$clinical_recommendation, "Youden index 0.70 at this cutpoint (sensitivity 0.90, specificity 0.80).")
})

test_that("F-19 / F-42 / F-43: the instructions drop survival status, the 'healthy' NPV and 'Full statistical analysis'", {
    ins <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m")$results$results$instructions$content
    for (bad in c("Survival Status", "DOD, DOOC", "patient is healthy", "Full statistical analysis", "automatically configure"))
        expect_false(grepl(bad, ins, fixed = TRUE), label = bad)
    expect_match(ins, "or a time-dependent ROC analysis for censored survival data", fixed = TRUE)
    expect_match(ins, "the probability that the patient does not have the condition named as the positive class", fixed = TRUE)
    expect_match(ins, "keeps the minimum sensitivity and specificity you entered", fixed = TRUE)
    # Research Comprehensive keeps the typed minimum: the same cutoff as Custom with Youden on.
    cut <- function(preset) rel11_run(rel11_inc(), outcome = "y", positiveClass = "Malignant", predictors = "m", clinicalPresets = preset,
                                      sensitivityThreshold = 0.9)$results$optimalCutoffSummary$asDF$optimal_cutoff
    expect_identical(cut("research_comprehensive"), cut("custom"))
})

test_that("F-20 / F-47: the frozen panel marks defaults as defaults and states the declared unit", {
    ctx <- rel11_frozen(analysisUnit = "patient")
    v <- ctx$results$results$validationContext$asDF
    expect_identical(v$value[v$setting == "Sampling design"], "Other / unknown (default setting)")  # default changed to Other / unknown (F-20); labels, not keys (release review)
    expect_identical(v$value[v$setting == "Threshold source"], "Prespecified independently (default setting)")
    expect_identical(v$value[v$setting == "Cohort role (declared, not verified)"], "Frozen external / temporal evaluation (default setting)")
    expect_match(v$value[v$setting == "Interpretation"], "Every analysed row (patient) weighs equally", fixed = TRUE)
    note <- rel11_note(ctx$results$results$frozenMetrics, "scope")
    expect_match(note, "Counts and metrics weight every analysed row equally (unit of observation: patient).", fixed = TRUE)
    for (txt in c(note, v$value)) expect_false(grepl("slide", txt, ignore.case = TRUE))
    set_by_user <- rel11_frozen(samplingDesign = "enriched")$results$results$validationContext$asDF
    expect_identical(set_by_user$value[set_by_user$setting == "Sampling design"], "Enriched")
})

test_that("F-22 / F-38: the DeLong WARNING says its coverages are design-level averages for equal-variance binormal data", {
    txt <- rel11_text(rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m"),
                      "DeLong confidence intervals may be too narrow")
    expect_match(txt, "which drew equal-variance binormal data", fixed = TRUE)
    expect_match(txt, "these are averages over all datasets of each size, and with unequal variances or skewed data they were about 90%, 84% and 92%.", fixed = TRUE)
})

test_that("F-36: a 'Marker Reads Backwards' ERROR from a small-sample DeLong interval says it fires more often than its level", {
    set.seed(1)
    d <- data.frame(y = factor(rep(c("Healthy", "Disease"), each = 12), levels = c("Healthy", "Disease")), m = c(rnorm(12), rnorm(12, 1.5)))
    txt <- rel11_text(rel11_obj(d, outcome = "y", positiveClass = "Healthy", predictors = "m"), "Marker Reads Backwards: m")
    expect_match(txt, "With 12 patients in the smaller group DeLong's interval is approximate", fixed = TRUE)
    expect_match(txt, "about 3.6 in 100 such markers at 10 positive and 10 negative patients", fixed = TRUE)
    set.seed(1)
    big <- data.frame(y = factor(rep(c("Healthy", "Disease"), each = 40), levels = c("Healthy", "Disease")), m = c(rnorm(40), rnorm(40, 1.5)))
    txt_big <- rel11_text(rel11_obj(big, outcome = "y", positiveClass = "Healthy", predictors = "m"), "Marker Reads Backwards: m")
    expect_true(nzchar(txt_big))
    expect_false(grepl("3.6 in 100", txt_big, fixed = TRUE))
})

test_that("F-39: 'Small Sample Size' no longer promises bootstrap 'more stable estimates'", {
    run <- function(boot) rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m",
                                    useBootstrap = boot, bootstrapSamples = 200, seed = 1)
    a <- run(FALSE)
    txt <- rel11_text(a, "Small Sample Size")
    expect_false(grepl("more stable estimates", txt, fixed = TRUE))
    expect_match(txt, "no interval method is exact at this size", fixed = TRUE)
    # the bootstrap changes the limits, never the AUC (Mann-Whitney 0.88)
    expect_equal(run(TRUE)$results$results$aucSummary$asDF$auc, a$results$results$aucSummary$asDF$auc, tolerance = 1e-15)
    expect_equal(a$results$results$aucSummary$asDF$auc, 0.88, tolerance = 1e-12)
})

test_that("F-41: with internal validation on, the copy-ready interpretation does not deny it", {
    rep_iv <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m", internalValidation = TRUE,
                        validationMethod = "bootstrap", seed = 1)$results$results$clinicalReport$content
    expect_false(grepl("no internal or external validation", rep_iv, fixed = TRUE))
    expect_match(rep_iv, "no external validation behind it; the Internal Validation section reports its internal validation.", fixed = TRUE)
    rep_off <- rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m")$results$results$clinicalReport$content
    expect_match(rep_off, "This is an in-sample estimate with no internal or external validation behind it.", fixed = TRUE)
})

test_that("F-44: 'Limited Diagnostic Performance' carries the bands caveat and no model-building advice", {
    txt <- rel11_text(rel11_obj(rel11_inc(), outcome = "y", positiveClass = "Malignant", predictors = "m"), "Limited Diagnostic Performance: m")
    for (bad in c("interaction terms", "multivariate", "Adding predictor")) expect_false(grepl(bad, txt, fixed = TRUE), label = bad)
    expect_match(txt, "These bands are a rough teaching scale, not a clinical standard", fixed = TRUE)
    expect_match(txt, "The confidence interval includes 0.5, so these data are compatible with no discrimination.", fixed = TRUE)
})

test_that("F-45: the screening prevalence note tests the positive-class share", {
    note <- function(n_neg, n_pos) {
        set.seed(5)
        d <- data.frame(y = factor(rep(c("neg", "pos"), c(n_neg, n_pos)), levels = c("neg", "pos")), m = c(rnorm(n_neg), rnorm(n_pos, 1)))
        # a notice since the 2026-10-11 production pass (was HTML in the instructions panel)
        grepl("High disease prevalence", rel11_text(rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "m",
                                                              clinicalContext = "screening"), "Screening Context Note"), fixed = TRUE)
    }
    expect_true(note(8, 32))    # 80% positive: silent before (it tested the smaller group's 20%)
    expect_true(note(24, 16))   # 40%
    expect_false(note(32, 8))   # 20%
})

test_that("F-46: no 'consider transformation' note; a monotone transform leaves the AUC unchanged", {
    set.seed(6)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 25), levels = c("neg", "pos")), m = c(rexp(25)^3, rexp(25, .5)^3))
    r <- rel11_run(d, outcome = "y", positiveClass = "pos", predictors = "m")$results
    expect_false(grepl("transformation", r$instructions$content, fixed = TRUE))
    d$m <- log(d$m)
    expect_equal(rel11_run(d, outcome = "y", positiveClass = "pos", predictors = "m")$results$aucSummary$asDF$auc,
                 r$aucSummary$asDF$auc, tolerance = 1e-15)
})

test_that("F-53: the Methods give the cited sources' own conclusions", {
    me <- function(...) rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m",
                                  comprehensive_output = TRUE, ...)$results$results$methodsExplanation$content
    expect_match(me(), "this empirical estimator, the most commonly used, had the worst overall bias and root mean square error", fixed = TRUE)
    expect_match(me(youdenOptimization = FALSE), "only the Youden index is optimal with respect to the overall misclassification rate", fixed = TRUE)
})

test_that("FIX-B nit: the single-case bootstrap line does not call DeLong's standard error 0", {
    d <- data.frame(y = factor(c("pos", rep("neg", 23)), levels = c("neg", "pos")), x = c(2, seq(-2, 2, length.out = 23)))
    html <- rel11_iv(rel11_obj(d, outcome = "y", positiveClass = "pos", predictors = "x", internalValidation = TRUE,
                               validationMethod = "bootstrap", seed = 1))
    expect_match(html, "no DeLong interval, its standard error is 0 or undefined", fixed = TRUE)
})

# ---- Security audit 2026-10-11: syntax pane, user text in cells, Methods partial range ----

test_that("SEC-1: a quote in free text or a level cannot inject code into asSource()", {
    payload <- 'v1", seed = {stop("injected")}, referenceDescription = "'
    d <- data.frame(y = factor(rep(c("neg", 'p"os'), each = 15), levels = c("neg", 'p"os')), x = c(1:15, 6:20))
    src <- enhancedROCClass$new(options = enhancedROCOptions$new(outcome = "y", positiveClass = 'p"os',
        predictors = "x", modelVersion = payload), data = d)$asSource()
    args <- as.list(str2lang(src))
    expect_identical(args$modelVersion, payload)
    expect_identical(args$positiveClass, 'p"os')
    expect_false("seed" %in% names(args))
})

test_that("SEC-2: user text in frozen and multi-class cells is not read as markup", {
    v <- rel11_frozen(referenceDescription = "<Histology> review")$results$results$validationContext$asDF
    expect_identical(v$value[v$setting == "Reference standard"], "< Histology> review")
    d <- data.frame(y = factor(rep(c("<LOD", "low", "high"), each = 10)), x = c(1:10, 11:20, 21:30))
    mc <- rel11_obj(d, outcome = "y", predictors = "x", multiClassROC = TRUE)$results$results$multiClassAUC$asDF
    expect_true("< LOD" %in% mc$class)
})

test_that("SEC-3: the Methods quote the partial AUC range only when it parses", {
    me <- function(...) rel11_obj(rel11_k(), outcome = "y", positiveClass = "Malignant", predictors = "m",
                                  comprehensive_output = TRUE, partialAuc = TRUE, ...)$results$results$methodsExplanation$content
    expect_match(me(), "from 0.8 to 1.0", fixed = TRUE)
    expect_false(grepl("NA to NA", me(partialRange = ""), fixed = TRUE))
})

test_that("multi-class orientation notes survive with table notes off (2026-10-21)", {
    # A perfect A > B > C marker read against the default Direction shows one-vs-one AUC 0.000
    # "Below chance"; the notes saying why were stripped because showTableNotes defaults FALSE.
    set.seed(4); g <- sample(c("A", "B", "C"), 150, TRUE)
    d <- data.frame(st3 = factor(g), x = rnorm(150, (g == "A") * 6 + (g == "B") * 3, 0.5))
    ovr <- rel11_run(d, outcome = "st3", positiveClass = "A", predictors = "x", multiClassROC = TRUE,
                     showTableNotes = FALSE)
    expect_match(rel11_note(ovr$results$multiClassAverage, "ovr_average"), "not a summary of discrimination", fixed = TRUE)
    expect_true(nzchar(rel11_note(ovr$results$multiClassAUC, "ovr_direction")))
    ovo <- rel11_run(d, outcome = "st3", positiveClass = "A", predictors = "x", multiClassROC = TRUE,
                     multiClassStrategy = "ovo", showTableNotes = FALSE)
    expect_true(nzchar(rel11_note(ovo$results$multiClassAUC, "ovo_direction")))
    expect_true(nzchar(rel11_note(ovo$results$multiClassAUC, "pair_order")))
})
