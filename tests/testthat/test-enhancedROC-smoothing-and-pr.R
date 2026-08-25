# Regression cover for two defects that made enhancedROC report wrong or nothing at all.
#
# 1. smoothMethod: pROC::smooth() returns a "smooth.roc" that carries ZERO thresholds (against
#    ~500 sensitivity/specificity points) and does not inherit from "roc". The backend used to
#    overwrite the empirical ROC object with it, so every threshold-based calculation downstream
#    died with "all arguments must have the same length" -- and the failure went to the console
#    error handler, which jamovi never displays. Two of the option's three levels produced a
#    completely empty analysis with no message at all.
#
# 2. Precision/recall were read off index 2 of a vector with a prepended (0,1) sentinel, i.e. the
#    single top-ranked patient. Precision was therefore always exactly 1.0 and recall always
#    exactly 1/(number of positives), and neither could be reconciled with the F1 printed in the
#    same row.

library(testthat)

er_data <- function(seed = 9, n = 300) {
    set.seed(seed)
    g <- rbinom(n, 1, 0.34)
    data.frame(
        gold = factor(ifelse(g == 1, "Positive", "Negative"), levels = c("Negative", "Positive")),
        m1   = g * 1.4 + rnorm(n),
        m2   = g * 0.7 + rnorm(n)
    )
}

run_er <- function(...) {
    suppressWarnings(do.call(ClinicoPath::enhancedROC, utils::modifyList(
        list(data = er_data(), outcome = "gold", predictors = c("m1", "m2"),
             positiveClass = "Positive"),
        list(...))))
}

test_that("every smoothMethod level still produces a complete analysis", {
    for (sm in c("none", "binormal", "kernel")) {
        res <- run_er(smoothMethod = sm)
        auc <- as.data.frame(res$results$aucSummary)
        cut <- as.data.frame(res$results$optimalCutoffSummary)
        expect_equal(nrow(auc), 2, info = paste("aucSummary empty for smoothMethod =", sm))
        expect_equal(nrow(cut), 2, info = paste("no cut-points for smoothMethod =", sm))
    }
})

test_that("smoothing does not move a single reported statistic", {
    # It is a display choice. If smoothing ever changes an AUC, a cut-point or a CI, the
    # smoothed curve has leaked into the computation again.
    base <- as.data.frame(run_er(smoothMethod = "none")$results$aucSummary)
    for (sm in c("binormal", "kernel")) {
        got <- as.data.frame(run_er(smoothMethod = sm)$results$aucSummary)
        expect_equal(as.numeric(got$auc), as.numeric(base$auc), tolerance = 1e-12,
                     info = paste("smoothMethod =", sm, "changed the reported AUC"))
    }
})

test_that("smoothing does change the drawn curve", {
    # The complement of the test above: if the smoothed object is never drawn, the option is a
    # no-op and the user is being lied to by the UI.
    render_md5 <- function(sm) {
        res <- run_er(smoothMethod = sm)
        f <- tempfile(fileext = ".png")
        grDevices::png(f, width = 600, height = 600)
        try(res$results$rocCurvePlot$.render(), silent = TRUE)
        grDevices::dev.off()
        unname(tools::md5sum(f))
    }
    plain <- render_md5("none")
    expect_false(identical(plain, render_md5("binormal")))
    expect_false(identical(plain, render_md5("kernel")))
})

test_that("a smoothed ROC really does lack thresholds (the premise of the fix)", {
    skip_if_not_installed("pROC")
    set.seed(1)
    g <- rbinom(200, 1, 0.4)
    r <- pROC::roc(g, g + rnorm(200), quiet = TRUE)
    s <- pROC::smooth(r, method = "binormal")
    expect_gt(length(r$thresholds), 0)
    expect_equal(length(s$thresholds), 0)      # nothing to build a cut-point from
    expect_false(inherits(s, "roc"))           # so plot guards on inherits(., "roc") skip it
})

test_that("precision and recall reconcile with the F1 printed beside them", {
    pr <- as.data.frame(run_er(detectImbalance = TRUE)$results$precisionRecallTable)
    expect_gt(nrow(pr), 0)
    for (i in seq_len(nrow(pr))) {
        P <- as.numeric(pr$precision[i])
        R <- as.numeric(pr$recall[i])
        expect_equal(2 * P * R / (P + R), as.numeric(pr$f1_score[i]), tolerance = 1e-9,
                     info = paste("row", pr$predictor[i], "is internally inconsistent"))
    }
})

test_that("precision/recall are not the top-ranked observation", {
    # The exact signature of the old defect: precision pinned at 1.0 and recall at 1/n_positive.
    d <- er_data()
    n_pos <- sum(d$gold == "Positive")
    pr <- as.data.frame(run_er(detectImbalance = TRUE)$results$precisionRecallTable)
    expect_false(any(abs(as.numeric(pr$precision) - 1) < 1e-12))
    expect_false(any(abs(as.numeric(pr$recall) - 1 / n_pos) < 1e-12))
})

test_that("AUC-PR and average precision are not the same number twice", {
    # They were both computed as the step-wise sum, so one value was printed under two headings.
    pr <- as.data.frame(run_er(detectImbalance = TRUE)$results$precisionRecallTable)
    expect_false(isTRUE(all.equal(as.numeric(pr$auc_pr), as.numeric(pr$average_precision))))
    # ...but they must stay close; a large gap would mean one of them is wrong.
    expect_true(all(abs(as.numeric(pr$auc_pr) - as.numeric(pr$average_precision)) < 0.05))
})

# ── Options that were silently inert ──────────────────────────────────────────

test_that("bootstrapMethod actually changes the confidence interval", {
    # pROC::ci.auc() has no boot.ci.type argument, so the value used to be swallowed by its
    # `...` and all three settings returned byte-identical limits.
    skip_if_not_installed("boot")
    ci <- vapply(c("bca", "percentile", "basic"), function(bm) {
        a <- as.data.frame(run_er(useBootstrap = TRUE, bootstrapMethod = bm,
                                  bootstrapSamples = 300)$results$aucSummary)
        paste(round(as.numeric(a$auc_lower[1]), 8), round(as.numeric(a$auc_upper[1]), 8))
    }, character(1))
    expect_equal(length(unique(ci)), 3L)
})

test_that("a bootstrap AUC interval never leaves [0, 1]", {
    # The basic bootstrap reflects limits about the point estimate and can overshoot.
    skip_if_not_installed("boot")
    for (bm in c("bca", "percentile", "basic")) {
        a <- as.data.frame(run_er(useBootstrap = TRUE, bootstrapMethod = bm,
                                  bootstrapSamples = 300)$results$aucSummary)
        lo <- as.numeric(a$auc_lower); hi <- as.numeric(a$auc_upper)
        expect_true(all(lo >= 0 & lo <= 1, na.rm = TRUE), info = bm)
        expect_true(all(hi >= 0 & hi <= 1, na.rm = TRUE), info = bm)
    }
})

test_that("useObservedPrevalence reaches the clinical impact table", {
    # `self$options$prevalence %||% prevalence` never fell through - the option has an .a.yaml
    # default, so it is never NULL - and the checkbox therefore did nothing.
    off <- as.data.frame(run_er(clinicalImpact = TRUE, useObservedPrevalence = FALSE)$results$clinicalImpactTable)
    on  <- as.data.frame(run_er(clinicalImpact = TRUE, useObservedPrevalence = TRUE)$results$clinicalImpactTable)
    expect_false(isTRUE(all.equal(off, on)))
    # ticking the box must put the OBSERVED prevalence on the threshold
    expect_equal(as.numeric(on$threshold[1]), mean(er_data()$gold == "Positive"), tolerance = 1e-9)
})

test_that("the clinical impact table says where its threshold came from", {
    # NNT and net benefit move when the box is ticked; a threshold probability is a
    # harm-to-benefit exchange rate, not a base rate, so the source has to be stated.
    res <- run_er(clinicalImpact = TRUE, useObservedPrevalence = TRUE)
    notes <- paste(unlist(lapply(res$results$clinicalImpactTable$notes, as.character)), collapse = " ")
    expect_match(notes, "risk threshold", fixed = TRUE)
})

test_that("plotWidth and plotHeight reach the image", {
    res <- run_er(plotWidth = 1000, plotHeight = 400)
    sz <- res$results$rocCurvePlot$size
    expect_equal(sz$width, 1000)
    expect_equal(sz$height, 400)
})

test_that("plotTheme changes the rendered plot", {
    render_md5 <- function(th) {
        res <- run_er(plotTheme = th)
        f <- tempfile(fileext = ".png")
        grDevices::png(f, width = 600, height = 600)
        try(res$results$rocCurvePlot$.render(), silent = TRUE)
        grDevices::dev.off()
        unname(tools::md5sum(f))
    }
    expect_equal(length(unique(c(render_md5("clinical"), render_md5("classic"), render_md5("modern")))), 3L)
})

test_that("hlGroups drives the calibration plot, not just the H-L test", {
    render_md5 <- function(k) {
        res <- run_er(calibrationAnalysis = TRUE, calibrationPlot = TRUE, hlGroups = k)
        f <- tempfile(fileext = ".png")
        grDevices::png(f, width = 600, height = 600)
        try(res$results$calibrationPlotImage$.render(), silent = TRUE)
        grDevices::dev.off()
        unname(tools::md5sum(f))
    }
    expect_equal(length(unique(c(render_md5(5), render_md5(10), render_md5(20)))), 3L)
})

# ── Where errors and explanations are delivered ───────────────────────────────

test_that("a fatal validation error leaves the instructions panel intact", {
    # Ten validation failures used to setContent() the shared instructions item, replacing the
    # whole welcome/glossary panel with a bare error paragraph.
    res <- run_er(partialAuc = TRUE, partialRange = "not,numbers")
    expect_gt(nchar(res$results$instructions$content), 3000)
    expect_match(res$results$notices$content, "Invalid Partial AUC Range")
})

test_that("multi-class ROC on a binary outcome explains itself instead of going blank", {
    res <- run_er(multiClassROC = TRUE)
    # the notice fires from .run(), where notices actually render
    expect_match(res$results$notices$content, "3 or More Outcome Levels")
    # and the visible panel carries a drawn explanation rather than nothing
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 600, height = 600)
    drew <- try(res$results$multiClassROCPlot$.render(), silent = TRUE)
    grDevices::dev.off()
    expect_true(isTRUE(drew))
})

test_that("no renderer raises a notice, because a renderer cannot render one", {
    # .renderNotices() is only ever called from .run(). A .addNotice() inside a plot renderer
    # is unreachable by construction; renderers must draw on the canvas instead.
    src <- readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE)
    meth <- NA_character_
    offenders <- character(0)
    for (ln in src) {
        m <- regmatches(ln, regexec("^\\s*(\\.[A-Za-z0-9_]+)\\s*=\\s*function", ln))[[1]]
        if (length(m) == 2) meth <- m[2]
        if (grepl(".addNotice(", ln, fixed = TRUE) && !is.na(meth) && grepl("plot|Plot", meth)) {
            offenders <- c(offenders, meth)
        }
    }
    expect_equal(unique(offenders), character(0))
})

test_that("the 'Weighted AUC' column is not filled with an unweighted statistic", {
    # It used to carry the Hand-Till PAIRWISE AUC under a "Weighted" heading, at the default
    # setting, where the unimplemented-option guard never fires.
    src <- paste(readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE), collapse = "\n")
    expect_false(grepl("weighted_auc = mc_auc_val", src, fixed = TRUE))
})
