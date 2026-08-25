# Release-review pass: regression cover for the second tier of confirmed defects.

library(testthat)

rl_run <- function(...) suppressWarnings(do.call(ClinicoPath::enhancedROC, list(...)))
rl_strip <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", if (is.null(x)) "" else x)))

rl_data <- function(seed = 21, n = 240) {
    set.seed(seed)
    g <- rbinom(n, 1, 0.38)
    data.frame(gold = factor(ifelse(g == 1, "Positive", "Negative"), levels = c("Negative", "Positive")),
               m1 = g * 1.4 + rnorm(n), m2 = g * 0.7 + rnorm(n))
}

# ── H-4  Do not shadow jmvcore::Analysis's own private .data field ────────────

test_that("the analysis frame is not stored in jmvcore's private .data slot", {
    # jmvcore::Analysis owns `.data` (self$data is an active binding onto it) and
    # Analysis$run() clears it when the caller did not supply the frame - the GUI path. A
    # subclass field of the same name therefore vanishes between .run() and the renderers,
    # which read it after run() has returned.
    src <- readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE)
    expect_false(any(grepl("^\\s*\\.data\\s*=\\s*NULL", src)))
    expect_true(any(grepl("^\\s*\\.analysisData\\s*=\\s*NULL", src)))
    expect_false(any(grepl("private\\$\\.data([^A-Za-z0-9_.]|$)", src)))
})

test_that("every table still populates after the rename", {
    d <- rl_data()
    r <- rl_run(data = d, outcome = "gold", predictors = c("m1", "m2"), positiveClass = "Positive",
                cutoffTable = TRUE, clinicalMetrics = TRUE, clinicalImpact = TRUE,
                calibrationAnalysis = TRUE, calibrationMetrics = TRUE, brierScore = TRUE,
                hosmerLemeshow = TRUE, detectImbalance = TRUE, partialAuc = TRUE,
                crocAnalysis = TRUE, convexHull = TRUE)
    for (tbl in c("aucSummary", "optimalCutoffSummary", "cutoffAnalysis", "diagnosticPerformance",
                  "clinicalApplicationMetrics", "clinicalImpactTable", "calibrationSummary",
                  "hosmerLemeshowTable", "precisionRecallTable", "partialAucAnalysis",
                  "crocAnalysisTable", "convexHullTable")) {
        expect_gt(nrow(as.data.frame(r$results[[tbl]])), 0, label = tbl)
    }
})

test_that("all eleven renderers still draw", {
    d <- rl_data()
    r <- rl_run(data = d, outcome = "gold", predictors = c("m1", "m2"), positiveClass = "Positive",
                analysisType = "comparative", cutoffTable = TRUE, clinicalImpact = TRUE,
                clinicalUtilityCurve = TRUE, calibrationAnalysis = TRUE, calibrationPlot = TRUE,
                crocAnalysis = TRUE, convexHull = TRUE, detectImbalance = TRUE,
                recommendPRC = TRUE, statisticalComparison = TRUE)
    for (im in c("rocCurvePlot", "prcPlot", "comparativeROCPlot", "cutoffAnalysisPlot",
                 "youdenIndexPlot", "clinicalDecisionPlot", "crocCurvePlot", "convexHullPlot",
                 "calibrationPlotImage", "multiClassROCPlot", "clinicalUtilityPlot")) {
        f <- tempfile(fileext = ".png")
        grDevices::png(f, width = 600, height = 600)
        out <- tryCatch({ r$results[[im]]$.render(); TRUE }, error = function(e) conditionMessage(e))
        grDevices::dev.off()
        expect_true(isTRUE(out), label = paste(im, "->", out))
    }
})

# ── H-8  Stale multi-class outcome ────────────────────────────────────────────

test_that("the multi-class outcome is cleared at the top of every run", {
    # Assigned only on the >2-level branch of .prepareData() and never cleared, so switching to
    # a two-level outcome left the multi-class tables computing on the previous variable.
    src <- paste(readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE), collapse = "\n")
    expect_match(src, "private\\$\\.multiClassOutcome <- NULL")
})

# ── H-9  Validation must not fire for a switched-off feature ──────────────────

test_that("a malformed partial-AUC range does not abort when partial AUC is off", {
    d <- rl_data()
    r <- rl_run(data = d, outcome = "gold", predictors = "m1", positiveClass = "Positive",
                partialAuc = FALSE, partialRange = "not,numbers")
    expect_equal(nrow(as.data.frame(r$results$aucSummary)), 1L)
})

test_that("...but it is still reported when partial AUC is on", {
    d <- rl_data()
    r <- rl_run(data = d, outcome = "gold", predictors = "m1", positiveClass = "Positive",
                partialAuc = TRUE, partialRange = "not,numbers")
    expect_match(rl_strip(r$results$notices$content), "Invalid Partial AUC Range")
})

# ── H-7  An infinite likelihood ratio is not a number ─────────────────────────

test_that("infinite LR+/LR-/DOR are blank, not the literal 9999", {
    set.seed(1)
    g <- rbinom(100, 1, 0.5)
    d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")),
                    sep = ifelse(g == 1, rnorm(100, 10, 0.1), rnorm(100, 0, 0.1)))
    cm <- as.data.frame(rl_run(data = d, outcome = "y", predictors = "sep", positiveClass = "P",
                               clinicalMetrics = TRUE)$results$clinicalApplicationMetrics)
    num <- suppressWarnings(as.numeric(unlist(cm[, intersect(c("lr_positive", "lr_negative", "dor"), names(cm))])))
    expect_false(any(num == 9999, na.rm = TRUE))
    src <- paste(readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE), collapse = "\n")
    expect_false(grepl("<- 9999", src, fixed = TRUE))
})

# ── H-5  A preset must not silently overrule visible controls ─────────────────

test_that("a clinical preset announces that it overrode the threshold boxes", {
    d <- rl_data()
    r <- rl_run(data = d, outcome = "gold", predictors = "m1", positiveClass = "Positive",
                clinicalPresets = "confirmatory_testing",
                sensitivityThreshold = 0.1, specificityThreshold = 0.1)
    expect_match(rl_strip(r$results$notices$content), "Clinical Preset Overrode Your Settings")
})

test_that("the custom preset raises no override notice", {
    d <- rl_data()
    r <- rl_run(data = d, outcome = "gold", predictors = "m1", positiveClass = "Positive",
                clinicalPresets = "custom")
    expect_false(grepl("Clinical Preset Overrode", rl_strip(r$results$notices$content), fixed = TRUE))
})

# ── Theme safety ──────────────────────────────────────────────────────────────

test_that("no opaque white panel survives in the clinical report", {
    # White-on-white in jamovi's dark theme.
    src <- paste(readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE), collapse = "\n")
    expect_false(grepl("background-color: white", src, fixed = TRUE))
})
