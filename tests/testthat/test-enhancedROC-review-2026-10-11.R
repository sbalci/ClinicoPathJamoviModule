# /review-function pass 2026-10-11 (TODO.md, "enhancedROC - /fix-function of the /review-function findings").
# One test_that per finding, each written to FAIL on the code before its fix.

rv_obj <- function(data, ...) {
    options <- do.call(enhancedROCOptions$new, list(...))
    analysis <- enhancedROCClass$new(options = options, data = data)
    suppressWarnings(analysis$run())
    analysis
}
rv_titled <- function(analysis, pattern) {
    Filter(function(n) grepl(pattern, n$title, fixed = TRUE), analysis$.__enclos_env__$private$.noticeList)
}
rv_cell <- function(table, col, rowKey) table$getCell(col, rowKey = rowKey)
rv_blank_with_reason <- function(table, col, rowKey) {
    cell <- rv_cell(table, col, rowKey)
    expect_identical(cell$value, "", info = paste(col, rowKey))
    expect_gt(length(cell$footnotes), 0L)
}
rv_asah <- function() {
    skip_if_not_installed("pROC")
    d <- pROC::aSAH
    d$wfns <- as.numeric(as.character(d$wfns))
    d
}
rv_perfect <- function() {
    y <- factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos"))
    data.frame(y = y, a = c(1:15, 101:115), b = c(seq(0.5, 7.5, 0.5), seq(50, 57, 0.5)))
}

test_that("R-01: a constrained cut-off is never the infinite 'test nobody / test everybody' threshold", {
    d <- rv_asah()
    # the highest finite wfns cut-point has specificity 68/72 = 0.944, so no real cut-point meets 0.95
    a <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = "wfns", specificityThreshold = 0.95,
                showClinicalReport = TRUE)
    cut <- a$results$results$optimalCutoffSummary$asDF$optimal_cutoff
    expect_true(all(is.finite(cut)))
    expect_length(rv_titled(a, "Constraints Not Met"), 1L)
    expect_length(rv_titled(a, "Clinical Report Withheld"), 0L)
    # a minimum that finite cut-points do meet still constrains the search
    b <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = "wfns", sensitivityThreshold = 0.9)
    row <- b$results$results$optimalCutoffSummary$asDF
    expect_true(is.finite(row$optimal_cutoff))
    expect_gte(row$sensitivity, 0.9)
})

test_that("R-02: a constant predictor gets no cut-point rows to contradict its ERROR", {
    set.seed(3)
    y <- factor(rep(c("neg", "pos"), each = 30), levels = c("neg", "pos"))
    d <- data.frame(y = y, a = rnorm(60, ifelse(y == "pos", 1.5, 0)), k = 5)
    a <- rv_obj(d, outcome = "y", positiveClass = "pos", predictors = c("a", "k"), clinicalMetrics = TRUE,
                cutoffTable = TRUE)
    r <- a$results$results
    expect_length(rv_titled(a, "Constant Predictor"), 1L)
    for (name in c("optimalCutoffSummary", "diagnosticPerformance", "clinicalApplicationMetrics", "cutoffAnalysis")) {
        df <- r[[name]]$asDF
        expect_false("k" %in% df$predictor, info = name)
        expect_true("a" %in% df$predictor, info = name)
    }
})

test_that("R-03: two perfect curves are not given pROC's hard-coded Z = 0, p = 1", {
    a <- rv_obj(rv_perfect(), outcome = "y", positiveClass = "pos", predictors = c("a", "b"),
                analysisType = "comparative", pairwiseComparisons = TRUE, statisticalComparison = TRUE,
                showMetricsDiff = TRUE, comparisonMethod = "delong")
    r <- a$results$results
    rv_blank_with_reason(r$rocComparisons, "p_value", "a_vs_b")
    rv_blank_with_reason(r$rocComparisons, "test_statistic", "a_vs_b")
    expect_false(grepl("significantly", rv_cell(r$rocComparisons, "result", "a_vs_b")$value, ignore.case = TRUE))
    rv_blank_with_reason(r$statisticalSummary, "p_value", "a_b_stat")
    expect_false(grepl("significant", rv_cell(r$statisticalSummary, "conclusion", "a_b_stat")$value, ignore.case = TRUE))
    expect_identical(rv_cell(r$detailedComparison, "p_value", "a_b_AUC")$value, "")
    expect_length(rv_titled(a, "Comparison Error"), 0L)
    # Venkatraman's permutation p for two identical curves is computed, not hard-coded: it stays
    v <- rv_obj(rv_perfect(), outcome = "y", positiveClass = "pos", predictors = c("a", "b"),
                analysisType = "comparative", pairwiseComparisons = TRUE, comparisonMethod = "venkatraman",
                bootstrapSamples = 100)
    expect_true(is.numeric(rv_cell(v$results$results$rocComparisons, "p_value", "a_vs_b")$value))
})

test_that("R-04: an undefined impact or workload ratio is a blank cell with its reason, never NaN", {
    a <- rv_obj(rv_asah(), outcome = "outcome", positiveClass = "Poor", predictors = "s100b",
                clinicalImpact = TRUE, impactCutoffs = "0.5, 100", nntCalculation = TRUE,
                decisionImpactTable = TRUE, showTableNotes = TRUE)
    r <- a$results$results
    # at 100 nobody is flagged: net benefit 0, se + sp = 1, no positive call, no true positive
    rv_blank_with_reason(r$clinicalImpactTable, "nnt", "s100b_100")
    rv_blank_with_reason(r$clinicalImpactTable, "nnd", "s100b_100")
    rv_blank_with_reason(r$decisionImpactSummary, "ppv", "s100b_100")
    for (col in c("ppv", "flags_per_detected", "tests_per_detected")) {
        rv_blank_with_reason(r$screeningWorkload, col, "s100b_100_1")
    }
    # defined values are untouched
    expect_true(is.numeric(rv_cell(r$clinicalImpactTable, "nnt", "s100b_0.5")$value))
})

test_that("R-05: with table notes off, a blank cell keeps the footnote that says why it is blank", {
    set.seed(5)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 40)),
                    m = c(runif(36), rep(10, 4), runif(40, 0.5, 1.5)))
    a <- rv_obj(d, outcome = "y", positiveClass = "pos", predictors = "m", partialAuc = TRUE,
                partialRange = "0.9,1.0", partialAucType = "specificity", showTableNotes = FALSE)
    cell <- a$results$results$partialAucAnalysis$getCell(rowNo = 1, "normalized_pauc")
    expect_identical(cell$value, "")
    expect_gt(length(cell$footnotes), 0L)
    # a filled cell's footnote still goes with the notes
    expect_length(names(a$results$results$partialAucAnalysis$notes), 0L)
})

test_that("R-06: a run that ends with no notices hides the panel instead of leaving the last run's", {
    d <- rv_asah()[, c("outcome", "s100b")]
    a <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = "s100b", validationPanel = TRUE,
                validationNegative = "Good", direction = "higher", modelVersion = "v1", impactCutoffs = "0.2")
    n <- a$results$results$notices
    expect_identical(n$content, "")
    expect_false(n$visible)
    b <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = "s100b")
    expect_true(b$results$results$notices$visible)
})

test_that("R-07: 'Rows Excluded' never claims rows were analysed when nothing was computed", {
    d <- rv_asah()[, c("outcome", "s100b")]
    d$s100b[1:5] <- NA
    a <- rv_obj(d, outcome = "outcome", predictors = "s100b")          # no positive class yet
    expect_length(rv_titled(a, "Choose the positive class"), 1L)
    n <- rv_titled(a, "Rows Excluded")
    expect_length(n, 1L)                                                 # still worth knowing before choosing
    expect_false(grepl("were analysed", n[[1]]$content, fixed = TRUE))
    expect_match(n[[1]]$content, "108 of 113 rows can be analysed", fixed = TRUE)
})

test_that("R-08: every seed-dependent table shows its seed", {
    d <- rv_asah()
    a <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = c("s100b", "ndka"),
                analysisType = "comparative", showMetricsDiff = TRUE, comparisonMethod = "bootstrap",
                bootstrapSamples = 100, seed = 11)
    expect_true("seed" %in% names(a$results$results$detailedComparison$notes))
    dl <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = c("s100b", "ndka"),
                 analysisType = "comparative", showMetricsDiff = TRUE, comparisonMethod = "delong")
    expect_false("seed" %in% names(dl$results$results$detailedComparison$notes))
    f <- rv_obj(d[, c("outcome", "s100b")], outcome = "outcome", positiveClass = "Poor", predictors = "s100b",
                validationPanel = TRUE, validationNegative = "Good", direction = "higher", modelVersion = "v1",
                impactCutoffs = "0.2", validationBootstrap = TRUE, validationDraws = 100, seed = 11)
    expect_match(f$results$results$frozenMetrics$notes$seed$note, "11", fixed = TRUE)
})

test_that("R-09: the Methods text does not claim more Hosmer-Lemeshow groups than were used", {
    d <- rv_asah()
    a <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = "wfns", comprehensive_output = TRUE,
                calibrationAnalysis = TRUE, hosmerLemeshow = TRUE, hlGroups = 10)
    txt <- a$results$results$methodsExplanation$content
    expect_false(grepl("used 10 groups", txt, fixed = TRUE))
    expect_match(txt, "up to 10 groups", fixed = TRUE)
})

test_that("R-10: Analysis Summary and Clinical Report are opt-in", {
    d <- rv_asah()
    base <- list(d, outcome = "outcome", positiveClass = "Poor", predictors = "s100b")
    r <- do.call(rv_obj, base)$results$results
    expect_identical(r$analysisSummary$content, "")
    expect_identical(r$clinicalReport$content, "")
    expect_false(r$analysisSummary$visible)
    expect_false(r$clinicalReport$visible)
    on <- do.call(rv_obj, c(base, showAnalysisSummary = TRUE, showClinicalReport = TRUE))$results$results
    expect_true(nzchar(on$analysisSummary$content))
    expect_true(nzchar(on$clinicalReport$content))
    expect_true(on$analysisSummary$visible)
    expect_true(on$clinicalReport$visible)
    # internal validation writes into the summary panel, so it shows that panel on its own
    iv <- do.call(rv_obj, c(base, internalValidation = TRUE, validationMethod = "cv"))$results$results
    expect_true(iv$analysisSummary$visible)
    expect_match(iv$analysisSummary$content, "Internal Validation", fixed = TRUE)
})

test_that("R-11: the imbalance threshold and PR recommendation stay live (the check always runs)", {
    path <- file.path(testthat::test_path("..", ".."), "jamovi", "enhancedROC.u.yaml")
    skip_if_not(file.exists(path))
    ui <- yaml::read_yaml(path)
    find <- function(node, name) {
        if (is.list(node)) {
            if (identical(node$name, name)) return(node)
            for (child in node) { hit <- find(child, name); if (!is.null(hit)) return(hit) }
        }
        NULL
    }
    expect_null(find(ui, "imbalanceThreshold")$enable)
    expect_null(find(ui, "recommendPRC")$enable)
    # and the option still acts with detection switched off
    a <- rv_obj(rv_asah(), outcome = "outcome", positiveClass = "Poor", predictors = "s100b",
                detectImbalance = FALSE, imbalanceThreshold = 1.5)
    expect_length(rv_titled(a, "Class Imbalance"), 1L)
})

test_that("R-12: the comparison, CROC and hull plots follow the plot theme (no forced minor grid)", {
    d <- rv_asah()
    a <- rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = c("wfns", "s100b"),
                direction = "higher", analysisType = "comparative", crocAnalysis = TRUE, convexHull = TRUE,
                plotTheme = "classic")
    out <- tempfile(fileext = ".png")
    on.exit(unlink(out), add = TRUE)
    for (nm in c("comparativeROCPlot", "crocCurvePlot", "convexHullPlot")) {
        a$results$results[[nm]]$saveAs(out)
        p <- ggplot2::last_plot()
        g <- ggplot2::calc_element("panel.grid.minor", ggplot2::theme_get() + p$theme)
        expect_s3_class(g, "element_blank")
    }
    a$results$results$comparativeROCPlot$saveAs(out)
    text <- Filter(function(l) inherits(l$geom, "GeomText"), ggplot2::last_plot()$layers)
    expect_false(any(vapply(text, function(l) identical(l$aes_params$colour, "darkblue"), logical(1))))
})

test_that("R-13: the error handler no longer pins the call stack in a package-global log", {
    entry <- utils::capture.output(res <- clinicopath_error_handler(simpleError("x"), "enhancedROC"))
    expect_null(res$detailed_log$call_stack)
    expect_null(res$detailed_log$session_info)
    w <- utils::capture.output(wres <- clinicopath_warning_handler(simpleWarning("y"), "enhancedROC"))
    expect_null(wres$detailed_log$call_stack)
})

# --- /release-review-function pass (2026-10-11) ---

rv_frozen <- function(...) {
    d <- rv_asah()[, c("outcome", "s100b", "ndka")]
    rv_obj(d, outcome = "outcome", positiveClass = "Poor", predictors = c("s100b", "ndka"), validationPanel = TRUE,
           validationNegative = "Good", direction = "higher", modelVersion = "v1", impactCutoffs = "0.2", ...)
}

test_that("R-14: the frozen panel's AUC-PR is the interpolated area, as on the main path", {
    # tied scores: the PR-space trapezoid reads 0.75 for a marker with no information
    expect_equal(.dvAuprc(rep(1, 4), c(0, 0, 1, 1), 1), 0.5)
    expect_equal(.dvPR(rep(1, 4), c(0, 0, 1, 1), 1)$auprc, 0.75)          # shared helper unchanged
    expect_equal(.dvScoreMetrics(c(0, 0, 1, 1), rep(1, 4), 0.5)[["pr_auc"]], 0.5)
    skip_if_not_installed("PRROC")
    d <- rv_asah()
    m <- rv_frozen()$results$results$frozenMetrics$asDF
    pr_auc <- m$estimate[m$model == "s100b" & m$site == "All" & m$metric == "AUC-PR (interpolated)"]
    oracle <- PRROC::pr.curve(scores.class0 = d$s100b[d$outcome == "Poor"],
                              scores.class1 = d$s100b[d$outcome == "Good"])$auc.integral
    expect_equal(pr_auc, oracle, tolerance = 1e-10)
})

test_that("R-15: frozen tables show labels, not internal keys", {
    r <- rv_frozen()$results$results
    m <- r$frozenMetrics$asDF
    expect_false(any(c("pr_auc", "flags_per_1000", "calibration_in_large", "ap") %in% m$metric))
    expect_true(all(c("AUC-PR (interpolated)", "Flagged per 1000", "Calibration-in-the-large") %in% m$metric))
    expect_true("ndka minus s100b" %in% m$model)
    expect_true("CI not requested" %in% m$status)
    ctx <- r$validationContext$asDF
    value <- function(s) ctx$value[ctx$setting == s]
    expect_identical(value("Cohort role (declared, not verified)"), "Frozen external / temporal evaluation (default setting)")
    expect_identical(value("Prediction scale"), "Score")
    expect_identical(value("Reference standard"), "Not stated")
    expect_true("Analyzed common cases" %in% r$denominatorLedger$asDF$status)
})

test_that("R-16: Calibration analysis with no measure ticked says so", {
    base <- list(rv_asah(), outcome = "outcome", positiveClass = "Poor", predictors = "s100b", calibrationAnalysis = TRUE)
    expect_length(rv_titled(do.call(rv_obj, base), "No Calibration Measure Selected"), 1L)
    expect_length(rv_titled(do.call(rv_obj, c(base, brierScore = TRUE)), "No Calibration Measure Selected"), 0L)
})
