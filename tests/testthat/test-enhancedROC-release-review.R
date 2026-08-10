# Regression tests from the `enhancedROC` release review.
#
# Each case corresponds to a defect confirmed in the shipped code. The AUC figures are checked
# against pROC directly rather than against the module's own arithmetic.

er_data <- function(seed = 1, n = 200) {
    set.seed(seed)
    data.frame(
        status = factor(rep(c("neg", "pos"), each = n / 2), levels = c("neg", "pos")),
        m1  = c(rnorm(n / 2, 0), rnorm(n / 2, 1.2)),
        m2  = c(rnorm(n / 2, 0), rnorm(n / 2, 0.8)),
        bad = c(rnorm(n / 2, 0), rnorm(n / 2, -1.2)))   # inverted: true AUC well below 0.5
}

run_er <- function(...) {
    args <- utils::modifyList(
        list(outcome = "status", positiveClass = "pos"), list(...))
    do.call(ClinicoPath::enhancedROC, args)
}

notices_of <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$results$notices$content, collapse = " ")))

tnote_er <- function(res, key = "direction_used") {
    n <- res$results$aucSummary$.__enclos_env__$private$.notes
    if (!(key %in% names(n))) return("(none)")
    gsub("<[^>]+>", "", get("note", envir = n[[key]]))
}

er_private <- function(...) {
    o <- do.call(ClinicoPath:::enhancedROCOptions$new, list(...))
    a <- ClinicoPath:::enhancedROCClass$new(options = o, data = er_data())
    a$init()
    list(a = a, o = o, p = a$.__enclos_env__$private)
}


test_that("AUC matches pROC exactly under both directions", {
    skip_if_not_installed("pROC")
    d <- er_data()
    for (pred in c("m1", "bad")) for (dir in c("auto", "higher")) {
        got <- run_er(data = d, predictors = pred, direction = dir)$results$aucSummary$asDF$auc[1]
        want <- as.numeric(pROC::roc(d$status, d[[pred]], quiet = TRUE,
                                     direction = if (dir == "auto") "auto" else "<")$auc)
        expect_equal(got, want, tolerance = 1e-9, label = paste(pred, dir))
    }
})


test_that("tables do not accumulate rows across re-runs", {
    # 21 addRow() calls and not one deleteRows(). jamovi re-runs .run() on the SAME object
    # whenever an option changes, so every table doubled: aucSummary 2 -> 4 -> 6 and
    # cutoffAnalysis 32 -> 64 -> 96. From the second run on, $asDF failed outright with
    # "duplicate 'row.names' are not allowed".
    h <- er_private(outcome = "status", positiveClass = "pos", predictors = c("m1", "m2"),
                    analysisType = "comparative", pairwiseComparisons = TRUE,
                    cutoffTable = TRUE, optimalCutoffs = TRUE,
                    diagnosticMetrics = TRUE, clinicalMetrics = TRUE)
    tbls <- c("aucSummary", "optimalCutoffSummary", "cutoffAnalysis",
              "diagnosticPerformance", "clinicalApplicationMetrics", "rocComparisons")

    counts <- lapply(1:3, function(i) {
        h$p$.run()
        vapply(tbls, function(t) h$a$results$results[[t]]$rowCount, numeric(1))
    })
    expect_equal(counts[[2]], counts[[1]])
    expect_equal(counts[[3]], counts[[1]])
    expect_equal(unname(counts[[1]]["aucSummary"]), 2)
    expect_equal(unname(counts[[1]]["rocComparisons"]), 1)

    # and the tables stay readable
    expect_silent(invisible(h$a$results$results$aucSummary$asDF))
    expect_equal(nrow(h$a$results$results$aucSummary$asDF), 2L)
})


test_that("bootstrap confidence intervals are reproducible", {
    # Nothing in 4,500 lines called set.seed(), so bootstrap AUC intervals, bootstrap ROC
    # comparisons, internal-validation resamples and CV folds all changed on every run --
    # the same data and options gave a different confidence interval each time.
    d <- er_data()
    ci <- function() {
        x <- run_er(data = d, predictors = "m1", useBootstrap = TRUE,
                    bootstrapSamples = 200)$results$aucSummary$asDF
        c(x$auc_lower[1], x$auc_upper[1])
    }
    expect_equal(ci(), ci())
})


test_that("the analysis leaves the caller's random number stream untouched", {
    d <- er_data()
    set.seed(99); before <- runif(3)
    set.seed(99)
    invisible(run_er(data = d, predictors = "m1", useBootstrap = TRUE, bootstrapSamples = 100))
    after <- runif(3)
    expect_equal(before, after)
})


test_that("automatic direction detection discloses that it biases the AUC upward", {
    # pROC's "auto" compares the two groups' MEDIANS -- it does not maximise the AUC, and the
    # result is not floored at 0.5. But the direction is fitted from the same data that supply
    # the AUC, so the AUC is biased upward: simulating a marker with no information at all gives
    # a mean reported AUC of 0.593 at n = 20 against 0.502 with the direction fixed, exceeding
    # 0.60 in 43% of runs. The notice named the chosen direction but not this consequence, and
    # was filed as INFO.
    d <- er_data()
    auto <- run_er(data = d, predictors = "bad", direction = "auto")
    fixed <- run_er(data = d, predictors = "bad", direction = "higher")

    expect_gt(auto$results$aucSummary$asDF$auc[1], 0.5)
    expect_lt(fixed$results$aucSummary$asDF$auc[1], 0.5)
    # the two are reflections of one another
    expect_equal(auto$results$aucSummary$asDF$auc[1],
                 1 - fixed$results$aucSummary$asDF$auc[1], tolerance = 1e-9)

    n <- notices_of(auto)
    expect_match(n, "Direction Auto-Detected")
    expect_match(n, "biased upward")
    expect_match(n, "median values")
    expect_match(n, "set Direction explicitly")
    # not raised when the user fixed the direction themselves
    expect_false(grepl("Direction Auto-Detected", notices_of(fixed)))
})


test_that("auto direction follows the median rule, not AUC maximisation", {
    # Recorded because the notice text depends on it, and the obvious assumption is wrong.
    # pROC picks the direction by comparing group medians; the reported AUC is therefore NOT
    # floored at 0.5. On this pure-noise sample auto chooses "<" and reports 0.4838 where ">"
    # would have given 0.5162.
    skip_if_not_installed("pROC")
    set.seed(1)
    n <- 1000
    y <- factor(rep(c("neg", "pos"), each = n / 2), levels = c("neg", "pos"))
    x <- rnorm(n)
    d <- data.frame(status = y, m = x)

    auto <- run_er(data = d, predictors = "m", direction = "auto")$results$aucSummary$asDF$auc[1]
    hi <- as.numeric(pROC::roc(y, x, direction = "<", quiet = TRUE)$auc)
    lo <- as.numeric(pROC::roc(y, x, direction = ">", quiet = TRUE)$auc)

    expect_lt(auto, 0.5)                       # not floored at 0.5
    expect_equal(auto, hi, tolerance = 1e-9)   # it took "<" ...
    expect_lt(auto, max(hi, lo))               # ... which was NOT the AUC-maximising choice
    expect_equal(unname(median(x[y == "pos"]) > median(x[y == "neg"])), TRUE)  # the median rule
})


test_that("comparison options explain themselves instead of silently doing nothing", {
    # pairwiseComparisons, showMetricsDiff and statisticalComparison all require
    # analysisType == "comparative", but the UI offers them as plain checkboxes with no such
    # dependency, so ticking one under the default Analysis Type produced no output at all.
    d <- er_data()
    for (opt in c("pairwiseComparisons", "showMetricsDiff", "statisticalComparison")) {
        args <- list(data = d, predictors = c("m1", "m2"), analysisType = "single")
        args[[opt]] <- TRUE
        expect_match(notices_of(do.call(run_er, args)),
                     "Comparison Options Need Comparative Analysis", info = opt)
    }
    # under comparative it works and says nothing
    ok <- run_er(data = d, predictors = c("m1", "m2"), analysisType = "comparative",
                 pairwiseComparisons = TRUE)
    expect_false(grepl("Comparison Options Need Comparative", notices_of(ok)))
    expect_equal(ok$results$rocComparisons$rowCount, 1)
})


test_that("unimplemented options are labelled as such in the documentation", {
    # 19 options reach the public R wrapper and man/enhancedROC.Rd described them as working
    # features ("Calculate Harrell's concordance index for time-to-event outcomes"), while the
    # backend only lists them in a "planned features" notice. None has a UI control, so this
    # affects R callers and the help page rather than the jamovi GUI.
    a_yaml <- paste(readLines("../../jamovi/enhancedroc.a.yaml", warn = FALSE), collapse = "\n")
    unimplemented <- c("harrellCIndex", "unoCStatistic", "incidentDynamic", "cumulativeDynamic",
                       "competingRisksConcordance", "splineCalibration", "splineKnots", "eoRatio",
                       "namDagostino", "greenwoodNam", "calibrationBelt", "calibrationDensity",
                       "optimismCorrection", "externalValidation", "decisionImpactCurves",
                       "netBenefitRegression", "modelUpdating", "transportability",
                       "bootstrapPartialAUC", "bootstrapCutoffCI")
    for (o in unimplemented) {
        blk <- regmatches(a_yaml, regexpr(sprintf("(?s)    - name: %s\\n.*?(?=\\n    - name: |\\Z)", o),
                                          a_yaml, perl = TRUE))
        expect_true(nzchar(blk), label = paste("found block for", o))
        expect_match(blk, "NOT YET IMPLEMENTED", info = o)
    }
    # All 20 have LIVE checkboxes in jamovi/enhancedroc.u.yaml, so a GUI user can tick one and
    # receive nothing. That makes it a warning, not the INFO it used to be filed as.
    u_yaml <- paste(readLines("../../jamovi/enhancedroc.u.yaml", warn = FALSE), collapse = "\n")
    for (o in unimplemented)
        expect_match(u_yaml, sprintf("(?m)^\\s*name:\\s*%s\\s*$", o), info = o,
                     all = FALSE, perl = TRUE)

    res <- run_er(data = er_data(), predictors = "m1", harrellCIndex = TRUE)
    n <- notices_of(res)
    expect_match(n, "not yet implemented")
    expect_match(n, "Selected Features Produced No Output")
    # .renderNotices signals severity by colour rather than a text label, so assert on the
    # WARNING palette (#ca8a04 on #fefce8) -- it used to be filed at INFO.
    raw <- paste(res$results$notices$content, collapse = " ")
    block <- sub(".*(<div[^>]*>(?:(?!<div).)*Selected Features Produced No Output).*", "\\1",
                 raw, perl = TRUE)
    expect_match(block, "#ca8a04", fixed = TRUE)
})


test_that("the output states which way each marker was read", {
    # Same wording as psychopdaROC's note, so the two analyses can be compared line for line.
    d <- er_data()
    n <- tnote_er(run_er(data = d, predictors = "bad", direction = "auto"))
    expect_match(n, "values of bad were taken to indicate")
    expect_match(n, "read from the data, not specified in advance")
    expect_match(n, "reversed")

    n2 <- tnote_er(run_er(data = d, predictors = "m1", direction = "higher"))
    expect_match(n2, "HIGHER values of m1 were taken to indicate")
    expect_match(n2, "what you specified")
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/enhancedroc.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    # `data` is the Data option; `nntCalculation` gates a column declaratively via a .r.yaml
    # visible: expression; `splineKnots` configures splineCalibration, which is itself flagged
    # NOT YET IMPLEMENTED, so there is nothing to read it yet.
    declared <- setdiff(declared, c("data", "nntCalculation", "splineKnots"))
    backend <- paste(readLines("../../R/enhancedroc.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})
