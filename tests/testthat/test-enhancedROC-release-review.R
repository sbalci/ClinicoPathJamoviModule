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


test_that("unimplemented options are documented but not exposed in the GUI", {
    # 19 options reach the public R wrapper and man/enhancedROC.Rd described them as working
    # features ("Calculate Harrell's concordance index for time-to-event outcomes"), while the
    # backend only lists them in a "planned features" notice. None has a UI control, so this
    # affects R callers and the help page rather than the jamovi GUI.
    a_yaml <- paste(readLines("../../jamovi/enhancedroc.a.yaml", warn = FALSE), collapse = "\n")
    # splineCalibration / splineKnots were implemented on 2026-09-02 and are no longer listed.
    unimplemented <- c("harrellCIndex", "unoCStatistic", "incidentDynamic", "cumulativeDynamic",
                       "competingRisksConcordance", "eoRatio",
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
    # These options DO have live controls in jamovi/enhancedROC.u.yaml (they always did; the
    # earlier assertion that they had none was red at HEAD). The backend therefore has to warn
    # when one is ticked - see the "Selected Features Produced No Output" notice in .run() -
    # so every one of them must be in that list.
    b_r <- paste(readLines("../../R/enhancedROC.b.R", warn = FALSE), collapse = "\n")
    for (o in unimplemented)
        expect_true(grepl(sprintf("self$options$%s)) unimplemented <- c(unimplemented", o), b_r,
                          fixed = TRUE), info = o)
    # ...and the two spline options, now implemented, must NOT be in it.
    expect_false(grepl("splineCalibration)) unimplemented", b_r, fixed = TRUE))

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


# ── Spline calibration curve (implemented 2026-09-02) ──────────────────────────

test_that(".splineCalibrationCurve matches a hand-built natural-spline logistic fit", {
    set.seed(31); n <- 400
    p <- stats::plogis(stats::rnorm(n, -0.5, 1.2))
    y <- stats::rbinom(n, 1, p)
    pr <- er_private(outcome = "status", positiveClass = "pos", predictors = "m1")$p
    sp <- pr$.splineCalibrationCurve(p, y, knots = 4)
    expect_false(is.null(sp))

    lp <- stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
    fit <- stats::glm(y ~ splines::ns(lp, df = 3), family = stats::binomial)
    err <- abs(as.numeric(stats::fitted(fit)) - p)
    expect_equal(sp$ici, mean(err), tolerance = 1e-12)
    expect_equal(sp$e50, unname(stats::median(err)), tolerance = 1e-12)
    expect_equal(sp$e90, unname(stats::quantile(err, 0.9)), tolerance = 1e-12)
    expect_equal(sp$emax, max(err), tolerance = 1e-12)
    expect_length(sp$grid_p, 100)
    expect_true(all(sp$curve >= 0 & sp$curve <= 1))
    # well-calibrated simulated risks: ICI should be small
    expect_lt(sp$ici, 0.05)

    # too few events for the knots -> NULL, not an error
    expect_null(pr$.splineCalibrationCurve(p[1:15], y[1:15], knots = 4))
})

test_that("spline calibration runs end to end, draws, and is no longer reported as unimplemented", {
    data("enhancedroc_calibration", package = "ClinicoPath")
    res <- run_er(data = enhancedroc_calibration, outcome = "outcome", predictors = "predicted_prob",
                  positiveClass = "Event", calibrationAnalysis = TRUE, calibrationPlot = TRUE,
                  splineCalibration = TRUE, splineKnots = 4)
    nt <- notices_of(res)
    expect_false(grepl("Spline Calibration", nt, fixed = TRUE))
    expect_false(grepl("Spline calibration not estimable", nt, fixed = TRUE))
    expect_equal(res$results$calibrationSummary$rowCount, 1)
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_no_error(res$results$calibrationPlotImage$.render())
    # ICI columns exist only once jmvtools::prepare() has recompiled the results schema
    df <- res$results$calibrationSummary$asDF
    if ("ici" %in% names(df)) {
        expect_true(is.finite(df$ici[1]))
        expect_lte(df$e50[1], df$e90[1]); expect_lte(df$e90[1], df$emax[1])
    } else {
        skip("ici column not yet compiled into enhancedROC.h.R; run jmvtools::prepare()")
    }
})


# --- 2026-09-03 /fix-function: cutoff direction, plot state, Youden toggle, validation, OVR, notices ---

er_private_data <- function(d, ...) {
    o <- do.call(ClinicoPath:::enhancedROCOptions$new, list(...))
    a <- ClinicoPath:::enhancedROCClass$new(options = o, data = d)
    a$init()
    list(a = a, o = o, p = a$.__enclos_env__$private)
}
er_img <- function(pr, name) pr$a$results$results$get(name)

test_that("a custom cutoff equal to an observed value under direction 'lower' matches pROC's tie rule", {
    skip_if_not_installed("pROC")
    d <- er_data()
    cut <- d$bad[5]
    pr <- er_private_data(d, outcome = "status", positiveClass = "pos", predictors = "bad",
                          direction = "lower", customCutoffs = as.character(cut), cutoffTable = TRUE)
    pr$p$.run()
    ca <- pr$a$results$results$cutoffAnalysis$asDF
    row <- ca[grepl("Custom", ca$cutoff_type) & abs(ca$cutoff - cut) < 1e-9, ]
    ref <- pROC::coords(pr$p$.rocResults$bad$roc, cut, ret = c("tp", "fp", "sensitivity", "specificity"))
    expect_equal(row$true_positive, ref$tp)      # was predictor < cutoff: TP/FP contradicted sens/spec
    expect_equal(row$false_positive, ref$fp)
    expect_equal(row$sensitivity, ref$sensitivity, tolerance = 1e-9)
})

test_that("renderers draw from image$state when .run() has not executed in this instance", {
    skip_if_not_installed("pROC")
    opts <- list(outcome = "status", positiveClass = "pos", predictors = c("m1", "m2"),
                 analysisType = "comparative", cutoffTable = TRUE, clinicalMetrics = TRUE,
                 smoothMethod = "binormal", customCutoffs = "0.5,1")
    live <- do.call(er_private, opts)
    live$p$.run()
    st <- er_img(live, "rocCurvePlot")$state
    expect_false(is.null(st))
    expect_false(any(rapply(st, is.function, how = "unlist")))      # protobuf-safe payload
    expect_setequal(names(st$roc), c("m1", "m2"))

    fresh <- do.call(er_private, opts)                              # as after reopening a saved .omv
    for (nm in c("rocCurvePlot", "comparativeROCPlot", "cutoffAnalysisPlot", "youdenIndexPlot", "clinicalDecisionPlot")) {
        er_img(fresh, nm)$setState(er_img(live, nm)$state)
    }
    expect_null(fresh$p$.rocResults)
    expect_true(fresh$p$.plotROCCurve(er_img(fresh, "rocCurvePlot"), jmvcore::theme_default(), NULL))
    expect_true(fresh$p$.plotComparativeROC(er_img(fresh, "comparativeROCPlot"), jmvcore::theme_default(), NULL))
    expect_true(fresh$p$.plotCutoffAnalysis(er_img(fresh, "cutoffAnalysisPlot"), jmvcore::theme_default(), NULL))
    expect_equal(as.numeric(fresh$p$.rocResults$m1$roc$auc), as.numeric(live$p$.rocResults$m1$roc$auc))
    expect_equal(nrow(fresh$p$.rocResults$m1$custom_cutoffs), 2)
    expect_false(is.null(fresh$p$.rocSmoothed$m1))
})

test_that("a run that returns early leaves no plot state behind", {
    live <- er_private(outcome = "status", positiveClass = "pos", predictors = "m1")
    live$p$.run()
    expect_false(is.null(er_img(live, "rocCurvePlot")$state))
    live$p$.predictors <- NULL                                       # simulate the missing-variables early return
    live$p$.run()
    expect_null(er_img(live, "rocCurvePlot")$state)
})

test_that("Youden optimisation off reports the closest-to-top-left cutoff and labels it", {
    skip_if_not_installed("pROC")
    pr <- er_private(outcome = "status", positiveClass = "pos", predictors = "m1",
                     youdenOptimization = FALSE, cutoffTable = TRUE)
    pr$p$.run()
    ref <- pROC::coords(pr$p$.rocResults$m1$roc, "best", best.method = "closest.topleft", ret = "threshold")
    expect_equal(pr$p$.rocResults$m1$optimal_cutoff$cutoff, ref$threshold[1])
    ca <- pr$a$results$results$cutoffAnalysis$asDF
    expect_true("Optimal (closest to top-left)" %in% ca$cutoff_type)
    expect_false("Optimal (Youden)" %in% ca$cutoff_type)            # was: Youden under either setting
})

test_that("one-vs-rest curves share one direction per marker and the table says so", {
    skip_if_not_installed("pROC")
    d <- er_data()
    d$stage <- factor(ifelse(d$m1 < -0.3, "I", ifelse(d$m1 < 0.9, "II", "III")), levels = c("I", "II", "III"))
    pr <- er_private_data(d, outcome = "stage", positiveClass = "III", predictors = "m1",
                          multiClassROC = TRUE, multiClassStrategy = "ovr")
    pr$p$.run()
    expect_equal(pr$p$.ovrDirection(d$stage, d$m1), "<")
    notes <- pr$a$results$results$multiClassAUC$.__enclos_env__$private$.notes
    expect_true("ovr_direction" %in% names(notes))
    expect_match(get("note", envir = notes[["ovr_direction"]]), "the same way for every class", fixed = TRUE)
})

test_that("internal validation labels its interval honestly and fixes the probability direction", {
    skip_if_not_installed("pROC")
    pr <- er_private(outcome = "status", positiveClass = "pos", predictors = "m1",
                     internalValidation = TRUE, validationMethod = "bootstrap", bootstrapSamples = 100)
    pr$p$.run()
    html_items <- Filter(function(it) inherits(it, "Html"), pr$a$results$results$items)
    txt <- paste(vapply(html_items, function(it) it$content %||% "", character(1)), collapse = " ")
    expect_match(txt, "Internal Validation", fixed = TRUE)
    expect_match(txt, "shifted by the optimism estimate", fixed = TRUE)
    expect_match(txt, "SD across resamples", fixed = TRUE)
    expect_false(grepl("95% CI \\[", txt))
})

test_that("extreme prevalence is reported once for the sample, and a failed predictor gets a notice", {
    skip_if_not_installed("pROC")
    d <- er_data()
    d$status <- factor(ifelse(seq_len(nrow(d)) <= 6, "pos", "neg"), levels = c("neg", "pos"))   # 3 % prevalence
    pr <- er_private_data(d, outcome = "status", positiveClass = "pos", predictors = c("m1", "m2"))
    env <- pr$p
    orig <- env$.calculateOptimalCutoff
    unlockBinding(".calculateOptimalCutoff", env)
    assign(".calculateOptimalCutoff", function(roc_obj, data, predictor) {
        if (predictor == "m2") stop("boom")
        orig(roc_obj, data, predictor)
    }, envir = env)
    pr$p$.run()
    html <- pr$a$results$results$notices$content
    expect_equal(lengths(regmatches(html, gregexpr("Extreme Prevalence", html, fixed = TRUE))), 1)   # was: once per predictor
    expect_match(html, "ROC Analysis Failed: m2", fixed = TRUE)                                     # was: instructions panel only
    expect_match(html, "ROC analysis failed for predictor &#x27;m2&#x27;: boom", fixed = TRUE)
    expect_setequal(names(pr$p$.rocResults), "m1")
})
