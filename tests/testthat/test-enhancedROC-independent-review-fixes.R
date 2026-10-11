# Regressions for IR-enhancedROC-01/02/03 (independent review, 2026-10-10).
ir_roc_data <- function() {
    data.frame(
        outcome = factor(rep(c("positive", "negative"), each = 12),
                         levels = c("negative", "positive")),
        score = c(.95, .90, .85, .80, .75, .65, .60, .55, .50, .40, .30, .10,
                  .90, .75, .65, .60, .55, .45, .40, .35, .25, .20, .15, .05))
}

ir_roc_object <- function(data = ir_roc_data(), ...) {
    args <- utils::modifyList(list(outcome = "outcome", positiveClass = "positive",
        predictors = "score", direction = "higher", rocCurve = FALSE,
        comprehensive_output = TRUE, showTableNotes = TRUE, showAnalysisSummary = TRUE,
        showClinicalReport = TRUE), list(...), keep.null = TRUE)
    options <- do.call(ClinicoPath:::enhancedROCOptions$new, args)
    analysis <- ClinicoPath:::enhancedROCClass$new(options = options, data = data)
    analysis$init()
    list(a = analysis, p = analysis$.__enclos_env__$private, o = options)
}

ir_roc_note <- function(table, key) {
    notes <- table$.__enclos_env__$private$.notes
    if (!key %in% names(notes)) return("")
    get("note", envir = notes[[key]])
}

ir_roc_inject <- function(object, replacement) {
    unlockBinding(".bootstrapAucCi", object$p)
    object$p$.bootstrapAucCi <- replacement
    lockBinding(".bootstrapAucCi", object$p)
}

test_that("failed bootstrap reports DeLong limits, SE and method consistently", {
    h <- ir_roc_object(useBootstrap = TRUE, bootstrapSamples = 100)
    ir_roc_inject(h, function(...) stop("injected bootstrap failure"))
    h$p$.run()
    r <- h$a$results$results
    row <- r$aucSummary$asDF
    # Independent pairwise-placement oracle from the audit, not pROC.
    expect_equal(row$auc, .6875, tolerance = 1e-12)
    expect_equal(row$std_error, .11224953413024889, tolerance = 1e-12)
    expect_equal(c(row$auc_lower, row$auc_upper),
                 c(.46749495582331269, .90750504417668731), tolerance = 1e-12)
    expect_match(r$notices$content, "injected bootstrap failure", fixed = TRUE)
    expect_identical(ir_roc_note(r$aucSummary, "std_error_blank"), "")
    for (text in c(ir_roc_note(r$aucSummary, "ci_method"),
                   r$methodsExplanation$content, r$clinicalReport$content)) {
        expect_match(text, "DeLong.*method")
        expect_match(text, "fallback", fixed = TRUE)
        expect_false(grepl("are bootstrap intervals", text, fixed = TRUE))
    }
})

test_that("mixed outcomes retain each marker's actual interval and SE policy", {
    d <- ir_roc_data()
    d$second <- 1 - d$score
    for (method in c("bca", "basic", "percentile")) {
        h <- ir_roc_object(d, predictors = c("score", "second"),
            useBootstrap = TRUE, bootstrapMethod = method, bootstrapSamples = 200,
            stratifiedBootstrap = TRUE, seed = 51)
        original <- h$p$.bootstrapAucCi
        ir_roc_inject(h, function(roc_obj, ...) {
            if (identical(roc_obj$predictor, d$score)) stop("first marker failed")
            original(roc_obj, ...)
        })
        h$p$.run()
        r <- h$a$results$results
        expect_equal(r$aucSummary$asDF$std_error[1], .11224953413024889,
                     tolerance = 1e-12)
        expect_identical(r$aucSummary$getCell("std_error", rowKey = "second")$value, "")
        expect_true(all(is.finite(unlist(r$aucSummary$asDF[2, c("auc_lower", "auc_upper")]))))
        for (text in c(ir_roc_note(r$aucSummary, "ci_method"),
                       r$methodsExplanation$content, r$clinicalReport$content)) {
            expect_match(text, "score: 95% confidence intervals for the AUC use DeLong", fixed = TRUE)
            expect_match(text, paste0("second: 95% confidence intervals for the AUC are bootstrap intervals (",
                if (method == "bca") "BCa" else method, ")"), fixed = TRUE)
        }
        # A later non-bootstrap run must clear the prior bootstrap note and fallback.
        option <- h$o$option("useBootstrap")
        option$value <- FALSE
        h$p$.run()
        expect_identical(ir_roc_note(r$aucSummary, "std_error_blank"), "")
        expect_false(grepl("fallback", r$methodsExplanation$content, fixed = TRUE))
        expect_true(all(is.finite(r$aucSummary$asDF$std_error)))
    }
})

test_that("BCa downgrade records the actual percentile method and usable draws", {
    d <- data.frame(outcome = factor(c("positive", "positive", rep("negative", 18)),
                                    levels = c("negative", "positive")),
                    score = c(7, 17, 1:6, 8:16, 18:20))
    h <- ir_roc_object(d, useBootstrap = TRUE, bootstrapMethod = "bca",
                       bootstrapSamples = 100, stratifiedBootstrap = FALSE, seed = 7)
    h$p$.run()
    ci <- h$p$.rocResults$score$roc$ci
    expect_identical(attr(ci, "boot.type"), "perc")
    expect_lt(attr(ci, "boot.used"), attr(ci, "boot.n"))
    for (text in c(ir_roc_note(h$a$results$results$aucSummary, "ci_method"),
                   h$a$results$results$methodsExplanation$content,
                   h$a$results$results$clinicalReport$content)) {
        expect_match(text, "bootstrap intervals (percentile)", fixed = TRUE)
        expect_match(text, paste(attr(ci, "boot.used"), "usable resamples of 100"), fixed = TRUE)
        expect_false(grepl("bootstrap intervals (BCa)", text, fixed = TRUE))
    }
})

test_that("distinct labels and composite identities never collapse to the same key", {
    h <- ir_roc_object()
    names <- c("A-B", "A B", "A_B", "A.B", "A+B", "A[1]", "\u00c9preuve",
               "_x412d42", "A", "A_vs_B", "A\nB", "B")
    keys <- h$p$.escapeVar(names)
    expect_equal(length(unique(keys)), length(names))
    expect_identical(keys, rev(h$p$.escapeVar(rev(names))))
    pairs <- expand.grid(first = keys, second = keys, stringsAsFactors = FALSE)
    for (separator in c("_", "_vs_")) {
        expect_equal(length(unique(paste(pairs$first, pairs$second, sep = separator))), nrow(pairs))
    }
})

test_that("colliding legacy marker names preserve all public tables and associations", {
    d <- ir_roc_data()
    d[["A-B"]] <- d$score
    d[["A B"]] <- 1 - d$score
    for (markers in list(c("A-B", "A B"), c("A B", "A-B"))) {
        h <- ir_roc_object(d, predictors = markers, analysisType = "comparative",
            pairwiseComparisons = TRUE, statisticalComparison = TRUE, showMetricsDiff = TRUE,
            cutoffTable = TRUE, customCutoffs = "0.5",
            clinicalMetrics = TRUE, partialAuc = TRUE, partialRange = "0.4,0.9",
            crocAnalysis = TRUE, convexHull = TRUE, detectImbalance = TRUE)
        suppressWarnings(h$p$.run())
        r <- h$a$results$results
        for (name in c("aucSummary", "optimalCutoffSummary", "cutoffAnalysis",
            "diagnosticPerformance", "clinicalApplicationMetrics", "rocComparisons",
            "statisticalSummary", "partialAucAnalysis", "crocAnalysisTable",
            "convexHullTable", "precisionRecallTable")) {
            expect_no_error(df <- r[[name]]$asDF)
            expect_gt(nrow(df), 0, label = name)
            expect_false(anyDuplicated(rownames(df)) > 0, info = name)
        }
        row <- r$aucSummary$asDF
        expect_identical(row$predictor, markers)
        expect_equal(row$auc[match(c("A-B", "A B"), row$predictor)], c(.6875, .3125),
                     tolerance = 1e-12)
        expect_equal(nrow(r$rocComparisons$asDF), 1L)
    }
})

test_that("multi-class tables preserve punctuation in marker and class identities", {
    d <- ir_roc_data()
    d$outcome <- factor(rep(c("A-B", "A B", "A_B"), each = 8),
                        levels = c("A-B", "A B", "A_B"))
    d[["A-B"]] <- d$score
    d[["A B"]] <- 1 - d$score
    for (strategy in c("ovr", "ovo")) {
        h <- ir_roc_object(d, positiveClass = NULL, predictors = c("A-B", "A B"),
            multiClassROC = TRUE, multiClassStrategy = strategy)
        suppressWarnings(h$p$.run())
        r <- h$a$results$results
        expect_no_error(rows <- r$multiClassAUC$asDF)
        expect_equal(nrow(rows), 6L)
        expect_equal(length(unique(rows$class)), 6L)
        expect_equal(nrow(r$multiClassAverage$asDF), 2L)
        expect_equal(rows$auc[1:3] + rows$auc[4:6], rep(1, 3), tolerance = 1e-12)
    }
})

test_that("the 20-complete-observation policy cannot be bypassed by missing rows", {
    d <- ir_roc_data()
    # Keep both classes present at every tested sample size.
    d <- d[as.vector(rbind(1:12, 13:24)), ]
    for (n in c(9L, 10L, 19L, 20L)) for (pad in c(0L, 24L)) {
        input <- rbind(d[seq_len(n), ], d[rep(NA_integer_, pad), ])
        h <- ir_roc_object(input)
        h$p$.run()
        expect_equal(nrow(h$a$results$results$aucSummary$asDF), as.integer(n >= 20),
                     info = paste(n, "complete;", pad, "missing"))
        if (n < 20) {
            expect_match(h$a$results$results$notices$content,
                         paste("Only", n, "complete observations remain"), fixed = TRUE)
            expect_match(h$a$results$results$notices$content, "at least 20", fixed = TRUE)
        }
    }
    d$second <- d$score
    d$second[1:5] <- NA_real_
    h <- ir_roc_object(d, predictors = c("score", "second"))
    h$p$.run()
    expect_equal(nrow(h$a$results$results$aucSummary$asDF), 0L)
    expect_match(h$a$results$results$notices$content, "Only 19 complete", fixed = TRUE)
})

test_that("ROC curves and confidence ribbons follow the selected jamovi palette", {
    d <- ir_roc_data()
    d$second <- 1 - d$score
    h <- ir_roc_object(d, predictors = c("score", "second"), rocCurve = TRUE,
        showConfidenceBands = TRUE, bootstrapSamples = 100, showCutoffPoints = FALSE)
    h$p$.run()
    for (palette in c("jmv", "spss")) {
        expect_true(h$p$.plotROCCurve(h$a$results$results$rocCurvePlot,
            ggtheme = ggplot2::theme_minimal(), theme = list(palette = palette)))
        plot <- ggplot2::last_plot()
        expected <- jmvcore::colorPalette(2, palette, "color")
        expect_setequal(ggplot2::layer_data(plot, 1)$colour, expected)
        expect_setequal(ggplot2::layer_data(plot, 3)$fill, expected)
    }
})

test_that("each binary image restores from its own coordinates without input rows", {
    plot_file <- withr::local_tempfile(fileext = ".png")
    grDevices::png(plot_file, type = "cairo")
    withr::defer(grDevices::dev.off())
    d <- ir_roc_data()
    d$second <- 1 - d$score
    args <- list(predictors = c("score", "second"), analysisType = "comparative",
        rocCurve = TRUE, cutoffTable = TRUE, clinicalMetrics = TRUE,
        detectImbalance = TRUE, crocAnalysis = TRUE, convexHull = TRUE,
        calibrationAnalysis = TRUE, calibrationPlot = TRUE, predictionScale = "probability",
        clinicalImpact = TRUE, clinicalUtilityCurve = TRUE, decisionImpactCurves = TRUE,
        showConfidenceBands = TRUE, bootstrapSamples = 100, seed = 37)
    live <- do.call(ir_roc_object, c(list(data = d), args))
    suppressWarnings(live$p$.run())
    images <- c(rocCurvePlot = ".plotROCCurve", comparativeROCPlot = ".plotComparativeROC",
        cutoffAnalysisPlot = ".plotCutoffAnalysis", youdenIndexPlot = ".plotYoudenIndex",
        clinicalDecisionPlot = ".plotClinicalDecision", prcPlot = ".plotPRC",
        crocCurvePlot = ".plotCROC", convexHullPlot = ".plotConvexHull")
    forbidden <- c("analysisData", "factorLevels", "multiClassOutcome", "response",
                   "predictor", "cases", "controls", "model", "call")
    state_names <- function(x) {
        if (!is.list(x)) return(character())
        c(names(x), unlist(lapply(x, state_names), use.names = FALSE))
    }
    for (name in live$p$.plotImageNames()) {
        state <- live$a$results$results[[name]]$state
        expect_false(any(state_names(state) %in% forbidden), info = name)
        expect_false(any(rapply(state, is.function, how = "unlist")), info = name)
    }
    for (name in names(images)) {
        fresh <- do.call(ir_roc_object, c(list(data = d), args))
        fresh$p$.data <- NULL
        fresh$p$.analysisData <- NULL
        image <- fresh$a$results$results[[name]]
        image$setState(unserialize(serialize(live$a$results$results[[name]]$state, NULL)))
        expect_null(fresh$p$.analysisData)
        expect_true(fresh$p[[images[[name]]]](image, ggplot2::theme_minimal(), list(palette = "jmv")))
        restored <- ggplot2::ggplot_build(ggplot2::last_plot())$data
        expect_true(live$p[[images[[name]]]](live$a$results$results[[name]],
            ggplot2::theme_minimal(), list(palette = "jmv")))
        expect_equal(restored, ggplot2::ggplot_build(ggplot2::last_plot())$data,
                     info = name, tolerance = 1e-12)
        expect_null(fresh$p$.analysisData)
        image$setState(NULL)
        expect_false(fresh$p[[images[[name]]]](image, ggplot2::theme_minimal(), NULL))
    }
})

test_that("legacy plot state asks for recalculation instead of silently disappearing", {
    h <- ir_roc_object(rocCurve = TRUE, detectImbalance = TRUE, multiClassROC = TRUE)
    images <- c(rocCurvePlot = ".plotROCCurve", comparativeROCPlot = ".plotComparativeROC",
        cutoffAnalysisPlot = ".plotCutoffAnalysis", youdenIndexPlot = ".plotYoudenIndex",
        clinicalDecisionPlot = ".plotClinicalDecision", prcPlot = ".plotPRC",
        crocCurvePlot = ".plotCROC", convexHullPlot = ".plotConvexHull",
        multiClassROCPlot = ".plotMultiClassROC")
    for (name in names(images)) {
        image <- h$a$results$results[[name]]
        image$setState(list(analysisData = ir_roc_data(), predictors = "score",
            multiClassOutcome = ir_roc_data()$outcome,
            roc = list(score = list(direction = "<"))))
        expect_true(h$p[[images[[name]]]](image, ggplot2::theme_minimal(), NULL))
        expect_match(ggplot2::layer_data(ggplot2::last_plot(), 1)$label,
                     "Saved Plot Needs Recalculation", fixed = TRUE)
    }
})

test_that("target reference page fields are protobuf strings", {
    refs <- getFromNamespace(".jmvrefs", "ClinicoPath")
    for (key in c("LinnGrunau2006", "Robin2011", "VanCalster2019Calibration", "Vickers2019DCA")) {
        expect_type(refs[[key]]$pages, "character")
    }
})
