# End-to-end regressions for the full audit: verify the actual analysis outputs.
ps_audit_data <- function() {
    first <- rep(c(1L, 2L, 3L, 4L, 5L, 6L, NA_integer_, NA_integer_), 6)
    data.frame(total = rep(10L, 48), first = first,
        positive = ifelse(is.na(first), 0L, 2L),
        cassettes = ifelse(is.na(first), 0L, 2L),
        nodes = rep(c(8L, 16L, 16L, 16L), 12),
        positive_nodes = rep(c(0L, 1L, 3L, 5L), 12),
        population = rep(20L, 48), successes = rep(c(1L, 4L, 8L), 16),
        total_foci = rep(6L, 48), single_foci = rep(3L, 48))
}

ps_audit_run <- function(data = ps_audit_data(), ..., core = TRUE) {
    args <- list(...)
    if (core) args <- utils::modifyList(list(totalSamples = "total", firstDetection = "first"), args)
    do.call(pathsampling, c(list(data = data), args))
}

test_that("empirical numerator and denominator use the same complete cases", {
    d <- data.frame(total = rep(10L, 20), first = rep(2L, 20), positive = c(rep(2L, 10), rep(NA, 10)))
    for (missing_value in c(NA, -1, 11, 1.5)) {
        d$positive[11:20] <- missing_value
        r <- ps_audit_run(d, positiveCount = "positive", estimationMethod = "empirical")
        expect_equal(as.data.frame(r$binomialTable)$cumProb[1], 0.2)
        rec <- as.data.frame(r$recommendTable)
        expect_equal(rec$minSamples[rec$confidence == 0.95], 14)
        expect_match(r$notices$content, "10 cases.*count", ignore.case = TRUE)
    }
    d$positive[] <- NA
    r <- ps_audit_run(d, positiveCount = "positive", estimationMethod = "empirical")
    expect_true(all(is.na(as.data.frame(r$binomialTable)$cumProb)))
})

test_that("bootstrap headline interval is the interval at its recommended position", {
    r <- ps_audit_run(showBootstrap = TRUE, bootstrapIterations = 200,
        setSeed = TRUE, seedValue = 101, showClinicalSummary = TRUE)
    b <- as.data.frame(r$bootstrapTable)
    k <- which(b$meanSens >= 0.95)[1]
    expected <- sprintf("95%% CI %.1f%%-%.1f%%", 100 * b$ciLower[k], 100 * b$ciUpper[k])
    expect_match(r$keyResults$content, expected, fixed = TRUE)
    expect_match(r$clinicalSummary$content, sprintf("%.1f%%-%.1f%%", 100 * b$ciLower[k], 100 * b$ciUpper[k]), fixed = TRUE)
    expect_false(grepl("%%", r$keyResults$content, fixed = TRUE))
})

test_that("model prerequisites do not depend on the binomial display toggle", {
    for (on in c(FALSE, TRUE)) {
        r <- ps_audit_run(showBinomialModel = on, showMultifocalAnalysis = TRUE,
            showPopulationDetection = TRUE, showModelFit = TRUE, showObsPred = TRUE)
        expect_equal(as.data.frame(r$multifocalProbTable)$detectOne[1], 1 / 3.5)
        expect_equal(as.data.frame(r$populationDetectionTable)$population[1], 0.75 / 3.5)
        expect_gt(nrow(as.data.frame(r$obsPredTable)), 0)
    }
    r <- ps_audit_run(positiveCassettes = "cassettes", showCorrelation = TRUE)
    expect_false(is.null(r$correlationPlot$state))
})

test_that("a rejected estimate is withheld from every prediction consumer", {
    d <- ps_audit_data(); d$first[] <- 2; d$positive <- rep(c(1L, 9L), 24)
    r <- ps_audit_run(d, positiveCount = "positive", estimationMethod = "empirical",
        showDetectionCurve = TRUE, showMultifocalAnalysis = TRUE, showObsPred = TRUE,
        showModelFit = TRUE, showSampleSizePlanning = TRUE, showProbabilityExplanation = TRUE)
    expect_true(all(is.na(as.data.frame(r$binomialTable)$cumProb)))
    expect_true(is.na(r$detectionCurve$state$pEstimate))
    expect_equal(nrow(as.data.frame(r$multifocalProbTable)), 0)
    expect_equal(nrow(as.data.frame(r$obsPredTable)), 0)
    expect_equal(nrow(as.data.frame(r$sampleSizePlanningTable)), 1)
    expect_match(r$notices$content, "predictions withheld", ignore.case = TRUE)
})

test_that("independent analyses survive missing core data and local model failures", {
    d <- ps_audit_data(); d$population[] <- NA
    r <- ps_audit_run(d, core = FALSE, showHypergeometric = TRUE,
        totalPopulation = "population", successStates = "successes",
        totalLymphNodes = "nodes", positiveLymphNodes = "positive_nodes",
        showLNAnalysis = TRUE, showEffectSizes = TRUE, showSampleSizePlanning = TRUE,
        planningAssumedQ = 0.1)
    expect_equal(sum(as.data.frame(r$lnrClassification)$cases), nrow(d))
    expect_equal(as.data.frame(r$sampleSizePlanningTable)$nSamples, 16)
    expect_gt(nrow(as.data.frame(r$effectSizesTable)), 0)
    d$first[] <- NA
    r <- ps_audit_run(d, showSampleSizePlanning = TRUE, planningAssumedQ = 1)
    expect_equal(as.data.frame(r$sampleSizePlanningTable)$nSamples, 1)
    expect_no_error(ps_audit_run(core = FALSE))
})

test_that("invalid counts are excluded without misaligning optional vectors", {
    d <- ps_audit_data(); d$total[1] <- -1; d$first[1] <- NA; d$first[2] <- 1.5; d$total[3] <- Inf
    r <- ps_audit_run(d, positiveCassettes = "cassettes", showCorrelation = TRUE)
    expect_length(r$correlationPlot$state$totalSamples, 45)
    expect_length(r$correlationPlot$state$positiveCassettes, 45)
    expect_match(r$notices$content, "3 cases excluded")
    d <- ps_audit_data(); d$positive_nodes[1] <- 99; d$nodes[2] <- NA
    r <- ps_audit_run(d, totalLymphNodes = "nodes", positiveLymphNodes = "positive_nodes",
        showLNAnalysis = TRUE, showEffectSizes = TRUE)
    expect_equal(sum(as.data.frame(r$lnrClassification)$cases), 46)
    expect_match(r$notices$content, "2 cases excluded")
    r <- ps_audit_run(d, totalLymphNodes = "nodes", positiveLymphNodes = "positive_nodes",
        showLNAnalysis = TRUE, lnrThreshold1 = 0.5, lnrThreshold2 = 0.2)
    expect_equal(nrow(as.data.frame(r$lnrClassification)), 0)
    expect_match(r$notices$content, "Invalid LNR thresholds")
})

test_that("adequacy is a proportion with an interval, not a circular effect comparison", {
    r <- ps_audit_run(totalLymphNodes = "nodes", showEffectSizes = TRUE)
    tab <- as.data.frame(r$effectSizesTable)
    expect_true(any(grepl("36/48 (75.0%)", tab$value, fixed = TRUE)))
    ci <- suppressWarnings(stats::prop.test(36, 48, correct = FALSE)$conf.int)
    expect_true(any(tab$value == sprintf("%.1f%%-%.1f%%", 100 * ci[1], 100 * ci[2])))
    expect_false(any(grepl("Cliff|Odds Ratio|Relative Risk|Risk Difference", tab$measure)))
    r <- ps_audit_run(totalLymphNodes = "nodes", showEffectSizes = TRUE, adequacyThreshold = 20)
    expect_true(any(grepl("0/48 (0.0%)", as.data.frame(r$effectSizesTable)$value, fixed = TRUE)))
})

test_that("single versus summed classification uses total foci with consistent units", {
    r <- ps_audit_run(totalFoci = "total_foci", maxPositiveSingle = "single_foci",
        positiveCassettes = "cassettes", showDistributionPattern = TRUE)
    tab <- as.data.frame(r$distributionPatternTable)
    expect_equal(tab$count, c(0, 0, 48))
    expect_equal(sum(tab$percent), 1)
    d <- ps_audit_data(); d$single_foci[1] <- 7
    r <- ps_audit_run(d, totalFoci = "total_foci", maxPositiveSingle = "single_foci", showDistributionPattern = TRUE)
    expect_equal(sum(as.data.frame(r$distributionPatternTable)$count), 47)
})

test_that("labelled sample types keep labels and one-level groups do not yield a test", {
    d <- ps_audit_data()
    d$group <- structure(rep(c(1, 2), each = 24), labels = c(Core = 1, Wedge = 2),
        class = c("haven_labelled", "vctrs_vctr", "double"))
    r <- ps_audit_run(d, sampleType = "group", showStratifiedAnalysis = TRUE)
    expect_equal(as.data.frame(r$prevalenceTable)$sampleType, c("Core", "Wedge"))
    d$group <- factor(rep("Only", 48))
    r <- ps_audit_run(d, sampleType = "group", showHeterogeneityTest = TRUE)
    expect_equal(nrow(as.data.frame(r$heterogeneityTest)), 0)
    expect_match(r$heterogeneityText$content, "two sample types")
})

test_that("hypergeometric target changes the event without selecting easier cases", {
    d <- data.frame(n = c(10L, 10L), k = c(1L, 3L))
    r <- ps_audit_run(d, core = FALSE, totalPopulation = "n", successStates = "k",
        showHypergeometric = TRUE, targetDetections = 2)
    expected <- mean(stats::phyper(1, d$k, d$n - d$k, 5, lower.tail = FALSE))
    expect_equal(as.data.frame(r$hypergeometricTable)$cumProb[5], expected)
})

test_that("assumption notices are present without enabling bootstrap", {
    r <- ps_audit_run(showBootstrap = FALSE)
    expect_match(r$notices$content, "eventually observed")
    expect_match(r$notices$content, "not validated population sensitivity")
    r <- ps_audit_run(autoSelectModel = TRUE)
    expect_match(r$modelSelectionText$content, "Model applicability guide")
    expect_false(grepl("not available in this version", r$modelSelectionText$content, fixed = TRUE))
})

test_that("stratified estimates respect the selected estimator and rejection rules", {
    d <- data.frame(total = rep(10L, 40), first = rep(2L, 40),
        positive = rep(2L, 40), group = rep(c("A", "B"), each = 20))
    for (method in c("geometric", "empirical")) {
        r <- ps_audit_run(d, positiveCount = "positive", sampleType = "group",
            estimationMethod = method, showStratifiedAnalysis = TRUE)
        expected <- if (method == "geometric") 0.5 else 0.2
        expect_equal(as.data.frame(r$binomialTable)$cumProb[1], expected)
        expect_equal(as.data.frame(r$prevalenceTable)$qEstimate, rep(expected, 2))
    }
    d$positive <- rep(c(rep(1L, 19), 10L), 2)
    r <- ps_audit_run(d, positiveCount = "positive", sampleType = "group",
        estimationMethod = "empirical", showStratifiedAnalysis = TRUE)
    expect_true(all(is.na(as.data.frame(r$prevalenceTable)$qEstimate)))
    expect_equal(nrow(as.data.frame(r$stratifiedDetectionTable)), 0)
    expect_match(r$notices$content, "Subgroup estimate unavailable")

    # Pooled heterogeneity does not prohibit homogeneous subgroup estimates.
    d$positive <- rep(c(1L, 9L), each = 20)
    r <- ps_audit_run(d, positiveCount = "positive", sampleType = "group",
        estimationMethod = "empirical", showStratifiedAnalysis = TRUE)
    expect_true(is.na(as.data.frame(r$binomialTable)$cumProb[1]))
    expect_equal(as.data.frame(r$prevalenceTable)$qEstimate, c(0.1, 0.9))
})

test_that("zero-positive groups retain their observed prevalence", {
    d <- data.frame(total = rep(10L, 40), first = c(rep(2L, 20), rep(NA, 20)),
        positive = rep(c(2L, 0L), each = 20), group = rep(c("A", "B"), each = 20))
    r <- ps_audit_run(d, positiveCount = "positive", sampleType = "group",
        showStratifiedAnalysis = TRUE)
    p <- as.data.frame(r$prevalenceTable)
    expect_equal(p$sampleType, c("A", "B"))
    expect_equal(p$totalCases, c(20, 20))
    expect_equal(p$prevalence, c(1, 0))
    expect_true(is.na(p$qEstimate[2]))
})

test_that("empirical-count q does not produce an uncalibrated fit p-value", {
    d <- data.frame(total = rep(30L, 100), first = rep(1:10, 10), positive = 6L)
    r <- ps_audit_run(d, positiveCount = "positive", estimationMethod = "empirical",
        showModelFit = TRUE)
    expect_equal(nrow(as.data.frame(r$modelFitTable)), 0)
    expect_match(r$modelFitText$content, "only for the detected-case geometric estimator")
    r <- ps_audit_run(d, positiveCount = "positive", estimationMethod = "geometric",
        showModelFit = TRUE)
    expect_equal(nrow(as.data.frame(r$modelFitTable)), 1)
    expect_match(r$modelFitText$content, "untruncated geometric")
})

test_that("both bootstrap panels share the same resamples", {
    r <- ps_audit_run(showBootstrap = TRUE, showEmpiricalCumulative = TRUE,
        bootstrapIterations = 100, setSeed = TRUE, seedValue = 901)
    b <- as.data.frame(r$bootstrapTable)
    e <- as.data.frame(r$empiricalCumulativeTable)
    expect_equal(b$meanSens, e$cumDetection)
    expect_equal(b$ciLower, e$ciLower)
    expect_equal(b$ciUpper, e$ciUpper)
})

test_that("explanations distinguish observed detection from known disease", {
    r <- ps_audit_run(showPopulationDetection = TRUE, showInterpretText = TRUE,
        showGuidedInstructions = TRUE, showConciseInstructions = TRUE,
        showIncrementalYield = TRUE)
    text <- paste(r$populationDetectionText$content, r$interpretText$content,
        r$guidedInstructions$content, r$conciseInstructions$content,
        r$incrementalYieldText$content)
    expect_false(grepl("P(detect | lesion present)", text, fixed = TRUE))
    expect_false(grepl("Evidence-based sampling protocol", text, fixed = TRUE))
    expect_false(grepl("optimal stopping point", text, fixed = TRUE))
    expect_match(text, "does not establish a confirmed disease-negative case")
})

test_that("Turkish catalog messages render through the actual analysis", {
    catalog <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(catalog), "translation catalog is not available")
    lines <- readLines(catalog, encoding = "UTF-8", warn = FALSE)
    # Gettext can wrap both ids and translations across adjacent quoted lines.
    lines <- strsplit(gsub('"\n"', '', paste(lines, collapse = "\n"), fixed = TRUE),
        "\n", fixed = TRUE)[[1]]
    ids <- c("Subgroup estimate unavailable", "Observed-positive cases only",
        "Only {value} cases recorded a detected lesion. Estimates and bootstrap intervals may be unstable; assess the observation design and collect more cases before drawing conclusions.")
    messages <- setNames(lapply(ids, function(id) {
        line <- paste0("msgid ", jsonlite::toJSON(id, auto_unbox = TRUE))
        index <- which(lines == line)
        expect_length(index, 1)
        value <- jsonlite::fromJSON(sub("^msgstr ", "", lines[index + 1L]))
        expect_true(nzchar(value))
        list(value)
    }), ids)
    d <- data.frame(total = rep(10L, 6), first = c(2L, 2L, 2L, NA, NA, NA),
        positive = c(2L, 2L, 2L, 0L, 0L, 0L), group = rep(c("A", "B"), each = 3))
    o <- pathsamplingOptions$new(totalSamples = "total", firstDetection = "first",
        positiveCount = "positive", sampleType = "group", showStratifiedAnalysis = TRUE)
    # Inject the same translator class used by jmvcore with real catalog messages.
    translator <- get("Translator", asNamespace("jmvcore"))$new(
        list(locale_data = list(messages = messages)))
    o$.__enclos_env__$private$.translator <- translator
    a <- pathsamplingClass$new(options = o, data = d)
    a$init(); a$run()
    expect_match(a$results$notices$content, "Alt grup kestirimi")
    expect_match(a$results$notices$content, "Yaln\u0131zca 3 olguda")
    expect_false(grepl("{value}", a$results$notices$content, fixed = TRUE))
    expect_equal(as.data.frame(a$results$prevalenceTable)$prevalence, c(1, 0))
})

test_that("sequence groups remain descriptive when the probability estimate is rejected", {
    d <- data.frame(total = rep(10L, 20), first = rep(1L, 20),
        positive = c(rep(1L, 19), 10L),
        positions = c(rep("1", 19), "1,2,3,4,5,6,7,8,9,10"))
    r <- ps_audit_run(d, positiveCount = "positive", positiveSamplesList = "positions",
        estimationMethod = "empirical", showMultifocalAnalysis = TRUE,
        showSpatialClustering = TRUE)
    expect_true(is.na(as.data.frame(r$binomialTable)$cumProb[1]))
    expect_equal(nrow(as.data.frame(r$multifocalProbTable)), 0)
    groups <- as.data.frame(r$multifocalTable)
    expect_match(r$multifocalAnalysisText$content, "predictions are unavailable")
    expect_equal(groups$fociCount, "One sample-position group")
    expect_equal(groups$cases, 20)
    expect_match(r$multifocalText$content, "sampling-order heuristic")
    expect_false(grepl("predictions are unavailable", r$multifocalText$content))
    expect_equal(as.data.frame(r$clusteringTable)$pattern, c(
        "Below reference gap (index < 0.7)",
        "Near reference gap (index 0.7-1.3)",
        "Above reference gap (index > 1.3)"))
})

test_that("sequence gaps count groups without assigning anatomical focality", {
    d <- data.frame(total = rep(10L, 30), first = rep(1L, 30),
        positions = rep(c("1", "1,5", "1,5,9"), each = 10))
    r <- ps_audit_run(d, positiveSamplesList = "positions", showMultifocalAnalysis = TRUE,
        fociGapThreshold = 2)
    groups <- as.data.frame(r$multifocalTable)
    expect_equal(groups$fociCount, c("One sample-position group",
        "Two sample-position groups", "Three or more sample-position groups"))
    expect_equal(groups$cases, rep(10, 3))
    expect_equal(groups$percent, rep(1 / 3, 3))
    r <- ps_audit_run(d, positiveSamplesList = "positions", showMultifocalAnalysis = TRUE,
        fociGapThreshold = 4)
    expect_equal(as.data.frame(r$multifocalTable)$cases, 30)
})

test_that("fitted beta-binomial detection agrees with the VGAM distribution", {
    d <- ps_audit_data()
    r <- ps_audit_run(d, core = FALSE, totalPopulation = "population",
        successStates = "successes", showBetaBinomial = TRUE)
    fit <- VGAM::vglm(cbind(successes, population - successes) ~ 1,
        family = VGAM::betabinomial, data = d)
    parameters <- VGAM::Coef(fit)
    concentration <- (1 - parameters["rho"]) / parameters["rho"]
    expected <- 1 - VGAM::dbetabinom.ab(0, 5,
        parameters["mu"] * concentration, (1 - parameters["mu"]) * concentration)
    expect_equal(as.data.frame(r$betaBinomialTable)$cumProb[5], unname(expected),
        tolerance = 1e-9)
})
