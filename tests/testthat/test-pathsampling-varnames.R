# Regression tests for pathsampling input handling.

test_that('pathsampling accepts variable names that are not valid R identifiers', {
    # Before the fix, every column was looked up through an .escapeVar() helper whose
    # output named no existing column ("Total Blocks" -> "Total_Blocks"), so
    # data[[esc]] returned NULL and a perfectly valid selection reported "no data".
    set.seed(42)
    n <- 60
    total <- sample(8:12, n, replace = TRUE)
    d <- data.frame(
        check.names = FALSE,
        # Homogeneous per-case positivity (~25%) so the binomial model stays
        # applicable -- with a high coefficient of variation the analysis
        # deliberately disables it and every cumProb is NA.
        `Total Blocks`    = total,
        `First Detection` = pmin(sample(1:4, n, replace = TRUE), total),
        `Pozitif örnek (n)` = round(total * 0.25)
    )

    res <- pathsampling(
        data              = d,
        totalSamples      = 'Total Blocks',
        firstDetection    = 'First Detection',
        positiveCount     = 'Pozitif örnek (n)',
        showBinomialModel = TRUE,
        showBootstrap     = FALSE
    )

    # These counts are only reachable if the space-bearing columns were found.
    info <- as.data.frame(res$dataInfo)
    expect_equal(info[info$measure == 'Total cases supplied', 'value'], '60')

    # Rows only appear, with finite probabilities, if the data reached the model.
    tbl <- as.data.frame(res$binomialTable)
    expect_gt(nrow(tbl), 0)
    expect_true(all(is.finite(tbl$cumProb)))
})

test_that('the high-heterogeneity path does not reference a non-existent result item', {
    # This path fetched self$results$binomialRecommendTable, which the schema does not
    # define (it is called `recommendTable`), so any dataset with CV > 0.5 across cases
    # crashed the whole analysis with "does not exist in this results element".
    set.seed(11)
    n <- 40
    total <- rep(10L, n)
    # Wildly varying per-case positivity -> coefficient of variation well above 0.5.
    positive <- c(rep(1L, n / 2), rep(9L, n / 2))

    d <- data.frame(
        totalSamples   = total,
        firstDetection = sample(1:4, n, replace = TRUE),
        positiveCount  = positive
    )

    expect_no_error(
        pathsampling(
            data              = d,
            totalSamples      = 'totalSamples',
            firstDetection    = 'firstDetection',
            positiveCount     = 'positiveCount',
            estimationMethod  = 'empirical',
            showBinomialModel = TRUE,
            showBootstrap     = FALSE
        )
    )
})

test_that('labelled numeric counts survive the finite-population models', {
    # Guards the labelled-data handling that the beta-binomial branch was missing
    # (the hypergeometric branch above it already had it).
    set.seed(7)
    n <- 40
    lab <- function(x) { attr(x, 'labels') <- c(low = 20, high = 40); x }

    d <- data.frame(
        totalSamples    = sample(5:10, n, replace = TRUE),
        firstDetection  = sample(c(1:4, NA), n, replace = TRUE),
        totalPopulation = lab(sample(c(20, 30, 40), n, replace = TRUE)),
        successStates   = sample(c(2, 4, 6), n, replace = TRUE)
    )

    expect_no_error(
        pathsampling(
            data              = d,
            totalSamples      = 'totalSamples',
            firstDetection    = 'firstDetection',
            totalPopulation   = 'totalPopulation',
            successStates     = 'successStates',
            showHypergeometric = TRUE,
            showBetaBinomial  = TRUE,
            showBootstrap     = FALSE
        )
    )
})

test_that("each display option runs on its own without a companion option", {
    # Every one of these defaults to FALSE, so a user ticking a single box is the
    # ordinary case. showPopulationDetection used to abort the whole analysis with
    # "object 'pForCalc' not found" -- pForCalc was assigned only inside
    # `if (self$options$showBinomialModel)` but read unconditionally further down.
    set.seed(5)
    n <- 40
    d <- data.frame(
        totalSamples        = rep(12L, n),
        firstDetection      = ifelse(runif(n) < 0.8, sample(1:6, n, TRUE), NA_integer_),
        positiveCount       = sample(2:5, n, TRUE),
        positiveSamplesList = replicate(n, "1,3,5"),
        sampleType          = rep(c("Core", "Wedge"), length.out = n),
        positiveCassettes   = sample(2:5, n, TRUE),
        maxPositiveSingle   = sample(1:3, n, TRUE),
        totalPopulation     = rep(30L, n),
        successStates       = sample(2:6, n, TRUE),
        totalLymphNodes     = sample(10:20, n, TRUE),
        positiveLymphNodes  = sample(0:4, n, TRUE),
        stringsAsFactors    = FALSE
    )
    vars <- list(totalSamples = 'totalSamples', firstDetection = 'firstDetection',
        positiveCount = 'positiveCount', positiveSamplesList = 'positiveSamplesList',
        sampleType = 'sampleType', positiveCassettes = 'positiveCassettes',
        maxPositiveSingle = 'maxPositiveSingle', totalPopulation = 'totalPopulation',
        successStates = 'successStates', totalLymphNodes = 'totalLymphNodes',
        positiveLymphNodes = 'positiveLymphNodes')

    opts <- c("showBinomialModel", "showBootstrap", "showTumorBurden", "showStageMigration",
              "showCorrelation", "showDistributionPattern", "showHypergeometric",
              "showBetaBinomial", "showLNAnalysis", "showEffectSizes", "showOmentumAnalysis",
              "showClinicalSummary", "showEmpiricalCumulative", "showSpatialClustering",
              "showStratifiedAnalysis", "showPopulationDetection", "showIncrementalYield",
              "showMultifocalAnalysis", "showProbabilityExplanation", "showKeyResults",
              "showRecommendText", "showInterpretText", "showHeterogeneityTest",
              "showModelFit", "showObsPred", "showSampleSizePlanning", "autoSelectModel")

    failures <- character()
    for (o in opts) {
        err <- tryCatch({
            do.call(pathsampling, c(list(data = d), vars, stats::setNames(list(TRUE), o)))
            NULL
        }, error = function(e) conditionMessage(e))
        if (!is.null(err)) failures <- c(failures, paste0(o, ": ", err))
    }
    expect_equal(failures, character(0))
})

test_that("re-running the same analysis instance does not duplicate table rows", {
    # addRow() accepts a duplicate rowKey. jamovi re-runs .run() on the SAME instance
    # whenever an option changes, so without deleteRows() every table appended a second
    # copy of its rows and then failed with "non-unique values when setting 'row.names'".
    # The public wrapper builds a fresh instance per call, so this needs the class directly.
    Opt <- getFromNamespace("pathsamplingOptions", "ClinicoPath")
    Cls <- getFromNamespace("pathsamplingClass",   "ClinicoPath")

    set.seed(4)
    n <- 60
    tot <- sample(8:16, n, TRUE)
    d <- data.frame(
        totalSamples = tot,
        firstDetection = ifelse(runif(n) < 0.8, pmin(rgeom(n, 0.3) + 1L, tot), NA_integer_),
        positiveCount = pmax(1L, round(tot * 0.3)),
        sampleType = sample(c("A", "B", "C"), n, TRUE),
        positiveCassettes = pmax(1L, round(tot * 0.3)),
        maxPositiveSingle = pmax(1L, round(tot * 0.15)),
        totalPopulation = rep(30L, n),
        successStates = pmax(1L, round(tot * 0.25)),
        totalLymphNodes = sample(8:25, n, TRUE),
        positiveLymphNodes = sample(0:6, n, TRUE)
    )
    o <- Opt$new(
        totalSamples = 'totalSamples', firstDetection = 'firstDetection',
        positiveCount = 'positiveCount', sampleType = 'sampleType',
        positiveCassettes = 'positiveCassettes', maxPositiveSingle = 'maxPositiveSingle',
        totalPopulation = 'totalPopulation', successStates = 'successStates',
        totalLymphNodes = 'totalLymphNodes', positiveLymphNodes = 'positiveLymphNodes',
        showBinomialModel = TRUE, showBootstrap = TRUE, showTumorBurden = TRUE,
        showStageMigration = TRUE, showCorrelation = TRUE, showDistributionPattern = TRUE,
        showHypergeometric = TRUE, showBetaBinomial = TRUE, showLNAnalysis = TRUE,
        showEffectSizes = TRUE, showModelFit = TRUE, showObsPred = TRUE,
        showSampleSizePlanning = TRUE, showHeterogeneityTest = TRUE,
        showEmpiricalCumulative = TRUE, showStratifiedAnalysis = TRUE,
        showPopulationDetection = TRUE, showIncrementalYield = TRUE,
        showMultifocalAnalysis = TRUE, showSpatialClustering = TRUE
    )
    a <- Cls$new(options = o, data = d)
    a$init()

    rowcounts <- function() vapply(a$results$itemNames, function(nm) {
        it <- a$results$get(nm)
        if (!inherits(it, "Table")) return(NA_integer_)
        tryCatch(nrow(as.data.frame(it)), error = function(e) -1L)
    }, integer(1))

    a$run(); first  <- rowcounts()
    a$run(); second <- rowcounts()
    a$run(); third  <- rowcounts()

    expect_false(any(first < 0, na.rm = TRUE))   # -1 == as.data.frame() threw
    expect_equal(second, first)
    expect_equal(third, first)
})

test_that("model-rejection paths do not crash", {
    # These call deleteRows() on the result tables. They used to call clearRows(),
    # which is not a jmvcore Table method, so every rejection died with
    # "'clearRows' does not exist in this results element".
    n <- 20
    d <- data.frame(
        totalSamples    = rep(10L, n),
        firstDetection  = sample(1:4, n, TRUE),
        totalPopulation = rep(5L, n),      # successStates > totalPopulation everywhere
        successStates   = rep(40L, n)      # -> both finite-population models reject
    )
    for (opt in c("showHypergeometric", "showBetaBinomial")) {
        expect_no_error(
            do.call(pathsampling, c(
                list(data = d, totalSamples = 'totalSamples', firstDetection = 'firstDetection',
                     totalPopulation = 'totalPopulation', successStates = 'successStates'),
                stats::setNames(list(TRUE), opt)
            ))
        )
    }
})

test_that("first run produces the headline result without any box being ticked", {
    # Deliberate departure from the repo's defaults_false convention, confirmed with the
    # maintainer: with everything off the analysis populated 3 of 67 outputs and the
    # recommended sample size -- the thing the analysis is named for -- required the user to
    # find showBinomialModel among 33 checkboxes. These two defaults are pinned here so a
    # later sweep cannot flip them back silently.
    a <- yaml::read_yaml("../../jamovi/pathsampling.a.yaml")
    on <- vapply(a$options, function(o)
        isTRUE(o$type == "Bool") && isTRUE(o$default), logical(1))
    names(on) <- vapply(a$options, function(o) o$name, character(1))
    expect_true(on[["showBinomialModel"]])
    expect_true(on[["showKeyResults"]])

    set.seed(7)
    n <- 45
    tot <- sample(10:14, n, TRUE)
    d <- data.frame(
        totalSamples   = tot,
        firstDetection = ifelse(runif(n) < 0.75, pmin(rgeom(n, 0.28) + 1L, tot), NA_integer_)
    )
    r <- pathsampling(data = d, totalSamples = 'totalSamples', firstDetection = 'firstDetection')

    rec <- as.data.frame(r$recommendTable)
    expect_gt(nrow(rec), 0)
    expect_true(all(is.finite(rec$minSamples)))
    expect_false(is.unsorted(rec$minSamples))   # more confidence needs more samples
})
