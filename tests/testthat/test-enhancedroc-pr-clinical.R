library(testthat)

test_that("enhancedROC precision-recall and clinical metrics honor prevalence and impact options", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    library(R6)
    library(pROC)
    library(caret)

    # Minimal jmvcore mocks
    MockTable <- R6::R6Class("MockTable",
        public = list(
            rows = list(),
            addRow = function(rowKey, values) {
                self$rows[[length(self$rows) + 1]] <- list(rowKey = rowKey, values = values)
            },
            setRow = function(rowNo = NULL, rowKey = NULL, values) {
                if (!is.null(rowNo) && rowNo <= length(self$rows)) {
                    self$rows[[rowNo]]$values <- values
                } else {
                    self$addRow(rowKey, values)
                }
            },
            asDF = function() {
                if (length(self$rows) == 0) return(data.frame())
                cols <- unique(unlist(lapply(self$rows, function(r) names(r$values))))
                df <- as.data.frame(do.call(rbind, lapply(self$rows, function(r) {
                    vals <- r$values
                    missing <- setdiff(cols, names(vals))
                    vals[missing] <- NA
                    vals[cols]
                })), stringsAsFactors = FALSE)
                colnames(df) <- cols
                df
            }
        )
    )
    MockHtml <- R6::R6Class("MockHtml", public = list(content = NULL, setContent = function(x) self$content <- x))
    MockImage <- R6::R6Class("MockImage", public = list(setState = function(...) {}))

    mock_jmvcore <- new.env()
    mock_jmvcore$Table <- MockTable
    mock_jmvcore$Html <- MockHtml
    mock_jmvcore$Image <- MockImage
    mock_jmvcore$`%||%` <- function(a, b) if (!is.null(a)) a else b
    mock_jmvcore$. <- function(text) text
    mock_jmvcore$format <- function(fmt, ...) sprintf(fmt, ...)
    attach(mock_jmvcore, name = "jmvcore", warn.conflicts = FALSE)
    on.exit(detach("jmvcore", character.only = TRUE), add = TRUE)

    # Analysis stub
    Analysis <- R6::R6Class("Analysis",
        public = list(
            options = NULL,
            data = NULL,
            results = NULL,
            initialize = function(options, data) {
                self$options <- options
                self$data <- data
                self$results <- list(
                    results = list(
                        instructions = MockHtml$new(),
                        aucSummary = MockTable$new(),
                        optimalCutoffSummary = MockTable$new(),
                        cutoffAnalysis = MockTable$new(),
                        diagnosticPerformance = MockTable$new(),
                        clinicalApplicationMetrics = MockTable$new(),
                        rocComparisons = MockTable$new(),
                        detailedComparison = MockTable$new(),
                        statisticalSummary = MockTable$new(),
                        partialAucAnalysis = MockTable$new(),
                        crocAnalysisTable = MockTable$new(),
                        convexHullTable = MockTable$new(),
                        comprehensiveAnalysisSummary = MockTable$new(),
                        clinicalInterpretationGuide = MockHtml$new(),
                        imbalanceMetrics = MockTable$new(),
                        imbalanceWarning = MockHtml$new(),
                        analysisSummary = MockHtml$new(),
                        clinicalReport = MockHtml$new(),
                        precisionRecallTable = MockTable$new(),
                        clinicalImpactTable = MockTable$new(),
                        decisionImpactSummary = MockTable$new()
                    )
                )
            },
            run = function() private$.run()
        ),
        private = list(.run = function() {}, .checkpoint = function(...) {})
    )

    enhancedROCBase <- R6::R6Class("enhancedROCBase",
        inherit = Analysis,
        public = list(
            initialize = function(options, data) {
                super$initialize(options, data)
                private$.init()
            }
        )
    )
    assign("enhancedROCBase", enhancedROCBase, envir = .GlobalEnv)

    # Load backend
    backend_path <- testthat::test_path("..", "..", "R", "enhancedroc.b.R")
    source(backend_path)

    # Imbalanced data for PR metrics
    set.seed(123)
    outcome <- factor(c(rep("Case", 20), rep("Control", 80)), levels = c("Control", "Case"))
    predictor <- c(rnorm(20, 2), rnorm(80, 0))
    data_pr <- data.frame(outcome = outcome, pred = predictor)

    pr_opts <- list(
        outcome = "outcome",
        predictors = c("pred"),
        positiveClass = "Case",
        analysisType = "single",
        direction = "auto",
        youdenOptimization = TRUE,
        confidenceLevel = 95,
        useBootstrap = FALSE,
        stratifiedBootstrap = TRUE,
        bootstrapMethod = "bca",
        bootstrapSamples = 200,
        partialAuc = FALSE,
        aucTable = TRUE,
        optimalCutoffs = TRUE,
        cutoffTable = TRUE,
        diagnosticMetrics = TRUE,
        clinicalMetrics = TRUE,
        detectImbalance = TRUE,
        pairwiseComparisons = FALSE,
        statisticalComparison = FALSE,
        recommendPRC = TRUE,
        showImbalanceWarning = TRUE,
        crocAnalysis = FALSE,
        convexHull = FALSE,
        comprehensive_output = FALSE,
        clinical_interpretation = FALSE,
        clinicalImpact = FALSE,
        calibrationAnalysis = FALSE,
        multiClassROC = FALSE,
        timeDependentROC = FALSE,
        survivalROC = FALSE,
        internalValidation = FALSE,
        useObservedPrevalence = TRUE,
        smoothMethod = "none",
        customCutoffs = "",
        clinicalPresets = "custom",
        clinicalContext = "general",
        showMetricsDiff = FALSE
    )
    pr_analysis <- enhancedROCClass$new(pr_opts, data_pr)
    pr_analysis$run()
    pr_df <- pr_analysis$results$results$precisionRecallTable$asDF()
    expect_true(nrow(pr_df) == 1)
    expect_true(pr_df$auc_pr >= 0 && pr_df$auc_pr <= 1)

    # Clinical metrics prevalence toggle
    outcome2 <- factor(c(rep("Case", 30), rep("Control", 20)), levels = c("Control", "Case"))
    data_prev <- data.frame(outcome = outcome2, marker = c(rnorm(30, 2), rnorm(20, 0)))
    observed_prev <- mean(outcome2 == "Case")

    opts_obs <- pr_opts
    opts_obs$predictors <- c("marker")
    opts_obs$useObservedPrevalence <- TRUE
    opts_obs$clinicalMetrics <- TRUE
    opts_obs$detectImbalance <- FALSE
    opts_obs$pairwiseComparisons <- FALSE
    opts_obs$statisticalComparison <- FALSE
    opts_obs$partialAuc <- FALSE
    prev_analysis <- enhancedROCClass$new(opts_obs, data_prev)
    prev_analysis$run()
    clin_df <- prev_analysis$results$results$clinicalApplicationMetrics$asDF()
    expect_true(abs(as.numeric(clin_df$prevalence) - observed_prev) < 1e-6)

    opts_override <- opts_obs
    opts_override$useObservedPrevalence <- FALSE
    opts_override$prevalence <- 0.2
    prev_analysis2 <- enhancedROCClass$new(opts_override, data_prev)
    prev_analysis2$run()
    clin_df2 <- prev_analysis2$results$results$clinicalApplicationMetrics$asDF()
    expect_equal(as.numeric(clin_df2$prevalence), 0.2)

    # Clinical impact tables populate
    impact_opts <- opts_obs
    impact_opts$clinicalImpact <- TRUE
    impact_opts$decisionImpactTable <- TRUE
    impact_opts$predictors <- c("marker")
    impact_opts$pairwiseComparisons <- FALSE
    impact_opts$statisticalComparison <- FALSE
    impact_opts$partialAuc <- FALSE
    impact_analysis <- enhancedROCClass$new(impact_opts, data_prev)
    impact_analysis$run()
    impact_df <- impact_analysis$results$results$clinicalImpactTable$asDF()
    expect_true(nrow(impact_df) >= 1)
    expect_true("net_benefit_per_100" %in% colnames(impact_df))
})
