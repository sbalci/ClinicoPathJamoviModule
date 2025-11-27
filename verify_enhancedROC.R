
# Verification Script for enhancedROC Function

# 0. Setup ----------------------------------------------------------------
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(testthat)
library(R6)
library(pROC)
library(caret)
library(dplyr)

# Unload jmvcore if loaded to avoid conflict with mocks
if ("package:jmvcore" %in% search()) {
    detach("package:jmvcore", unload = TRUE)
}

# 1. Mocks for jmvcore and dependencies -----------------------------------

# Mock jmvcore::Option classes
Option <- R6::R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value = NULL) {
            self$name <- name
            self$value <- value
        }
    )
)

OptionBool <- R6::R6Class("OptionBool", inherit = Option)
OptionList <- R6::R6Class("OptionList", inherit = Option)
OptionNumber <- R6::R6Class("OptionNumber", inherit = Option)
OptionString <- R6::R6Class("OptionString", inherit = Option)
OptionInteger <- R6::R6Class("OptionInteger", inherit = Option)
OptionVariables <- R6::R6Class("OptionVariables", inherit = Option)

# Mock jmvcore::Analysis
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
                    clinicalReport = MockHtml$new()
                )
            )
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .run = function() {},
        .checkpoint = function() {}
    )
)

# Mock Table and Html classes
MockTable <- R6::R6Class("MockTable",
    public = list(
        rows = list(),
        columns = list(),
        visible = TRUE,
        initialize = function() {
            self$rows <- list()
        },
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- list(rowKey = rowKey, values = values)
        },
        setRow = function(rowKey, values) {
            # Find row by key or index
            found <- FALSE
            for (i in seq_along(self$rows)) {
                if (!is.null(self$rows[[i]]$rowKey) && self$rows[[i]]$rowKey == rowKey) {
                    # Update values
                    for (name in names(values)) {
                        self$rows[[i]]$values[[name]] <- values[[name]]
                    }
                    found <- TRUE
                    break
                }
            }
            if (!found) {
                self$addRow(rowKey, values)
            }
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            
            # Extract all unique column names
            col_names <- unique(unlist(lapply(self$rows, function(r) names(r$values))))
            
            # Create data frame
            df_list <- lapply(col_names, function(col) {
                sapply(self$rows, function(r) {
                    val <- r$values[[col]]
                    if (is.null(val)) NA else val
                })
            })
            names(df_list) <- col_names
            as.data.frame(df_list, stringsAsFactors = FALSE)
        },
        setVisible = function(visible) {
            self$visible <- visible
        },
        isFilled = function() {
            return(length(self$rows) > 0)
        },
        setTitle = function(title) {},
        setNote = function(key, note) {},
        addFootnote = function(rowKey, col, note) {}
    )
)

MockHtml <- R6::R6Class("MockHtml",
    public = list(
        content = NULL,
        visible = TRUE,
        setContent = function(content) {
            self$content <- content
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

# Mock jmvcore environment
assign("Table", MockTable, envir = .GlobalEnv)
assign("Html", MockHtml, envir = .GlobalEnv)
assign("Analysis", Analysis, envir = .GlobalEnv)

# Mock jmvcore functions
mock_jmvcore <- new.env()
mock_jmvcore$format <- function(fmt, ...) {
    args <- list(...)
    for (name in names(args)) {
        fmt <- gsub(paste0("\\{", name, "\\}"), args[[name]], fmt)
    }
    fmt
}
mock_jmvcore$. <- function(text) text

# Define . globally as well to ensure it's found
. <- function(text) text

# Attach mock jmvcore
if ("package:jmvcore" %in% search()) detach("package:jmvcore", unload = TRUE)
attach(mock_jmvcore, name = "jmvcore_mock", warn.conflicts = FALSE)

# Source the enhancedROC implementation
source("R/enhancedROC.b.R")

# Define enhancedROCBase for inheritance
enhancedROCBase <- R6::R6Class(
    "enhancedROCBase",
    inherit = Analysis,
    public = list(
        initialize = function(options, data) {
            super$initialize(options, data)
            private$.init()
        }
    )
)

# 2. Test Data Generation -------------------------------------------------

set.seed(123)
n <- 200
# Generate data with known properties
# Predictor 1: Good discriminator (AUC ~ 0.8)
pred1 <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 2, sd = 1))
# Predictor 2: Poor discriminator (AUC ~ 0.5)
pred2 <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 0.2, sd = 1))
# Outcome: 0 for first 100, 1 for next 100
outcome <- factor(c(rep("Control", 100), rep("Case", 100)), levels = c("Control", "Case"))

test_data <- data.frame(
    outcome = outcome,
    pred1 = pred1,
    pred2 = pred2
)

# 3. Test Cases -----------------------------------------------------------

test_that("Single ROC Analysis", {
    options <- list(
        outcome = "outcome",
        predictors = c("pred1"),
        positiveClass = "Case",
        analysisType = "single",
        direction = "auto",
        youdenOptimization = TRUE,
        sensitivityThreshold = 0,
        specificityThreshold = 0,
        confidenceLevel = 95,
        useBootstrap = FALSE,
        aucTable = TRUE,
        optimalCutoffs = TRUE,
        cutoffTable = TRUE,
        diagnosticMetrics = TRUE,
        clinicalMetrics = TRUE,
        pairwiseComparisons = FALSE,
        statisticalComparison = FALSE,
        partialAuc = FALSE,
        crocAnalysis = FALSE,
        convexHull = FALSE,
        comprehensive_output = FALSE,
        clinical_interpretation = FALSE,
        detectImbalance = FALSE,
        smoothMethod = "none",
        customCutoffs = "",
        clinicalPresets = "custom",
        clinicalContext = "general",
        showMetricsDiff = TRUE
    )
    
    analysis <- enhancedROCClass$new(options, test_data)
    analysis$run()
    
    # Check AUC calculation
    auc_table <- analysis$results$results$aucSummary$asDF()
    expect_equal(nrow(auc_table), 1)
    
    # Calculate expected AUC using pROC
    expected_roc <- pROC::roc(test_data$outcome, test_data$pred1, direction = "<", levels = c("Control", "Case"))
    expected_auc <- as.numeric(expected_roc$auc)
    
    expect_equal(auc_table$auc, expected_auc, tolerance = 1e-4)
    
    # Check Optimal Cutoff
    cutoff_table <- analysis$results$results$optimalCutoffSummary$asDF()
    expect_equal(nrow(cutoff_table), 1)
    
    # Check Diagnostic Metrics
    diag_table <- analysis$results$results$diagnosticPerformance$asDF()
    expect_true(nrow(diag_table) > 0)
})

test_that("Comparative ROC Analysis", {
    options <- list(
        outcome = "outcome",
        predictors = c("pred1", "pred2"),
        positiveClass = "Case",
        analysisType = "comparative",
        direction = "auto",
        youdenOptimization = TRUE,
        sensitivityThreshold = 0,
        specificityThreshold = 0,
        confidenceLevel = 95,
        useBootstrap = FALSE,
        aucTable = TRUE,
        optimalCutoffs = TRUE,
        cutoffTable = TRUE,
        diagnosticMetrics = TRUE,
        clinicalMetrics = TRUE,
        pairwiseComparisons = TRUE,
        comparisonMethod = "delong",
        statisticalComparison = TRUE,
        partialAuc = FALSE,
        crocAnalysis = FALSE,
        convexHull = FALSE,
        comprehensive_output = FALSE,
        clinical_interpretation = FALSE,
        detectImbalance = FALSE,
        smoothMethod = "none",
        customCutoffs = "",
        clinicalPresets = "custom",
        clinicalContext = "general",
        showMetricsDiff = TRUE
    )
    
    analysis <- enhancedROCClass$new(options, test_data)
    analysis$run()
    
    # Check AUC table has 2 rows
    auc_table <- analysis$results$results$aucSummary$asDF()
    expect_equal(nrow(auc_table), 2)
    
    # Check Comparison Table
    comp_table <- analysis$results$results$rocComparisons$asDF()
    expect_equal(nrow(comp_table), 1) # 1 comparison: pred1 vs pred2
    
    # Verify DeLong test p-value
    roc1 <- pROC::roc(test_data$outcome, test_data$pred1, direction = "<", levels = c("Control", "Case"))
    roc2 <- pROC::roc(test_data$outcome, test_data$pred2, direction = "<", levels = c("Control", "Case"))
    test_res <- pROC::roc.test(roc1, roc2, method = "delong")
    
    expect_equal(comp_table$p_value, test_res$p.value, tolerance = 1e-4)
})

test_that("Input Validation", {
    # Test with non-binary outcome
    bad_data <- test_data
    bad_data$outcome <- factor(rep("A", 200))
    
    options <- list(
        outcome = "outcome",
        predictors = c("pred1"),
        positiveClass = "A",
        analysisType = "single",
        direction = "auto",
        youdenOptimization = TRUE,
        sensitivityThreshold = 0,
        specificityThreshold = 0,
        confidenceLevel = 95,
        useBootstrap = FALSE,
        aucTable = TRUE,
        optimalCutoffs = TRUE,
        cutoffTable = TRUE,
        diagnosticMetrics = TRUE,
        clinicalMetrics = TRUE,
        pairwiseComparisons = FALSE,
        statisticalComparison = FALSE,
        partialAuc = FALSE,
        crocAnalysis = FALSE,
        convexHull = FALSE,
        comprehensive_output = FALSE,
        clinical_interpretation = FALSE,
        detectImbalance = FALSE,
        smoothMethod = "none",
        customCutoffs = "",
        clinicalPresets = "custom",
        clinicalContext = "general",
        showMetricsDiff = TRUE
    )
    
    analysis <- enhancedROCClass$new(options, bad_data)
    
    # Should throw error
    analysis$run()
    
    # Should have updated instructions with error message
    instructions <- analysis$results$results$instructions$content
    expect_true(grepl("Insufficient Outcome Variable Levels", instructions))
})

test_that("Clinical Presets", {
    # Test biomarker_screening preset (Sensitivity >= 0.90)
    options <- list(
        outcome = "outcome",
        predictors = c("pred1"),
        positiveClass = "Case",
        analysisType = "single",
        direction = "auto",
        youdenOptimization = FALSE, # Should be overridden by preset
        sensitivityThreshold = 0,
        specificityThreshold = 0,
        confidenceLevel = 95,
        useBootstrap = FALSE,
        aucTable = TRUE,
        optimalCutoffs = TRUE,
        cutoffTable = TRUE,
        diagnosticMetrics = TRUE,
        clinicalMetrics = TRUE,
        pairwiseComparisons = FALSE,
        statisticalComparison = FALSE,
        partialAuc = FALSE,
        crocAnalysis = FALSE,
        convexHull = FALSE,
        comprehensive_output = FALSE,
        clinical_interpretation = FALSE,
        detectImbalance = FALSE,
        smoothMethod = "none",
        customCutoffs = "",
        clinicalPresets = "biomarker_screening",
        clinicalContext = "screening",
        prevalence = 0.1,
        showMetricsDiff = TRUE
    )
    
    analysis <- enhancedROCClass$new(options, test_data)
    analysis$run()
    
    # Check optimal cutoff table
    cutoff_table <- analysis$results$results$optimalCutoffSummary$asDF()
    
    # Sensitivity should be >= 0.90
    expect_true(cutoff_table$sensitivity >= 0.90)
})

print("Verification Script Created Successfully")
