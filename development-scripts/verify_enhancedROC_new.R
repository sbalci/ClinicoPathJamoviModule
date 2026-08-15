
# Verification Script for enhancedROC Function (New Features)

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
                    clinicalReport = MockHtml$new(),
                    
                    # New Tables
                    calibrationSummary = MockTable$new(),
                    hosmerLemeshowTable = MockTable$new(),
                    multiClassAUC = MockTable$new(),
                    multiClassAverage = MockTable$new(),
                    clinicalImpactTable = MockTable$new(),
                    decisionImpactSummary = MockTable$new(),
                    
                    # Plots (Mocked as Image objects but we don't test rendering logic deeply)
                    calibrationPlotImage = MockImage$new(),
                    multiClassROCPlot = MockImage$new(),
                    clinicalUtilityPlot = MockImage$new()
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

# Mock Table, Html, Image classes
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
        setRow = function(rowNo, values) {
             # Simple mock for setRow
             if (rowNo > length(self$rows)) {
                 self$rows[[rowNo]] <- list(values = values)
             } else {
                 # Update existing
                 for (name in names(values)) {
                     self$rows[[rowNo]]$values[[name]] <- values[[name]]
                 }
             }
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            col_names <- unique(unlist(lapply(self$rows, function(r) names(r$values))))
            df_list <- lapply(col_names, function(col) {
                sapply(self$rows, function(r) {
                    val <- r$values[[col]]
                    if (is.null(val)) NA else val
                })
            })
            names(df_list) <- col_names
            as.data.frame(df_list, stringsAsFactors = FALSE)
        },
        setVisible = function(visible) { self$visible <- visible },
        isFilled = function() { return(length(self$rows) > 0) },
        setTitle = function(title) {},
        setNote = function(key, note) {},
        addFootnote = function(rowKey, col, note) {}
    )
)

MockHtml <- R6::R6Class("MockHtml",
    public = list(
        content = NULL,
        visible = TRUE,
        setContent = function(content) { self$content <- content },
        setVisible = function(visible) { self$visible <- visible }
    )
)

MockImage <- R6::R6Class("MockImage",
    public = list(
        visible = TRUE,
        state = list(),
        setVisible = function(visible) { self$visible <- visible },
        setState = function(state) { self$state <- state }
    )
)

# Mock jmvcore environment
assign("Table", MockTable, envir = .GlobalEnv)
assign("Html", MockHtml, envir = .GlobalEnv)
assign("Image", MockImage, envir = .GlobalEnv)
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
. <- function(text) text

if ("package:jmvcore" %in% search()) detach("package:jmvcore", unload = TRUE)
attach(mock_jmvcore, name = "jmvcore_mock", warn.conflicts = FALSE)

# Define enhancedROCBase for inheritance (minimal stub)
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
assign("enhancedROCBase", enhancedROCBase, envir = .GlobalEnv)

# Source the enhancedROC implementation
source("../R/enhancedroc.b.R")

# 2. Test Data Generation -------------------------------------------------

set.seed(123)
n <- 200
# Binary Outcome Data
pred1 <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 2, sd = 1))
outcome <- factor(c(rep("Control", 100), rep("Case", 100)), levels = c("Control", "Case"))
test_data <- data.frame(outcome = outcome, pred1 = pred1)

# Multi-Class Outcome Data
n_mc <- 300
outcome_mc <- factor(c(rep("A", 100), rep("B", 100), rep("C", 100)), levels = c("A", "B", "C"))
pred_mc <- c(rnorm(100, 0), rnorm(100, 2), rnorm(100, 4))
test_data_mc <- data.frame(outcome = outcome_mc, pred = pred_mc)

# 3. Test Cases -----------------------------------------------------------

test_that("Calibration Analysis", {
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
        showMetricsDiff = TRUE,
        
        # New Options
        calibrationAnalysis = TRUE,
        hosmerLemeshow = TRUE,
        hlGroups = 10,
        calibrationPlot = FALSE,
        multiClassROC = FALSE,
        clinicalImpact = FALSE,
        internalValidation = FALSE,
        timeDependentROC = FALSE,
        survivalROC = FALSE
    )
    
    analysis <- enhancedROCClass$new(options, test_data)
    analysis$run()
    
    cal_table <- analysis$results$results$calibrationSummary$asDF()
    expect_equal(nrow(cal_table), 1)
    expect_true(!is.na(cal_table$brier_score))
    
    hl_table <- analysis$results$results$hosmerLemeshowTable$asDF()
    expect_equal(nrow(hl_table), 1)
    expect_true(!is.na(hl_table$p_value))
})

test_that("Multi-Class ROC Analysis", {
    options <- list(
        outcome = "outcome",
        predictors = c("pred"),
        positiveClass = "A", # Ignored for multi-class
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
        showMetricsDiff = TRUE,
        
        # New Options
        calibrationAnalysis = FALSE,
        multiClassROC = TRUE,
        multiClassStrategy = "pairwise", # or "ovr"
        clinicalImpact = FALSE,
        internalValidation = FALSE,
        timeDependentROC = FALSE,
        survivalROC = FALSE
    )
    
    analysis <- enhancedROCClass$new(options, test_data_mc)
    analysis$run()
    
    avg_table <- analysis$results$results$multiClassAverage$asDF()
    expect_equal(nrow(avg_table), 1)
    expect_true(!is.na(avg_table$macro_auc))
    
    auc_table <- analysis$results$results$multiClassAUC$asDF()
    # Pairwise for 3 classes: A vs B, A vs C, B vs C = 3 pairs
    expect_equal(nrow(auc_table), 3)
})

test_that("Clinical Impact Analysis", {
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
        showMetricsDiff = TRUE,
        
        # New Options
        calibrationAnalysis = FALSE,
        multiClassROC = FALSE,
        clinicalImpact = TRUE,
        clinicalUtilityCurve = FALSE,
        internalValidation = FALSE,
        timeDependentROC = FALSE,
        survivalROC = FALSE
    )
    
    analysis <- enhancedROCClass$new(options, test_data)
    analysis$run()
    
    impact_table <- analysis$results$results$clinicalImpactTable$asDF()
    expect_equal(nrow(impact_table), 1)
    expect_true(!is.na(impact_table$net_benefit))
})

test_that("Internal Validation", {
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
        showMetricsDiff = TRUE,
        
        # New Options
        calibrationAnalysis = FALSE,
        multiClassROC = FALSE,
        clinicalImpact = FALSE,
        internalValidation = TRUE,
        validationMethod = "bootstrap",
        timeDependentROC = FALSE,
        survivalROC = FALSE
    )
    
    analysis <- enhancedROCClass$new(options, test_data)
    analysis$run()
    
    # Check if summary text contains validation results
    summary_content <- analysis$results$results$analysisSummary$content
    expect_true(grepl("Internal Validation", summary_content))
    expect_true(grepl("Bootstrap", summary_content))
})

print("New Verification Script Created Successfully")
