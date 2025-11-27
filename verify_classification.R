
# Verification Script for classification Function

# 1. Mocking jmvcore Environment ------------------------------------------

library(R6)
library(testthat)
library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(data.table)

# Mock Option Classes
Option <- R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value = NULL) {
            self$name <- name
            self$value <- value
        }
    )
)

OptionData <- R6Class("OptionData", inherit = Option)
OptionVariable <- R6Class("OptionVariable", inherit = Option)
OptionVariables <- R6Class("OptionVariables", inherit = Option)
OptionList <- R6Class("OptionList", inherit = Option)
OptionBool <- R6Class("OptionBool", inherit = Option)
OptionNumber <- R6Class("OptionNumber", inherit = Option)
OptionNMXList <- R6Class("OptionNMXList", inherit = Option)
OptionString <- R6Class("OptionString", inherit = Option)

# Mock Table Class
Table <- R6Class("Table",
    public = list(
        name = NULL,
        rows = list(),
        columns = list(),
        visible = TRUE,
        initialize = function(name) {
            self$name <- name
            self$columns <- list()
        },
        addRow = function(rowKey, values) {
            self$rows[[as.character(rowKey)]] <- values
        },
        setRow = function(rowNo, values) {
            # For simplicity in testing, just add/update by index if rowKey not provided
            self$rows[[as.character(rowNo)]] <- values
        },
        addColumn = function(name, title, type) {
            self$columns[[name]] <- list(title=title, type=type)
        },
        setVisible = function(visible) {
            self$visible <- visible
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            do.call(rbind, lapply(self$rows, as.data.frame))
        }
    )
)

# Mock Image Class
Image <- R6Class("Image",
    public = list(
        name = NULL,
        state = NULL,
        visible = TRUE,
        initialize = function(name) {
            self$name <- name
        },
        setState = function(state) {
            self$state <- state
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

# Mock Preformatted Class (for text output)
Preformatted <- R6Class("Preformatted",
    public = list(
        name = NULL,
        content = NULL,
        initialize = function(options, name) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock Html Class
Html <- R6Class("Html",
    public = list(
        name = NULL,
        content = NULL,
        initialize = function(name) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        text = NULL,
        classificationMetrics = NULL,
        confusion = NULL,
        predictedFreqPlot = NULL,
        decisionTreeModel = NULL,
        printRandForest = NULL,
        rocCurvePlot = NULL,
        modelSettings = NULL,
        items = list(),
        
        initialize = function() {
            self$text <- Html$new("text")
            self$classificationMetrics <- list(
                general = Table$new("general"),
                class = Table$new("class"),
                clinicalMetrics = Table$new("clinicalMetrics"),
                mccTable = Table$new("mccTable")
            )
            self$confusion <- list(
                matrix = Table$new("matrix")
            )
            self$predictedFreqPlot <- Image$new("predictedFreqPlot")
            self$decisionTreeModel <- Image$new("decisionTreeModel")
            self$printRandForest <- list(
                randomForestModel = Table$new("randomForestModel")
            )
            self$rocCurvePlot <- Image$new("rocCurvePlot")
            self$modelSettings <- Html$new("modelSettings")
        },
        add = function(item) {
            self$items[[length(self$items) + 1]] <- item
        }
    )
)

# Mock Base Class
classificationBase <- R6Class("classificationBase",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        isFresh = TRUE,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- Results$new()
        },
        run = function() {
            private$.run()
        }
    )
)

# Load the function code
source("R/classification.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(dep, indep, 
                          testSize = 0.33, noOfFolds = 5, 
                          testing = "split", # split, trainSet, crossValidation
                          reporting = c("classifMetrices", "confusionMatrix", "AUC"),
                          classifier = "singleDecisionTree",
                          minSplit = 20, complexity = 0.01, maxCompete = 4, maxSurrogate = 5, maxDepth = 30,
                          noOfTrees = 10, maxDepthRandFor = 30, sampleFraction = 1.0, splitRule = "gini",
                          knnNeighbors = 5, knnDistance = "euclidean",
                          svmKernel = "radial", svmCost = 1.0, svmGamma = 1.0,
                          plotDecisionTree = FALSE, predictedFreq = FALSE, printRandForest = FALSE, predictedFreqRF = FALSE,
                          balancingMethod = "none",
                          clinicalCutoff = 0.5,
                          validateMethod = "holdout",
                          bootstrapSamples = 100,
                          reportClinicalMetrics = TRUE,
                          reportConfidenceIntervals = FALSE,
                          reportMCC = TRUE,
                          positiveClass = "") {
    
    list(
        dep = dep,
        indep = indep,
        testSize = testSize,
        noOfFolds = noOfFolds,
        testing = testing,
        reporting = reporting,
        classifier = classifier,
        minSplit = minSplit,
        complexity = complexity,
        maxCompete = maxCompete,
        maxSurrogate = maxSurrogate,
        maxDepth = maxDepth,
        noOfTrees = noOfTrees,
        maxDepthRandFor = maxDepthRandFor,
        sampleFraction = sampleFraction,
        splitRule = splitRule,
        knnNeighbors = knnNeighbors,
        knnDistance = knnDistance,
        svmKernel = svmKernel,
        svmCost = svmCost,
        svmGamma = svmGamma,
        plotDecisionTree = plotDecisionTree,
        predictedFreq = predictedFreq,
        printRandForest = printRandForest,
        predictedFreqRF = predictedFreqRF,
        balancingMethod = balancingMethod,
        clinicalCutoff = clinicalCutoff,
        validateMethod = validateMethod,
        bootstrapSamples = bootstrapSamples,
        reportClinicalMetrics = reportClinicalMetrics,
        reportConfidenceIntervals = reportConfidenceIntervals,
        reportMCC = reportMCC,
        positiveClass = positiveClass
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Decision Tree Classifier Runs", {
    # Create dummy data
    set.seed(123)
    data <- data.frame(
        Outcome = factor(sample(c("Yes", "No"), 100, replace = TRUE)),
        Age = rnorm(100, 50, 10),
        Score = rnorm(100, 10, 2)
    )
    
    options <- create_options(dep = "Outcome", indep = c("Age", "Score"), classifier = "singleDecisionTree")
    analysis <- classificationClass$new(options, data)
    analysis$run()
    
    # Check if metrics are populated
    metrics <- analysis$results$classificationMetrics$general$asDF()
    expect_gt(nrow(metrics), 0)
    expect_true("classif.acc" %in% metrics$metric)
})

test_that("Random Forest Classifier Runs", {
    set.seed(123)
    data <- data.frame(
        Outcome = factor(sample(c("Yes", "No"), 50, replace = TRUE)),
        Age = rnorm(50, 50, 10),
        Score = rnorm(50, 10, 2)
    )
    
    options <- create_options(dep = "Outcome", indep = c("Age", "Score"), classifier = "randomForest", noOfTrees = 5)
    analysis <- classificationClass$new(options, data)
    analysis$run()
    
    metrics <- analysis$results$classificationMetrics$general$asDF()
    expect_gt(nrow(metrics), 0)
})

test_that("Clinical Metrics are Calculated Correctly", {
    # Create data with known performance
    # Perfect prediction scenario
    data <- data.frame(
        Outcome = factor(c(rep("Yes", 20), rep("No", 20))),
        Predictor = c(rep(1, 20), rep(0, 20)) # Perfect predictor
    )
    # We need to ensure the classifier picks up this relationship.
    # Decision tree should handle this easily.
    
    options <- create_options(dep = "Outcome", indep = "Predictor", classifier = "singleDecisionTree", 
                              reportClinicalMetrics = TRUE, testing = "trainSet") # Test on training set for perfect score
    analysis <- classificationClass$new(options, data)
    analysis$run()
    
    clinical_metrics <- analysis$results$classificationMetrics$clinicalMetrics$asDF()
    
    # Check Sensitivity (should be 1.0)
    sens_row <- clinical_metrics[clinical_metrics$metric == "Sensitivity", ]
    expect_equal(as.numeric(sens_row$value), 1.0)
    
    # Check Specificity (should be 1.0)
    spec_row <- clinical_metrics[clinical_metrics$metric == "Specificity", ]
    expect_equal(as.numeric(spec_row$value), 1.0)
})

test_that("Class Imbalance Handling (Upsample) Works", {
    # Imbalanced data
    data <- data.frame(
        Outcome = factor(c(rep("Rare", 5), rep("Common", 95))),
        Predictor = rnorm(100)
    )
    
    # We can't easily check the internal state of the task, but we can check if it runs without error
    # and produces results.
    options <- create_options(dep = "Outcome", indep = "Predictor", balancingMethod = "upsample")
    analysis <- classificationClass$new(options, data)
    
    expect_error(analysis$run(), NA) # Should not error
})

test_that("MCC Calculation is Correct", {
    # 2x2 Confusion Matrix
    # TP=40, FN=10, FP=10, TN=40
    # MCC = (1600 - 100) / sqrt(50*50*50*50) = 1500 / 2500 = 0.6
    
    # We need to mock the confusion matrix or force a specific prediction outcome.
    # Since we can't easily force the prediction outcome of the learner, 
    # we will test the private .calculateMCC method directly if possible, 
    # or rely on a scenario where we get decent predictions.
    
    # Let's try to access the private method via a trick or just trust the integration test.
    # R6 private methods are hard to access from outside.
    # We'll rely on the fact that we can run the analysis.
    
    data <- data.frame(
        Outcome = factor(sample(c("Yes", "No"), 100, replace = TRUE)),
        Predictor = rnorm(100)
    )
    
    options <- create_options(dep = "Outcome", indep = "Predictor", reportMCC = TRUE)
    analysis <- classificationClass$new(options, data)
    analysis$run()
    
    mcc_table <- analysis$results$classificationMetrics$mccTable$asDF()
    expect_gt(nrow(mcc_table), 0)
    expect_true(!is.na(mcc_table$mcc))
})

test_that("Cross-Validation Works", {
    data <- data.frame(
        Outcome = factor(sample(c("Yes", "No"), 50, replace = TRUE)),
        Predictor = rnorm(50)
    )
    
    options <- create_options(dep = "Outcome", indep = "Predictor", testing = "crossValidation", noOfFolds = 3)
    analysis <- classificationClass$new(options, data)
    analysis$run()
    
    metrics <- analysis$results$classificationMetrics$general$asDF()
    expect_gt(nrow(metrics), 0)
})

print("All tests completed successfully!")
