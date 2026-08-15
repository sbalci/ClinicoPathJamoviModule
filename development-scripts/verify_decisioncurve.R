
library(testthat)
library(jmvcore)
library(dplyr)
library(ggplot2)

# Mock JMV Classes
MockOption <- R6::R6Class("MockOption",
    public = list(
        value = NULL,
        initialize = function(value) self$value <- value
    )
)

MockOptions <- R6::R6Class("MockOptions",
    public = list(
        outcome = NULL,
        outcomePositive = NULL,
        models = NULL,
        modelNames = NULL,
        thresholdRange = NULL,
        thresholdMin = NULL,
        thresholdMax = NULL,
        thresholdStep = NULL,
        showTable = NULL,
        selectedThresholds = NULL,
        showPlot = NULL,
        plotStyle = NULL,
        showReferenceLinesLabels = NULL,
        highlightRange = NULL,
        highlightMin = NULL,
        highlightMax = NULL,
        calculateClinicalImpact = NULL,
        populationSize = NULL,
        showInterventionAvoided = NULL,
        confidenceIntervals = NULL,
        bootReps = NULL,
        ciLevel = NULL,
        showOptimalThreshold = NULL,
        compareModels = NULL,
        weightedAUC = NULL,
        clinicalDecisionRule = NULL,
        decisionRuleVar = NULL,
        decisionRulePositive = NULL,
        decisionRuleLabel = NULL,
        showClinicalImpactPlot = NULL,
        showNetBenefitCI = NULL,
        costBenefitAnalysis = NULL,
        testCost = NULL,
        treatmentCost = NULL,
        benefitCorrectTreatment = NULL,
        harmFalseTreatment = NULL,
        showStandardizedNetBenefit = NULL,
        multiModelComparison = NULL,
        comparisonMethod = NULL,
        showDecisionConsequences = NULL,
        resourceUtilization = NULL,
        showRelativeUtility = NULL,
        
        initialize = function(
            outcome = "outcome",
            outcomePositive = "1",
            models = "pred",
            modelNames = "",
            thresholdRange = "clinical",
            thresholdMin = 0.05,
            thresholdMax = 0.50,
            thresholdStep = 0.01,
            showTable = TRUE,
            selectedThresholds = "0.05, 0.10, 0.15, 0.20, 0.25, 0.30",
            showPlot = TRUE,
            plotStyle = "standard",
            showReferenceLinesLabels = TRUE,
            highlightRange = FALSE,
            highlightMin = 0.10,
            highlightMax = 0.30,
            calculateClinicalImpact = FALSE,
            populationSize = 1000,
            showInterventionAvoided = FALSE,
            confidenceIntervals = FALSE,
            bootReps = 100,
            ciLevel = 0.95,
            showOptimalThreshold = TRUE,
            compareModels = FALSE,
            weightedAUC = FALSE,
            clinicalDecisionRule = FALSE,
            decisionRuleVar = NULL,
            decisionRulePositive = NULL,
            decisionRuleLabel = "Clinical Rule",
            showClinicalImpactPlot = FALSE,
            showNetBenefitCI = FALSE,
            costBenefitAnalysis = FALSE,
            testCost = 100,
            treatmentCost = 1000,
            benefitCorrectTreatment = 10000,
            harmFalseTreatment = 500,
            showStandardizedNetBenefit = FALSE,
            multiModelComparison = TRUE,
            comparisonMethod = "bootstrap",
            showDecisionConsequences = TRUE,
            resourceUtilization = FALSE,
            showRelativeUtility = FALSE
        ) {
            self$outcome <- outcome
            self$outcomePositive <- outcomePositive
            self$models <- models
            self$modelNames <- modelNames
            self$thresholdRange <- thresholdRange
            self$thresholdMin <- thresholdMin
            self$thresholdMax <- thresholdMax
            self$thresholdStep <- thresholdStep
            self$showTable <- showTable
            self$selectedThresholds <- selectedThresholds
            self$showPlot <- showPlot
            self$plotStyle <- plotStyle
            self$showReferenceLinesLabels <- showReferenceLinesLabels
            self$highlightRange <- highlightRange
            self$highlightMin <- highlightMin
            self$highlightMax <- highlightMax
            self$calculateClinicalImpact <- calculateClinicalImpact
            self$populationSize <- populationSize
            self$showInterventionAvoided <- showInterventionAvoided
            self$confidenceIntervals <- confidenceIntervals
            self$bootReps <- bootReps
            self$ciLevel <- ciLevel
            self$showOptimalThreshold <- showOptimalThreshold
            self$compareModels <- compareModels
            self$weightedAUC <- weightedAUC
            self$clinicalDecisionRule <- clinicalDecisionRule
            self$decisionRuleVar <- decisionRuleVar
            self$decisionRulePositive <- decisionRulePositive
            self$decisionRuleLabel <- decisionRuleLabel
            self$showClinicalImpactPlot <- showClinicalImpactPlot
            self$showNetBenefitCI <- showNetBenefitCI
            self$costBenefitAnalysis <- costBenefitAnalysis
            self$testCost <- testCost
            self$treatmentCost <- treatmentCost
            self$benefitCorrectTreatment <- benefitCorrectTreatment
            self$harmFalseTreatment <- harmFalseTreatment
            self$showStandardizedNetBenefit <- showStandardizedNetBenefit
            self$multiModelComparison <- multiModelComparison
            self$comparisonMethod <- comparisonMethod
            self$showDecisionConsequences <- showDecisionConsequences
            self$resourceUtilization <- resourceUtilization
            self$showRelativeUtility <- showRelativeUtility
        }
    )
)

MockTable <- R6::R6Class("MockTable",
    public = list(
        rows = list(),
        columns = list(),
        visible = TRUE,
        initialize = function() {},
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- list(rowKey = rowKey, values = values)
        },
        deleteRows = function() {
            self$rows <- list()
        },
        addColumn = function(name, title, type, format) {
            self$columns[[name]] <- list(title = title, type = type, format = format)
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

MockHtml <- R6::R6Class("MockHtml",
    public = list(
        content = NULL,
        visible = TRUE,
        initialize = function() {},
        setContent = function(content) {
            self$content <- content
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

MockImage <- R6::R6Class("MockImage",
    public = list(
        state = NULL,
        visible = TRUE,
        initialize = function() {},
        setState = function(state) {
            self$state <- state
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

MockResults <- R6::R6Class("MockResults",
    public = list(
        instructions = NULL,
        procedureNotes = NULL,
        resultsTable = NULL,
        optimalTable = NULL,
        clinicalImpactTable = NULL,
        comparisonTable = NULL,
        weightedAUCTable = NULL,
        dcaPlot = NULL,
        clinicalImpactPlot = NULL,
        interventionsAvoidedPlot = NULL,
        summaryText = NULL,
        costBenefitTable = NULL,
        decisionConsequencesTable = NULL,
        modelComparisonEnhanced = NULL,
        resourceUtilizationTable = NULL,
        relativeUtilityPlot = NULL,
        standardizedNetBenefitPlot = NULL,
        
        initialize = function() {
            self$instructions <- MockHtml$new()
            self$procedureNotes <- MockHtml$new()
            self$resultsTable <- MockTable$new()
            self$optimalTable <- MockTable$new()
            self$clinicalImpactTable <- MockTable$new()
            self$comparisonTable <- MockTable$new()
            self$weightedAUCTable <- MockTable$new()
            self$dcaPlot <- MockImage$new()
            self$clinicalImpactPlot <- MockImage$new()
            self$interventionsAvoidedPlot <- MockImage$new()
            self$summaryText <- MockHtml$new()
            self$costBenefitTable <- MockTable$new()
            self$decisionConsequencesTable <- MockTable$new()
            self$modelComparisonEnhanced <- MockTable$new()
            self$resourceUtilizationTable <- MockTable$new()
            self$relativeUtilityPlot <- MockImage$new()
            self$standardizedNetBenefitPlot <- MockImage$new()
        }
    )
)

# Source the file
source("R/decisioncurve.b.R")

# Mock Base Class
decisioncurveBase <- R6::R6Class("decisioncurveBase",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- MockResults$new()
        },
        run = function() {
            private$.run()
        }
    )
)

# Test Data Generation
set.seed(123)
n <- 200
outcome <- rbinom(n, 1, 0.3)
pred <- plogis(qlogis(0.3) + 2 * (outcome - 0.3) + rnorm(n))
test_data <- data.frame(outcome = factor(outcome), pred = pred)

# Verification Tests
test_that("Net Benefit Calculation is Correct", {
    threshold <- 0.2
    tp <- sum(pred >= threshold & outcome == 1)
    fp <- sum(pred >= threshold & outcome == 0)
    n_total <- n
    
    expected_nb <- (tp / n_total) - (fp / n_total) * (threshold / (1 - threshold))
    
    options <- MockOptions$new(
        outcome = "outcome",
        outcomePositive = "1",
        models = "pred",
        thresholdRange = "custom",
        thresholdMin = 0.2,
        thresholdMax = 0.21,
        thresholdStep = 0.01,
        selectedThresholds = "0.2"
    )
    
    analysis <- decisioncurveClass$new(options, test_data)
    analysis$run()
    
    rows <- analysis$results$resultsTable$rows
    found_match <- FALSE
    for (row in rows) {
        col_name <- "model_pred"
        if (!is.null(row$values[[col_name]])) {
            val <- row$values[[col_name]]
            if (abs(val - expected_nb) < 1e-5) {
                found_match <- TRUE
                break
            }
        }
    }
    
    expect_true(found_match, label = paste("Expected NB:", expected_nb))
})

test_that("Cost-Benefit Analysis Table is Populated", {
    options <- MockOptions$new(
        outcome = "outcome",
        outcomePositive = "1",
        models = "pred",
        costBenefitAnalysis = TRUE,
        testCost = 100,
        treatmentCost = 1000,
        benefitCorrectTreatment = 10000,
        harmFalseTreatment = 500
    )
    
    analysis <- decisioncurveClass$new(options, test_data)
    analysis$run()
    
    expect_gt(length(analysis$results$costBenefitTable$rows), 0)
    
    # Check first row values
    row1 <- analysis$results$costBenefitTable$rows[[1]]$values
    expect_true(!is.null(row1$net_monetary_benefit))
    expect_true(!is.null(row1$total_cost))
})

test_that("Decision Consequences Table is Populated", {
    options <- MockOptions$new(
        outcome = "outcome",
        outcomePositive = "1",
        models = "pred",
        showDecisionConsequences = TRUE
    )
    
    analysis <- decisioncurveClass$new(options, test_data)
    analysis$run()
    
    expect_gt(length(analysis$results$decisionConsequencesTable$rows), 0)
    
    row1 <- analysis$results$decisionConsequencesTable$rows[[1]]$values
    expect_true(!is.null(row1$true_positive))
    expect_true(!is.null(row1$sensitivity))
})

test_that("Resource Utilization Table is Populated", {
    options <- MockOptions$new(
        outcome = "outcome",
        outcomePositive = "1",
        models = "pred",
        resourceUtilization = TRUE
    )
    
    analysis <- decisioncurveClass$new(options, test_data)
    analysis$run()
    
    expect_gt(length(analysis$results$resourceUtilizationTable$rows), 0)
    
    row1 <- analysis$results$resourceUtilizationTable$rows[[1]]$values
    expect_true(!is.null(row1$tests_per_1000))
    expect_true(!is.null(row1$treatments_per_1000))
})

print("Verification Script Created Successfully")
