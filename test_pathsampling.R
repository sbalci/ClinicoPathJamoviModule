# Test pathsampling function with generated data
# This script tests the refactored pathsampling module

library(jmvcore)

# Load the module functions
source("R/pathsampling.b.R")
source("R/pathsampling.h.R")

# Load test data (Level 2: Enhanced)
data <- read.csv("data/pathsampling_enhanced.csv")

cat("===== Testing Path Sampling Module =====\n")
cat("Data loaded:", nrow(data), "cases\n")
cat("Columns:", paste(names(data), collapse = ", "), "\n\n")

# Create analysis options
options <- list(
    totalSamples = "n_samples",
    firstDetection = "first_pos",
    positiveCount = "pos_count",
    targetConfidence = 0.95,
    maxSamples = 10,
    bootstrapIterations = 1000,  # Reduced for quick test
    showBinomialModel = TRUE,
    showBootstrap = TRUE,
    showDetectionCurve = FALSE,  # Skip plots for quick test
    showSensitivityCI = FALSE,
    setSeed = TRUE,
    seedValue = 42,
    showEmpiricalCumulative = TRUE,
    showIncrementalYield = TRUE,
    estimationMethod = "auto",
    showClinicalSummary = TRUE,
    showSpatialClustering = FALSE,
    showStratifiedAnalysis = FALSE,
    showPopulationDetection = FALSE,
    showMultifocalAnalysis = FALSE,
    showTumorBurden = FALSE,
    showStageMigration = FALSE,
    showCorrelation = FALSE,
    showDistributionPattern = FALSE,
    showHypergeometric = FALSE,
    showBetaBinomial = FALSE,
    showLNAnalysis = FALSE,
    showEffectSizes = FALSE,
    showOmentumAnalysis = FALSE,
    showGuidedChecklist = FALSE
)

cat("===== Creating Analysis Object =====\n")
# Try to create the analysis
tryCatch({
    analysis <- pathsampling(
        data = data,
        totalSamples = "n_samples",
        firstDetection = "first_pos",
        positiveCount = "pos_count",
        targetConfidence = 0.95,
        maxSamples = 10,
        bootstrapIterations = 1000,
        showBinomialModel = TRUE,
        showBootstrap = TRUE,
        showDetectionCurve = FALSE,
        showSensitivityCI = FALSE,
        setSeed = TRUE,
        seedValue = 42,
        showEmpiricalCumulative = TRUE,
        showIncrementalYield = TRUE,
        estimationMethod = "auto",
        showClinicalSummary = TRUE
    )
    cat("Analysis object created successfully\n")

    cat("\n===== Running Analysis =====\n")
    analysis$run()
    cat("Analysis completed successfully\n")

    cat("\n===== Checking Results =====\n")
    # Check if keyResults exists
    if ("keyResults" %in% names(analysis$results)) {
        cat("✓ keyResults element exists\n")
    } else {
        cat("✗ keyResults element NOT FOUND\n")
        cat("Available results:\n")
        print(names(analysis$results))
    }

    # Check dataInfo
    if (!is.null(analysis$results$dataInfo$asDF())) {
        cat("✓ dataInfo populated\n")
        print(head(analysis$results$dataInfo$asDF()))
    }

    # Check binomialTable
    if (!is.null(analysis$results$binomialTable$asDF())) {
        cat("✓ binomialTable populated\n")
        print(head(analysis$results$binomialTable$asDF()))
    }

    # Check empiricalCumulativeTable
    if (!is.null(analysis$results$empiricalCumulativeTable$asDF())) {
        cat("✓ empiricalCumulativeTable populated\n")
        print(head(analysis$results$empiricalCumulativeTable$asDF()))
    }

    cat("\n===== Test PASSED =====\n")

}, error = function(e) {
    cat("\n===== Test FAILED =====\n")
    cat("Error:", conditionMessage(e), "\n")
    cat("\nTraceback:\n")
    print(sys.calls())
})
