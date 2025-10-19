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

    # pathsampling() returns the results object after running analysis
    if (inherits(analysis, "pathsamplingResults")) {
        results <- analysis
    } else if (!is.null(analysis$results)) {
        results <- analysis$results
    } else {
        stop("Unexpected return type from pathsampling()")
    }

    cat("\n===== Checking Results =====\n")
    # Check if keyResults exists
    if ("keyResults" %in% names(results)) {
        cat("✓ keyResults element exists\n")
    } else {
        cat("✗ keyResults element NOT FOUND\n")
        cat("Available results:\n")
        print(names(results))
    }

    # Check dataInfo
    data_info_fn <- results$dataInfo$asDF
    if (is.function(data_info_fn)) {
        data_info_df <- data_info_fn()
        if (!is.null(data_info_df)) {
            cat("✓ dataInfo populated\n")
            print(head(data_info_df))
        }
    }

    # Check binomialTable
    binom_fn <- results$binomialTable$asDF
    if (is.function(binom_fn)) {
        binom_df <- binom_fn()
        if (!is.null(binom_df)) {
            cat("✓ binomialTable populated\n")
            print(head(binom_df))
        }
    }

    # Check empiricalCumulativeTable
    empirical_fn <- results$empiricalCumulativeTable$asDF
    if (is.function(empirical_fn)) {
        empirical_df <- empirical_fn()
        if (!is.null(empirical_df)) {
            cat("✓ empiricalCumulativeTable populated\n")
            print(head(empirical_df))
        }
    }

    cat("\n===== Test PASSED =====\n")

}, error = function(e) {
    cat("\n===== Test FAILED =====\n")
    cat("Error:", conditionMessage(e), "\n")
    cat("\nTraceback:\n")
    print(sys.calls())
})
