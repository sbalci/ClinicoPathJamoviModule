
# Verification Script for pathsampling Function
# Tests: Basic Analysis, Heterogeneity, Model Fit, Variable Appending

library(jmv)
library(jmvcore)
library(R6)

options(error = function() traceback(2))

# Source the pathsampling file
source("R/pathsampling.h.R")
source("R/pathsampling.b.R")

# Mock the pathsampling class for testing
# We need to mock the results object structure as expected by the .run method
MockResults <- R6::R6Class("MockResults",
    public = list(
        dataInfo = NULL,
        probabilityExplanation = NULL,
        keyResults = NULL,
        clinicalSummary = NULL,
        binomialText = NULL,
        binomialTable = NULL,
        recommendTable = NULL,
        bootstrapText = NULL,
        bootstrapTable = NULL,
        heterogeneityText = NULL,
        heterogeneityTest = NULL,
        modelFitText = NULL,
        modelFitTable = NULL,
        obsPredText = NULL,
        obsPredTable = NULL,
        interpretText = NULL,
        referencesText = NULL,
        
        initialize = function() {
            self$dataInfo <- MockTable$new()
            self$probabilityExplanation <- MockHtml$new()
            self$keyResults <- MockHtml$new()
            self$clinicalSummary <- MockHtml$new()
            self$binomialText <- MockHtml$new()
            self$binomialTable <- MockTable$new()
            self$recommendTable <- MockTable$new()
            self$bootstrapText <- MockHtml$new()
            self$bootstrapTable <- MockTable$new()
            self$heterogeneityText <- MockHtml$new()
            self$heterogeneityTest <- MockTable$new()
            self$modelFitText <- MockHtml$new()
            self$modelFitTable <- MockTable$new()
            self$obsPredText <- MockHtml$new()
            self$obsPredTable <- MockTable$new()
            self$interpretText <- MockHtml$new()
            self$referencesText <- MockHtml$new()
        }
    )
)

MockTable <- R6::R6Class("MockTable",
    public = list(
        rows = list(),
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- list(key = rowKey, values = values)
        }
    )
)

MockHtml <- R6::R6Class("MockHtml",
    public = list(
        content = NULL,
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock the Analysis class
MockAnalysis <- R6::R6Class("MockAnalysis",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- MockResults$new()
        }
    )
)

# Create test data
set.seed(123)
n_cases <- 50
total_samples <- rep(10, n_cases)
# Create heterogeneity: Group A has q=0.5, Group B has q=0.1
groups <- rep(c("A", "B"), each = n_cases/2)
first_detection <- numeric(n_cases)

# Group A: q=0.5 (Geometric)
first_detection[1:25] <- rgeom(25, 0.5) + 1
# Group B: q=0.1 (Geometric)
first_detection[26:50] <- rgeom(25, 0.1) + 1

# Cap at total samples
first_detection[first_detection > 10] <- NA 

test_data <- data.frame(
    Total = total_samples,
    FirstDet = first_detection,
    Group = groups
)

# Define options
options <- pathsamplingOptions$new(
    totalSamples = "Total",
    firstDetection = "FirstDet",
    sampleType = "Group",
    targetConfidence = 0.95,
    maxSamples = 10,
    bootstrapIterations = 1000,
    showBinomialModel = TRUE,
    showBootstrap = FALSE,
    showHeterogeneityTest = TRUE,
    showModelFit = TRUE,
    showObsPred = TRUE,
    appendVariables = TRUE,
    appendPrefix = "ps_",
    autoDetectHeterogeneity = TRUE,
    estimationMethod = "auto",
    analysisContext = "general",
    setSeed = TRUE,
    seedValue = 42,
    showKeyResults = TRUE,
    showClinicalSummary = TRUE,
    showReferencesText = FALSE,
    showOmentumAnalysis = FALSE
)

# Create a Test Class that copies methods from pathsamplingClass but removes inheritance
# to bypass the jmvcore initialization issues

# Extract methods and fields
public_methods <- pathsamplingClass$public_methods
private_methods <- pathsamplingClass$private_methods
private_fields <- pathsamplingClass$private_fields

# Merge private members
private_members <- c(private_methods, private_fields)

# Remove clone from public methods
public_methods$clone <- NULL

# Add options and data members
public_methods$options <- NA
public_methods$data <- NA
public_methods$results <- NA

# Add run method
public_methods$run <- function() {
    private$.run()
}

# Define new class without inheritance
TestPathSamplingClass <- R6::R6Class(
    "TestPathSamplingClass",
    public = public_methods,
    private = private_members
)

# Override initialize
TestPathSamplingClass$set("public", "initialize", function(options, data) {
    self$options <- options
    self$data <- data
    self$results <- MockResults$new()
    # Initialize private storage if needed
    private$.totalSamplesData <- NULL
    private$.firstDetectionData <- NULL
    private$.pEstimate <- NULL
    private$.maxSamp <- NULL
    private$.bootstrapResults <- NULL
    private$.positiveCassettesData <- NULL
    private$.maxPositiveSingleData <- NULL
})

# Instantiate the test class
path_analysis <- TestPathSamplingClass$new(
    options = options,
    data = test_data
)

# Overwrite results with our mock
path_analysis$results <- MockResults$new()

# Run the analysis
print("Running pathsampling analysis...")
path_analysis$run()
print("Analysis completed successfully.")

# Verify Results

# 1. Heterogeneity Test
print("--- Heterogeneity Test Results ---")
het_rows <- path_analysis$results$heterogeneityTest$rows
if (length(het_rows) > 0) {
    print(het_rows[[1]]$values)
} else {
    print("No heterogeneity test results found.")
}
print(paste("Heterogeneity Text:", substr(path_analysis$results$heterogeneityText$content, 1, 100)))

# 2. Model Fit
print("--- Model Fit Results ---")
fit_rows <- path_analysis$results$modelFitTable$rows
if (length(fit_rows) > 0) {
    print(fit_rows[[1]]$values)
} else {
    print("No model fit results found.")
}

# 3. Observed vs Predicted
print("--- Observed vs Predicted (First 3 rows) ---")
op_rows <- path_analysis$results$obsPredTable$rows
if (length(op_rows) > 0) {
    for (i in 1:min(3, length(op_rows))) {
        print(op_rows[[i]]$values)
    }
} else {
    print("No observed vs predicted results found.")
}

# 4. Auto-Detect Heterogeneity Warning
print("--- Auto-Detect Heterogeneity Warning ---")
print(paste("Interpret Text (Start):", substr(path_analysis$results$interpretText$content, 1, 100)))

# 5. Binomial Model
print("--- Binomial Model Results ---")
print(paste("Binomial Text (Start):", substr(path_analysis$results$binomialText$content, 1, 100)))

print("Verification Completed.")
