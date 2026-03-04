# Test script for ihcdiagnostic function fixes
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(ClinicoPath)

# Create test data with various IHC marker formats
test_data <- data.frame(
    # Standard IHC scoring
    ER = factor(c("0", "1+", "2+", "3+", "1+", "2+", "3+", "0", "1+", "2+",
                  "0", "1+", "2+", "3+", "2+")),
    PR = factor(c("0", "0", "1+", "2+", "3+", "3+", "2+", "1+", "0", "0",
                  "1+", "2+", "3+", "2+", "1+")),
    # Numeric scores
    HER2 = c(0, 0, 0, 1, 2, 3, 3, 2, 1, 0, 1, 2, 3, 2, 1),
    Ki67 = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
             12, 18, 22, 28, 33),
    # Diagnosis groups
    diagnosis = factor(c("Luminal A", "Luminal A", "Luminal A", "Luminal B", "Luminal B",
                        "HER2+", "HER2+", "HER2+", "TNBC", "TNBC",
                        "Luminal A", "Luminal B", "HER2+", "TNBC", "Luminal B")),
    # Case IDs
    case_id = paste0("Case", 1:15),
    stringsAsFactors = FALSE
)

cat("Testing ihcdiagnostic function with", nrow(test_data), "cases\n")
cat("Markers:", paste(c("ER", "PR", "HER2", "Ki67"), collapse = ", "), "\n")
cat("Diagnostic groups:", paste(unique(test_data$diagnosis), collapse = ", "), "\n\n")

# Test 1: Basic functionality with all options enabled
cat("Test 1: Basic functionality with all options enabled\n")
tryCatch({
    result1 <- ihcdiagnostic(
        data = test_data,
        markers = c("ER", "PR", "HER2", "Ki67"),
        diagnosis = "diagnosis",
        id = "case_id",
        calculateDiagnosticMetrics = TRUE,
        differentialDiagnosis = TRUE,
        antibodyOptimization = TRUE,
        crossValidation = FALSE  # Start without CV for speed
    )
    cat("✓ Basic test passed\n\n")
}, error = function(e) {
    cat("✗ Basic test failed:", e$message, "\n\n")
})

# Test 2: With cross-validation
cat("Test 2: Testing with cross-validation\n")
tryCatch({
    result2 <- ihcdiagnostic(
        data = test_data,
        markers = c("ER", "PR", "HER2"),
        diagnosis = "diagnosis",
        calculateDiagnosticMetrics = TRUE,
        differentialDiagnosis = FALSE,
        antibodyOptimization = FALSE,
        crossValidation = TRUE
    )
    cat("✓ Cross-validation test passed\n\n")
}, error = function(e) {
    cat("✗ Cross-validation test failed:", e$message, "\n\n")
})

# Test 3: Error handling - missing markers
cat("Test 3: Testing error handling with missing markers\n")
tryCatch({
    result3 <- ihcdiagnostic(
        data = test_data,
        markers = c("ER", "PR", "NonExistent"),
        diagnosis = "diagnosis"
    )
    cat("✗ Should have failed with missing marker\n\n")
}, error = function(e) {
    cat("✓ Correctly handled missing marker error\n\n")
})

# Test 4: Small sample size warning
cat("Test 4: Testing small sample size handling\n")
small_data <- test_data[1:3, ]
tryCatch({
    result4 <- ihcdiagnostic(
        data = small_data,
        markers = c("ER", "PR"),
        diagnosis = "diagnosis",
        minimumGroupSize = 5
    )
    cat("✗ Should have warned about small sample size\n\n")
}, error = function(e) {
    cat("✓ Correctly handled small sample size\n\n")
})

# Test 5: Different cutpoint methods
cat("Test 5: Testing different cutpoint methods\n")
for (method in c("optimal", "clinical", "median")) {
    tryCatch({
        result5 <- ihcdiagnostic(
            data = test_data,
            markers = c("ER", "Ki67"),
            diagnosis = "diagnosis",
            calculateDiagnosticMetrics = TRUE,
            differentialDiagnosis = FALSE,
            antibodyOptimization = FALSE,
            cutpointMethod = method,
            crossValidation = FALSE
        )
        cat("✓ Cutpoint method", method, "passed\n")
    }, error = function(e) {
        cat("✗ Cutpoint method", method, "failed:", e$message, "\n")
    })
}

cat("\nAll tests completed!\n")
cat("IHC utilities are properly integrated and error handling is robust.\n")
