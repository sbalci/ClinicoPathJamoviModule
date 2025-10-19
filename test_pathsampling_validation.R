# Test pathsampling edge case validation
# Tests the validation improvements added for various edge cases

library(jmvcore)

# Load the module functions
source("R/pathsampling.b.R")
source("R/pathsampling.h.R")

cat("=====TEST 1: All Missing Total Samples =====\n")
data1 <- data.frame(
    total_samples = rep(NA, 10),
    first_detection = c(1, 2, 3, NA, NA, 2, 1, 4, 3, 2)
)

tryCatch({
    analysis1 <- pathsampling(
        data = data1,
        totalSamples = "total_samples",
        firstDetection = "first_detection",
        targetConfidence = 0.95,
        maxSamples = 10
    )
    cat("Test 1 Result: Should show error message about no valid data\n")
}, error = function(e) {
    cat("Error caught:", conditionMessage(e), "\n")
})

cat("\n=====TEST 2: Zero Positive Cases =====\n")
data2 <- data.frame(
    total_samples = sample(5:15, 20, replace = TRUE),
    first_detection = rep(NA, 20)  # All negative
)

tryCatch({
    analysis2 <- pathsampling(
        data = data2,
        totalSamples = "total_samples",
        firstDetection = "first_detection",
        targetConfidence = 0.95,
        maxSamples = 10
    )
    cat("Test 2 Result: Should show error about no positive cases\n")
}, error = function(e) {
    cat("Error caught:", conditionMessage(e), "\n")
})

cat("\n=====TEST 3: Invalid Detection (first > total) =====\n")
data3 <- data.frame(
    total_samples = rep(5, 10),
    first_detection = rep(10, 10)  # All invalid
)

tryCatch({
    analysis3 <- pathsampling(
        data = data3,
        totalSamples = "total_samples",
        firstDetection = "first_detection",
        targetConfidence = 0.95,
        maxSamples = 10
    )
    cat("Test 3 Result: Should show error about data quality issues\n")
}, error = function(e) {
    cat("Error caught:", conditionMessage(e), "\n")
})

cat("\n=====TEST 4: Insufficient Data for Bootstrap (2 positive cases) =====\n")
data4 <- data.frame(
    total_samples = c(10, 8, 12, 15, 9),
    first_detection = c(2, NA, NA, 3, NA)  # Only 2 positive
)

tryCatch({
    analysis4 <- pathsampling(
        data = data4,
        totalSamples = "total_samples",
        firstDetection = "first_detection",
        targetConfidence = 0.95,
        maxSamples = 10,
        showBootstrap = TRUE,
        bootstrapIterations = 1000
    )
    cat("Test 4 Result: Should show error about insufficient data for bootstrap\n")
}, error = function(e) {
    cat("Error caught:", conditionMessage(e), "\n")
})

cat("\n=====TEST 5: Small Sample Warning (5 positive cases) =====\n")
data5 <- data.frame(
    total_samples = sample(5:15, 20, replace = TRUE),
    first_detection = c(1, 2, 3, 2, 4, rep(NA, 15))  # Only 5 positive
)

tryCatch({
    analysis5 <- pathsampling(
        data = data5,
        totalSamples = "total_samples",
        firstDetection = "first_detection",
        targetConfidence = 0.95,
        maxSamples = 10
    )
    cat("Test 5 Result: Should show warning about small sample size\n")
}, error = function(e) {
    cat("Error caught:", conditionMessage(e), "\n")
})

cat("\n=====TEST 6: Valid Data (should work normally) =====\n")
set.seed(42)
data6 <- data.frame(
    total_samples = sample(5:15, 100, replace = TRUE),
    first_detection = c(sample(1:10, 60, replace = TRUE), rep(NA, 40))  # 60% positive
)

tryCatch({
    analysis6 <- pathsampling(
        data = data6,
        totalSamples = "total_samples",
        firstDetection = "first_detection",
        targetConfidence = 0.95,
        maxSamples = 10,
        showBinomialModel = TRUE
    )
    cat("Test 6 Result: Should complete successfully with 60 positive cases\n")
}, error = function(e) {
    cat("Error caught:", conditionMessage(e), "\n")
})

cat("\n=====VALIDATION TEST SUMMARY =====\n")
cat("All edge case validation tests completed.\n")
cat("Check output above to verify error/warning messages are displayed correctly.\n")
