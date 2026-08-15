# Test psychopdaROC with problematic variable names
# This test validates that the variable escaping fixes work correctly

cat("\n========================================\n")
cat("psychopdaROC Variable Escaping Tests\n")
cat("========================================\n\n")

# Create test data with spaces and special characters in column names
set.seed(123)
test_data <- data.frame(
  `Patient Age` = rnorm(100, 50, 10),
  `Marker-1` = rnorm(100, 5, 2),
  `Test (New)` = rnorm(100, 3, 1),
  `Score/Total` = runif(100, 0, 1),
  `Disease Status` = sample(c("Healthy", "Disease"), 100, replace = TRUE),
  check.names = FALSE  # CRITICAL: preserve special characters
)

# Test 1: Single variable with spaces
cat("\n=== TEST 1: Variable with spaces ===\n")
result1 <- tryCatch({
  psychopdaROC(
    data = test_data,
    dependentVars = "Patient Age",
    classVar = "Disease Status",
    positiveClass = "Disease",
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result1, "\n")

# Test 2: Variable with hyphen
cat("\n=== TEST 2: Variable with hyphen ===\n")
result2 <- tryCatch({
  psychopdaROC(
    data = test_data,
    dependentVars = "Marker-1",
    classVar = "Disease Status",
    positiveClass = "Disease",
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result2, "\n")

# Test 3: Variable with parentheses
cat("\n=== TEST 3: Variable with parentheses ===\n")
result3 <- tryCatch({
  psychopdaROC(
    data = test_data,
    dependentVars = "Test (New)",
    classVar = "Disease Status",
    positiveClass = "Disease",
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result3, "\n")

# Test 4: Variable with forward slash
cat("\n=== TEST 4: Variable with forward slash ===\n")
result4 <- tryCatch({
  psychopdaROC(
    data = test_data,
    dependentVars = "Score/Total",
    classVar = "Disease Status",
    positiveClass = "Disease",
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result4, "\n")

# Test 5: Multiple variables with special names + DeLong test
cat("\n=== TEST 5: Multiple variables + DeLong test ===\n")
result5 <- tryCatch({
  psychopdaROC(
    data = test_data,
    dependentVars = c("Patient Age", "Marker-1", "Test (New)"),
    classVar = "Disease Status",
    positiveClass = "Disease",
    delongTest = TRUE,
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result5, "\n")

# Test 6: IDI/NRI with special names
cat("\n=== TEST 6: IDI/NRI calculations ===\n")
result6 <- tryCatch({
  psychopdaROC(
    data = test_data,
    dependentVars = c("Patient Age", "Marker-1"),
    classVar = "Disease Status",
    positiveClass = "Disease",
    calculateIDI = TRUE,
    calculateNRI = TRUE,
    refVar = "Patient Age",
    idiNriBootRuns = 100,  # Reduced for faster testing
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result6, "\n")

# Test 7: Class variable with spaces
cat("\n=== TEST 7: Class variable with spaces ===\n")
test_data2 <- test_data
names(test_data2)[5] <- "Disease Status (Confirmed)"
result7 <- tryCatch({
  psychopdaROC(
    data = test_data2,
    dependentVars = "Patient Age",
    classVar = "Disease Status (Confirmed)",
    positiveClass = "Disease",
    plotROC = FALSE
  )
  "✅ PASSED"
}, error = function(e) {
  paste("❌ FAILED:", e$message)
})
cat(result7, "\n")

# Collect results
all_results <- c(result1, result2, result3, result4, result5, result6, result7)
passed <- sum(grepl("✅ PASSED", all_results))
failed <- sum(grepl("❌ FAILED", all_results))

cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat(sprintf("Tests Passed: %d / %d\n", passed, length(all_results)))
cat(sprintf("Tests Failed: %d / %d\n", failed, length(all_results)))

if (failed > 0) {
  cat("\n⚠️  Some tests failed. Review error messages above.\n")
} else {
  cat("\n✅ All tests passed! Variable escaping working correctly.\n")
}

cat("\n")
