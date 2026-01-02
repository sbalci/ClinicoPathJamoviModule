# Comprehensive Test Suite for oddsratio Function
# Tests all features, options, and edge cases

devtools::load_all()

# Test 1: Load test data ----
cat("\n=== Test 1: Loading test data ===\n")
data(histopathology)
cat("✓ Data loaded successfully\n")
cat("Dimensions:", nrow(histopathology), "rows,", ncol(histopathology), "cols\n")
cat("Variables:", paste(names(histopathology), collapse=", "), "\n")

# Test 2: Basic odds ratio analysis with binary variables ----
cat("\n=== Test 2: Basic binary analysis ===\n")
tryCatch({
    result2 <- oddsratio(
        data = histopathology,
        explanatory = "New Test",
        outcome = "Golden Standart",
        outcomeLevel = "1"
    )
    cat("✓ Basic binary analysis succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 3: Multiple binary predictors ----
cat("\n=== Test 3: Multiple binary predictors ===\n")
tryCatch({
    result3 <- oddsratio(
        data = histopathology,
        explanatory = c("New Test", "Rater 1", "Rater 2"),
        outcome = "Golden Standart",
        outcomeLevel = "1"
    )
    cat("✓ Multiple binary predictors succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 4: With explanations enabled ----
cat("\n=== Test 4: Educational explanations ===\n")
tryCatch({
    result4 <- oddsratio(
        data = histopathology,
        explanatory = c("New Test", "Rater 1"),
        outcome = "Golden Standart",
        outcomeLevel = "1",
        showExplanations = TRUE
    )
    cat("✓ Explanations enabled succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 5: With nomogram ----
cat("\n=== Test 5: Nomogram generation ===\n")
tryCatch({
    result5 <- oddsratio(
        data = histopathology,
        explanatory = c("New Test", "Rater 1"),
        outcome = "Golden Standart",
        outcomeLevel = "1",
        showNomogram = TRUE
    )
    cat("✓ Nomogram generation succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 6: With specific diagnostic predictor ----
cat("\n=== Test 6: Specific diagnostic predictor ===\n")
tryCatch({
    result6 <- oddsratio(
        data = histopathology,
        explanatory = c("New Test", "Rater 1", "Rater 2"),
        outcome = "Golden Standart",
        outcomeLevel = "1",
        showNomogram = TRUE,
        diagnosticPredictor = "Rater 1"
    )
    cat("✓ Specific diagnostic predictor succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 7: All options enabled ----
cat("\n=== Test 7: All options enabled ===\n")
tryCatch({
    result7 <- oddsratio(
        data = histopathology,
        explanatory = c("New Test", "Rater 1", "Rater 2"),
        outcome = "Golden Standart",
        outcomeLevel = "1",
        showNomogram = TRUE,
        showExplanations = TRUE,
        diagnosticPredictor = "New Test"
    )
    cat("✓ All options enabled succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 8: Create synthetic data with mixed variable types ----
cat("\n=== Test 8: Creating synthetic mixed data ===\n")
set.seed(123)
n <- 200

mixed_data <- data.frame(
    # Binary outcome
    Mortality = factor(sample(c("Alive", "Dead"), n, replace = TRUE, prob = c(0.6, 0.4))),

    # Binary predictors
    Sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    Treatment = factor(sample(c("Control", "Treated"), n, replace = TRUE)),
    Stage = factor(sample(c("Early", "Late"), n, replace = TRUE)),

    # Continuous predictors
    Age = rnorm(n, mean = 55, sd = 12),
    BMI = rnorm(n, mean = 26, sd = 4),

    # Categorical with >2 levels (should be handled)
    Histology = factor(sample(c("Type1", "Type2", "Type3"), n, replace = TRUE))
)

cat("✓ Synthetic data created\n")
cat("Dimensions:", nrow(mixed_data), "rows,", ncol(mixed_data), "cols\n")

# Test 9: Mixed variable types ----
cat("\n=== Test 9: Mixed binary and continuous predictors ===\n")
tryCatch({
    result9 <- oddsratio(
        data = mixed_data,
        explanatory = c("Sex", "Age", "Treatment"),
        outcome = "Mortality",
        outcomeLevel = "Dead"
    )
    cat("✓ Mixed variable types succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 10: Single continuous predictor ----
cat("\n=== Test 10: Single continuous predictor ===\n")
tryCatch({
    result10 <- oddsratio(
        data = mixed_data,
        explanatory = "Age",
        outcome = "Mortality",
        outcomeLevel = "Dead"
    )
    cat("✓ Single continuous predictor succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 11: Multiple continuous predictors ----
cat("\n=== Test 11: Multiple continuous predictors ===\n")
tryCatch({
    result11 <- oddsratio(
        data = mixed_data,
        explanatory = c("Age", "BMI"),
        outcome = "Mortality",
        outcomeLevel = "Dead"
    )
    cat("✓ Multiple continuous predictors succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 12: All predictors ----
cat("\n=== Test 12: All available predictors ===\n")
tryCatch({
    result12 <- oddsratio(
        data = mixed_data,
        explanatory = c("Sex", "Treatment", "Stage", "Age", "BMI"),
        outcome = "Mortality",
        outcomeLevel = "Dead",
        showExplanations = TRUE
    )
    cat("✓ All predictors succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 13: Nomogram with mixed predictors ----
cat("\n=== Test 13: Nomogram with mixed predictors ===\n")
tryCatch({
    result13 <- oddsratio(
        data = mixed_data,
        explanatory = c("Sex", "Age", "Treatment"),
        outcome = "Mortality",
        outcomeLevel = "Dead",
        showNomogram = TRUE,
        diagnosticPredictor = "Sex"
    )
    cat("✓ Nomogram with mixed predictors succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 14: Different positive outcome level ----
cat("\n=== Test 14: Alternative positive level ===\n")
tryCatch({
    result14 <- oddsratio(
        data = mixed_data,
        explanatory = c("Sex", "Treatment"),
        outcome = "Mortality",
        outcomeLevel = "Alive"  # Different level
    )
    cat("✓ Alternative positive level succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 15: Variable names with spaces ----
cat("\n=== Test 15: Variable names with spaces ===\n")
space_data <- mixed_data
names(space_data)[1:3] <- c("Patient Status", "Patient Sex", "Treatment Group")

tryCatch({
    result15 <- oddsratio(
        data = space_data,
        explanatory = c("Patient Sex", "Age", "Treatment Group"),
        outcome = "Patient Status",
        outcomeLevel = "Dead"
    )
    cat("✓ Variable names with spaces succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Test 16: Edge case - single binary predictor with nomogram ----
cat("\n=== Test 16: Single binary predictor with nomogram ===\n")
tryCatch({
    result16 <- oddsratio(
        data = mixed_data,
        explanatory = "Sex",
        outcome = "Mortality",
        outcomeLevel = "Dead",
        showNomogram = TRUE
    )
    cat("✓ Single binary with nomogram succeeded\n")
}, error = function(e) {
    cat("✗ FAILED:", e$message, "\n")
})

# Summary ----
cat("\n" , rep("=", 60), "\n", sep="")
cat("TEST SUITE COMPLETED\n")
cat(rep("=", 60), "\n", sep="")
cat("\nPlease review the results above.\n")
cat("All tests with ✓ passed successfully.\n")
cat("Any tests with ✗ indicate failures that need attention.\n")
