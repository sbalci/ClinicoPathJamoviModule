# Comprehensive Testing Script for modelbuilder Function
# Tests all features and arguments with realistic clinical data

# Load required libraries
devtools::load_all()  # Or whatever the package name is
library(dplyr)

# Load all test datasets
cardiac_data <- read.csv("data/cardiac_risk_prediction.csv", stringsAsFactors = TRUE)
cancer_data <- read.csv("data/cancer_prognosis_data.csv", stringsAsFactors = TRUE)
diabetes_data <- read.csv("data/diabetes_complications.csv", stringsAsFactors = TRUE)
minimal_data <- read.csv("data/minimal_sample_test.csv", stringsAsFactors = TRUE)
separation_data <- read.csv("data/separation_challenge.csv", stringsAsFactors = TRUE)

cat("🧪 COMPREHENSIVE MODELBUILDER TESTING\n")
cat("=====================================\n\n")

# ==========================================
# TEST 1: BASIC FUNCTIONALITY
# ==========================================
cat("TEST 1: Basic Clinical Model\n")
cat("-----------------------------\n")

test1_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    # Basic model
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes", "hypertension", "smoking"),
    basicModelName = "cardiac_basic",
    
    # Data setup
    splitData = TRUE,
    randomSeed = 123,
    missingDataMethod = "complete_cases",
    
    # Output options
    showModelSummary = TRUE,
    showPerformanceMetrics = TRUE,
    compareModels = TRUE,
    createPredictions = TRUE,
    exportForDCA = TRUE
  )
  cat("✅ Basic model: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Basic model: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 2: ALL MODEL TYPES
# ==========================================
cat("\nTEST 2: All Model Types Comparison\n")
cat("----------------------------------\n")

test2_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event", 
    outcomePositive = "Yes",
    
    # All model types
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes", "hypertension"),
    basicModelName = "basic_clinical",
    
    buildEnhancedModel = TRUE,
    enhancedPredictors = c("age", "sex", "diabetes", "hypertension", "bmi", "systolic_bp", "exercise"),
    enhancedModelName = "enhanced_clinical",
    
    buildBiomarkerModel = TRUE,
    biomarkerPredictors = c("age", "sex", "diabetes", "cholesterol_total", "hdl_cholesterol", "crp", "troponin"),
    biomarkerModelName = "biomarker_model",
    
    buildCustomModel = TRUE,
    customPredictors = c("age", "sex", "stress_score", "medication_count", "income_bracket"),
    customModelName = "psychosocial_model",
    
    # Validation
    splitData = TRUE,
    crossValidation = TRUE,
    cvFolds = 5,
    
    # Output
    compareModels = TRUE,
    showROCCurves = TRUE,
    showCalibrationPlots = TRUE
  )
  cat("✅ All model types: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ All model types: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================  
# TEST 3: PENALIZED REGRESSION
# ==========================================
cat("\nTEST 3: Penalized Regression (High-Dimensional Data)\n")
cat("----------------------------------------------------\n")

# Test LASSO
test3a_result <- tryCatch({
  result <- modelbuilder(
    data = diabetes_data,
    outcome = "nephropathy_progression",
    outcomePositive = "Yes",
    
    buildCustomModel = TRUE,
    customPredictors = c("age", "sex", "diabetes_duration", "hba1c_recent", "fasting_glucose", 
                        "systolic_bp", "bmi", "creatinine", "egfr", "albumin_urine", 
                        "exercise_minutes", "smoking_packyears"),
    
    # Penalized regression
    penalizedRegression = TRUE,
    penaltyType = "lasso",
    
    splitData = TRUE,
    missingDataMethod = "mean_imputation",
    showModelSummary = TRUE
  )
  cat("✅ LASSO regression: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ LASSO regression: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test Ridge
test3b_result <- tryCatch({
  result <- modelbuilder(
    data = diabetes_data,
    outcome = "nephropathy_progression",
    outcomePositive = "Yes",
    
    buildCustomModel = TRUE,
    customPredictors = c("age", "sex", "diabetes_duration", "hba1c_recent", "systolic_bp", "bmi"),
    
    penalizedRegression = TRUE,
    penaltyType = "ridge",
    
    splitData = TRUE
  )
  cat("✅ Ridge regression: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Ridge regression: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test Elastic Net
test3c_result <- tryCatch({
  result <- modelbuilder(
    data = diabetes_data,
    outcome = "nephropathy_progression", 
    outcomePositive = "Yes",
    
    buildCustomModel = TRUE,
    customPredictors = c("age", "sex", "diabetes_duration", "hba1c_recent", "systolic_bp"),
    
    penalizedRegression = TRUE,
    penaltyType = "elastic_net",
    
    splitData = TRUE
  )
  cat("✅ Elastic Net regression: SUCCESS\n")
  return("SUCCESS")  
}, error = function(e) {
  cat("❌ Elastic Net regression: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 4: MISSING DATA HANDLING
# ==========================================
cat("\nTEST 4: Missing Data Methods\n")
cat("----------------------------\n")

# Test mean imputation
test4a_result <- tryCatch({
  result <- modelbuilder(
    data = diabetes_data,
    outcome = "nephropathy_progression",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes_duration", "albumin_urine", "egfr"),
    
    missingDataMethod = "mean_imputation",
    splitData = TRUE
  )
  cat("✅ Mean imputation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Mean imputation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test exclude missing variables
test4b_result <- tryCatch({
  result <- modelbuilder(
    data = diabetes_data,
    outcome = "nephropathy_progression",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes_duration", "albumin_urine", "egfr", "exercise_minutes"),
    
    missingDataMethod = "exclude_missing",
    splitData = TRUE
  )
  cat("✅ Exclude missing variables: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Exclude missing variables: FAILED -", e$message, "\n") 
  return(paste("FAILED:", e$message))
})

# Test multiple imputation (if mice is available)
test4c_result <- tryCatch({
  result <- modelbuilder(
    data = cancer_data,
    outcome = "death_within_2years",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "stage", "tumor_size", "ca199", "albumin"),
    
    missingDataMethod = "multiple_imputation",
    imputationSets = 3,
    splitData = TRUE
  )
  cat("✅ Multiple imputation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Multiple imputation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 5: VARIABLE TRANSFORMATIONS
# ==========================================
cat("\nTEST 5: Variable Transformations\n")
cat("--------------------------------\n")

# Test log transformation
test5a_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "bmi", "cholesterol_total", "crp"),
    
    transformVariables = TRUE,
    transformMethod = "log",
    splitData = TRUE
  )
  cat("✅ Log transformation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Log transformation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test polynomial transformation  
test5b_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "bmi", "systolic_bp"),
    
    transformVariables = TRUE,
    transformMethod = "polynomial",
    splitData = TRUE
  )
  cat("✅ Polynomial transformation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Polynomial transformation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test spline transformation (if rms is available)
test5c_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "bmi", "cholesterol_total"),
    
    transformVariables = TRUE,
    transformMethod = "spline",
    splitData = TRUE
  )
  cat("✅ Spline transformation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Spline transformation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 6: INTERACTIONS AND FEATURE ENGINEERING
# ==========================================
cat("\nTEST 6: Interactions and Feature Engineering\n")
cat("--------------------------------------------\n")

# Test automatic interactions
test6a_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "bmi", "systolic_bp"),
    
    includeInteractions = TRUE,
    splitData = TRUE
  )
  cat("✅ Automatic interactions: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Automatic interactions: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test specific interactions
test6b_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event", 
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "bmi", "diabetes", "sex"),
    
    includeInteractions = TRUE,
    interactionTerms = "age*sex, bmi*diabetes",
    splitData = TRUE
  )
  cat("✅ Specific interactions: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Specific interactions: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 7: STEPWISE SELECTION
# ==========================================
cat("\nTEST 7: Stepwise Variable Selection\n")
cat("-----------------------------------\n")

# Test forward selection
test7a_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes", "hypertension", "bmi", "cholesterol_total"),
    
    useStepwise = TRUE,
    stepwiseDirection = "forward",
    selectionCriterion = "aic",
    splitData = TRUE
  )
  cat("✅ Forward selection: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Forward selection: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test backward elimination with BIC
test7b_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes", "hypertension", "bmi", "cholesterol_total"),
    
    useStepwise = TRUE,
    stepwiseDirection = "backward",
    selectionCriterion = "bic",
    splitData = TRUE
  )
  cat("✅ Backward elimination (BIC): SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Backward elimination (BIC): FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 8: VALIDATION METHODS
# ==========================================
cat("\nTEST 8: Validation Methods\n")
cat("--------------------------\n")

# Test cross-validation
test8a_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes", "hypertension"),
    
    crossValidation = TRUE,
    cvFolds = 5,
    splitData = TRUE
  )
  cat("✅ Cross-validation: SUCCESS\n")
  return("SUCCESS")  
}, error = function(e) {
  cat("❌ Cross-validation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test bootstrap validation
test8b_result <- tryCatch({
  result <- modelbuilder(
    data = cancer_data,
    outcome = "death_within_2years",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "stage", "grade", "tumor_size"),
    
    bootstrapValidation = TRUE,
    bootstrapReps = 100,  # Reduced for testing speed
    splitData = TRUE
  )
  cat("✅ Bootstrap validation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Bootstrap validation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 9: ADVANCED METRICS
# ==========================================
cat("\nTEST 9: Advanced Model Comparison Metrics\n")
cat("-----------------------------------------\n")

# Test NRI and IDI
test9_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    # Build multiple models for comparison
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes"),
    
    buildEnhancedModel = TRUE,
    enhancedPredictors = c("age", "sex", "diabetes", "cholesterol_total", "crp"),
    
    # Advanced metrics
    calculateNRI = TRUE,
    nriThresholds = "0.10, 0.20",
    calculateIDI = TRUE,
    
    splitData = TRUE
  )
  cat("✅ NRI and IDI calculations: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ NRI and IDI calculations: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 10: RISK SCORE GENERATION
# ==========================================
cat("\nTEST 10: Clinical Risk Score Generation\n")
cat("--------------------------------------\n")

test10_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "cardiovascular_event",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "diabetes", "hypertension"),
    
    generateRiskScore = TRUE,
    riskScorePoints = "simple",
    splitData = TRUE
  )
  cat("✅ Risk score generation: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Risk score generation: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# ==========================================
# TEST 11: EDGE CASES
# ==========================================
cat("\nTEST 11: Edge Cases and Error Handling\n")
cat("--------------------------------------\n")

# Test minimal sample size
test11a_result <- tryCatch({
  result <- modelbuilder(
    data = minimal_data,
    outcome = "outcome",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "comorbid"),
    
    splitData = FALSE,  # Don't split small sample
    showModelSummary = TRUE
  )
  cat("✅ Minimal sample size: SUCCESS\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Minimal sample size: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test separation detection
test11b_result <- tryCatch({
  result <- modelbuilder(
    data = separation_data,
    outcome = "outcome",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex", "rare_condition"),
    
    splitData = FALSE,
    showModelSummary = TRUE
  )
  cat("✅ Separation detection: SUCCESS (with warnings expected)\n")
  return("SUCCESS")
}, error = function(e) {
  cat("❌ Separation detection: FAILED -", e$message, "\n")
  return(paste("FAILED:", e$message))
})

# Test error handling - invalid outcome
test11c_result <- tryCatch({
  result <- modelbuilder(
    data = cardiac_data,
    outcome = "nonexistent_variable",
    outcomePositive = "Yes",
    
    buildBasicModel = TRUE,
    basicPredictors = c("age", "sex")
  )
  cat("❌ Error handling test failed - should have caught invalid outcome\n")
  return("FAILED - should have caught error")
}, error = function(e) {
  cat("✅ Error handling: SUCCESS - correctly caught invalid outcome\n")
  return("SUCCESS")
})

# ==========================================
# SUMMARY OF TEST RESULTS
# ==========================================
cat("\n\n🎯 COMPREHENSIVE TEST RESULTS SUMMARY\n")
cat("=====================================\n")

test_results <- list(
  "Basic Functionality" = test1_result,
  "All Model Types" = test2_result, 
  "LASSO Regression" = test3a_result,
  "Ridge Regression" = test3b_result,
  "Elastic Net" = test3c_result,
  "Mean Imputation" = test4a_result,
  "Exclude Missing" = test4b_result,
  "Multiple Imputation" = test4c_result,
  "Log Transformation" = test5a_result,
  "Polynomial Transformation" = test5b_result,
  "Spline Transformation" = test5c_result,
  "Automatic Interactions" = test6a_result,
  "Specific Interactions" = test6b_result,
  "Forward Selection" = test7a_result,
  "Backward Elimination" = test7b_result,
  "Cross-validation" = test8a_result,
  "Bootstrap Validation" = test8b_result,
  "NRI and IDI" = test9_result,
  "Risk Score Generation" = test10_result,
  "Minimal Sample" = test11a_result,
  "Separation Detection" = test11b_result,
  "Error Handling" = test11c_result
)

# Count successes and failures
successes <- sum(grepl("SUCCESS", test_results))
total_tests <- length(test_results)
success_rate <- round(successes / total_tests * 100, 1)

cat(sprintf("📊 Overall Results: %d/%d tests passed (%.1f%%)\n\n", 
            successes, total_tests, success_rate))

# Print detailed results
for (test_name in names(test_results)) {
  result <- test_results[[test_name]]
  status_icon <- if (grepl("SUCCESS", result)) "✅" else "❌"
  cat(sprintf("%s %-25s: %s\n", status_icon, test_name, result))
}

cat("\n🏆 TESTING COMPLETE!\n")
cat("===================\n")

if (success_rate >= 90) {
  cat("🎉 EXCELLENT: modelbuilder function is ready for production!\n")
} else if (success_rate >= 75) {
  cat("⚠️  GOOD: modelbuilder function works well with minor issues to address.\n")  
} else {
  cat("🚨 NEEDS WORK: Several issues need to be addressed before production use.\n")
}

cat("\n📋 Next Steps:\n")
cat("- Review any failed tests and fix underlying issues\n")
cat("- Test with real clinical datasets\n")
cat("- Validate outputs against known statistical software\n") 
cat("- Performance testing with large datasets\n")
cat("- User acceptance testing with clinical researchers\n")

# Return test summary for potential further analysis
return(list(
  total_tests = total_tests,
  successes = successes, 
  success_rate = success_rate,
  detailed_results = test_results
))