# ============================================================================
# MODEL BUILDER TEST DATASET GENERATOR
# ============================================================================
# Creates test data specifically for the Model Builder module to demonstrate
# the complete workflow from raw variables to DCA-ready predictions

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Generate realistic clinical dataset
generate_modelbuilder_test_data <- function(n = 600) {

  # Patient demographics
  age <- round(rnorm(n, mean = 65, sd = 12))
  age <- pmax(35, pmin(85, age))  # Constrain to realistic range

  sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))

  # Clinical risk factors
  diabetes <- sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.65, 0.35))
  hypertension <- sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.55, 0.45))
  smoking <- sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.45, 0.35, 0.20))

  # Laboratory values
  cholesterol <- round(rnorm(n, mean = 210, sd = 35))
  cholesterol <- pmax(120, pmin(320, cholesterol))

  # Biomarkers (with some missing values to test handling)
  troponin <- rlnorm(n, meanlog = 1.2, sdlog = 0.6)
  troponin <- round(troponin, 2)
  troponin[sample(n, floor(n * 0.08))] <- NA  # 8% missing

  creatinine <- round(rnorm(n, mean = 1.0, sd = 0.25), 2)
  creatinine <- pmax(0.5, pmin(2.5, creatinine))
  creatinine[sample(n, floor(n * 0.05))] <- NA  # 5% missing

  # Additional clinical variables
  bmi <- round(rnorm(n, mean = 27, sd = 4), 1)
  bmi <- pmax(18, pmin(45, bmi))

  systolic_bp <- round(rnorm(n, mean = 135, sd = 20))
  systolic_bp <- pmax(90, pmin(200, systolic_bp))

  # Family history
  family_history <- sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.70, 0.30))

  # Generate realistic outcome based on established relationships
  # Create risk score based on clinical knowledge
  risk_score <-
    0.05 * (age - 65) +                          # Age effect
    0.8 * (sex == "Male") +                      # Male higher risk
    1.2 * (diabetes == "Yes") +                  # Diabetes major risk
    0.7 * (hypertension == "Yes") +              # Hypertension moderate risk
    1.5 * (smoking == "Current") +               # Current smoking high risk
    0.6 * (smoking == "Former") +                # Former smoking moderate risk
    0.008 * (cholesterol - 200) +                # Cholesterol effect
    0.3 * log(troponin + 0.1) +                  # Troponin effect (log scale)
    0.8 * (creatinine - 1.0) +                   # Creatinine effect
    0.04 * (bmi - 25) +                          # BMI effect
    0.01 * (systolic_bp - 120) +                 # Blood pressure effect
    0.5 * (family_history == "Yes") +            # Family history
    rnorm(n, 0, 0.8)                             # Individual variation

  # Convert to probability and generate outcome
  prob_outcome <- plogis(risk_score - 1.5)  # Adjust baseline to get ~25% event rate
  outcome_numeric <- rbinom(n, 1, prob_outcome)
  outcome_factor <- factor(outcome_numeric, levels = c(0, 1), labels = c("No", "Yes"))

  # Create hospital/site variable for potential subgroup analysis
  hospital <- sample(c("General Hospital", "University Medical Center", "Community Hospital"),
                    n, replace = TRUE, prob = c(0.4, 0.35, 0.25))

  # Create final dataset
  test_data <- data.frame(
    # Patient identifiers
    patient_id = sprintf("PT%04d", 1:n),
    hospital = hospital,

    # Demographics (Basic Model Predictors)
    age = age,
    sex = sex,

    # Primary risk factors (Basic Model Predictors)
    diabetes = diabetes,
    hypertension = hypertension,

    # Additional clinical variables (Enhanced Model Predictors)
    smoking = smoking,
    cholesterol = cholesterol,
    bmi = bmi,
    systolic_bp = systolic_bp,
    family_history = family_history,

    # Biomarkers (Biomarker Model Predictors)
    troponin = troponin,
    creatinine = creatinine,

    # Outcome
    cardiovascular_event = outcome_factor,

    # Additional variables for analysis
    true_risk = round(prob_outcome, 3),
    risk_category = cut(prob_outcome,
                       breaks = c(0, 0.1, 0.2, 0.4, 1),
                       labels = c("Low", "Moderate", "High", "Very High"),
                       include.lowest = TRUE),

    stringsAsFactors = FALSE
  )

  return(test_data)
}

# Generate the test dataset
modelbuilder_test_data <- generate_modelbuilder_test_data(600)

# ============================================================================
# DATA VALIDATION AND SUMMARY
# ============================================================================

cat("=== Model Builder Test Data Summary ===\n")
cat("Sample size:", nrow(modelbuilder_test_data), "\n")
cat("Event rate:", round(mean(modelbuilder_test_data$cardiovascular_event == "Yes") * 100, 1), "%\n")
cat("Events:", sum(modelbuilder_test_data$cardiovascular_event == "Yes"), "\n")

cat("\nVariable summary:\n")
cat("Missing data:\n")
missing_summary <- sapply(modelbuilder_test_data, function(x) sum(is.na(x)))
print(missing_summary[missing_summary > 0])

cat("\nOutcome distribution by hospital:\n")
outcome_by_hospital <- table(modelbuilder_test_data$hospital, modelbuilder_test_data$cardiovascular_event)
print(outcome_by_hospital)

cat("\nAge distribution:\n")
print(summary(modelbuilder_test_data$age))

# ============================================================================
# SAVE TEST DATA
# ============================================================================

# Save as CSV for jamovi import
write.csv(modelbuilder_test_data, "./data/modelbuilder_test_data.csv", row.names = FALSE)

# Save as RData for R use
save(modelbuilder_test_data, file = "./data/modelbuilder_test_data.RData")

cat("\nTest data saved as:\n")
cat("- modelbuilder_test_data.csv (for jamovi)\n")
cat("- modelbuilder_test_data.RData (for R)\n")

# ============================================================================
# TESTING SCENARIOS FOR MODEL BUILDER
# ============================================================================

cat("\n=== Model Builder Testing Scenarios ===\n")

cat("\n1. BASIC WORKFLOW TEST:\n")
cat("   Outcome: cardiovascular_event (select 'Yes' as positive)\n")
cat("   Basic Model Predictors: age, sex, diabetes, hypertension\n")
cat("   Expected: AUC ~0.72-0.78, reasonable calibration\n")

cat("\n2. ENHANCED MODEL TEST:\n")
cat("   Enhanced Model Predictors: age, sex, diabetes, hypertension, smoking, cholesterol, bmi\n")
cat("   Expected: AUC ~0.76-0.82, better than basic model\n")

cat("\n3. BIOMARKER MODEL TEST:\n")
cat("   Biomarker Model Predictors: All above + troponin, creatinine\n")
cat("   Expected: AUC ~0.78-0.85, best performance\n")
cat("   Note: Tests missing data handling for biomarkers\n")

cat("\n4. STEPWISE SELECTION TEST:\n")
cat("   Use all variables with stepwise selection enabled\n")
cat("   Expected: Automatic variable selection, optimized models\n")

cat("\n5. INTERACTION EFFECTS TEST:\n")
cat("   Enable interactions between key variables\n")
cat("   Expected: Potential improvement in model performance\n")

cat("\n6. CROSS-VALIDATION TEST:\n")
cat("   Enable 5-fold cross-validation\n")
cat("   Expected: Robust performance estimates, optimism assessment\n")

cat("\n7. MISSING DATA HANDLING TEST:\n")
cat("   Test different missing data methods with biomarker variables\n")
cat("   Expected: Graceful handling of missing troponin/creatinine\n")

cat("\n8. COMPLETE WORKFLOW TEST:\n")
cat("   Build multiple models → Export for DCA → Run DCA analysis\n")
cat("   Expected: Seamless integration, consistent results\n")

# ============================================================================
# EXPECTED PERFORMANCE BENCHMARKS
# ============================================================================

cat("\n=== Expected Model Performance ===\n")

cat("\nBasic Clinical Model (age, sex, diabetes, hypertension):\n")
cat("- AUC: 0.72-0.78\n")
cat("- Calibration slope: 0.9-1.1\n")
cat("- Variables: 4-5 (including factors)\n")

cat("\nEnhanced Clinical Model (+smoking, cholesterol, bmi, bp, family_hx):\n")
cat("- AUC: 0.76-0.82\n")
cat("- Calibration slope: 0.9-1.1\n")
cat("- Variables: 8-12 (including factors)\n")

cat("\nBiomarker Model (+troponin, creatinine):\n")
cat("- AUC: 0.78-0.85\n")
cat("- Calibration slope: 0.9-1.1\n")
cat("- Variables: 10-14 (including factors)\n")
cat("- Note: May have reduced sample size due to missing biomarkers\n")

cat("\nModel Ranking (expected):\n")
cat("1. Biomarker Model (highest AUC, best clinical utility)\n")
cat("2. Enhanced Clinical Model (good performance, practical)\n")
cat("3. Basic Clinical Model (acceptable performance, simple)\n")

# ============================================================================
# INTEGRATION TESTING WORKFLOW
# ============================================================================

cat("\n=== Complete Integration Test Workflow ===\n")

cat("\nPhase 1: Model Builder\n")
cat("1. Import modelbuilder_test_data.csv into jamovi\n")
cat("2. Go to meddecide → Modeling → Prediction Model Builder\n")
cat("3. Set outcome: cardiovascular_event, positive: Yes\n")
cat("4. Build all three models with specified predictors\n")
cat("5. Enable: Data splitting, Cross-validation, Performance metrics\n")
cat("6. Enable: Add predictions to dataset, Prepare for DCA\n")
cat("7. Run analysis and review model performance\n")

cat("\nPhase 2: Decision Curve Analysis\n")
cat("1. Go to meddecide → Decision → Decision Curve Analysis\n")
cat("2. Set outcome: cardiovascular_event, positive: Yes\n")
cat("3. Add prediction models: basic_clinical_prob, enhanced_clinical_prob, biomarker_enhanced_prob\n")
cat("4. Set threshold range: Clinical (5% to 50%)\n")
cat("5. Enable: Clinical impact analysis, Model comparison\n")
cat("6. Run DCA and compare clinical utility\n")

cat("\nPhase 3: Clinical Decision\n")
cat("1. Review net benefit curves and optimal thresholds\n")
cat("2. Assess clinical impact metrics\n")
cat("3. Consider implementation feasibility\n")
cat("4. Make model selection decision\n")

cat("\nSuccess Criteria:\n")
cat("✓ All models build successfully without errors\n")
cat("✓ Performance metrics within expected ranges\n")
cat("✓ Prediction columns created automatically\n")
cat("✓ DCA integration works seamlessly\n")
cat("✓ Clinical utility assessment provides clear guidance\n")
cat("✓ Complete workflow takes <5 minutes\n")

print("Model Builder test data generation complete!")
