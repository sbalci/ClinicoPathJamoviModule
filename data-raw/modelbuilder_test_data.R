# ============================================================================
# ENHANCED MODEL BUILDER TEST DATASET GENERATOR
# ============================================================================
# Creates comprehensive test data specifically for the Model Builder module
# to demonstrate the complete workflow from raw variables to DCA-ready predictions.
# Enhanced version includes multiple scenarios, missing data patterns, and
# realistic clinical relationships.

library(dplyr)

# Set seed for reproducibility
set.seed(42)

cat("=== Enhanced Model Builder Test Data Generation ===\n")

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

# ============================================================================
# ADDITIONAL TEST DATASETS FOR EDGE CASES AND VALIDATION
# ============================================================================

# Generate small sample dataset for minimum requirements testing
generate_small_sample_data <- function(n = 50) {
    set.seed(123)
    
    data <- data.frame(
        patient_id = paste0("S", sprintf("%03d", 1:n)),
        age = round(rnorm(n, mean = 60, sd = 10)),
        sex = sample(c("Male", "Female"), n, replace = TRUE),
        diabetes = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)),
        hypertension = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.6, 0.4)),
        stringsAsFactors = FALSE
    )
    
    # Generate minimal but realistic outcome
    risk_score <- 0.05 * (data$age - 60) + 
                  0.8 * (data$sex == "Male") + 
                  1.0 * (data$diabetes == "Yes") + 
                  rnorm(n, 0, 0.5)
    
    prob_outcome <- plogis(risk_score - 0.5)
    outcome_numeric <- rbinom(n, 1, prob_outcome)
    data$outcome <- factor(outcome_numeric, levels = c(0, 1), labels = c("No", "Yes"))
    
    return(data)
}

# Generate high missing data dataset
generate_high_missing_data <- function(n = 200) {
    set.seed(456)
    
    data <- generate_modelbuilder_test_data(n)
    
    # Introduce high missing rates (up to 50%)
    high_missing_vars <- c("troponin", "creatinine", "bmi", "systolic_bp")
    
    for (var in high_missing_vars) {
        if (var %in% names(data)) {
            missing_rate <- runif(1, 0.3, 0.5)  # 30-50% missing
            missing_indices <- sample(1:n, floor(n * missing_rate))
            data[[var]][missing_indices] <- NA
        }
    }
    
    return(data)
}

# Generate imbalanced dataset (low event rate)
generate_imbalanced_data <- function(n = 300) {
    set.seed(789)
    
    data <- generate_modelbuilder_test_data(n)
    
    # Force low event rate (5-10%)
    current_events <- sum(data$cardiovascular_event == "Yes")
    target_events <- floor(n * 0.08)  # 8% event rate
    
    if (current_events > target_events) {
        # Convert some events to non-events
        event_indices <- which(data$cardiovascular_event == "Yes")
        convert_indices <- sample(event_indices, current_events - target_events)
        data$cardiovascular_event[convert_indices] <- "No"
    }
    
    return(data)
}

# Generate perfect separation dataset
generate_separation_data <- function(n = 100) {
    set.seed(101)
    
    data <- data.frame(
        patient_id = paste0("PS", sprintf("%03d", 1:n)),
        age = round(rnorm(n, mean = 65, sd = 15)),
        sex = sample(c("Male", "Female"), n, replace = TRUE),
        diabetes = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)),
        separation_var = sample(c("Low", "High"), n, replace = TRUE, prob = c(0.7, 0.3)),
        stringsAsFactors = FALSE
    )
    
    # Create perfect separation scenario
    # All "High" separation_var patients have event, all "Low" do not
    data$outcome <- factor(ifelse(data$separation_var == "High", "Yes", "No"), 
                          levels = c("No", "Yes"))
    
    return(data)
}

# Generate multi-collinearity dataset
generate_collinear_data <- function(n = 200) {
    set.seed(202)
    
    data <- data.frame(
        patient_id = paste0("MC", sprintf("%03d", 1:n)),
        age = round(rnorm(n, mean = 65, sd = 12)),
        sex = sample(c("Male", "Female"), n, replace = TRUE),
        bmi = round(rnorm(n, mean = 27, sd = 4), 1),
        stringsAsFactors = FALSE
    )
    
    # Create highly correlated variables
    data$weight <- round(data$bmi * 3 + rnorm(n, 0, 5), 1)  # High correlation with BMI
    data$weight_kg <- data$weight  # Perfect correlation
    data$bmi_calculated <- data$weight / 3  # Near perfect correlation
    
    # Add some noise to prevent perfect correlation
    data$weight_kg <- data$weight_kg + rnorm(n, 0, 0.1)
    
    # Generate outcome
    risk_score <- 0.05 * (data$age - 65) + 
                  0.8 * (data$sex == "Male") + 
                  0.1 * (data$bmi - 27) + 
                  rnorm(n, 0, 0.8)
    
    prob_outcome <- plogis(risk_score - 1.0)
    outcome_numeric <- rbinom(n, 1, prob_outcome)
    data$outcome <- factor(outcome_numeric, levels = c(0, 1), labels = c("No", "Yes"))
    
    return(data)
}

# Generate longitudinal dataset for time-dependent analysis
generate_longitudinal_data <- function(n_patients = 100, n_visits = 4) {
    set.seed(303)
    
    # Create baseline patient data
    patients <- data.frame(
        patient_id = paste0("L", sprintf("%03d", 1:n_patients)),
        baseline_age = round(rnorm(n_patients, mean = 60, sd = 12)),
        sex = sample(c("Male", "Female"), n_patients, replace = TRUE),
        diabetes = sample(c("No", "Yes"), n_patients, replace = TRUE, prob = c(0.7, 0.3)),
        stringsAsFactors = FALSE
    )
    
    # Create longitudinal observations
    long_data <- data.frame()
    
    for (i in 1:n_patients) {
        patient <- patients[i, ]
        
        for (visit in 1:n_visits) {
            visit_data <- data.frame(
                patient_id = patient$patient_id,
                visit = visit,
                age = patient$baseline_age + (visit - 1) * 0.5,  # Age increases with visits
                sex = patient$sex,
                diabetes = patient$diabetes,
                time_since_baseline = (visit - 1) * 6,  # 6 months between visits
                
                # Time-varying covariates
                systolic_bp = round(rnorm(1, mean = 130 + visit * 2, sd = 15)),
                cholesterol = round(rnorm(1, mean = 200 - visit * 5, sd = 30)),
                
                stringsAsFactors = FALSE
            )
            
            # Generate time-dependent outcome
            risk_score <- 0.05 * (visit_data$age - 60) + 
                          0.8 * (visit_data$sex == "Male") + 
                          1.0 * (visit_data$diabetes == "Yes") + 
                          0.01 * (visit_data$systolic_bp - 130) + 
                          0.1 * visit +  # Risk increases with time
                          rnorm(1, 0, 0.8)
            
            prob_outcome <- plogis(risk_score - 1.2)
            outcome_numeric <- rbinom(1, 1, prob_outcome)
            visit_data$outcome <- factor(outcome_numeric, levels = c(0, 1), labels = c("No", "Yes"))
            
            long_data <- rbind(long_data, visit_data)
        }
    }
    
    return(long_data)
}

# ============================================================================
# GENERATE ALL TEST DATASETS
# ============================================================================

cat("\n1. Generating main test dataset (n=600)...\n")
modelbuilder_test_data <- generate_modelbuilder_test_data(600)

cat("2. Generating small sample dataset (n=50)...\n")
modelbuilder_small_data <- generate_small_sample_data(50)

cat("3. Generating high missing data dataset (n=200)...\n")
modelbuilder_high_missing_data <- generate_high_missing_data(200)

cat("4. Generating imbalanced dataset (n=300)...\n")
modelbuilder_imbalanced_data <- generate_imbalanced_data(300)

cat("5. Generating separation dataset (n=100)...\n")
modelbuilder_separation_data <- generate_separation_data(100)

cat("6. Generating collinear dataset (n=200)...\n")
modelbuilder_collinear_data <- generate_collinear_data(200)

cat("7. Generating longitudinal dataset (n=400 observations)...\n")
modelbuilder_longitudinal_data <- generate_longitudinal_data(100, 4)

# ============================================================================
# SAVE ALL DATASETS
# ============================================================================

cat("\n=== Saving All Test Datasets ===\n")

# Save main dataset
save(modelbuilder_test_data, file = "data/modelbuilder_test_data.RData")
write.csv(modelbuilder_test_data, "data/modelbuilder_test_data.csv", row.names = FALSE)

# Save additional datasets
save(modelbuilder_small_data, file = "data/modelbuilder_small_data.RData")
save(modelbuilder_high_missing_data, file = "data/modelbuilder_high_missing_data.RData")
save(modelbuilder_imbalanced_data, file = "data/modelbuilder_imbalanced_data.RData")
save(modelbuilder_separation_data, file = "data/modelbuilder_separation_data.RData")
save(modelbuilder_collinear_data, file = "data/modelbuilder_collinear_data.RData")
save(modelbuilder_longitudinal_data, file = "data/modelbuilder_longitudinal_data.RData")

# ============================================================================
# COMPREHENSIVE SUMMARY AND VALIDATION SCENARIOS
# ============================================================================

cat("\n=== COMPREHENSIVE TEST DATASET SUMMARY ===\n")

# Main dataset summary
cat("\n1. MAIN TEST DATASET (n =", nrow(modelbuilder_test_data), "):\n")
cat("   - Event rate:", round(mean(modelbuilder_test_data$cardiovascular_event == "Yes") * 100, 1), "%\n")
cat("   - Complete for basic model variables\n")
cat("   - Realistic clinical relationships\n")
cat("   - Suitable for all model types\n")

# Small sample summary
cat("\n2. SMALL SAMPLE DATASET (n =", nrow(modelbuilder_small_data), "):\n")
cat("   - Event rate:", round(mean(modelbuilder_small_data$outcome == "Yes") * 100, 1), "%\n")
cat("   - Tests minimum sample size requirements\n")
cat("   - Validates EPV (Events Per Variable) calculations\n")

# High missing data summary
cat("\n3. HIGH MISSING DATA DATASET (n =", nrow(modelbuilder_high_missing_data), "):\n")
cat("   - Event rate:", round(mean(modelbuilder_high_missing_data$cardiovascular_event == "Yes") * 100, 1), "%\n")
missing_summary <- sapply(modelbuilder_high_missing_data, function(x) round(sum(is.na(x))/length(x)*100, 1))
high_missing_vars <- names(missing_summary)[missing_summary > 20]
cat("   - High missing variables:", paste(high_missing_vars, collapse = ", "), "\n")
cat("   - Tests missing data handling methods\n")

# Imbalanced data summary
cat("\n4. IMBALANCED DATASET (n =", nrow(modelbuilder_imbalanced_data), "):\n")
cat("   - Event rate:", round(mean(modelbuilder_imbalanced_data$cardiovascular_event == "Yes") * 100, 1), "%\n")
cat("   - Tests low event rate scenarios\n")
cat("   - Validates stratified sampling\n")

# Separation data summary
cat("\n5. SEPARATION DATASET (n =", nrow(modelbuilder_separation_data), "):\n")
cat("   - Event rate:", round(mean(modelbuilder_separation_data$outcome == "Yes") * 100, 1), "%\n")
cat("   - Tests perfect separation detection\n")
cat("   - Validates convergence warnings\n")

# Collinear data summary
cat("\n6. COLLINEAR DATASET (n =", nrow(modelbuilder_collinear_data), "):\n")
cat("   - Event rate:", round(mean(modelbuilder_collinear_data$outcome == "Yes") * 100, 1), "%\n")
cat("   - Tests multicollinearity detection\n")
cat("   - Validates variable selection\n")

# Longitudinal data summary
cat("\n7. LONGITUDINAL DATASET (n =", nrow(modelbuilder_longitudinal_data), "):\n")
cat("   - Patients:", length(unique(modelbuilder_longitudinal_data$patient_id)), "\n")
cat("   - Visits per patient:", nrow(modelbuilder_longitudinal_data) / length(unique(modelbuilder_longitudinal_data$patient_id)), "\n")
cat("   - Tests time-dependent modeling\n")

# ============================================================================
# COMPREHENSIVE TESTING SCENARIOS
# ============================================================================

cat("\n=== COMPREHENSIVE TESTING SCENARIOS ===\n")

cat("\nA. BASIC FUNCTIONALITY TESTS:\n")
cat("   Dataset: modelbuilder_test_data.csv\n")
cat("   Tests: Basic model building, enhanced models, biomarker models\n")
cat("   Expected: All models build successfully, AUC > 0.7\n")

cat("\nB. SAMPLE SIZE VALIDATION:\n")
cat("   Dataset: modelbuilder_small_data.RData\n")
cat("   Tests: Minimum sample size, EPV calculations\n")
cat("   Expected: Warnings for low EPV, successful model building\n")

cat("\nC. MISSING DATA HANDLING:\n")
cat("   Dataset: modelbuilder_high_missing_data.RData\n")
cat("   Tests: Complete cases, mean imputation, multiple imputation\n")
cat("   Expected: Appropriate handling of missing data methods\n")

cat("\nD. IMBALANCED DATA SCENARIOS:\n")
cat("   Dataset: modelbuilder_imbalanced_data.RData\n")
cat("   Tests: Stratified sampling, low event rate handling\n")
cat("   Expected: Maintained event rates across train/validation\n")

cat("\nE. STATISTICAL ISSUES:\n")
cat("   Dataset: modelbuilder_separation_data.RData\n")
cat("   Tests: Perfect separation detection, convergence monitoring\n")
cat("   Expected: Warnings for separation issues\n")

cat("\nF. MULTICOLLINEARITY:\n")
cat("   Dataset: modelbuilder_collinear_data.RData\n")
cat("   Tests: High correlation detection, variable selection\n")
cat("   Expected: Warnings for high correlation between predictors\n")

cat("\nG. LONGITUDINAL ANALYSIS:\n")
cat("   Dataset: modelbuilder_longitudinal_data.RData\n")
cat("   Tests: Time-dependent covariates, repeated measures\n")
cat("   Expected: Appropriate handling of longitudinal structure\n")

# ============================================================================
# INTEGRATION TESTING WORKFLOW
# ============================================================================

cat("\n=== INTEGRATION TESTING WORKFLOW ===\n")

cat("\nPHASE 1: BASIC MODEL BUILDING\n")
cat("1. Load modelbuilder_test_data.csv\n")
cat("2. Build basic clinical model (age, sex, diabetes, hypertension)\n")
cat("3. Verify model convergence and performance\n")
cat("4. Check AUC > 0.7 and calibration slope ≈ 1.0\n")

cat("\nPHASE 2: ENHANCED MODELING\n")
cat("1. Add enhanced predictors (smoking, cholesterol, BMI)\n")
cat("2. Enable stepwise selection\n")
cat("3. Test interaction effects\n")
cat("4. Verify improved performance\n")

cat("\nPHASE 3: BIOMARKER INTEGRATION\n")
cat("1. Add biomarker predictors (troponin, creatinine)\n")
cat("2. Test missing data handling\n")
cat("3. Verify biomarker model performance\n")
cat("4. Compare to clinical models\n")

cat("\nPHASE 4: VALIDATION AND DIAGNOSTICS\n")
cat("1. Enable cross-validation and bootstrap\n")
cat("2. Check calibration plots and diagnostics\n")
cat("3. Test optimism-corrected estimates\n")
cat("4. Validate all performance metrics\n")

cat("\nPHASE 5: EDGE CASE HANDLING\n")
cat("1. Test with small sample dataset\n")
cat("2. Test with high missing data\n")
cat("3. Test with imbalanced outcomes\n")
cat("4. Verify appropriate warnings and errors\n")

cat("\nPHASE 6: DCA INTEGRATION\n")
cat("1. Generate prediction columns\n")
cat("2. Export for Decision Curve Analysis\n")
cat("3. Test seamless DCA workflow\n")
cat("4. Verify clinical utility assessment\n")

cat("\n=== SUCCESS CRITERIA ===\n")
cat("✓ All datasets load successfully\n")
cat("✓ Models build without errors\n")
cat("✓ Performance metrics within expected ranges\n")
cat("✓ Missing data handled appropriately\n")
cat("✓ Edge cases trigger appropriate warnings\n")
cat("✓ DCA integration works seamlessly\n")
cat("✓ Complete workflow takes <2 minutes\n")

cat("\n=== FILES CREATED ===\n")
cat("Main datasets:\n")
cat("- data/modelbuilder_test_data.csv (primary dataset)\n")
cat("- data/modelbuilder_test_data.RData (primary dataset)\n")
cat("\nSpecialized datasets:\n")
cat("- data/modelbuilder_small_data.RData (n=50)\n")
cat("- data/modelbuilder_high_missing_data.RData (high missing)\n")
cat("- data/modelbuilder_imbalanced_data.RData (low event rate)\n")
cat("- data/modelbuilder_separation_data.RData (perfect separation)\n")
cat("- data/modelbuilder_collinear_data.RData (multicollinearity)\n")
cat("- data/modelbuilder_longitudinal_data.RData (time-dependent)\n")

print("Enhanced Model Builder test data generation complete!")
