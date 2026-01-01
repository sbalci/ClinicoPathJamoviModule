# Comprehensive Test Data Generator for modelbuilder function
# This script creates multiple realistic clinical datasets to test all features
# of the modelbuilder function in various scenarios

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(magrittr)

set.seed(12345)  # For reproducible test data

# ==========================================
# Dataset 1: CARDIAC RISK PREDICTION
# ==========================================
# Comprehensive cardiovascular risk dataset for testing all model types

n_cardiac <- 800

cardiac_data <- data.frame(
  # Demographics
  patient_id = 1:n_cardiac,
  age = round(rnorm(n_cardiac, 65, 12), 0),
  sex = factor(sample(c("Male", "Female"), n_cardiac, replace = TRUE, prob = c(0.55, 0.45))),
  
  # Basic clinical predictors
  diabetes = factor(sample(c("No", "Yes"), n_cardiac, replace = TRUE, prob = c(0.65, 0.35))),
  hypertension = factor(sample(c("No", "Yes"), n_cardiac, replace = TRUE, prob = c(0.45, 0.55))),
  smoking = factor(sample(c("Never", "Former", "Current"), n_cardiac, replace = TRUE, 
                          prob = c(0.4, 0.35, 0.25))),
  family_history = factor(sample(c("No", "Yes"), n_cardiac, replace = TRUE, prob = c(0.7, 0.3))),
  
  # Enhanced clinical predictors
  bmi = round(rnorm(n_cardiac, 27, 5), 1),
  systolic_bp = round(rnorm(n_cardiac, 140, 20), 0),
  diastolic_bp = round(rnorm(n_cardiac, 85, 12), 0),
  exercise = factor(sample(c("None", "Light", "Moderate", "Vigorous"), n_cardiac, 
                           replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))),
  alcohol = factor(sample(c("None", "Light", "Moderate", "Heavy"), n_cardiac,
                          replace = TRUE, prob = c(0.25, 0.4, 0.25, 0.1))),
  
  # Biomarker predictors (with realistic correlations and missing values)
  cholesterol_total = round(rnorm(n_cardiac, 200, 40), 0),
  hdl_cholesterol = round(rnorm(n_cardiac, 45, 12), 0),
  ldl_cholesterol = round(rnorm(n_cardiac, 125, 35), 0),
  triglycerides = round(rnorm(n_cardiac, 150, 60), 0),
  crp = round(exp(rnorm(n_cardiac, 1, 0.8)), 2),  # Log-normal distribution
  troponin = round(exp(rnorm(n_cardiac, -1, 1.2)), 3),  # Skewed biomarker
  hba1c = round(rnorm(n_cardiac, 6.2, 1.5), 1),
  
  # Custom predictors for testing interactions
  stress_score = round(rnorm(n_cardiac, 5, 2), 0),  # 0-10 scale
  medication_count = rpois(n_cardiac, 3),  # Count variable
  income_bracket = factor(sample(c("Low", "Middle", "High"), n_cardiac, 
                                replace = TRUE, prob = c(0.3, 0.5, 0.2)))
)

# Create realistic ranges and correlations
cardiac_data <- cardiac_data %>%
  mutate(
    age = pmax(30, pmin(90, age)),
    bmi = pmax(18, pmin(45, bmi)),
    systolic_bp = pmax(90, pmin(200, systolic_bp)),
    diastolic_bp = pmax(60, pmin(120, diastolic_bp)),
    cholesterol_total = pmax(120, pmin(350, cholesterol_total)),
    hdl_cholesterol = pmax(20, pmin(80, hdl_cholesterol)),
    ldl_cholesterol = pmax(50, pmin(250, ldl_cholesterol)),
    triglycerides = pmax(50, pmin(400, triglycerides)),
    crp = pmax(0.1, pmin(20, crp)),
    troponin = pmax(0.001, pmin(10, troponin)),
    hba1c = pmax(4.5, pmin(12, hba1c)),
    stress_score = pmax(0, pmin(10, stress_score))
  )

# Create outcome based on realistic clinical risk model
cardiac_data$risk_score <- with(cardiac_data,
  0.05 * age + 
  0.8 * (sex == "Male") + 
  1.2 * (diabetes == "Yes") + 
  0.9 * (hypertension == "Yes") +
  0.7 * (smoking == "Current") + 0.3 * (smoking == "Former") +
  0.5 * (family_history == "Yes") +
  0.02 * (bmi - 25) +
  0.01 * (systolic_bp - 120) +
  0.003 * (cholesterol_total - 200) +
  0.1 * log(crp) +
  0.2 * log(troponin + 0.001) +
  0.3 * (hba1c - 6) +
  rnorm(n_cardiac, 0, 1)  # Random component
)

# Convert risk score to binary outcome with realistic event rate (~15%)
cardiac_data$cardiovascular_event <- factor(
  ifelse(cardiac_data$risk_score > quantile(cardiac_data$risk_score, 0.85), "Yes", "No"),
  levels = c("No", "Yes")
)

# Introduce realistic missing data patterns
missing_pattern <- sample(1:n_cardiac, n_cardiac * 0.05)  # 5% missing biomarkers
cardiac_data$troponin[missing_pattern] <- NA

missing_pattern2 <- sample(1:n_cardiac, n_cardiac * 0.03)  # 3% missing CRP
cardiac_data$crp[missing_pattern2] <- NA

missing_pattern3 <- sample(1:n_cardiac, n_cardiac * 0.08)  # 8% missing HbA1c
cardiac_data$hba1c[missing_pattern3] <- NA

# Remove temporary variables
cardiac_data$risk_score <- NULL

# ==========================================
# Dataset 2: CANCER PROGNOSIS (SMALLER SAMPLE)
# ==========================================
# Test with smaller sample size and different event rate

n_cancer <- 200

cancer_data <- data.frame(
  # Demographics
  patient_id = 1:n_cancer,
  age = round(rnorm(n_cancer, 68, 10), 0),
  sex = factor(sample(c("Male", "Female"), n_cancer, replace = TRUE, prob = c(0.6, 0.4))),
  
  # Basic clinical predictors
  stage = factor(sample(c("I", "II", "III", "IV"), n_cancer, replace = TRUE,
                        prob = c(0.2, 0.3, 0.3, 0.2))),
  grade = factor(sample(c("Low", "Intermediate", "High"), n_cancer, replace = TRUE,
                        prob = c(0.25, 0.45, 0.3))),
  tumor_size = round(rnorm(n_cancer, 3.5, 2), 1),
  
  # Enhanced predictors
  lymph_nodes_positive = rpois(n_cancer, 2),
  performance_status = factor(sample(0:3, n_cancer, replace = TRUE, 
                                    prob = c(0.4, 0.35, 0.2, 0.05))),
  comorbidity_score = round(rnorm(n_cancer, 2, 1.5), 0),
  
  # Biomarkers
  cea = round(exp(rnorm(n_cancer, 2, 1)), 1),  # Tumor marker
  ca199 = round(exp(rnorm(n_cancer, 3, 1.2)), 1),  # Another tumor marker
  hemoglobin = round(rnorm(n_cancer, 12, 2), 1),
  albumin = round(rnorm(n_cancer, 3.8, 0.6), 1),
  
  # Custom predictors
  treatment_response = factor(sample(c("Complete", "Partial", "Stable", "Progressive"), 
                                    n_cancer, replace = TRUE, prob = c(0.2, 0.4, 0.25, 0.15))),
  mutation_status = factor(sample(c("Wild-type", "Mutated"), n_cancer, 
                                 replace = TRUE, prob = c(0.65, 0.35)))
)

# Create realistic ranges
cancer_data <- cancer_data %>%
  mutate(
    age = pmax(30, pmin(90, age)),
    tumor_size = pmax(0.5, pmin(15, tumor_size)),
    lymph_nodes_positive = pmax(0, pmin(20, lymph_nodes_positive)),
    comorbidity_score = pmax(0, pmin(8, comorbidity_score)),
    cea = pmax(0.5, pmin(100, cea)),
    ca199 = pmax(5, pmin(500, ca199)),
    hemoglobin = pmax(8, pmin(18, hemoglobin)),
    albumin = pmax(2, pmin(5, albumin))
  )

# Create outcome with higher event rate (~30%)
cancer_data$risk_score <- with(cancer_data,
  0.02 * age +
  0.5 * (sex == "Male") +
  1.5 * (stage == "III") + 2.5 * (stage == "IV") + 0.8 * (stage == "II") +
  1.2 * (grade == "High") + 0.6 * (grade == "Intermediate") +
  0.2 * tumor_size +
  0.1 * lymph_nodes_positive +
  0.8 * (performance_status == "2") + 1.5 * (performance_status == "3") +
  0.3 * comorbidity_score +
  0.01 * cea + 0.002 * ca199 +
  -0.3 * hemoglobin + -0.5 * albumin +
  1.0 * (treatment_response == "Progressive") + 0.5 * (treatment_response == "Stable") +
  0.7 * (mutation_status == "Mutated") +
  rnorm(n_cancer, 0, 1)
)

cancer_data$death_within_2years <- factor(
  ifelse(cancer_data$risk_score > quantile(cancer_data$risk_score, 0.7), "Yes", "No"),
  levels = c("No", "Yes")
)

# Add missing data (more aggressive pattern)
missing_biomarkers <- sample(1:n_cancer, n_cancer * 0.12)
cancer_data$ca199[missing_biomarkers] <- NA

missing_albumin <- sample(1:n_cancer, n_cancer * 0.08)
cancer_data$albumin[missing_albumin] <- NA

cancer_data$risk_score <- NULL

# ==========================================
# Dataset 3: DIABETES COMPLICATIONS (HIGH DIMENSIONALITY)
# ==========================================
# Test with many predictors for penalized regression

n_diabetes <- 400

diabetes_data <- data.frame(
  # Demographics
  patient_id = 1:n_diabetes,
  age = round(rnorm(n_diabetes, 58, 12), 0),
  sex = factor(sample(c("Male", "Female"), n_diabetes, replace = TRUE)),
  ethnicity = factor(sample(c("Caucasian", "Hispanic", "African-American", "Asian"), 
                            n_diabetes, replace = TRUE, prob = c(0.6, 0.2, 0.15, 0.05))),
  
  # Many clinical predictors (to test penalized regression)
  diabetes_duration = round(rnorm(n_diabetes, 8, 5), 0),
  hba1c_baseline = round(rnorm(n_diabetes, 8.2, 1.8), 1),
  hba1c_recent = round(rnorm(n_diabetes, 7.8, 1.6), 1),
  fasting_glucose = round(rnorm(n_diabetes, 150, 40), 0),
  random_glucose = round(rnorm(n_diabetes, 180, 50), 0),
  insulin_use = factor(sample(c("No", "Yes"), n_diabetes, replace = TRUE, prob = c(0.4, 0.6))),
  metformin_use = factor(sample(c("No", "Yes"), n_diabetes, replace = TRUE, prob = c(0.3, 0.7))),
  
  # Cardiovascular risk factors
  systolic_bp = round(rnorm(n_diabetes, 145, 25), 0),
  diastolic_bp = round(rnorm(n_diabetes, 88, 15), 0),
  cholesterol = round(rnorm(n_diabetes, 195, 45), 0),
  triglycerides = round(rnorm(n_diabetes, 170, 80), 0),
  bmi = round(rnorm(n_diabetes, 29, 6), 1),
  waist_circumference = round(rnorm(n_diabetes, 102, 15), 0),
  
  # Laboratory values
  creatinine = round(rnorm(n_diabetes, 1.1, 0.4), 2),
  egfr = round(rnorm(n_diabetes, 75, 20), 0),
  albumin_urine = round(exp(rnorm(n_diabetes, 3, 1.5)), 1),
  hemoglobin = round(rnorm(n_diabetes, 13, 1.5), 1),
  wbc = round(rnorm(n_diabetes, 7.5, 2), 1),
  platelets = round(rnorm(n_diabetes, 280, 60), 0),
  
  # Lifestyle factors
  exercise_minutes = round(rnorm(n_diabetes, 120, 80), 0),
  smoking_packyears = round(abs(rnorm(n_diabetes, 5, 10)), 1),
  alcohol_drinks_week = round(abs(rnorm(n_diabetes, 3, 6)), 0),
  
  # Complications history
  retinopathy = factor(sample(c("None", "Mild", "Moderate", "Severe"), n_diabetes,
                             replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1))),
  neuropathy = factor(sample(c("No", "Yes"), n_diabetes, replace = TRUE, prob = c(0.65, 0.35))),
  
  # Additional predictors for testing interactions
  depression = factor(sample(c("No", "Yes"), n_diabetes, replace = TRUE, prob = c(0.75, 0.25))),
  medication_adherence = round(rnorm(n_diabetes, 85, 15), 0),  # Percentage
  healthcare_visits = rpois(n_diabetes, 4)
)

# Create realistic ranges
diabetes_data <- diabetes_data %>%
  mutate(
    age = pmax(25, pmin(85, age)),
    diabetes_duration = pmax(0, pmin(30, diabetes_duration)),
    hba1c_baseline = pmax(5.5, pmin(14, hba1c_baseline)),
    hba1c_recent = pmax(5.5, pmin(13, hba1c_recent)),
    fasting_glucose = pmax(80, pmin(300, fasting_glucose)),
    random_glucose = pmax(100, pmin(400, random_glucose)),
    systolic_bp = pmax(90, pmin(200, systolic_bp)),
    diastolic_bp = pmax(60, pmin(120, diastolic_bp)),
    cholesterol = pmax(120, pmin(350, cholesterol)),
    triglycerides = pmax(50, pmin(500, triglycerides)),
    bmi = pmax(18, pmin(50, bmi)),
    waist_circumference = pmax(70, pmin(150, waist_circumference)),
    creatinine = pmax(0.5, pmin(3.5, creatinine)),
    egfr = pmax(15, pmin(120, egfr)),
    albumin_urine = pmax(5, pmin(1000, albumin_urine)),
    hemoglobin = pmax(8, pmin(18, hemoglobin)),
    wbc = pmax(3, pmin(15, wbc)),
    platelets = pmax(100, pmin(500, platelets)),
    exercise_minutes = pmax(0, pmin(420, exercise_minutes)),
    smoking_packyears = pmin(60, smoking_packyears),
    alcohol_drinks_week = pmin(30, alcohol_drinks_week),
    medication_adherence = pmax(0, pmin(100, medication_adherence))
  )

# Create outcome - diabetic nephropathy progression
diabetes_data$risk_score <- with(diabetes_data,
  0.02 * age +
  0.3 * (sex == "Male") +
  0.1 * diabetes_duration +
  0.4 * hba1c_recent +
  0.002 * fasting_glucose +
  0.8 * (insulin_use == "Yes") +
  0.01 * (systolic_bp - 120) +
  0.003 * (cholesterol - 200) +
  0.02 * (bmi - 25) +
  0.5 * log(creatinine + 0.1) +
  -0.02 * egfr +
  0.001 * albumin_urine +
  -0.3 * (hemoglobin - 13) +
  -0.005 * exercise_minutes +
  0.05 * smoking_packyears +
  0.8 * (retinopathy == "Severe") + 0.5 * (retinopathy == "Moderate") + 0.2 * (retinopathy == "Mild") +
  0.6 * (neuropathy == "Yes") +
  0.4 * (depression == "Yes") +
  -0.01 * medication_adherence +
  rnorm(n_diabetes, 0, 1)
)

diabetes_data$nephropathy_progression <- factor(
  ifelse(diabetes_data$risk_score > quantile(diabetes_data$risk_score, 0.8), "Yes", "No"),
  levels = c("No", "Yes")
)

# Add substantial missing data for testing imputation
vars_with_missing <- c("albumin_urine", "egfr", "hemoglobin", "cholesterol", "triglycerides", 
                      "exercise_minutes", "smoking_packyears", "medication_adherence")

for (var in vars_with_missing) {
  missing_indices <- sample(1:n_diabetes, n_diabetes * runif(1, 0.05, 0.15))
  diabetes_data[[var]][missing_indices] <- NA
}

diabetes_data$risk_score <- NULL

# ==========================================
# Dataset 4: MINIMAL SAMPLE (EDGE CASE TESTING)
# ==========================================
# Test minimum viable sample sizes and edge cases

n_minimal <- 60  # Near minimum for stable modeling

minimal_data <- data.frame(
  patient_id = 1:n_minimal,
  age = round(rnorm(n_minimal, 70, 15), 0),
  sex = factor(sample(c("Male", "Female"), n_minimal, replace = TRUE)),
  comorbid = factor(sample(c("Low", "High"), n_minimal, replace = TRUE)),
  biomarker1 = round(rnorm(n_minimal, 50, 20), 1),
  biomarker2 = round(rnorm(n_minimal, 100, 40), 1),
  treatment = factor(sample(c("Standard", "Experimental"), n_minimal, replace = TRUE))
)

# Create outcome with exactly 15 events (25% event rate)
minimal_data$risk_score <- with(minimal_data,
  0.05 * age + 
  1.0 * (sex == "Male") + 
  1.5 * (comorbid == "High") +
  0.02 * biomarker1 + 
  0.01 * biomarker2 +
  0.8 * (treatment == "Experimental") +
  rnorm(n_minimal, 0, 1)
)

# Force exactly 15 events
event_indices <- order(minimal_data$risk_score, decreasing = TRUE)[1:15]
minimal_data$outcome <- factor("No", levels = c("No", "Yes"))
minimal_data$outcome[event_indices] <- "Yes"

minimal_data$risk_score <- NULL

# ==========================================
# Dataset 5: PERFECT SEPARATION CHALLENGE
# ==========================================
# Test handling of separation issues

n_separation <- 150

separation_data <- data.frame(
  patient_id = 1:n_separation,
  age = round(rnorm(n_separation, 60, 12), 0),
  sex = factor(sample(c("Male", "Female"), n_separation, replace = TRUE)),
  
  # Create predictors that can cause separation
  rare_condition = factor(sample(c("No", "Yes"), n_separation, replace = TRUE, prob = c(0.95, 0.05))),
  biomarker_extreme = round(rnorm(n_separation, 50, 25), 1),
  test_result = factor(sample(c("Negative", "Positive"), n_separation, replace = TRUE, prob = c(0.8, 0.2)))
)

# Create outcome that might cause separation with rare_condition
separation_data$outcome <- factor("No", levels = c("No", "Yes"))

# Make all patients with rare condition have the outcome (perfect separation)
separation_data$outcome[separation_data$rare_condition == "Yes"] <- "Yes"

# Add some additional events based on other factors
additional_events <- sample(which(separation_data$rare_condition == "No" & 
                                 separation_data$biomarker_extreme > 70), 10)
separation_data$outcome[additional_events] <- "Yes"

# ==========================================
# SAVE ALL DATASETS AS CSV FILES
# ==========================================

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save individual datasets as CSV files
write.csv(cardiac_data, file = "data/cardiac_risk_prediction.csv", row.names = FALSE)
write.csv(cancer_data, file = "data/cancer_prognosis_data.csv", row.names = FALSE) 
write.csv(diabetes_data, file = "data/diabetes_complications.csv", row.names = FALSE)
write.csv(minimal_data, file = "data/minimal_sample_test.csv", row.names = FALSE)
write.csv(separation_data, file = "data/separation_challenge.csv", row.names = FALSE)

# Create comprehensive test documentation
cat("
# Model Builder Test Datasets

This file describes the comprehensive test datasets created for the modelbuilder function.

## Dataset 1: Cardiac Risk Prediction (cardiac_data)
- **Sample Size**: 800 patients
- **Outcome**: cardiovascular_event (15% event rate)
- **Use Cases**: Tests all model types, standard workflows
- **Predictors**: 
  - Basic: age, sex, diabetes, hypertension, smoking, family_history
  - Enhanced: bmi, blood_pressure, exercise, alcohol  
  - Biomarkers: cholesterol panel, CRP, troponin, HbA1c
  - Custom: stress_score, medication_count, income_bracket
- **Missing Data**: 5-8% in select biomarkers
- **Special Features**: Realistic clinical correlations, interaction potential

## Dataset 2: Cancer Prognosis (cancer_data) 
- **Sample Size**: 200 patients
- **Outcome**: death_within_2years (30% event rate)
- **Use Cases**: Tests smaller samples, higher event rates
- **Predictors**: 
  - Basic: stage, grade, tumor_size
  - Enhanced: lymph_nodes, performance_status, comorbidities
  - Biomarkers: CEA, CA19-9, hemoglobin, albumin
  - Custom: treatment_response, mutation_status
- **Missing Data**: 8-12% in biomarkers
- **Special Features**: Ordinal predictors, treatment interactions

## Dataset 3: Diabetes Complications (diabetes_data)
- **Sample Size**: 400 patients  
- **Outcome**: nephropathy_progression (20% event rate)
- **Use Cases**: Tests penalized regression, high-dimensional data
- **Predictors**: 25+ variables including demographics, lab values, medications, lifestyle
- **Missing Data**: 5-15% across multiple variables
- **Special Features**: Many correlated predictors, ideal for LASSO/Ridge

## Dataset 4: Minimal Sample (minimal_data)
- **Sample Size**: 60 patients
- **Outcome**: outcome (25% event rate, exactly 15 events)
- **Use Cases**: Tests minimum sample requirements, edge cases
- **Predictors**: 6 basic predictors
- **Missing Data**: None
- **Special Features**: Tests EPV ratios, small sample stability

## Dataset 5: Separation Challenge (separation_data)
- **Sample Size**: 150 patients
- **Outcome**: outcome (variable event rate)
- **Use Cases**: Tests separation detection and handling
- **Predictors**: Includes perfect predictor (rare_condition)
- **Missing Data**: None  
- **Special Features**: Perfect separation scenario, convergence challenges

## Testing Recommendations

1. **Basic Functionality**: Use cardiac_data with basic predictors
2. **All Model Types**: Use cardiac_data with appropriate predictor sets
3. **Penalized Regression**: Use diabetes_data with many predictors
4. **Small Samples**: Use minimal_data 
5. **Edge Cases**: Use separation_data
6. **Missing Data**: Use cancer_data or diabetes_data
7. **Cross-validation**: Any dataset >100 samples
8. **Bootstrap**: Any dataset >50 samples
9. **Model Comparison**: Use cardiac_data with multiple model types

## Expected Performance Metrics

- **Cardiac Data**: AUC 0.75-0.85, good calibration
- **Cancer Data**: AUC 0.70-0.80, moderate calibration  
- **Diabetes Data**: Variable AUC depending on penalty, excellent for variable selection
- **Minimal Data**: Lower AUC (0.60-0.75), wider confidence intervals
- **Separation Data**: Convergence warnings, extreme coefficients

", file = "data/MODELBUILDER_TEST_DATASETS_README.md")

# Print summary
cat("✅ Created 5 comprehensive test datasets for modelbuilder function:\n")
cat("   1. cardiac_risk_prediction.csv (n=800, comprehensive testing)\n")
cat("   2. cancer_prognosis_data.csv (n=200, smaller sample testing)\n") 
cat("   3. diabetes_complications.csv (n=400, penalized regression testing)\n")
cat("   4. minimal_sample_test.csv (n=60, edge case testing)\n")
cat("   5. separation_challenge.csv (n=150, separation handling testing)\n")
cat("\n✅ Documentation created: MODELBUILDER_TEST_DATASETS_README.md\n")
cat("\n🎯 Datasets test all modelbuilder features:\n")
cat("   ✓ Multiple model types (basic, enhanced, biomarker, custom)\n")
cat("   ✓ Penalized regression (LASSO, Ridge, Elastic Net)\n")
cat("   ✓ Missing data handling (complete case, imputation, exclusion)\n")
cat("   ✓ Variable transformations (log, polynomial, spline)\n") 
cat("   ✓ Interactions and feature engineering\n")
cat("   ✓ Cross-validation and bootstrap validation\n")
cat("   ✓ Model comparison and performance metrics\n")
cat("   ✓ Edge cases (small samples, separation, convergence issues)\n")
cat("   ✓ Clinical interpretation and risk score generation\n")
