# Example Datasets for Bayesian Decision Curve Analysis
# This script creates realistic datasets for demonstrating DCA functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(ClinicoPath)

# Set seed for reproducibility
set.seed(20241201)

# Helper function to create correlated predictions
create_correlated_predictions <- function(outcome, base_logit, effect_size, noise_sd = 0.4) {
  plogis(base_logit + effect_size * outcome + rnorm(length(outcome), 0, noise_sd))
}

# 1. Cancer Screening Dataset (Low prevalence, low thresholds)
cancer_screening_data <- data.frame(
  patient_id = 1:500,
  age = round(rnorm(500, 55, 12)),
  sex = sample(c("Male", "Female"), 500, replace = TRUE, prob = c(0.45, 0.55)),
  family_history = rbinom(500, 1, 0.15),
  imaging_positive = rbinom(500, 1, 0.12),
  biopsy_indicated = rbinom(500, 1, 0.08),
  cancer_detected = rbinom(500, 1, 0.05)  # 5% prevalence
)

# Add prediction models with realistic performance
cancer_screening_data$biomarker_panel_prob <- create_correlated_predictions(
  cancer_screening_data$cancer_detected, -3, 2.5, 0.3)
cancer_screening_data$single_biomarker_prob <- create_correlated_predictions(
  cancer_screening_data$cancer_detected, -3.2, 2.0, 0.4)
cancer_screening_data$clinical_score_prob <- create_correlated_predictions(
  cancer_screening_data$cancer_detected, -2.8, 1.8, 0.5)

# 2. Treatment Decision Dataset (Moderate prevalence, moderate thresholds)
treatment_decision_data <- data.frame(
  patient_id = 1:300,
  age = round(rnorm(300, 68, 15)),
  comorbidity_score = round(rnorm(300, 3, 2)),
  disease_stage = sample(c("Early", "Intermediate", "Advanced"), 300, 
                        replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  high_risk_clinical = rbinom(300, 1, 0.25),
  positive_biomarker = rbinom(300, 1, 0.18),
  treatment_failure = rbinom(300, 1, 0.22)  # 22% failure rate
)

# Add prediction models
treatment_decision_data$genomic_risk_score <- create_correlated_predictions(
  treatment_decision_data$treatment_failure, -1.2, 2.8, 0.4)
treatment_decision_data$clinical_prediction_model <- create_correlated_predictions(
  treatment_decision_data$treatment_failure, -1.0, 2.3, 0.5)
treatment_decision_data$combined_model <- create_correlated_predictions(
  treatment_decision_data$treatment_failure, -0.8, 3.0, 0.3)

# 3. Diagnostic Test Evaluation Dataset (Moderate prevalence, wide threshold range)
diagnostic_test_data <- data.frame(
  patient_id = 1:400,
  presenting_symptoms = sample(c("Typical", "Atypical", "Asymptomatic"), 400,
                              replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  point_of_care_positive = rbinom(400, 1, 0.15),
  imaging_abnormal = rbinom(400, 1, 0.20),
  lab_elevated = rbinom(400, 1, 0.18),
  disease_present = rbinom(400, 1, 0.16)  # 16% prevalence
)

# Add test predictions
diagnostic_test_data$rapid_test_prob <- create_correlated_predictions(
  diagnostic_test_data$disease_present, -2.0, 3.5, 0.6)
diagnostic_test_data$standard_test_prob <- create_correlated_predictions(
  diagnostic_test_data$disease_present, -1.2, 4.0, 0.4)
diagnostic_test_data$novel_biomarker_prob <- create_correlated_predictions(
  diagnostic_test_data$disease_present, -1.5, 3.8, 0.5)

# 4. External Prevalence Dataset (Higher prevalence, specialist setting)
external_prevalence_data <- data.frame(
  patient_id = 1:150,
  simple_test = rbinom(150, 1, 0.22),
  outcome = rbinom(150, 1, 0.35)  # 35% prevalence in specialist setting
)

# Add model predictions
external_prevalence_data$model_score_A <- create_correlated_predictions(
  external_prevalence_data$outcome, -0.5, 2.2, 0.5)
external_prevalence_data$model_score_B <- create_correlated_predictions(
  external_prevalence_data$outcome, -0.8, 2.8, 0.4)

# 5. Comprehensive Dataset (Multiple scenarios in one dataset)
comprehensive_dca_data <- data.frame(
  id = 1:600,
  age = round(rnorm(600, 58, 16)),
  gender = sample(c("Male", "Female"), 600, replace = TRUE),
  sensitive_test = rbinom(600, 1, 0.25),
  specific_test = rbinom(600, 1, 0.08),
  balanced_test = rbinom(600, 1, 0.15),
  biomarker_high_good = rnorm(600, 50, 15),
  biomarker_low_good = rnorm(600, 30, 10),
  outcome = rbinom(600, 1, 0.18)  # 18% prevalence
)

# Add prediction models with different performance levels
comprehensive_dca_data$model_excellent <- create_correlated_predictions(
  comprehensive_dca_data$outcome, -2.2, 4.5, 0.3)  # Excellent discrimination
comprehensive_dca_data$model_good <- create_correlated_predictions(
  comprehensive_dca_data$outcome, -1.8, 3.2, 0.5)  # Good discrimination
comprehensive_dca_data$model_poor <- create_correlated_predictions(
  comprehensive_dca_data$outcome, -1.5, 1.8, 0.7)  # Poor discrimination
comprehensive_dca_data$model_miscalibrated <- create_correlated_predictions(
  comprehensive_dca_data$outcome, 0.8, 2.5, 0.4)   # Overconfident

# Adjust continuous biomarkers based on outcome
comprehensive_dca_data$biomarker_high_good <- with(comprehensive_dca_data,
  biomarker_high_good + 15 * outcome + rnorm(600, 0, 5))
comprehensive_dca_data$biomarker_low_good <- with(comprehensive_dca_data,
  biomarker_low_good - 8 * outcome + rnorm(600, 0, 4))

# Convert one biomarker to probability scale for testing
comprehensive_dca_data$biomarker_high_prob <- with(comprehensive_dca_data,
  (biomarker_high_good - min(biomarker_high_good)) / 
  (max(biomarker_high_good) - min(biomarker_high_good)))

# Export datasets to data/ folder
use_data_multi_format(cancer_screening_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(treatment_decision_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(diagnostic_test_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(external_prevalence_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(comprehensive_dca_data, overwrite = TRUE, save_csv = TRUE)

# Create documentation for the datasets
cat("📊 Bayesian DCA Example Datasets Created Successfully!\n\n")

# Print AUC values to show realistic performance
library(pROC)

cat("Dataset Performance Metrics:\n")
cat("===========================\n\n")

# Cancer screening dataset
auc1 <- roc(cancer_screening_data$cancer_detected, cancer_screening_data$biomarker_panel_prob)$auc
auc2 <- roc(cancer_screening_data$cancer_detected, cancer_screening_data$clinical_score_prob)$auc
cat("Cancer Screening Dataset:\n")
cat(sprintf("  - Biomarker Panel AUC: %.3f\n", auc1))
cat(sprintf("  - Clinical Score AUC: %.3f\n", auc2))
cat(sprintf("  - Prevalence: %.1f%%\n", mean(cancer_screening_data$cancer_detected) * 100))
cat(sprintf("  - Suggested thresholds: 1-10%%\n\n"))

# Treatment decision dataset
auc3 <- roc(treatment_decision_data$treatment_failure, treatment_decision_data$genomic_risk_score)$auc
auc4 <- roc(treatment_decision_data$treatment_failure, treatment_decision_data$combined_model)$auc
cat("Treatment Decision Dataset:\n")
cat(sprintf("  - Genomic Score AUC: %.3f\n", auc3))
cat(sprintf("  - Combined Model AUC: %.3f\n", auc4))
cat(sprintf("  - Prevalence: %.1f%%\n", mean(treatment_decision_data$treatment_failure) * 100))
cat(sprintf("  - Suggested thresholds: 15-45%%\n\n"))

# Diagnostic test dataset
auc5 <- roc(diagnostic_test_data$disease_present, diagnostic_test_data$standard_test_prob)$auc
auc6 <- roc(diagnostic_test_data$disease_present, diagnostic_test_data$novel_biomarker_prob)$auc
cat("Diagnostic Test Dataset:\n")
cat(sprintf("  - Standard Test AUC: %.3f\n", auc5))
cat(sprintf("  - Novel Biomarker AUC: %.3f\n", auc6))
cat(sprintf("  - Prevalence: %.1f%%\n", mean(diagnostic_test_data$disease_present) * 100))
cat(sprintf("  - Suggested thresholds: 5-50%%\n\n"))

cat("🎯 All datasets are ready for Bayesian DCA analysis!\n")
cat("Use these in vignettes and examples to demonstrate various DCA features.\n")
