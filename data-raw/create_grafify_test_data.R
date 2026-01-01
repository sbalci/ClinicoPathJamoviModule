# Create comprehensive test data for grafify function
# This script generates datasets specifically designed to test all grafify features

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility  
set.seed(123)

# 1. Comprehensive Grafify Test Data
# ==================================

grafify_comprehensive_data <- data.frame(
  # Subject identifiers
  subject_id = rep(1:80, each = 2),
  
  # Treatment groups (3 levels for ANOVA)
  treatment = rep(c("Control", "Low_Dose", "High_Dose"), length.out = 160),
  
  # Timepoints for before-after analysis
  timepoint = rep(c("Baseline", "Follow_up"), 80),
  
  # Blocking variables for randomized block design
  study_site = rep(c("Site_A", "Site_B", "Site_C", "Site_D"), 40),
  batch = rep(c("Batch_1", "Batch_2", "Batch_3", "Batch_4"), 40),
  
  # Primary continuous outcome
  biomarker_concentration = c(
    # Control group - baseline and follow-up
    rnorm(54, mean = 100, sd = 15),
    # Low dose group - with treatment effect
    rnorm(53, mean = 110, sd = 18), 
    # High dose group - with larger treatment effect
    rnorm(53, mean = 125, sd = 20)
  ),
  
  # Secondary continuous outcome
  clinical_score = c(
    rnorm(54, mean = 50, sd = 8),
    rnorm(53, mean = 58, sd = 10),
    rnorm(53, mean = 68, sd = 12)
  ),
  
  # Demographic variables
  gender = rep(c("Male", "Female"), 80),
  age_group = sample(c("Young_Adult", "Middle_Age", "Elderly"), 160, 
                     replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  
  # Additional grouping variables
  response_category = sample(c("Responder", "Non_Responder"), 160, 
                            replace = TRUE, prob = c(0.6, 0.4)),
  severity_level = sample(c("Mild", "Moderate", "Severe"), 160,
                         replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  
  # Variables for multidimensional analysis
  dose_level = sample(c("0mg", "5mg", "10mg", "20mg"), 160, replace = TRUE),
  comorbidity_score = round(runif(160, min = 0, max = 10)),
  
  stringsAsFactors = TRUE
)

# Add realistic treatment effects for follow-up timepoints
follow_up_indices <- which(grafify_comprehensive_data$timepoint == "Follow_up")

# Control group: minimal change
control_indices <- intersect(follow_up_indices, 
                           which(grafify_comprehensive_data$treatment == "Control"))
grafify_comprehensive_data$biomarker_concentration[control_indices] <- 
  grafify_comprehensive_data$biomarker_concentration[control_indices] + rnorm(length(control_indices), 2, 5)

# Low dose: moderate improvement
low_dose_indices <- intersect(follow_up_indices,
                            which(grafify_comprehensive_data$treatment == "Low_Dose"))
grafify_comprehensive_data$biomarker_concentration[low_dose_indices] <- 
  grafify_comprehensive_data$biomarker_concentration[low_dose_indices] + rnorm(length(low_dose_indices), 8, 6)

# High dose: larger improvement
high_dose_indices <- intersect(follow_up_indices,
                             which(grafify_comprehensive_data$treatment == "High_Dose"))
grafify_comprehensive_data$biomarker_concentration[high_dose_indices] <- 
  grafify_comprehensive_data$biomarker_concentration[high_dose_indices] + rnorm(length(high_dose_indices), 15, 8)

# Similar effects for clinical score
grafify_comprehensive_data$clinical_score[control_indices] <- 
  grafify_comprehensive_data$clinical_score[control_indices] + rnorm(length(control_indices), 1, 3)
grafify_comprehensive_data$clinical_score[low_dose_indices] <- 
  grafify_comprehensive_data$clinical_score[low_dose_indices] + rnorm(length(low_dose_indices), 5, 4)
grafify_comprehensive_data$clinical_score[high_dose_indices] <- 
  grafify_comprehensive_data$clinical_score[high_dose_indices] + rnorm(length(high_dose_indices), 10, 5)

# 2. Simple Grafify Test Data
# ===========================

grafify_simple_data <- data.frame(
  group = rep(c("A", "B", "C"), each = 30),
  measurement = c(
    rnorm(30, mean = 10, sd = 2),
    rnorm(30, mean = 15, sd = 3),
    rnorm(30, mean = 20, sd = 4)
  ),
  category = rep(c("Type1", "Type2"), 45),
  value_secondary = c(
    rnorm(30, mean = 25, sd = 5),
    rnorm(30, mean = 35, sd = 6), 
    rnorm(30, mean = 45, sd = 7)
  ),
  stringsAsFactors = TRUE
)

# 3. Longitudinal Grafify Data
# ============================

grafify_longitudinal_data <- data.frame(
  patient_id = rep(1:40, each = 4),
  visit = rep(c("Week_0", "Week_4", "Week_8", "Week_12"), 40),
  treatment_arm = rep(c("Placebo", "Active"), each = 80),
  primary_outcome = c(
    # Placebo group - minimal change over time
    rnorm(80, mean = rep(c(100, 102, 101, 103), 20), sd = 8),
    # Active group - improvement over time  
    rnorm(80, mean = rep(c(100, 110, 118, 125), 20), sd = 10)
  ),
  quality_of_life = c(
    rnorm(80, mean = rep(c(50, 52, 51, 53), 20), sd = 6),
    rnorm(80, mean = rep(c(50, 58, 65, 72), 20), sd = 8)
  ),
  gender = rep(c("Male", "Female"), 80),
  baseline_severity = sample(c("Mild", "Moderate", "Severe"), 160, replace = TRUE),
  stringsAsFactors = TRUE
)

# 4. Correlation Analysis Data
# ============================

# Create data with known correlations for testing
n <- 200
correlation_matrix <- matrix(c(1, 0.7, 0.5,
                              0.7, 1, 0.3,
                              0.5, 0.3, 1), nrow = 3)

# Generate correlated variables
library(MASS)
corr_data <- mvrnorm(n = n, mu = c(50, 100, 25), Sigma = correlation_matrix * 100)

grafify_correlation_data <- data.frame(
  biomarker_A = corr_data[, 1],
  biomarker_B = corr_data[, 2], 
  clinical_outcome = corr_data[, 3],
  treatment_group = sample(c("Control", "Treatment_1", "Treatment_2"), n, replace = TRUE),
  study_phase = sample(c("Phase_I", "Phase_II", "Phase_III"), n, replace = TRUE),
  patient_category = sample(c("Low_Risk", "Medium_Risk", "High_Risk"), n, replace = TRUE),
  stringsAsFactors = TRUE
)

# 5. Dose-Response Data
# =====================

grafify_dose_response_data <- data.frame(
  dose_mg = rep(c(0, 1, 5, 10, 25, 50), each = 25),
  response_percent = c(
    rnorm(25, mean = 5, sd = 3),    # Placebo
    rnorm(25, mean = 15, sd = 4),   # 1mg
    rnorm(25, mean = 35, sd = 6),   # 5mg
    rnorm(25, mean = 55, sd = 8),   # 10mg
    rnorm(25, mean = 70, sd = 10),  # 25mg
    rnorm(25, mean = 80, sd = 12)   # 50mg (plateau effect)
  ),
  adverse_events = c(
    rpois(25, lambda = 0.5),        # Low AE at placebo
    rpois(25, lambda = 1),          # Increasing with dose
    rpois(25, lambda = 2),
    rpois(25, lambda = 3),
    rpois(25, lambda = 5),
    rpois(25, lambda = 7)
  ),
  formulation = rep(c("Tablet", "Capsule"), 75),
  gender = rep(c("Male", "Female"), 75),
  age_category = sample(c("18-30", "31-50", "51-70"), 150, replace = TRUE),
  stringsAsFactors = TRUE
)

# Ensure positive values for log transformation tests
grafify_dose_response_data$response_percent <- pmax(grafify_dose_response_data$response_percent, 1)

# 6. Factorial Design Data
# ========================

grafify_factorial_data <- data.frame(
  factor_A = rep(c("A1", "A2"), each = 60),
  factor_B = rep(c("B1", "B2", "B3"), 40), 
  response = c(
    # A1-B1: baseline
    rnorm(20, mean = 10, sd = 3),
    # A1-B2: moderate effect
    rnorm(20, mean = 15, sd = 4),
    # A1-B3: large effect
    rnorm(20, mean = 20, sd = 5),
    # A2-B1: different baseline
    rnorm(20, mean = 12, sd = 3),
    # A2-B2: interaction effect
    rnorm(20, mean = 25, sd = 6),
    # A2-B3: strong interaction
    rnorm(20, mean = 35, sd = 8)
  ),
  block = rep(c("Block_1", "Block_2", "Block_3", "Block_4"), 30),
  replicate = rep(1:20, 6),
  stringsAsFactors = TRUE
)

# Add documentation for each dataset
attr(grafify_comprehensive_data, "description") <- 
  "Comprehensive test dataset for grafify function with treatment groups, timepoints, and multiple variables"

attr(grafify_simple_data, "description") <- 
  "Simple test dataset for basic grafify functionality"

attr(grafify_longitudinal_data, "description") <- 
  "Longitudinal dataset for testing before-after and repeated measures analysis"

attr(grafify_correlation_data, "description") <- 
  "Dataset with known correlations for testing correlation analysis features"

attr(grafify_dose_response_data, "description") <- 
  "Dose-response dataset for testing continuous predictor relationships"

attr(grafify_factorial_data, "description") <- 
  "Factorial design dataset for testing two-way ANOVA and interaction effects"

# Save all datasets
save(grafify_comprehensive_data, file = "data/grafify_comprehensive_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(grafify_comprehensive_data, "data/grafify_comprehensive_data.omv")
  message("✓ Created grafify_comprehensive_data.omv")
}
save(grafify_simple_data, file = "data/grafify_simple_data.rda") 

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(grafify_simple_data, "data/grafify_simple_data.omv")
  message("✓ Created grafify_simple_data.omv")
}
save(grafify_longitudinal_data, file = "data/grafify_longitudinal_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(grafify_longitudinal_data, "data/grafify_longitudinal_data.omv")
  message("✓ Created grafify_longitudinal_data.omv")
}
save(grafify_correlation_data, file = "data/grafify_correlation_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(grafify_correlation_data, "data/grafify_correlation_data.omv")
  message("✓ Created grafify_correlation_data.omv")
}
save(grafify_dose_response_data, file = "data/grafify_dose_response_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(grafify_dose_response_data, "data/grafify_dose_response_data.omv")
  message("✓ Created grafify_dose_response_data.omv")
}
save(grafify_factorial_data, file = "data/grafify_factorial_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(grafify_factorial_data, "data/grafify_factorial_data.omv")
  message("✓ Created grafify_factorial_data.omv")
}

# Print summary information
cat("Created grafify test datasets:\n")
cat("- grafify_comprehensive_data:", nrow(grafify_comprehensive_data), "rows,", ncol(grafify_comprehensive_data), "columns\n")
cat("- grafify_simple_data:", nrow(grafify_simple_data), "rows,", ncol(grafify_simple_data), "columns\n") 
cat("- grafify_longitudinal_data:", nrow(grafify_longitudinal_data), "rows,", ncol(grafify_longitudinal_data), "columns\n")
cat("- grafify_correlation_data:", nrow(grafify_correlation_data), "rows,", ncol(grafify_correlation_data), "columns\n")
cat("- grafify_dose_response_data:", nrow(grafify_dose_response_data), "rows,", ncol(grafify_dose_response_data), "columns\n")
cat("- grafify_factorial_data:", nrow(grafify_factorial_data), "rows,", ncol(grafify_factorial_data), "columns\n")

cat("\nDatasets saved to data/ directory as .rda files\n")
