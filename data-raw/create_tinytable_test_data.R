#!/usr/bin/env Rscript
# =============================================================================
# Comprehensive Test Data Generation for TinyTable Function
# =============================================================================
# 
# This script generates multiple test datasets for the tinytable function
# covering various data types, table formats, and clinical research scenarios
#
# Author: ClinicoPath Development Team
# Date: 2024
# =============================================================================

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(98765)

# =============================================================================
# Dataset 1: Clinical Research Demographics Table
# =============================================================================

create_clinical_demographics_data <- function(n = 250) {
  # Simulate a clinical research study demographics table
  
  # Generate basic demographics
  age <- round(rnorm(n, 65, 12))
  age <- pmax(18, pmin(90, age))  # Realistic age range
  
  sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))
  
  # Generate grouping variables
  treatment_group <- sample(c("Control", "Treatment A", "Treatment B"), 
                           n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  
  study_site <- sample(paste0("Site_", LETTERS[1:6]), n, replace = TRUE)
  
  # Generate clinical variables with realistic distributions
  bmi <- round(rnorm(n, 26.5, 4.2), 1)
  bmi <- pmax(16, pmin(45, bmi))
  
  systolic_bp <- round(rnorm(n, 135, 20))
  systolic_bp <- pmax(90, pmin(200, systolic_bp))
  
  diastolic_bp <- round(rnorm(n, 85, 12))
  diastolic_bp <- pmax(60, pmin(120, diastolic_bp))
  
  # Generate categorical clinical variables
  diabetes <- sample(c("No", "Type 1", "Type 2"), n, replace = TRUE, 
                    prob = c(0.7, 0.05, 0.25))
  
  smoking_status <- sample(c("Never", "Former", "Current"), n, replace = TRUE,
                          prob = c(0.5, 0.35, 0.15))
  
  education_level <- sample(c("Less than HS", "High School", "Some College", 
                             "Bachelor's", "Graduate"), n, replace = TRUE,
                            prob = c(0.15, 0.25, 0.25, 0.2, 0.15))
  
  # Lab values with realistic ranges
  hemoglobin <- round(rnorm(n, ifelse(sex == "Male", 14.5, 12.8), 1.8), 1)
  hemoglobin <- pmax(8, pmin(18, hemoglobin))
  
  glucose <- round(rnorm(n, 105, 25))
  glucose <- pmax(70, pmin(400, glucose))
  
  cholesterol <- round(rnorm(n, 200, 40))
  cholesterol <- pmax(120, pmin(350, cholesterol))
  
  # Add some missing values to test handling
  missing_indices_bmi <- sample(1:n, size = round(n * 0.03))
  bmi[missing_indices_bmi] <- NA
  
  missing_indices_chol <- sample(1:n, size = round(n * 0.05))
  cholesterol[missing_indices_chol] <- NA
  
  # Create final dataset
  clinical_data <- data.frame(
    patient_id = paste0("PT_", sprintf("%03d", 1:n)),
    age = age,
    sex = as.factor(sex),
    treatment_group = as.factor(treatment_group),
    study_site = as.factor(study_site),
    bmi = bmi,
    systolic_bp = systolic_bp,
    diastolic_bp = diastolic_bp,
    diabetes = as.factor(diabetes),
    smoking_status = as.factor(smoking_status),
    education_level = as.factor(education_level),
    hemoglobin = hemoglobin,
    glucose = glucose,
    cholesterol = cholesterol,
    stringsAsFactors = FALSE
  )
  
  return(clinical_data)
}

# =============================================================================
# Dataset 2: Laboratory Results Table
# =============================================================================

create_laboratory_results_data <- function(n = 180) {
  # Simulate laboratory results data for table formatting
  
  # Subject identifiers
  subject_id <- paste0("LAB_", sprintf("%04d", 1:n))
  
  # Visit information
  visit <- sample(c("Baseline", "Week 4", "Week 8", "Week 12"), n, replace = TRUE)
  visit_date <- as.Date("2024-01-01") + sample(0:365, n, replace = TRUE)
  
  # Laboratory panels
  # Complete Blood Count (CBC)
  wbc <- round(rnorm(n, 7.5, 2.2), 1)  # White blood cells
  wbc <- pmax(2.0, pmin(15.0, wbc))
  
  rbc <- round(rnorm(n, 4.8, 0.6), 2)  # Red blood cells
  rbc <- pmax(3.5, pmin(6.0, rbc))
  
  hematocrit <- round(rnorm(n, 42, 6), 1)
  hematocrit <- pmax(30, pmin(55, hematocrit))
  
  platelets <- round(rnorm(n, 275, 75))
  platelets <- pmax(100, pmin(500, platelets))
  
  # Chemistry panel
  sodium <- round(rnorm(n, 140, 3))
  sodium <- pmax(130, pmin(150, sodium))
  
  potassium <- round(rnorm(n, 4.2, 0.4), 1)
  potassium <- pmax(3.0, pmin(5.5, potassium))
  
  creatinine <- round(rnorm(n, 1.1, 0.3), 2)
  creatinine <- pmax(0.5, pmin(3.0, creatinine))
  
  bun <- round(rnorm(n, 18, 6))
  bun <- pmax(7, pmin(50, bun))
  
  # Liver function tests
  alt <- round(rnorm(n, 25, 12))
  alt <- pmax(5, pmin(100, alt))
  
  ast <- round(rnorm(n, 30, 15))
  ast <- pmax(5, pmin(120, ast))
  
  # Clinical categories
  lab_category <- sample(c("Normal", "Borderline", "Abnormal"), n, replace = TRUE,
                        prob = c(0.6, 0.25, 0.15))
  
  urgency <- sample(c("Routine", "STAT", "Priority"), n, replace = TRUE,
                   prob = c(0.7, 0.1, 0.2))
  
  # Add missing values for realistic scenarios
  missing_indices <- sample(1:n, size = round(n * 0.02))
  alt[missing_indices] <- NA
  
  laboratory_data <- data.frame(
    subject_id = subject_id,
    visit = as.factor(visit),
    visit_date = visit_date,
    lab_category = as.factor(lab_category),
    urgency = as.factor(urgency),
    wbc = wbc,
    rbc = rbc,
    hematocrit = hematocrit,
    platelets = platelets,
    sodium = sodium,
    potassium = potassium,
    creatinine = creatinine,
    bun = bun,
    alt = alt,
    ast = ast,
    stringsAsFactors = FALSE
  )
  
  return(laboratory_data)
}

# =============================================================================
# Dataset 3: Multi-Modal Data Summary Table
# =============================================================================

create_multimodal_summary_data <- function(n = 200) {
  # Mixed data types for comprehensive table testing
  
  # Identifiers and grouping
  record_id <- 1:n
  data_source <- sample(c("EHR", "Registry", "Clinical_Trial", "Survey"), 
                       n, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.2))
  
  region <- sample(c("North", "South", "East", "West", "Central"), 
                  n, replace = TRUE)
  
  # Continuous variables with different scales
  score_1 <- round(rnorm(n, 75, 15), 1)  # 0-100 scale
  score_1 <- pmax(0, pmin(100, score_1))
  
  score_2 <- round(rnorm(n, 3.2, 0.8), 2)  # 1-5 scale
  score_2 <- pmax(1, pmin(5, score_2))
  
  measurement_a <- round(rnorm(n, 150, 35), 1)  # Arbitrary units
  measurement_a <- pmax(50, pmin(300, measurement_a))
  
  # Ordinal variables
  severity <- sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE,
                    prob = c(0.5, 0.35, 0.15))
  severity <- factor(severity, levels = c("Mild", "Moderate", "Severe"), ordered = TRUE)
  
  priority <- sample(c("Low", "Medium", "High", "Critical"), n, replace = TRUE,
                    prob = c(0.3, 0.4, 0.2, 0.1))
  priority <- factor(priority, levels = c("Low", "Medium", "High", "Critical"), ordered = TRUE)
  
  # Binary variables
  flag_positive <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7))
  quality_check <- sample(c("Pass", "Fail"), n, replace = TRUE, prob = c(0.85, 0.15))
  
  # Categorical variables with many levels
  category_type <- sample(paste0("Type_", LETTERS[1:8]), n, replace = TRUE)
  
  # Date variables
  start_date <- as.Date("2023-01-01") + sample(0:730, n, replace = TRUE)
  end_date <- start_date + sample(1:365, n, replace = TRUE)
  
  # Add complex missing patterns
  missing_complex <- sample(1:n, size = round(n * 0.08))
  score_2[missing_complex] <- NA
  
  missing_systematic <- sample(1:n, size = round(n * 0.04))
  measurement_a[missing_systematic] <- NA
  
  multimodal_data <- data.frame(
    record_id = record_id,
    data_source = as.factor(data_source),
    region = as.factor(region),
    score_1 = score_1,
    score_2 = score_2,
    measurement_a = measurement_a,
    severity = severity,
    priority = priority,
    flag_positive = as.factor(flag_positive),
    quality_check = as.factor(quality_check),
    category_type = as.factor(category_type),
    start_date = start_date,
    end_date = end_date,
    stringsAsFactors = FALSE
  )
  
  return(multimodal_data)
}

# =============================================================================
# Dataset 4: Time Series Summary Data
# =============================================================================

create_timeseries_summary_data <- function(n = 150) {
  # Time-based data for longitudinal table formatting
  
  # Create time series structure
  subjects <- 30
  timepoints <- 5
  
  # Expand to create longitudinal data
  subject_id <- rep(paste0("TS_", sprintf("%02d", 1:subjects)), each = timepoints)
  timepoint <- rep(paste0("T", 1:timepoints), times = subjects)
  
  # Time variables
  months_from_baseline <- rep(c(0, 3, 6, 12, 24), times = subjects)
  assessment_date <- as.Date("2023-01-01") + months_from_baseline * 30
  
  # Generate correlated measurements over time
  baseline_score <- rep(rnorm(subjects, 50, 10), each = timepoints)
  time_effect <- rep(c(0, -2, -4, -3, -1), times = subjects)  # Improvement pattern
  random_variation <- rnorm(n, 0, 3)
  
  primary_outcome <- round(baseline_score + time_effect + random_variation, 1)
  primary_outcome <- pmax(0, pmin(100, primary_outcome))
  
  # Secondary outcomes
  secondary_outcome_1 <- round(primary_outcome * 0.8 + rnorm(n, 5, 8), 1)
  secondary_outcome_2 <- round(rnorm(n, 25, 8), 1)
  
  # Categorical outcomes
  response_status <- ifelse(primary_outcome >= 60, "Responder", "Non-responder")
  
  # Compliance and adherence
  compliance_pct <- round(runif(n, 75, 100), 1)
  
  # Treatment modifications
  dose_adjustment <- sample(c("None", "Increase", "Decrease", "Hold"), n, 
                           replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
  
  # Missing data patterns (more missing at later timepoints)
  missing_prob <- rep(c(0.02, 0.05, 0.08, 0.12, 0.20), times = subjects)
  missing_indices <- which(runif(n) < missing_prob)
  primary_outcome[missing_indices] <- NA
  
  timeseries_data <- data.frame(
    subject_id = subject_id,
    timepoint = as.factor(timepoint),
    months_from_baseline = months_from_baseline,
    assessment_date = assessment_date,
    primary_outcome = primary_outcome,
    secondary_outcome_1 = secondary_outcome_1,
    secondary_outcome_2 = secondary_outcome_2,
    response_status = as.factor(response_status),
    compliance_pct = compliance_pct,
    dose_adjustment = as.factor(dose_adjustment),
    stringsAsFactors = FALSE
  )
  
  return(timeseries_data)
}

# =============================================================================
# Dataset 5: Edge Cases and Quality Testing Data
# =============================================================================

create_edge_cases_data <- function(n = 100) {
  # Dataset with various edge cases for robust testing
  
  # Identifiers
  id <- paste0("EDGE_", sprintf("%03d", 1:n))
  
  # Create different challenging scenarios
  scenarios <- sample(1:6, n, replace = TRUE)
  
  # Initialize variables
  numeric_var_1 <- numeric(n)
  numeric_var_2 <- numeric(n)
  categorical_var_1 <- character(n)
  categorical_var_2 <- character(n)
  
  for (i in 1:n) {
    scenario <- scenarios[i]
    
    if (scenario == 1) {
      # Normal case
      numeric_var_1[i] <- rnorm(1, 50, 10)
      numeric_var_2[i] <- rnorm(1, 100, 20)
      categorical_var_1[i] <- sample(c("A", "B", "C"), 1)
      categorical_var_2[i] <- sample(c("Type1", "Type2"), 1)
      
    } else if (scenario == 2) {
      # Extreme values
      numeric_var_1[i] <- runif(1, 1000, 10000)  # Very large
      numeric_var_2[i] <- runif(1, -1000, -100)  # Negative
      categorical_var_1[i] <- "EXTREME"
      categorical_var_2[i] <- "OUTLIER"
      
    } else if (scenario == 3) {
      # Very small values
      numeric_var_1[i] <- runif(1, 0.0001, 0.001)
      numeric_var_2[i] <- runif(1, 0.01, 0.1)
      categorical_var_1[i] <- sample(c("A", "B", "C"), 1)
      categorical_var_2[i] <- sample(c("Type1", "Type2"), 1)
      
    } else if (scenario == 4) {
      # Many decimal places
      numeric_var_1[i] <- runif(1, 1, 10)
      numeric_var_2[i] <- pi * runif(1, 1, 100)  # Irrational numbers
      categorical_var_1[i] <- sample(c("A", "B", "C"), 1)
      categorical_var_2[i] <- sample(c("Type1", "Type2"), 1)
      
    } else if (scenario == 5) {
      # Zero and infinity edge cases
      numeric_var_1[i] <- 0
      numeric_var_2[i] <- sample(c(-0, 0, 0.0), 1)
      categorical_var_1[i] <- sample(c("A", "B", "C"), 1)
      categorical_var_2[i] <- sample(c("Type1", "Type2"), 1)
      
    } else if (scenario == 6) {
      # Will be set to missing
      numeric_var_1[i] <- NA
      numeric_var_2[i] <- NA
      categorical_var_1[i] <- NA
      categorical_var_2[i] <- NA
    }
  }
  
  # Additional variables with specific challenges
  special_characters <- sample(c("Normal", "Special@#$", "Unicode-é", "Spaces Here", 
                                "VeryLongCategoryNameThatExceedsTypicalLimits"), 
                              n, replace = TRUE)
  
  # Variables with all the same value (edge case)
  constant_numeric <- rep(42, n)
  constant_factor <- rep("SAME", n)
  
  # Variable with many categories
  many_categories <- sample(paste0("Cat_", sprintf("%02d", 1:25)), n, replace = TRUE)
  
  # Add systematic missing patterns
  systematic_missing_1 <- seq(5, n, by = 5)  # Every 5th observation
  numeric_var_1[systematic_missing_1] <- NA
  
  systematic_missing_2 <- seq(7, n, by = 7)  # Every 7th observation
  categorical_var_1[systematic_missing_2] <- NA
  
  edge_cases_data <- data.frame(
    id = id,
    scenario = as.factor(scenarios),
    numeric_var_1 = numeric_var_1,
    numeric_var_2 = numeric_var_2,
    categorical_var_1 = as.factor(categorical_var_1),
    categorical_var_2 = as.factor(categorical_var_2),
    special_characters = as.factor(special_characters),
    constant_numeric = constant_numeric,
    constant_factor = as.factor(constant_factor),
    many_categories = as.factor(many_categories),
    stringsAsFactors = FALSE
  )
  
  return(edge_cases_data)
}

# =============================================================================
# Dataset 6: Small Sample Size Data
# =============================================================================

create_small_sample_data <- function(n = 15) {
  # Very small dataset to test edge cases with minimal data
  
  small_data <- data.frame(
    id = 1:n,
    group = as.factor(sample(c("A", "B"), n, replace = TRUE)),
    value_1 = round(rnorm(n, 10, 3), 2),
    value_2 = round(runif(n, 0, 100), 1),
    category = as.factor(sample(c("X", "Y", "Z"), n, replace = TRUE)),
    flag = as.factor(sample(c("Yes", "No"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Add one missing value
  small_data$value_1[sample(1:n, 1)] <- NA
  
  return(small_data)
}

# =============================================================================
# Generate All Datasets
# =============================================================================

cat("Generating comprehensive tinytable test datasets...\n")

# Generate all datasets
tinytable_clinical_demographics <- create_clinical_demographics_data(250)
tinytable_laboratory_results <- create_laboratory_results_data(180)
tinytable_multimodal_summary <- create_multimodal_summary_data(200)
tinytable_timeseries_summary <- create_timeseries_summary_data(150)
tinytable_edge_cases <- create_edge_cases_data(100)
tinytable_small_sample <- create_small_sample_data(15)

# =============================================================================
# Save Datasets
# =============================================================================

# Ensure data directory exists
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Save as both CSV and RDA files
datasets <- list(
  tinytable_clinical_demographics = tinytable_clinical_demographics,
  tinytable_laboratory_results = tinytable_laboratory_results,
  tinytable_multimodal_summary = tinytable_multimodal_summary,
  tinytable_timeseries_summary = tinytable_timeseries_summary,
  tinytable_edge_cases = tinytable_edge_cases,
  tinytable_small_sample = tinytable_small_sample
)

# Save CSV files
for (name in names(datasets)) {
  write.csv(datasets[[name]], 
           file = paste0("data/", name, ".csv"), 
           row.names = FALSE)
  cat("Saved:", paste0("data/", name, ".csv"), "\n")
}

# Save RDA files  
for (name in names(datasets)) {
  data_obj <- datasets[[name]]
  assign(name, data_obj)
  save(list = name, file = paste0("data/", name, ".rda"))
  cat("Saved:", paste0("data/", name, ".rda"), "\n")
}

# =============================================================================
# Create Dataset Summary
# =============================================================================

summary_stats <- data.frame(
  Dataset = names(datasets),
  Observations = sapply(datasets, nrow),
  Variables = sapply(datasets, ncol),
  Description = c(
    "Clinical research demographics with treatment groups and lab values",
    "Laboratory results panel with multiple visits and test categories",
    "Multi-modal data with mixed variable types and complex missing patterns",
    "Longitudinal time series data with repeated measures per subject",
    "Edge cases with extreme values, special characters, and systematic missing data",
    "Small sample dataset for testing minimal data scenarios"
  ),
  Key_Features = c(
    "Demographics, clinical variables, treatment grouping, realistic missing data",
    "Lab panels, visit structure, categorical urgency, date variables",
    "Mixed data types, ordinal variables, complex missing patterns, multiple scales",
    "Longitudinal structure, time-dependent outcomes, dropout patterns",
    "Extreme values, special characters, systematic missing, edge cases",
    "Minimal sample size, basic variables, single missing value"
  ),
  Primary_Use_Case = c(
    "Testing grouped tables and demographic summaries",
    "Testing descriptive statistics and visit-based grouping",
    "Testing mixed variable type handling and formatting",
    "Testing longitudinal data presentation and time series tables",
    "Testing robustness and error handling capabilities",
    "Testing minimal data edge cases and small sample behavior"
  ),
  Recommended_Table_Types = c(
    "Grouped summary, descriptive statistics",
    "Descriptive statistics, raw data display",
    "Data summary, custom format",
    "Grouped summary by timepoint",
    "All types for robustness testing",
    "Summary and raw display"
  ),
  stringsAsFactors = FALSE
)

write.csv(summary_stats, "data/tinytable_datasets_summary.csv", row.names = FALSE)
save(summary_stats, file = "data/tinytable_datasets_summary.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(summary_stats, "data/tinytable_datasets_summary.omv")
  message("✓ Created tinytable_datasets_summary.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(summary_stats, "data/tinytable_datasets_summary.omv")
  message("✓ Created tinytable_datasets_summary.omv")
}

# =============================================================================
# Create Test Scenarios Documentation
# =============================================================================

test_scenarios <- data.frame(
  Scenario = c(
    "Basic Data Summary",
    "Clinical Demographics Table",
    "Laboratory Results Overview", 
    "Grouped Analysis by Treatment",
    "Descriptive Statistics Generation",
    "Mixed Data Type Handling",
    "Longitudinal Data Presentation",
    "Theme and Styling Options",
    "Missing Data Handling",
    "Edge Case Robustness",
    "Small Sample Size Testing",
    "Publication-Ready Formatting",
    "Multi-Format Output Testing",
    "Error Handling Validation"
  ),
  Dataset = c(
    "tinytable_clinical_demographics",
    "tinytable_clinical_demographics",
    "tinytable_laboratory_results",
    "tinytable_clinical_demographics", 
    "tinytable_multimodal_summary",
    "tinytable_multimodal_summary",
    "tinytable_timeseries_summary",
    "tinytable_clinical_demographics",
    "tinytable_edge_cases",
    "tinytable_edge_cases",
    "tinytable_small_sample",
    "tinytable_laboratory_results",
    "tinytable_clinical_demographics",
    "tinytable_edge_cases"
  ),
  Table_Type = c(
    "summary",
    "descriptive",
    "descriptive",
    "grouped",
    "descriptive",
    "summary",
    "grouped",
    "descriptive",
    "summary",
    "raw",
    "summary",
    "descriptive",
    "custom",
    "summary"
  ),
  Variables = c(
    "age, sex, bmi",
    "age, sex, treatment_group, bmi, systolic_bp",
    "wbc, rbc, sodium, potassium, alt",
    "age, bmi, hemoglobin (grouped by treatment_group)",
    "score_1, score_2, measurement_a",
    "All variables with mixed types",
    "primary_outcome, secondary_outcome_1 (grouped by timepoint)",
    "age, sex, bmi (with different themes)",
    "numeric_var_1, categorical_var_1 (with missing values)",
    "All variables including extreme values",
    "value_1, value_2, category",
    "wbc, rbc, platelets, sodium (publication theme)",
    "age, sex, treatment_group (multiple output formats)",
    "Variables with all missing or extreme values"
  ),
  Expected_Result = c(
    "Clean summary table with variable types and basic statistics",
    "Professional demographics table with treatment group comparisons",
    "Laboratory values with descriptive statistics and normal ranges",
    "Treatment group comparison with means and standard deviations",
    "Comprehensive descriptive statistics for mixed variable types",
    "Robust handling of different data types in single table",
    "Time-based presentation of longitudinal outcomes",
    "Visual demonstration of different styling themes",
    "Appropriate handling and reporting of missing values",
    "Robust performance with extreme values and edge cases",
    "Successful table generation despite minimal sample size",
    "Publication-quality table suitable for journal submission",
    "Consistent formatting across HTML, PDF, Word outputs",
    "Graceful error handling with informative messages"
  ),
  stringsAsFactors = FALSE
)

write.csv(test_scenarios, "data/tinytable_test_scenarios.csv", row.names = FALSE)
save(test_scenarios, file = "data/tinytable_test_scenarios.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_scenarios, "data/tinytable_test_scenarios.omv")
  message("✓ Created tinytable_test_scenarios.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_scenarios, "data/tinytable_test_scenarios.omv")
  message("✓ Created tinytable_test_scenarios.omv")
}

# =============================================================================
# Summary Report
# =============================================================================

cat("\n=== TinyTable Test Data Generation Complete ===\n")
cat("Total datasets created:", length(datasets), "\n")
cat("Total observations:", sum(sapply(datasets, nrow)), "\n")
cat("Total variables:", sum(sapply(datasets, ncol)), "\n")
cat("\nDatasets saved in both CSV and RDA formats:\n")
for (name in names(datasets)) {
  n_obs <- nrow(datasets[[name]])
  n_vars <- ncol(datasets[[name]])
  cat(sprintf("- %s (%d obs, %d vars)\n", name, n_obs, n_vars))
}

cat("\nAdditional files created:\n")
cat("- tinytable_datasets_summary: Overview of all datasets\n") 
cat("- tinytable_test_scenarios: Testing scenarios and expected results\n")

cat("\n=== Data Features Summary ===\n")
cat("✓ Clinical research demographics and lab data\n")
cat("✓ Mixed data types and complex variable structures\n") 
cat("✓ Longitudinal and time series data\n")
cat("✓ Edge cases and robustness testing scenarios\n")
cat("✓ Small sample size edge cases\n")
cat("✓ Systematic and random missing data patterns\n")
cat("✓ Multiple grouping variables for table organization\n")
cat("✓ Realistic clinical research data patterns\n")
cat("✓ Publication-ready table testing scenarios\n")

cat("\nReady for tinytable function testing and validation!\n")
