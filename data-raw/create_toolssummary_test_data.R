#!/usr/bin/env Rscript

# =============================================================================
# Comprehensive Test Data Generation for Enhanced toolssummary Function
# =============================================================================

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

cat("Creating comprehensive test datasets for enhanced toolssummary function...\n")

# Load required libraries
suppressMessages({
  library(dplyr)
  library(lubridate)
})

set.seed(42)  # For reproducible results

# =============================================================================
# Dataset 1: Clinical Research Demographics with Missing Data Patterns
# =============================================================================

cat("Creating clinical research demographics dataset...\n")

n1 <- 300
toolssummary_clinical_demographics <- data.frame(
  patient_id = sprintf("PT_%04d", 1:n1),
  age = sample(18:85, n1, replace = TRUE),
  sex = factor(sample(c("Male", "Female"), n1, replace = TRUE, prob = c(0.52, 0.48))),
  treatment_group = factor(sample(c("Control", "Treatment A", "Treatment B"), n1, 
                                 replace = TRUE, prob = c(0.33, 0.34, 0.33))),
  study_site = factor(sample(paste0("Site_", LETTERS[1:6]), n1, replace = TRUE)),
  bmi = round(rnorm(n1, 26, 4.5), 1),
  systolic_bp = sample(90:200, n1, replace = TRUE),
  diastolic_bp = sample(60:120, n1, replace = TRUE),
  diabetes = factor(sample(c("No", "Type 1", "Type 2"), n1, 
                          replace = TRUE, prob = c(0.7, 0.1, 0.2))),
  smoking_status = factor(sample(c("Never", "Former", "Current"), n1, 
                                replace = TRUE, prob = c(0.5, 0.3, 0.2))),
  education = factor(sample(c("Less than HS", "High School", "Some College", 
                             "Bachelor's", "Graduate"), n1, replace = TRUE),
                    levels = c("Less than HS", "High School", "Some College", 
                              "Bachelor's", "Graduate"), ordered = TRUE),
  hemoglobin = round(rnorm(n1, 13.5, 2), 1),
  glucose = sample(70:400, n1, replace = TRUE),
  cholesterol = sample(120:350, n1, replace = TRUE),
  enrollment_date = sample(seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day"), 
                          n1, replace = TRUE),
  followup_months = sample(1:24, n1, replace = TRUE),
  stringsAsFactors = FALSE
)

# Introduce realistic missing data patterns
toolssummary_clinical_demographics$bmi[sample(1:n1, floor(n1 * 0.05))] <- NA
toolssummary_clinical_demographics$cholesterol[sample(1:n1, floor(n1 * 0.08))] <- NA
toolssummary_clinical_demographics$hemoglobin[sample(1:n1, floor(n1 * 0.03))] <- NA

# Ensure factor levels are properly set
toolssummary_clinical_demographics$sex <- as.factor(toolssummary_clinical_demographics$sex)
toolssummary_clinical_demographics$treatment_group <- as.factor(toolssummary_clinical_demographics$treatment_group)
toolssummary_clinical_demographics$study_site <- as.factor(toolssummary_clinical_demographics$study_site)
toolssummary_clinical_demographics$diabetes <- as.factor(toolssummary_clinical_demographics$diabetes)
toolssummary_clinical_demographics$smoking_status <- as.factor(toolssummary_clinical_demographics$smoking_status)

cat("✅ Clinical demographics:", nrow(toolssummary_clinical_demographics), "observations,", 
    ncol(toolssummary_clinical_demographics), "variables\n")

# =============================================================================
# Dataset 2: Laboratory Results with Longitudinal Structure
# =============================================================================

cat("Creating laboratory results dataset...\n")

n2 <- 200
subjects_lab <- rep(sprintf("LAB_%04d", 1:50), each = 4)
toolssummary_laboratory_results <- data.frame(
  subject_id = subjects_lab,
  visit = factor(rep(c("Baseline", "Week 4", "Week 8", "Week 12"), 50),
                levels = c("Baseline", "Week 4", "Week 8", "Week 12")),
  visit_date = rep(seq(as.Date("2024-01-01"), by = "weeks", length.out = 4), 50) +
    rep(sample(0:6, 50, replace = TRUE), each = 4),
  lab_category = factor(sample(c("Normal", "Borderline", "Abnormal"), n2, 
                              replace = TRUE, prob = c(0.6, 0.25, 0.15))),
  urgency = factor(sample(c("Routine", "STAT", "Priority"), n2, 
                         replace = TRUE, prob = c(0.7, 0.1, 0.2))),
  wbc = round(runif(n2, 2.0, 15.0), 1),
  rbc = round(runif(n2, 3.5, 6.0), 2),
  hematocrit = round(runif(n2, 30, 55), 1),
  platelets = sample(100:500, n2, replace = TRUE),
  sodium = sample(130:150, n2, replace = TRUE),
  potassium = round(runif(n2, 3.0, 5.5), 1),
  creatinine = round(runif(n2, 0.5, 3.0), 2),
  bun = sample(7:50, n2, replace = TRUE),
  alt = sample(5:100, n2, replace = TRUE),
  ast = sample(5:120, n2, replace = TRUE),
  stringsAsFactors = FALSE
)

# Add missing values
toolssummary_laboratory_results$alt[sample(1:n2, floor(n2 * 0.02))] <- NA
toolssummary_laboratory_results$creatinine[sample(1:n2, floor(n2 * 0.04))] <- NA

cat("✅ Laboratory results:", nrow(toolssummary_laboratory_results), "observations,", 
    ncol(toolssummary_laboratory_results), "variables\n")

# =============================================================================
# Dataset 3: Mixed Data Types with Complex Categorical Variables
# =============================================================================

cat("Creating mixed data types dataset...\n")

n3 <- 250
toolssummary_mixed_datatypes <- data.frame(
  record_id = 1:n3,
  data_source = factor(sample(c("EHR", "Registry", "Clinical_Trial", "Survey"), n3, 
                             replace = TRUE, prob = c(0.4, 0.25, 0.25, 0.1))),
  region = factor(sample(c("North", "South", "East", "West", "Central"), n3, replace = TRUE)),
  score_continuous = round(runif(n3, 0, 100), 1),
  score_ordinal = sample(1:5, n3, replace = TRUE),
  measurement_value = round(rnorm(n3, 150, 50), 2),
  severity = factor(sample(c("Mild", "Moderate", "Severe"), n3, replace = TRUE),
                   levels = c("Mild", "Moderate", "Severe"), ordered = TRUE),
  priority_level = factor(sample(c("Low", "Medium", "High", "Critical"), n3, replace = TRUE),
                         levels = c("Low", "Medium", "High", "Critical"), ordered = TRUE),
  binary_flag = factor(sample(c("Yes", "No"), n3, replace = TRUE)),
  quality_status = factor(sample(c("Pass", "Fail", "Pending"), n3, 
                                replace = TRUE, prob = c(0.7, 0.2, 0.1))),
  category_multi = factor(sample(paste0("Type_", LETTERS[1:8]), n3, replace = TRUE)),
  assessment_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), 
                          n3, replace = TRUE),
  completion_date = sample(seq(as.Date("2024-02-01"), as.Date("2025-01-31"), by = "day"), 
                          n3, replace = TRUE),
  count_variable = sample(0:20, n3, replace = TRUE),
  percentage_score = round(runif(n3, 0, 100), 1),
  stringsAsFactors = FALSE
)

# Add complex missing patterns
toolssummary_mixed_datatypes$score_ordinal[sample(1:n3, floor(n3 * 0.08))] <- NA
toolssummary_mixed_datatypes$measurement_value[sample(1:n3, floor(n3 * 0.05))] <- NA
toolssummary_mixed_datatypes$completion_date[sample(1:n3, floor(n3 * 0.12))] <- NA

cat("✅ Mixed data types:", nrow(toolssummary_mixed_datatypes), "observations,", 
    ncol(toolssummary_mixed_datatypes), "variables\n")

# =============================================================================
# Dataset 4: Time Series Data for Longitudinal Analysis
# =============================================================================

cat("Creating time series analysis dataset...\n")

n4_subjects <- 40
n4_timepoints <- 5
n4 <- n4_subjects * n4_timepoints

subjects_ts <- rep(sprintf("TS_%03d", 1:n4_subjects), each = n4_timepoints)
toolssummary_timeseries_data <- data.frame(
  subject_id = subjects_ts,
  timepoint = factor(rep(paste0("T", 1:n4_timepoints), n4_subjects),
                    levels = paste0("T", 1:n4_timepoints)),
  months_from_baseline = rep(c(0, 3, 6, 12, 24), n4_subjects),
  assessment_date = rep(seq(as.Date("2023-01-01"), by = "months", length.out = n4_timepoints), 
                       n4_subjects) + rep(sample(0:30, n4_subjects, replace = TRUE), each = n4_timepoints),
  primary_outcome = round(rnorm(n4, 50, 15), 1),
  secondary_outcome_1 = round(rnorm(n4, 25, 8), 1),
  secondary_outcome_2 = round(rnorm(n4, 75, 12), 1),
  response_status = factor(sample(c("Responder", "Non-responder"), n4, 
                                 replace = TRUE, prob = c(0.6, 0.4))),
  compliance_percent = round(runif(n4, 75, 100), 1),
  dose_level = factor(sample(c("Low", "Medium", "High"), n4, replace = TRUE),
                     levels = c("Low", "Medium", "High"), ordered = TRUE),
  adverse_events = sample(0:5, n4, replace = TRUE),
  biomarker_level = round(rlnorm(n4, 2, 0.5), 2),
  stringsAsFactors = FALSE
)

# Add time-dependent missing data (more missing at later timepoints)
missing_probs <- c(0.02, 0.05, 0.08, 0.12, 0.18)  # Increasing missingness over time
for (i in 1:n4_timepoints) {
  timepoint_indices <- which(toolssummary_timeseries_data$timepoint == paste0("T", i))
  n_missing <- floor(length(timepoint_indices) * missing_probs[i])
  if (n_missing > 0) {
    missing_indices <- sample(timepoint_indices, n_missing)
    toolssummary_timeseries_data$primary_outcome[missing_indices] <- NA
    toolssummary_timeseries_data$secondary_outcome_1[missing_indices] <- NA
  }
}

cat("✅ Time series data:", nrow(toolssummary_timeseries_data), "observations,", 
    ncol(toolssummary_timeseries_data), "variables\n")

# =============================================================================
# Dataset 5: Edge Cases and Robustness Testing
# =============================================================================

cat("Creating edge cases dataset...\n")

n5 <- 150
toolssummary_edge_cases <- data.frame(
  id = sprintf("EDGE_%03d", 1:n5),
  scenario = factor(sample(1:6, n5, replace = TRUE)),
  numeric_extreme = c(
    rep(0, 25),  # Zero values
    sample(c(-999, -1000, -10000), 25, replace = TRUE),  # Large negative
    sample(c(999, 1000, 10000), 25, replace = TRUE),    # Large positive
    round(runif(25, 0.0001, 0.001), 6),  # Very small positive
    round(rnorm(25, 0, 0.1), 6),  # Near zero with noise
    sample(c(Inf, -Inf), 25, replace = TRUE)  # Infinite values
  ),
  numeric_decimal = round(runif(n5, 0, 1), 8),
  categorical_many = factor(sample(paste0("Cat_", sprintf("%02d", 1:30)), n5, replace = TRUE)),
  categorical_few = factor(sample(c("A", "B"), n5, replace = TRUE)),
  categorical_special = factor(sample(c("Normal", "N/A", "Missing", "Unknown", ""), n5, replace = TRUE)),
  constant_numeric = rep(42, n5),
  constant_factor = factor(rep("SAME", n5)),
  text_variable = sample(c("Short", "Medium length text", 
                          "This is a very long text string that might cause formatting issues in tables",
                          "Text with special characters: @#$%^&*()"), n5, replace = TRUE),
  date_variable = sample(seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"), 
                        n5, replace = TRUE),
  binary_numeric = sample(0:1, n5, replace = TRUE),
  stringsAsFactors = FALSE
)

# Add systematic missing patterns
toolssummary_edge_cases$numeric_extreme[seq(6, n5, 6)] <- NA  # Every 6th observation
toolssummary_edge_cases$categorical_many[seq(7, n5, 7)] <- NA  # Every 7th observation
toolssummary_edge_cases$text_variable[toolssummary_edge_cases$scenario == 6] <- NA  # All scenario 6

# Replace infinite values with NA for some variables
toolssummary_edge_cases$numeric_extreme[is.infinite(toolssummary_edge_cases$numeric_extreme)] <- NA

cat("✅ Edge cases:", nrow(toolssummary_edge_cases), "observations,", 
    ncol(toolssummary_edge_cases), "variables\n")

# =============================================================================
# Dataset 6: Small Sample Size Testing
# =============================================================================

cat("Creating small sample dataset...\n")

n6 <- 20
toolssummary_small_sample <- data.frame(
  id = 1:n6,
  group = factor(sample(c("A", "B", "C"), n6, replace = TRUE)),
  value_numeric = round(rnorm(n6, 50, 10), 1),
  value_integer = sample(1:100, n6, replace = TRUE),
  category_binary = factor(sample(c("Yes", "No"), n6, replace = TRUE)),
  category_small = factor(sample(c("X", "Y", "Z"), n6, replace = TRUE)),
  score = round(runif(n6, 0, 10), 1),
  stringsAsFactors = FALSE
)

# Add single missing value
toolssummary_small_sample$value_numeric[sample(1:n6, 1)] <- NA

cat("✅ Small sample:", nrow(toolssummary_small_sample), "observations,", 
    ncol(toolssummary_small_sample), "variables\n")

# =============================================================================
# Save All Datasets
# =============================================================================

cat("\nSaving datasets...\n")

# Save as .rda files
save(toolssummary_clinical_demographics, file = "data/toolssummary_clinical_demographics.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_clinical_demographics, "data/toolssummary_clinical_demographics.omv")
  message("✓ Created toolssummary_clinical_demographics.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_clinical_demographics, "data/toolssummary_clinical_demographics.omv")
  message("✓ Created toolssummary_clinical_demographics.omv")
}
save(toolssummary_laboratory_results, file = "data/toolssummary_laboratory_results.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_laboratory_results, "data/toolssummary_laboratory_results.omv")
  message("✓ Created toolssummary_laboratory_results.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_laboratory_results, "data/toolssummary_laboratory_results.omv")
  message("✓ Created toolssummary_laboratory_results.omv")
}
save(toolssummary_mixed_datatypes, file = "data/toolssummary_mixed_datatypes.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_mixed_datatypes, "data/toolssummary_mixed_datatypes.omv")
  message("✓ Created toolssummary_mixed_datatypes.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_mixed_datatypes, "data/toolssummary_mixed_datatypes.omv")
  message("✓ Created toolssummary_mixed_datatypes.omv")
}
save(toolssummary_timeseries_data, file = "data/toolssummary_timeseries_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_timeseries_data, "data/toolssummary_timeseries_data.omv")
  message("✓ Created toolssummary_timeseries_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_timeseries_data, "data/toolssummary_timeseries_data.omv")
  message("✓ Created toolssummary_timeseries_data.omv")
}
save(toolssummary_edge_cases, file = "data/toolssummary_edge_cases.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_edge_cases, "data/toolssummary_edge_cases.omv")
  message("✓ Created toolssummary_edge_cases.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_edge_cases, "data/toolssummary_edge_cases.omv")
  message("✓ Created toolssummary_edge_cases.omv")
}
save(toolssummary_small_sample, file = "data/toolssummary_small_sample.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_small_sample, "data/toolssummary_small_sample.omv")
  message("✓ Created toolssummary_small_sample.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(toolssummary_small_sample, "data/toolssummary_small_sample.omv")
  message("✓ Created toolssummary_small_sample.omv")
}

# Save as .csv files for external use
write.csv(toolssummary_clinical_demographics, "data/toolssummary_clinical_demographics.csv", row.names = FALSE)
write.csv(toolssummary_laboratory_results, "data/toolssummary_laboratory_results.csv", row.names = FALSE)
write.csv(toolssummary_mixed_datatypes, "data/toolssummary_mixed_datatypes.csv", row.names = FALSE)
write.csv(toolssummary_timeseries_data, "data/toolssummary_timeseries_data.csv", row.names = FALSE)
write.csv(toolssummary_edge_cases, "data/toolssummary_edge_cases.csv", row.names = FALSE)
write.csv(toolssummary_small_sample, "data/toolssummary_small_sample.csv", row.names = FALSE)

# =============================================================================
# Create Summary Statistics
# =============================================================================

cat("Creating dataset summary information...\n")

summary_stats <- data.frame(
  Dataset = c("toolssummary_clinical_demographics", "toolssummary_laboratory_results", 
              "toolssummary_mixed_datatypes", "toolssummary_timeseries_data",
              "toolssummary_edge_cases", "toolssummary_small_sample"),
  Observations = c(nrow(toolssummary_clinical_demographics), nrow(toolssummary_laboratory_results),
                  nrow(toolssummary_mixed_datatypes), nrow(toolssummary_timeseries_data),
                  nrow(toolssummary_edge_cases), nrow(toolssummary_small_sample)),
  Variables = c(ncol(toolssummary_clinical_demographics), ncol(toolssummary_laboratory_results),
               ncol(toolssummary_mixed_datatypes), ncol(toolssummary_timeseries_data),
               ncol(toolssummary_edge_cases), ncol(toolssummary_small_sample)),
  Description = c("Clinical research demographics with treatment groups",
                 "Laboratory results with longitudinal structure",
                 "Mixed data types with complex categorical variables",
                 "Time series data for longitudinal analysis",
                 "Edge cases and robustness testing scenarios",
                 "Small sample size for minimal data testing"),
  Key_Features = c("Treatment groups, missing data patterns, clinical variables",
                  "Longitudinal visits, lab values, clinical categories",
                  "Mixed types, ordinal factors, complex categories",
                  "Time series, dropout patterns, repeated measures",
                  "Extreme values, special characters, edge cases",
                  "Minimal data, basic structure, single missing value"),
  Primary_Use_Case = c("Grouped summaries by treatment",
                      "Longitudinal analysis, lab value summaries",
                      "Mixed data type handling, categorical analysis",
                      "Time-based analysis, dropout assessment",
                      "Robustness testing, error handling",
                      "Minimal sample behavior, edge case validation"),
  Recommended_summarytools_Features = c("dfSummary, descr, freq, ctable",
                                       "dfSummary, descr, freq by visit",
                                       "dfSummary, freq for categories, descr for numeric",
                                       "dfSummary, descr by timepoint, freq",
                                       "All functions for robustness testing",
                                       "Basic freq, descr for minimal data"),
  stringsAsFactors = FALSE
)

save(summary_stats, file = "data/toolssummary_datasets_summary.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(summary_stats, "data/toolssummary_datasets_summary.omv")
  message("✓ Created toolssummary_datasets_summary.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(summary_stats, "data/toolssummary_datasets_summary.omv")
  message("✓ Created toolssummary_datasets_summary.omv")
}
write.csv(summary_stats, "data/toolssummary_datasets_summary.csv", row.names = FALSE)

# =============================================================================
# Create Test Scenarios Documentation
# =============================================================================

test_scenarios <- data.frame(
  Scenario = c("Basic Data Summary", "Clinical Demographics Table", "Laboratory Results Analysis",
               "Grouped Analysis by Treatment", "Mixed Data Type Handling", "Longitudinal Data Analysis",
               "Cross-tabulation Analysis", "summarytools dfSummary Validation", "Enhanced Frequency Tables",
               "Descriptive Statistics Comparison", "Missing Data Assessment", "Edge Case Robustness",
               "Small Sample Behavior", "Multi-format Output Testing"),
  Dataset = c("toolssummary_clinical_demographics", "toolssummary_clinical_demographics", 
              "toolssummary_laboratory_results", "toolssummary_clinical_demographics",
              "toolssummary_mixed_datatypes", "toolssummary_timeseries_data", 
              "toolssummary_mixed_datatypes", "toolssummary_clinical_demographics",
              "toolssummary_laboratory_results", "toolssummary_mixed_datatypes",
              "toolssummary_edge_cases", "toolssummary_edge_cases", 
              "toolssummary_small_sample", "toolssummary_clinical_demographics"),
  summarytools_Feature = c("Basic summaries only", "dfSummary + freq", "dfSummary + descr",
                          "freq + ctable by treatment_group", "All features", "descr + freq by timepoint",
                          "ctable for categorical vars", "dfSummary comprehensive", "freq vs standard freq",
                          "descr vs standard stats", "Missing pattern analysis", "Error handling validation",
                          "Minimal data behavior", "HTML output consistency"),
  Variables = c("age, sex, bmi", "age, sex, treatment_group, bmi, diabetes", 
                "wbc, rbc, sodium, potassium", "age, bmi, diabetes grouped by treatment_group",
                "score_continuous, severity, region", "primary_outcome, timepoint, response_status",
                "region by data_source", "All numeric and categorical", 
                "lab_category, urgency, visit", "score_continuous, measurement_value",
                "All variables with missing", "numeric_extreme, categorical_special",
                "value_numeric, category_binary", "Complete dataset analysis"),
  Expected_Result = c("Standard summary tables", "Publication-ready demographics", 
                     "Lab value summaries with plots", "Stratified analysis by groups",
                     "Proper handling of all data types", "Time-based trend analysis",
                     "Cross-tabulation with chi-square", "Comprehensive data overview",
                     "Enhanced vs standard frequency tables", "Extended descriptive statistics",
                     "Missing data pattern identification", "Graceful error handling",
                     "Functional with minimal data", "Consistent formatting across outputs"),
  stringsAsFactors = FALSE
)

save(test_scenarios, file = "data/toolssummary_test_scenarios.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_scenarios, "data/toolssummary_test_scenarios.omv")
  message("✓ Created toolssummary_test_scenarios.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_scenarios, "data/toolssummary_test_scenarios.omv")
  message("✓ Created toolssummary_test_scenarios.omv")
}
write.csv(test_scenarios, "data/toolssummary_test_scenarios.csv", row.names = FALSE)

# =============================================================================
# Final Summary
# =============================================================================

total_obs <- sum(summary_stats$Observations)
total_vars <- sum(summary_stats$Variables)

cat("\n=== TOOLSSUMMARY TEST DATA GENERATION COMPLETE ===\n")
cat("✅ Total datasets created: 6\n")
cat("✅ Total observations:", total_obs, "\n")
cat("✅ Total variables:", total_vars, "\n")
cat("✅ summarytools features covered: dfSummary, freq, descr, ctable\n")
cat("✅ Missing data patterns: Systematic and realistic\n")
cat("✅ Data types covered: Numeric, factor, ordered factor, character, date\n")
cat("✅ Edge cases included: Extreme values, special characters, constants\n")
cat("✅ Test scenarios documented: 14 comprehensive scenarios\n")

cat("\nDatasets Created:\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf("- %s (%d obs, %d vars): %s\n", 
              summary_stats$Dataset[i], 
              summary_stats$Observations[i], 
              summary_stats$Variables[i], 
              summary_stats$Description[i]))
}

cat("\nAll datasets include both .rda and .csv formats\n")
cat("Enhanced toolssummary function ready for comprehensive testing with summarytools integration!\n")
