#!/usr/bin/env Rscript
# =============================================================================
# Comprehensive Test Data Generation for TimeInterval Function
# =============================================================================
# 
# This script generates multiple test datasets for the timeinterval function
# covering various date formats, clinical scenarios, and edge cases
#
# Author: ClinicoPath Development Team
# Date: 2024
# =============================================================================

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(12345)

# =============================================================================
# Dataset 1: Clinical Trial with Multiple Date Formats
# =============================================================================

create_clinical_trial_dates <- function(n = 200) {
  # Base study period
  study_start <- as.Date("2020-01-01")
  study_end <- as.Date("2023-12-31")
  
  # Generate enrollment dates (staggered entry)
  enrollment_dates <- sample(seq(study_start, study_start + 365, by = "day"), n, replace = TRUE)
  
  # Generate follow-up durations (realistic clinical trial)
  follow_up_days <- rpois(n, lambda = 450) + 30  # 30 days minimum follow-up
  follow_up_dates <- enrollment_dates + follow_up_days
  
  # Create different date format representations
  clinical_trial_ymd <- data.frame(
    patient_id = paste0("CT_", sprintf("%03d", 1:n)),
    treatment_group = sample(c("Treatment A", "Treatment B", "Control"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    age = round(rnorm(n, 65, 12)),
    sex = sample(c("Male", "Female"), n, replace = TRUE),
    enrollment_date_ymd = as.character(enrollment_dates),  # YYYY-MM-DD
    followup_date_ymd = as.character(follow_up_dates),     # YYYY-MM-DD
    event_occurred = rbinom(n, 1, 0.25),
    site_location = sample(c("Site A", "Site B", "Site C"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add some missing values for robustness testing
  missing_indices <- sample(1:n, size = round(n * 0.05))
  clinical_trial_ymd$followup_date_ymd[missing_indices] <- NA
  
  return(clinical_trial_ymd)
}

# =============================================================================
# Dataset 2: European-style Dates (DD/MM/YYYY)
# =============================================================================

create_european_dates <- function(n = 150) {
  # Generate realistic European clinical data
  start_dates <- seq(as.Date("2019-06-01"), as.Date("2022-12-31"), by = "day")
  diagnosis_dates <- sample(start_dates, n, replace = TRUE)
  
  # Follow-up periods (months to years)
  follow_up_months <- sample(6:36, n, replace = TRUE)
  follow_up_dates <- diagnosis_dates + (follow_up_months * 30)
  
  european_dates <- data.frame(
    study_id = paste0("EU_", sprintf("%03d", 1:n)),
    country = sample(c("Germany", "France", "Italy", "Spain", "UK"), n, replace = TRUE),
    diagnosis_date_dmy = format(diagnosis_dates, "%d/%m/%Y"),  # DD/MM/YYYY
    last_visit_dmy = format(follow_up_dates, "%d/%m/%Y"),      # DD/MM/YYYY
    disease_stage = sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
    outcome_status = sample(c("Alive", "Deceased", "Lost to Follow-up"), n, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    comorbidity_score = round(runif(n, 0, 10), 1),
    stringsAsFactors = FALSE
  )
  
  return(european_dates)
}

# =============================================================================
# Dataset 3: US-style Dates (MM/DD/YYYY) with Time Components
# =============================================================================

create_us_datetime_data <- function(n = 180) {
  # Hospital admission/discharge data
  admission_dates <- seq(as.POSIXct("2021-01-01 08:00:00"), 
                        as.POSIXct("2023-06-30 18:00:00"), 
                        by = "6 hours")
  
  admission_times <- sample(admission_dates, n, replace = TRUE)
  
  # Length of stay (hours to days)
  los_hours <- rpois(n, lambda = 72) + 2  # Minimum 2 hours
  discharge_times <- admission_times + hours(los_hours)
  
  us_datetime <- data.frame(
    record_id = paste0("US_", sprintf("%04d", 1:n)),
    hospital_unit = sample(c("ICU", "Emergency", "Surgery", "Medical", "Pediatric"), 
                          n, replace = TRUE, prob = c(0.15, 0.25, 0.2, 0.3, 0.1)),
    admission_datetime = format(admission_times, "%m/%d/%Y %H:%M:%S"),  # MM/DD/YYYY HH:MM:SS
    discharge_datetime = format(discharge_times, "%m/%d/%Y %H:%M:%S"),  # MM/DD/YYYY HH:MM:SS
    primary_diagnosis = sample(c("Cardiovascular", "Respiratory", "Infectious", "Oncology", "Trauma"), 
                              n, replace = TRUE),
    severity_score = round(rnorm(n, 5, 2)),
    readmission_30d = rbinom(n, 1, 0.12),
    insurance_type = sample(c("Private", "Medicare", "Medicaid", "Uninsured"), 
                           n, replace = TRUE, prob = c(0.45, 0.3, 0.2, 0.05)),
    stringsAsFactors = FALSE
  )
  
  # Add some data quality issues for testing
  # Negative intervals (discharge before admission)
  negative_indices <- sample(1:n, size = 5)
  us_datetime$discharge_datetime[negative_indices] <- format(
    admission_times[negative_indices] - hours(sample(1:24, 5, replace = TRUE)), 
    "%m/%d/%Y %H:%M:%S"
  )
  
  return(us_datetime)
}

# =============================================================================
# Dataset 4: Epidemiological Study with Various Edge Cases
# =============================================================================

create_epidemiological_data <- function(n = 250) {
  # Population-based cohort study
  cohort_entry <- as.Date("2018-01-01")
  study_end <- as.Date("2023-12-31")
  
  # Random entry into cohort
  entry_dates <- sample(seq(cohort_entry, cohort_entry + 730, by = "day"), n, replace = TRUE)
  
  # Various exit scenarios
  exit_types <- sample(c("event", "censored", "death", "emigration"), n, replace = TRUE, 
                      prob = c(0.15, 0.65, 0.15, 0.05))
  
  # Exit dates based on type
  exit_dates <- character(n)
  for (i in 1:n) {
    entry <- entry_dates[i]
    type <- exit_types[i]
    if (type == "event") {
      exit_date <- entry + sample(30:1095, 1)  # 1 month to 3 years
    } else if (type == "death") {
      exit_date <- entry + sample(60:1460, 1)  # 2 months to 4 years
    } else if (type == "emigration") {
      exit_date <- entry + sample(180:1095, 1) # 6 months to 3 years
    } else {  # censored
      exit_date <- study_end
    }
    exit_dates[i] <- as.character(exit_date)
  }
  
  epidemiological_data <- data.frame(
    participant_id = paste0("EPI_", sprintf("%04d", 1:n)),
    geographic_region = sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    cohort_entry_date = as.character(entry_dates),
    exit_date = exit_dates,
    exit_reason = exit_types,
    age_at_entry = round(rnorm(n, 45, 15)),
    exposure_status = sample(c("High", "Medium", "Low", "None"), n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)),
    socioeconomic_status = sample(c("High", "Medium", "Low"), n, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    baseline_health_score = round(rnorm(n, 75, 12), 1),
    stringsAsFactors = FALSE
  )
  
  # Add some extreme values and missing data for quality testing
  extreme_indices <- sample(1:n, size = 8)
  epidemiological_data$exit_date[extreme_indices[1:4]] <- as.character(
    as.Date(epidemiological_data$cohort_entry_date[extreme_indices[1:4]]) + sample(3650:7300, 4)  # 10-20 years
  )
  
  # Missing exit dates
  epidemiological_data$exit_date[extreme_indices[5:8]] <- NA
  
  return(epidemiological_data)
}

# =============================================================================
# Dataset 5: Landmark Analysis Test Data
# =============================================================================

create_landmark_analysis_data <- function(n = 120) {
  # Cancer study with landmark analysis at 6 months
  diagnosis_date <- as.Date("2020-03-01")
  
  # All patients start from the same date for landmark analysis
  diagnosis_dates <- rep(diagnosis_date, n)
  
  # Generate survival times with some before and after landmark
  landmark_months <- 6
  
  # 30% of patients have events before landmark (to be excluded)
  # 70% survive past landmark with varying additional follow-up
  survival_months <- c(
    runif(round(n * 0.3), 1, landmark_months - 0.5),      # Before landmark
    runif(round(n * 0.7), landmark_months + 0.5, 36)      # After landmark
  )
  
  # Add some right at the landmark time
  survival_months[1:5] <- landmark_months
  
  last_contact_dates <- diagnosis_dates + (survival_months * 30)
  
  landmark_data <- data.frame(
    patient_id = paste0("LM_", sprintf("%03d", 1:n)),
    cancer_type = sample(c("Breast", "Lung", "Colorectal", "Prostate", "Lymphoma"), 
                        n, replace = TRUE, prob = c(0.25, 0.2, 0.2, 0.15, 0.2)),
    diagnosis_date = as.character(diagnosis_dates),
    last_contact_date = as.character(last_contact_dates),
    vital_status = ifelse(survival_months < 24, 
                         sample(c("Deceased", "Alive"), n, replace = TRUE, prob = c(0.6, 0.4)),
                         sample(c("Deceased", "Alive"), n, replace = TRUE, prob = c(0.3, 0.7))),
    treatment_received = sample(c("Surgery Only", "Surgery + Chemo", "Surgery + Radio", "All Three"), 
                               n, replace = TRUE, prob = c(0.2, 0.35, 0.25, 0.2)),
    response_6m = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                        n, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
    landmark_eligible = survival_months >= landmark_months,
    stringsAsFactors = FALSE
  )
  
  return(landmark_data)
}

# =============================================================================
# Dataset 6: Mixed Format Challenge Data
# =============================================================================

create_mixed_format_data <- function(n = 100) {
  # Deliberately challenging dataset with mixed formats in same column
  base_dates <- seq(as.Date("2021-01-01"), as.Date("2023-06-30"), by = "day")
  start_dates_proper <- sample(base_dates, n, replace = TRUE)
  end_dates_proper <- start_dates_proper + sample(30:730, n, replace = TRUE)
  
  # Create mixed format strings (this tests the auto-detect feature)
  start_mixed <- character(n)
  end_mixed <- character(n)
  
  for (i in 1:n) {
    format_choice <- sample(1:4, 1)
    if (format_choice == 1) {
      start_mixed[i] <- format(start_dates_proper[i], "%Y-%m-%d")  # YYYY-MM-DD
      end_mixed[i] <- format(end_dates_proper[i], "%Y-%m-%d")
    } else if (format_choice == 2) {
      start_mixed[i] <- format(start_dates_proper[i], "%d/%m/%Y")  # DD/MM/YYYY  
      end_mixed[i] <- format(end_dates_proper[i], "%d/%m/%Y")
    } else if (format_choice == 3) {
      start_mixed[i] <- format(start_dates_proper[i], "%m/%d/%Y")  # MM/DD/YYYY
      end_mixed[i] <- format(end_dates_proper[i], "%m/%d/%Y")
    } else {
      start_mixed[i] <- format(start_dates_proper[i], "%Y/%d/%m")  # YYYY/DD/MM
      end_mixed[i] <- format(end_dates_proper[i], "%Y/%d/%m")
    }
  }
  
  mixed_format_data <- data.frame(
    sample_id = paste0("MX_", sprintf("%03d", 1:n)),
    data_source = sample(c("Manual Entry", "Electronic Import", "Scanner OCR", "Legacy System"), 
                        n, replace = TRUE),
    start_date_mixed = start_mixed,
    end_date_mixed = end_mixed,
    data_quality_flag = sample(c("High", "Medium", "Low"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    operator_id = sample(paste0("OP_", 1:10), n, replace = TRUE),
    verification_status = sample(c("Verified", "Pending", "Flagged"), n, replace = TRUE, prob = c(0.7, 0.25, 0.05)),
    stringsAsFactors = FALSE
  )
  
  # Add some obviously problematic entries
  mixed_format_data$start_date_mixed[1:3] <- c("Invalid", "2021-13-45", "32/15/2021")
  mixed_format_data$end_date_mixed[1:3] <- c("2021-12-31", "2022-01-15", "15/12/2021")
  
  return(mixed_format_data)
}

# =============================================================================
# Generate All Datasets
# =============================================================================

cat("Generating comprehensive timeinterval test datasets...\n")

# Generate all datasets
clinical_trial_data <- create_clinical_trial_dates(200)
european_data <- create_european_dates(150)
us_datetime_data <- create_us_datetime_data(180)
epidemiological_data <- create_epidemiological_data(250)
landmark_data <- create_landmark_analysis_data(120)
mixed_format_data <- create_mixed_format_data(100)

# =============================================================================
# Save Datasets
# =============================================================================

# Ensure data directory exists
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Save as both CSV and RDA files
datasets <- list(
  timeinterval_clinical_trial = clinical_trial_data,
  timeinterval_european_dates = european_data,
  timeinterval_us_datetime = us_datetime_data,
  timeinterval_epidemiological = epidemiological_data,
  timeinterval_landmark = landmark_data,
  timeinterval_mixed_formats = mixed_format_data
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
# Create Combined Summary Dataset
# =============================================================================

# Summary statistics for quick reference
summary_stats <- data.frame(
  Dataset = names(datasets),
  Observations = sapply(datasets, nrow),
  Description = c(
    "Clinical trial with YMD format and treatment groups",
    "European dates in DD/MM/YYYY format with disease staging",
    "US datetime data with admission/discharge times",
    "Epidemiological cohort with various exit reasons",
    "Landmark analysis cancer study data",
    "Mixed date formats for auto-detection testing"
  ),
  Key_Features = c(
    "Staggered enrollment, missing values",
    "European format, disease outcomes",
    "Datetime precision, negative intervals",
    "Population study, extreme values",
    "6-month landmark analysis",
    "Multiple formats, quality issues"
  ),
  stringsAsFactors = FALSE
)

write.csv(summary_stats, "data/timeinterval_datasets_summary.csv", row.names = FALSE)
save(summary_stats, file = "data/timeinterval_datasets_summary.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(summary_stats, "data/timeinterval_datasets_summary.omv")
  message("✓ Created timeinterval_datasets_summary.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(summary_stats, "data/timeinterval_datasets_summary.omv")
  message("✓ Created timeinterval_datasets_summary.omv")
}

# =============================================================================
# Create Test Scenarios Documentation
# =============================================================================

test_scenarios <- data.frame(
  Scenario = c(
    "Basic Time Calculation",
    "Date Format Detection", 
    "Landmark Analysis",
    "Data Quality Assessment",
    "Missing Value Handling",
    "Negative Interval Detection",
    "Extreme Value Flagging",
    "Multiple Output Units",
    "Clinical Trial Analysis",
    "Epidemiological Study"
  ),
  Dataset = c(
    "timeinterval_clinical_trial",
    "timeinterval_mixed_formats",
    "timeinterval_landmark", 
    "timeinterval_epidemiological",
    "timeinterval_clinical_trial",
    "timeinterval_us_datetime",
    "timeinterval_epidemiological",
    "timeinterval_european_dates",
    "timeinterval_clinical_trial",
    "timeinterval_epidemiological"
  ),
  Expected_Result = c(
    "Time intervals in months between enrollment and follow-up",
    "Automatic detection of predominant date format",
    "Exclusion of patients with <6 months follow-up",
    "Quality metrics showing data issues",
    "Proper handling of missing follow-up dates", 
    "Detection and flagging of discharge before admission",
    "Identification of unusually long follow-up periods",
    "Consistent results across days/weeks/months/years",
    "Treatment group comparisons with person-time",
    "Population incidence rates with confidence intervals"
  ),
  stringsAsFactors = FALSE
)

write.csv(test_scenarios, "data/timeinterval_test_scenarios.csv", row.names = FALSE)
save(test_scenarios, file = "data/timeinterval_test_scenarios.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_scenarios, "data/timeinterval_test_scenarios.omv")
  message("✓ Created timeinterval_test_scenarios.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(test_scenarios, "data/timeinterval_test_scenarios.omv")
  message("✓ Created timeinterval_test_scenarios.omv")
}

# =============================================================================
# Summary Report
# =============================================================================

cat("\n=== TimeInterval Test Data Generation Complete ===\n")
cat("Total datasets created:", length(datasets), "\n")
cat("Total observations:", sum(sapply(datasets, nrow)), "\n")
cat("\nDatasets saved in both CSV and RDA formats:\n")
for (name in names(datasets)) {
  cat("-", name, "(", nrow(datasets[[name]]), "observations )\n")
}

cat("\nAdditional files created:\n")
cat("- timeinterval_datasets_summary: Overview of all datasets\n") 
cat("- timeinterval_test_scenarios: Testing scenarios and expected results\n")

cat("\n=== Data Features Summary ===\n")
cat("✓ Multiple date formats (YMD, DMY, MDY, datetime)\n")
cat("✓ Clinical trial data with treatment groups\n") 
cat("✓ European-style dates for international compatibility\n")
cat("✓ US datetime data with time precision\n")
cat("✓ Epidemiological cohort with various outcomes\n")
cat("✓ Landmark analysis test cases\n")
cat("✓ Mixed format challenge data\n")
cat("✓ Data quality issues (missing, negative, extreme values)\n")
cat("✓ Real-world scenarios for comprehensive testing\n")

cat("\nReady for timeinterval function testing and validation!\n")
