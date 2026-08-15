#!/usr/bin/env Rscript

# ==============================================================================
# Generate Swimmer Plot Test Data
# ==============================================================================
# This script generates comprehensive test datasets for the swimmerplot function.
# It creates data in multiple formats (CSV, RDA, OMV) for testing various scenarios.
#
# Usage:
#   Rscript tests/generate_swimmerplot_test_data.R
#
# Output:
#   - CSV files in data/
#   - RDA files in data/
#   - OMV files in data/ (if jmvReadWrite available)
#
# Last Updated: 2025-12-28
# ==============================================================================

library(dplyr)

# Create a simple concatenation operator for cleaner output
`%+%` <- function(x, y) paste0(x, y)

# Set seed for reproducibility
set.seed(12345)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

cat("Generating swimmer plot test datasets...\n\n")

# ==============================================================================
# 1. swimmer_unified_basic.csv
# ==============================================================================
cat("1. Creating swimmer_unified_basic...\n")

swimmer_unified_basic <- data.frame(
  PatientID = paste0("PT", formatC(1:10, width = 3, flag = "0")),
  StartTime = rep(0, 10),
  EndTime = c(12, 8, 15, 6, 9, 11, 7, 14, 10, 5),
  Response = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "CR", "PD", "PR"),
  Treatment = c("Immunotherapy", "Chemotherapy", "Combination", "Chemotherapy",
                "Immunotherapy", "Targeted", "Chemotherapy", "Immunotherapy",
                "Chemotherapy", "Targeted"),
  Priority = c("High", "Medium", "High", "Low", "High", "Medium", "Low",
               "High", "Medium", "Low"),
  stringsAsFactors = FALSE
)

write.csv(swimmer_unified_basic, "data/swimmer_unified_basic.csv", row.names = FALSE)
save(swimmer_unified_basic, file = "data/swimmer_unified_basic.rda")

# ==============================================================================
# 2. swimmer_unified_comprehensive.csv
# ==============================================================================
cat("2. Creating swimmer_unified_comprehensive...\n")

swimmer_unified_comprehensive <- data.frame(
  PatientID = paste0("PT", formatC(1:20, width = 3, flag = "0")),
  StartTime = rep(0, 20),
  EndTime = c(24, 18, 36, 8, 42, 15, 6, 30, 12, 48, 9, 27, 21, 33, 7, 39, 16, 45, 11, 51),
  BestResponse = c("CR", "PR", "CR", "PD", "PR", "SD", "PD", "CR", "SD", "PR",
                   "PD", "CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD", "CR"),
  Surgery = c(2, 1, 3, NA, 4, 2, 1, 5, NA, 6, 2, 3, 4, 1, NA, 7, 2, 8, 3, 9),
  FirstResponse = c(6, 3, 9, NA, 12, NA, NA, 8, 6, 15, NA, 9, 6, 12, NA, 12, 4, 18, NA, 15),
  Progression = c(NA, 12, NA, 6, 24, 15, 4, NA, 12, 36, 6, NA, 18, 24, 5, NA, 12, 30, 8, NA),
  DeathLastFU = c(NA, 18, NA, 8, NA, NA, 6, NA, NA, NA, 9, NA, NA, NA, 7, NA, 16, NA, 11, NA),
  TreatmentType = c("Immunotherapy", "Chemotherapy", "Combination", "Chemotherapy",
                    "Immunotherapy", "Targeted", "Chemotherapy", "Immunotherapy",
                    "Chemotherapy", "Combination", "Chemotherapy", "Immunotherapy",
                    "Targeted", "Combination", "Chemotherapy", "Immunotherapy",
                    "Chemotherapy", "Targeted", "Chemotherapy", "Combination"),
  AgeGroup = c("Young", "Middle", "Elderly", "Middle", "Young", "Elderly", "Middle",
               "Young", "Elderly", "Middle", "Young", "Elderly", "Middle", "Young",
               "Elderly", "Middle", "Young", "Elderly", "Middle", "Young"),
  ECOG = c(0, 1, 0, 2, 0, 1, 2, 0, 1, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 0),
  stringsAsFactors = FALSE
)

write.csv(swimmer_unified_comprehensive, "data/swimmer_unified_comprehensive.csv", row.names = FALSE)
save(swimmer_unified_comprehensive, file = "data/swimmer_unified_comprehensive.rda")

# ==============================================================================
# 3. swimmer_unified_datetime.csv
# ==============================================================================
cat("3. Creating swimmer_unified_datetime...\n")

# Generate dates starting from 2023-01-01 with varying start and durations
start_dates <- seq.Date(as.Date("2023-01-15"), by = "2 weeks", length.out = 15)
durations_months <- c(11, 8, 14, 6, 18, 10, 12, 20, 9, 15, 7, 16, 11, 22, 13)
best_responses <- c("CR", "PR", "CR", "PD", "SD", "PR", "CR", "PR", "SD", "CR",
                    "PD", "PR", "SD", "CR", "PR")

swimmer_unified_datetime <- data.frame(
  PatientID = paste0("PT", formatC(1:15, width = 3, flag = "0")),
  StartDate = as.character(start_dates),
  EndDate = as.character(start_dates + (durations_months * 30)),  # Approximate months
  BestResponse = best_responses,
  SurgeryDate = as.character(ifelse(
    runif(15) > 0.3,
    start_dates + sample(30:90, 15, replace = TRUE),
    NA
  )),
  ProgressionDate = as.character(ifelse(
    best_responses %in% c("PD", "SD"),
    start_dates + sample(120:300, 15, replace = TRUE),
    NA
  )),
  stringsAsFactors = FALSE
)

# Clean up NA character strings
swimmer_unified_datetime$SurgeryDate[swimmer_unified_datetime$SurgeryDate == "NA"] <- NA
swimmer_unified_datetime$ProgressionDate[swimmer_unified_datetime$ProgressionDate == "NA"] <- NA

write.csv(swimmer_unified_datetime, "data/swimmer_unified_datetime.csv", row.names = FALSE)
save(swimmer_unified_datetime, file = "data/swimmer_unified_datetime.rda")

# ==============================================================================
# 4. swimmer_unified_events.csv
# ==============================================================================
cat("4. Creating swimmer_unified_events...\n")

# Create base patient data
base_patients <- data.frame(
  PatientID = paste0("PT", formatC(1:10, width = 3, flag = "0")),
  StartTime = rep(0, 10),
  EndTime = c(12, 8, 15, 6, 9, 11, 7, 14, 10, 5),
  Response = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "CR", "PD", "PR"),
  stringsAsFactors = FALSE
)

# Generate events for each patient (2-4 events each)
events_list <- lapply(1:nrow(base_patients), function(i) {
  n_events <- sample(2:4, 1)
  patient_id <- base_patients$PatientID[i]
  end_time <- base_patients$EndTime[i]

  event_times <- sort(sample(seq(1, end_time - 1, by = 1), n_events, replace = FALSE))
  event_types <- sample(c("Toxicity", "Response Assessment", "Scan",
                          "Dose Modification", "Hospitalization"),
                        n_events, replace = TRUE)

  data.frame(
    PatientID = rep(patient_id, n_events),
    StartTime = rep(base_patients$StartTime[i], n_events),
    EndTime = rep(end_time, n_events),
    Response = rep(base_patients$Response[i], n_events),
    EventType = event_types,
    EventTime = event_times,
    stringsAsFactors = FALSE
  )
})

swimmer_unified_events <- do.call(rbind, events_list)

write.csv(swimmer_unified_events, "data/swimmer_unified_events.csv", row.names = FALSE)
save(swimmer_unified_events, file = "data/swimmer_unified_events.rda")

# ==============================================================================
# 5. swimmer_unified_oncology.csv
# ==============================================================================
cat("5. Creating swimmer_unified_oncology...\n")

n_patients <- 30

swimmer_unified_oncology <- data.frame(
  PatientID = paste0("PT", formatC(1:n_patients, width = 3, flag = "0")),
  StartTime = rep(0, n_patients),
  EndTime = sample(c(6:48), n_patients, replace = TRUE),
  BestResponse = sample(c("CR", "PR", "SD", "PD"), n_patients, replace = TRUE,
                        prob = c(0.15, 0.30, 0.30, 0.25)),
  Baseline = rep(0, n_patients),
  FirstAssessment = rep(8, n_patients),
  BestResponseTime = sample(8:24, n_patients, replace = TRUE),
  Progression = NA,
  Death = NA,
  Stage = sample(c("I", "II", "III", "IV"), n_patients, replace = TRUE,
                 prob = c(0.1, 0.2, 0.4, 0.3)),
  Arm = rep(c("Experimental", "Control"), each = n_patients / 2),
  Site = sample(c("Lung", "Breast", "Colon", "Melanoma", "Gastric", "Ovarian"),
                n_patients, replace = TRUE),
  Age = sample(45:79, n_patients, replace = TRUE),
  Gender = sample(c("Male", "Female"), n_patients, replace = TRUE),
  stringsAsFactors = FALSE
)

# Add progression for PD and some SD/PR patients
swimmer_unified_oncology$Progression <- ifelse(
  swimmer_unified_oncology$BestResponse == "PD",
  pmin(swimmer_unified_oncology$BestResponseTime + sample(2:8, n_patients, replace = TRUE),
       swimmer_unified_oncology$EndTime),
  ifelse(runif(n_patients) < 0.3,  # 30% of other responses progress
         pmin(swimmer_unified_oncology$BestResponseTime + sample(6:24, n_patients, replace = TRUE),
              swimmer_unified_oncology$EndTime),
         NA)
)

# Add death for some patients (20%)
swimmer_unified_oncology$Death <- ifelse(
  runif(n_patients) < 0.2,
  swimmer_unified_oncology$EndTime,
  NA
)

write.csv(swimmer_unified_oncology, "data/swimmer_unified_oncology.csv", row.names = FALSE)
save(swimmer_unified_oncology, file = "data/swimmer_unified_oncology.rda")

# ==============================================================================
# 6. swimmerplot_edge_cases.csv
# ==============================================================================
cat("6. Creating swimmerplot_edge_cases...\n")

swimmerplot_edge_cases <- data.frame(
  PatientID = paste0("PT", formatC(1:7, width = 3, flag = "0")),
  StartTime = c(0, 0, 5, 0, 0, NA, 0),
  EndTime = c(0, -5, 10, 1000, 5, 10, NA),
  Response = c("CR", "PR", "SD", "PD", NA, "CR", "PR"),
  stringsAsFactors = FALSE
)

write.csv(swimmerplot_edge_cases, "data/swimmerplot_edge_cases.csv", row.names = FALSE)
save(swimmerplot_edge_cases, file = "data/swimmerplot_edge_cases.rda")

# ==============================================================================
# 7. swimmerplot_single_patient.csv
# ==============================================================================
cat("7. Creating swimmerplot_single_patient...\n")

swimmerplot_single_patient <- data.frame(
  PatientID = "PT001",
  StartTime = 0,
  EndTime = 12,
  Response = "PR",
  stringsAsFactors = FALSE
)

write.csv(swimmerplot_single_patient, "data/swimmerplot_single_patient.csv", row.names = FALSE)
save(swimmerplot_single_patient, file = "data/swimmerplot_single_patient.rda")

# ==============================================================================
# 8. swimmerplot_censoring.csv
# ==============================================================================
cat("8. Creating swimmerplot_censoring...\n")

swimmerplot_censoring <- data.frame(
  PatientID = paste0("PT", formatC(1:10, width = 3, flag = "0")),
  StartTime = rep(0, 10),
  EndTime = c(12, 18, 8, 24, 6, 15, 20, 10, 14, 22),
  Response = c("CR", "PR", "SD", "PR", "PD", "CR", "PR", "SD", "CR", "PR"),
  CensorStatus = c(1, 0, 1, 0, 1, 1, 0, 1, 0, 0),  # 1=event, 0=censored
  stringsAsFactors = FALSE
)

write.csv(swimmerplot_censoring, "data/swimmerplot_censoring.csv", row.names = FALSE)
save(swimmerplot_censoring, file = "data/swimmerplot_censoring.rda")

# ==============================================================================
# 9. swimmerplot_group_comparison.csv
# ==============================================================================
cat("9. Creating swimmerplot_group_comparison...\n")

swimmerplot_group_comparison <- data.frame(
  PatientID = paste0("PT", formatC(1:20, width = 3, flag = "0")),
  StartTime = rep(0, 20),
  EndTime = sample(6:24, 20, replace = TRUE),
  Response = c(
    # Experimental arm (better responses)
    "CR", "PR", "CR", "PR", "PR", "SD", "CR", "PR", "SD", "PR",
    # Control arm (worse responses)
    "SD", "PD", "SD", "PD", "PR", "PD", "SD", "PD", "SD", "PD"
  ),
  TreatmentArm = rep(c("Experimental", "Control"), each = 10),
  ResponseStatus = c(
    # Experimental: 7 responders, 3 non-responders
    1, 1, 1, 1, 1, 0, 1, 1, 0, 1,
    # Control: 1 responder, 9 non-responders
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0
  ),
  stringsAsFactors = FALSE
)

write.csv(swimmerplot_group_comparison, "data/swimmerplot_group_comparison.csv", row.names = FALSE)
save(swimmerplot_group_comparison, file = "data/swimmerplot_group_comparison.rda")

# ==============================================================================
# 10. swimmerplot_milestones_comprehensive.csv
# ==============================================================================
cat("10. Creating swimmerplot_milestones_comprehensive...\n")

swimmerplot_milestones_comprehensive <- data.frame(
  PatientID = paste0("PT", formatC(1:15, width = 3, flag = "0")),
  StartTime = rep(0, 15),
  EndTime = c(24, 18, 30, 12, 36, 15, 9, 27, 21, 33, 16, 42, 20, 28, 14),
  Surgery = c(2, 1, 3, 2, 4, 2, 1, 3, 3, 4, 2, 5, 3, 3, 2),
  Treatment = c(4, 3, 6, 4, 8, 4, 3, 6, 5, 8, 4, 10, 6, 6, 4),
  Response = c(8, 6, 12, 8, 16, 8, 6, 12, 10, 16, 8, 20, 12, 12, 8),
  Progression = c(NA, 15, NA, NA, 28, 15, 9, NA, 18, 24, 16, NA, NA, 22, NA),
  Death = c(NA, 18, NA, NA, NA, NA, 9, NA, NA, NA, NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

write.csv(swimmerplot_milestones_comprehensive, "data/swimmerplot_milestones_comprehensive.csv", row.names = FALSE)
save(swimmerplot_milestones_comprehensive, file = "data/swimmerplot_milestones_comprehensive.rda")

# ==============================================================================
# Create OMV files (if jmvReadWrite is available)
# ==============================================================================
cat("\nCreating OMV files (jamovi format)...\n")

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  datasets <- c(
    "swimmer_unified_basic",
    "swimmer_unified_comprehensive",
    "swimmer_unified_datetime",
    "swimmer_unified_events",
    "swimmer_unified_oncology",
    "swimmerplot_edge_cases",
    "swimmerplot_single_patient",
    "swimmerplot_censoring",
    "swimmerplot_group_comparison",
    "swimmerplot_milestones_comprehensive"
  )

  for (dataset_name in datasets) {
    tryCatch({
      dataset <- get(dataset_name)
      omv_path <- paste0("data/", dataset_name, ".omv")
      jmvReadWrite::write_omv(dataset, omv_path)
      cat("  ✓ Created", omv_path, "\n")
    }, error = function(e) {
      cat("  ✗ Failed to create", dataset_name, ".omv:", e$message, "\n")
    })
  }
} else {
  cat("  Note: jmvReadWrite package not available. Skipping OMV file generation.\n")
  cat("  Install with: install.packages('jmvReadWrite')\n")
}

# ==============================================================================
# Summary
# ==============================================================================
cat("\n" %+% paste0(rep("=", 70), collapse = "") %+% "\n")
cat("Test data generation complete!\n\n")
cat("Generated datasets:\n")
cat("  1. swimmer_unified_basic (10 patients, basic timeline)\n")
cat("  2. swimmer_unified_comprehensive (20 patients, 4 milestones)\n")
cat("  3. swimmer_unified_datetime (15 patients, date/time format)\n")
cat("  4. swimmer_unified_events (10 patients, multiple events)\n")
cat("  5. swimmer_unified_oncology (30 patients, clinical trial)\n")
cat("  6. swimmerplot_edge_cases (7 edge cases)\n")
cat("  7. swimmerplot_single_patient (1 patient)\n")
cat("  8. swimmerplot_censoring (10 patients, censoring arrows)\n")
cat("  9. swimmerplot_group_comparison (20 patients, 2 groups)\n")
cat(" 10. swimmerplot_milestones_comprehensive (15 patients, 5 milestones)\n")
cat("\nFormats created:\n")
cat("  - CSV (data/*.csv)\n")
cat("  - RDA (data/*.rda)\n")
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  cat("  - OMV (data/*.omv)\n")
}
cat("\nNext steps:\n")
cat("  1. Review test data files in data/ directory\n")
cat("  2. Run automated tests: testthat::test_file('tests/testthat/test-swimmerplot.R')\n")
cat("  3. Manual testing in jamovi with .omv files\n")
cat("  4. Review SWIMMERPLOT_TEST_DATA_GUIDE.md for detailed documentation\n")
cat(paste0(rep("=", 70), collapse = "") %+% "\n")
