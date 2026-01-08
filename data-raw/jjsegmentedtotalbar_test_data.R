# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjsegmentedtotalbar
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the jjsegmentedtotalbar
# jamovi function, which creates 100% stacked bar charts (segmented total bars)
# showing proportional composition within categories.
#
# Generated: 2026-01-06
# Function: jjsegmentedtotalbar (segmented total bar charts with proportions)

library(tidyverse)
library(jmvReadWrite)

set.seed(20260106)

# ═══════════════════════════════════════════════════════════
# 1. MAIN TEST DATASET: Treatment Response Over Time
# ═══════════════════════════════════════════════════════════
# General dataset for testing treatment response proportions across timepoints

n_treatment <- 200

# Generate patient IDs
patient_ids <- paste0("PT", sprintf("%04d", 1:n_treatment))

# Treatment groups
treatment_groups <- sample(c("Chemotherapy", "Immunotherapy", "Combination"),
                           n_treatment, replace = TRUE,
                           prob = c(0.35, 0.35, 0.30))

# Timepoints
timepoints <- sample(c("Baseline", "Week 6", "Week 12", "Week 24"),
                     n_treatment, replace = TRUE,
                     prob = c(0.25, 0.25, 0.25, 0.25))

# Response categories with realistic progression patterns
# Create response based on treatment and timepoint
response_cats <- character(n_treatment)
for (i in 1:n_treatment) {
  if (timepoints[i] == "Baseline") {
    response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                "Stable Disease", "Progressive Disease"),
                              1, prob = c(0.05, 0.20, 0.50, 0.25))
  } else if (timepoints[i] == "Week 6") {
    if (treatment_groups[i] == "Immunotherapy") {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.10, 0.35, 0.40, 0.15))
    } else if (treatment_groups[i] == "Combination") {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.15, 0.40, 0.35, 0.10))
    } else {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.08, 0.30, 0.45, 0.17))
    }
  } else if (timepoints[i] == "Week 12") {
    if (treatment_groups[i] == "Immunotherapy") {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.20, 0.40, 0.30, 0.10))
    } else if (treatment_groups[i] == "Combination") {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.25, 0.45, 0.25, 0.05))
    } else {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.12, 0.38, 0.40, 0.10))
    }
  } else { # Week 24
    if (treatment_groups[i] == "Immunotherapy") {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.30, 0.40, 0.20, 0.10))
    } else if (treatment_groups[i] == "Combination") {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.35, 0.45, 0.15, 0.05))
    } else {
      response_cats[i] <- sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"),
                                1, prob = c(0.18, 0.42, 0.30, 0.10))
    }
  }
}

# Tumor response metric (continuous variable for y_var)
tumor_response_score <- case_when(
  response_cats == "Complete Response" ~ rnorm(n_treatment, mean = 95, sd = 5),
  response_cats == "Partial Response" ~ rnorm(n_treatment, mean = 70, sd = 10),
  response_cats == "Stable Disease" ~ rnorm(n_treatment, mean = 45, sd = 8),
  response_cats == "Progressive Disease" ~ rnorm(n_treatment, mean = 20, sd = 10)
)
tumor_response_score <- pmax(0, pmin(100, tumor_response_score))

# Disease stage
disease_stage <- sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                       n_treatment, replace = TRUE,
                       prob = c(0.15, 0.30, 0.35, 0.20))

# Add some missing data (~3%)
response_cats[sample(1:n_treatment, round(n_treatment * 0.03))] <- NA
tumor_response_score[sample(1:n_treatment, round(n_treatment * 0.03))] <- NA

jjsegmentedtotalbar_test <- tibble(
  patient_id = patient_ids,
  timepoint = factor(timepoints, levels = c("Baseline", "Week 6", "Week 12", "Week 24")),
  treatment = factor(treatment_groups),
  response_category = factor(response_cats,
                            levels = c("Complete Response", "Partial Response",
                                     "Stable Disease", "Progressive Disease")),
  tumor_response_score = tumor_response_score,
  disease_stage = factor(disease_stage,
                        levels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
)

# ═══════════════════════════════════════════════════════════
# 2. DEMOGRAPHICS DATASET: Patient Demographics by Treatment
# ═══════════════════════════════════════════════════════════

n_demo <- 150

demo_ids <- paste0("PAT", sprintf("%04d", 1:n_demo))

# Treatment centers
treatment_centers <- sample(c("Center A", "Center B", "Center C", "Center D"),
                           n_demo, replace = TRUE,
                           prob = c(0.30, 0.25, 0.25, 0.20))

# Age groups
age_groups <- sample(c("18-40", "41-55", "56-65", "66-75", "76+"),
                     n_demo, replace = TRUE,
                     prob = c(0.10, 0.25, 0.30, 0.25, 0.10))

# Gender distribution
gender <- sample(c("Male", "Female", "Other"),
                n_demo, replace = TRUE,
                prob = c(0.48, 0.50, 0.02))

# Ethnicity
ethnicity <- sample(c("Caucasian", "African American", "Asian", "Hispanic", "Other"),
                   n_demo, replace = TRUE,
                   prob = c(0.45, 0.25, 0.15, 0.12, 0.03))

# ECOG performance status
ecog_status <- sample(c("0", "1", "2", "3"),
                     n_demo, replace = TRUE,
                     prob = c(0.30, 0.40, 0.20, 0.10))

# Patient count (for aggregation)
patient_count <- rep(1, n_demo)

# Add some missing data (~3%)
age_groups[sample(1:n_demo, round(n_demo * 0.03))] <- NA
ethnicity[sample(1:n_demo, round(n_demo * 0.03))] <- NA

jjsegmentedtotalbar_demographics <- tibble(
  patient_id = demo_ids,
  treatment_center = factor(treatment_centers),
  age_group = factor(age_groups, levels = c("18-40", "41-55", "56-65", "66-75", "76+")),
  gender = factor(gender),
  ethnicity = factor(ethnicity),
  ecog_performance_status = factor(ecog_status, levels = c("0", "1", "2", "3")),
  patient_count = patient_count
)

# ═══════════════════════════════════════════════════════════
# 3. BIOMARKER DATASET: Biomarker Categories by Disease Stage
# ═══════════════════════════════════════════════════════════

n_biomarker <- 200

biomarker_ids <- paste0("BM", sprintf("%04d", 1:n_biomarker))

# Disease stages
bio_disease_stage <- sample(c("Early Stage", "Locally Advanced", "Metastatic"),
                           n_biomarker, replace = TRUE,
                           prob = c(0.35, 0.35, 0.30))

# Biomarker types
biomarker_types <- sample(c("PD-L1", "HER2", "EGFR", "ALK", "ROS1"),
                         n_biomarker, replace = TRUE,
                         prob = c(0.30, 0.25, 0.20, 0.15, 0.10))

# Expression levels depend on stage and marker
expression_levels <- character(n_biomarker)
for (i in 1:n_biomarker) {
  if (bio_disease_stage[i] == "Metastatic") {
    if (biomarker_types[i] %in% c("PD-L1", "HER2")) {
      expression_levels[i] <- sample(c("Negative", "Low", "Moderate", "High"),
                                    1, prob = c(0.20, 0.20, 0.30, 0.30))
    } else {
      expression_levels[i] <- sample(c("Negative", "Low", "Moderate", "High"),
                                    1, prob = c(0.40, 0.30, 0.20, 0.10))
    }
  } else if (bio_disease_stage[i] == "Locally Advanced") {
    expression_levels[i] <- sample(c("Negative", "Low", "Moderate", "High"),
                                  1, prob = c(0.30, 0.30, 0.25, 0.15))
  } else { # Early Stage
    expression_levels[i] <- sample(c("Negative", "Low", "Moderate", "High"),
                                  1, prob = c(0.40, 0.35, 0.20, 0.05))
  }
}

# Tumor type
tumor_types <- sample(c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell"),
                     n_biomarker, replace = TRUE,
                     prob = c(0.50, 0.30, 0.15, 0.05))

# Expression score (continuous)
expression_score <- case_when(
  expression_levels == "Negative" ~ rnorm(n_biomarker, mean = 5, sd = 3),
  expression_levels == "Low" ~ rnorm(n_biomarker, mean = 25, sd = 8),
  expression_levels == "Moderate" ~ rnorm(n_biomarker, mean = 55, sd = 10),
  expression_levels == "High" ~ rnorm(n_biomarker, mean = 85, sd = 10)
)
expression_score <- pmax(0, pmin(100, expression_score))

# Add some missing data (~3%)
expression_levels[sample(1:n_biomarker, round(n_biomarker * 0.03))] <- NA
expression_score[sample(1:n_biomarker, round(n_biomarker * 0.03))] <- NA

jjsegmentedtotalbar_biomarker <- tibble(
  sample_id = biomarker_ids,
  disease_stage = factor(bio_disease_stage,
                        levels = c("Early Stage", "Locally Advanced", "Metastatic")),
  biomarker = factor(biomarker_types),
  expression_level = factor(expression_levels,
                           levels = c("Negative", "Low", "Moderate", "High")),
  expression_score = expression_score,
  tumor_type = factor(tumor_types)
)

# ═══════════════════════════════════════════════════════════
# 4. QUALITY DATASET: Quality Metrics by Hospital
# ═══════════════════════════════════════════════════════════

n_quality <- 180

quality_ids <- paste0("QM", sprintf("%04d", 1:n_quality))

# Hospital sites
hospital_sites <- sample(c("Hospital A", "Hospital B", "Hospital C", "Hospital D", "Hospital E"),
                        n_quality, replace = TRUE,
                        prob = c(0.25, 0.20, 0.20, 0.20, 0.15))

# Reporting quarters
reporting_quarters <- sample(c("Q1 2025", "Q2 2025", "Q3 2025", "Q4 2025"),
                            n_quality, replace = TRUE,
                            prob = c(0.25, 0.25, 0.25, 0.25))

# Quality metric types
metric_types <- sample(c("Diagnostic Accuracy", "Treatment Adherence",
                        "Patient Safety", "Timely Reporting"),
                      n_quality, replace = TRUE,
                      prob = c(0.25, 0.25, 0.25, 0.25))

# Quality grades depend on hospital performance
quality_grades <- character(n_quality)
for (i in 1:n_quality) {
  if (hospital_sites[i] == "Hospital A") {
    quality_grades[i] <- sample(c("Excellent", "Good", "Fair", "Poor"),
                               1, prob = c(0.50, 0.35, 0.12, 0.03))
  } else if (hospital_sites[i] == "Hospital B") {
    quality_grades[i] <- sample(c("Excellent", "Good", "Fair", "Poor"),
                               1, prob = c(0.40, 0.40, 0.15, 0.05))
  } else if (hospital_sites[i] == "Hospital C") {
    quality_grades[i] <- sample(c("Excellent", "Good", "Fair", "Poor"),
                               1, prob = c(0.30, 0.45, 0.20, 0.05))
  } else if (hospital_sites[i] == "Hospital D") {
    quality_grades[i] <- sample(c("Excellent", "Good", "Fair", "Poor"),
                               1, prob = c(0.25, 0.45, 0.25, 0.05))
  } else {
    quality_grades[i] <- sample(c("Excellent", "Good", "Fair", "Poor"),
                               1, prob = c(0.20, 0.40, 0.30, 0.10))
  }
}

# Compliance score (continuous)
compliance_score <- case_when(
  quality_grades == "Excellent" ~ rnorm(n_quality, mean = 95, sd = 3),
  quality_grades == "Good" ~ rnorm(n_quality, mean = 80, sd = 5),
  quality_grades == "Fair" ~ rnorm(n_quality, mean = 65, sd = 7),
  quality_grades == "Poor" ~ rnorm(n_quality, mean = 45, sd = 10)
)
compliance_score <- pmax(0, pmin(100, compliance_score))

# Case volume
case_volume <- round(rnorm(n_quality, mean = 150, sd = 40))
case_volume <- pmax(20, case_volume)

# Add some missing data (~3%)
quality_grades[sample(1:n_quality, round(n_quality * 0.03))] <- NA
compliance_score[sample(1:n_quality, round(n_quality * 0.03))] <- NA

jjsegmentedtotalbar_quality <- tibble(
  record_id = quality_ids,
  hospital = factor(hospital_sites),
  quarter = factor(reporting_quarters,
                  levels = c("Q1 2025", "Q2 2025", "Q3 2025", "Q4 2025")),
  metric_type = factor(metric_types),
  quality_grade = factor(quality_grades,
                        levels = c("Excellent", "Good", "Fair", "Poor")),
  compliance_score = compliance_score,
  case_volume = case_volume
)

# ═══════════════════════════════════════════════════════════
# 5. TEMPORAL DATASET: Disease Progression Over Time
# ═══════════════════════════════════════════════════════════

n_temporal <- 200

temporal_ids <- paste0("TP", sprintf("%04d", 1:n_temporal))

# Time periods
time_periods <- sample(c("Month 0", "Month 3", "Month 6", "Month 9", "Month 12"),
                      n_temporal, replace = TRUE,
                      prob = c(0.20, 0.20, 0.20, 0.20, 0.20))

# Intervention groups
intervention_groups <- sample(c("Standard Care", "Enhanced Monitoring", "Experimental"),
                             n_temporal, replace = TRUE,
                             prob = c(0.40, 0.35, 0.25))

# Disease status progression over time
disease_status <- character(n_temporal)
for (i in 1:n_temporal) {
  if (time_periods[i] == "Month 0") {
    disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                 "Active Disease", "Progressive Disease"),
                               1, prob = c(0.15, 0.30, 0.40, 0.15))
  } else if (time_periods[i] == "Month 3") {
    if (intervention_groups[i] == "Experimental") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.30, 0.35, 0.25, 0.10))
    } else if (intervention_groups[i] == "Enhanced Monitoring") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.25, 0.35, 0.30, 0.10))
    } else {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.20, 0.30, 0.35, 0.15))
    }
  } else if (time_periods[i] == "Month 6") {
    if (intervention_groups[i] == "Experimental") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.40, 0.35, 0.20, 0.05))
    } else if (intervention_groups[i] == "Enhanced Monitoring") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.30, 0.35, 0.25, 0.10))
    } else {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.25, 0.30, 0.30, 0.15))
    }
  } else if (time_periods[i] == "Month 9") {
    if (intervention_groups[i] == "Experimental") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.50, 0.30, 0.15, 0.05))
    } else if (intervention_groups[i] == "Enhanced Monitoring") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.35, 0.35, 0.20, 0.10))
    } else {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.25, 0.30, 0.30, 0.15))
    }
  } else { # Month 12
    if (intervention_groups[i] == "Experimental") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.55, 0.30, 0.12, 0.03))
    } else if (intervention_groups[i] == "Enhanced Monitoring") {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.40, 0.35, 0.20, 0.05))
    } else {
      disease_status[i] <- sample(c("No Evidence of Disease", "Minimal Disease",
                                   "Active Disease", "Progressive Disease"),
                                 1, prob = c(0.30, 0.30, 0.30, 0.10))
    }
  }
}

# Disease burden score (continuous)
disease_burden <- case_when(
  disease_status == "No Evidence of Disease" ~ rnorm(n_temporal, mean = 5, sd = 3),
  disease_status == "Minimal Disease" ~ rnorm(n_temporal, mean = 25, sd = 8),
  disease_status == "Active Disease" ~ rnorm(n_temporal, mean = 60, sd = 12),
  disease_status == "Progressive Disease" ~ rnorm(n_temporal, mean = 85, sd = 10)
)
disease_burden <- pmax(0, pmin(100, disease_burden))

# Patient age
patient_age <- round(rnorm(n_temporal, mean = 62, sd = 12))
patient_age <- pmax(25, pmin(90, patient_age))

# Add some missing data (~3%)
disease_status[sample(1:n_temporal, round(n_temporal * 0.03))] <- NA
disease_burden[sample(1:n_temporal, round(n_temporal * 0.03))] <- NA

jjsegmentedtotalbar_temporal <- tibble(
  patient_id = temporal_ids,
  time_period = factor(time_periods,
                      levels = c("Month 0", "Month 3", "Month 6", "Month 9", "Month 12")),
  intervention = factor(intervention_groups),
  disease_status = factor(disease_status,
                         levels = c("No Evidence of Disease", "Minimal Disease",
                                  "Active Disease", "Progressive Disease")),
  disease_burden_score = disease_burden,
  patient_age = patient_age
)

# ═══════════════════════════════════════════════════════════
# 6. SMALL SAMPLE DATASET: Edge Cases Testing
# ═══════════════════════════════════════════════════════════

n_small <- 30

small_ids <- paste0("SM", sprintf("%03d", 1:n_small))

# Simplified categories for small sample
small_categories <- sample(c("Category A", "Category B", "Category C"),
                          n_small, replace = TRUE,
                          prob = c(0.35, 0.35, 0.30))

small_groups <- sample(c("Group 1", "Group 2"),
                      n_small, replace = TRUE,
                      prob = c(0.50, 0.50))

# Response types
small_response <- sample(c("Positive", "Negative", "Neutral"),
                        n_small, replace = TRUE,
                        prob = c(0.40, 0.35, 0.25))

# Numeric value
small_value <- rnorm(n_small, mean = 50, sd = 15)
small_value <- pmax(10, pmin(90, small_value))

# Add some missing data (~5% for small sample)
small_response[sample(1:n_small, round(n_small * 0.05))] <- NA

jjsegmentedtotalbar_small <- tibble(
  id = small_ids,
  category = factor(small_categories),
  group = factor(small_groups),
  response = factor(small_response, levels = c("Positive", "Negative", "Neutral")),
  value = small_value
)

# ═══════════════════════════════════════════════════════════
# SAVE ALL DATASETS
# ═══════════════════════════════════════════════════════════

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save as RDA files
save(jjsegmentedtotalbar_test, file = "data/jjsegmentedtotalbar_test.rda", compress = "xz")
save(jjsegmentedtotalbar_demographics, file = "data/jjsegmentedtotalbar_demographics.rda", compress = "xz")
save(jjsegmentedtotalbar_biomarker, file = "data/jjsegmentedtotalbar_biomarker.rda", compress = "xz")
save(jjsegmentedtotalbar_quality, file = "data/jjsegmentedtotalbar_quality.rda", compress = "xz")
save(jjsegmentedtotalbar_temporal, file = "data/jjsegmentedtotalbar_temporal.rda", compress = "xz")
save(jjsegmentedtotalbar_small, file = "data/jjsegmentedtotalbar_small.rda", compress = "xz")

# Save as CSV files
write_csv(jjsegmentedtotalbar_test, "data/jjsegmentedtotalbar_test.csv")
write_csv(jjsegmentedtotalbar_demographics, "data/jjsegmentedtotalbar_demographics.csv")
write_csv(jjsegmentedtotalbar_biomarker, "data/jjsegmentedtotalbar_biomarker.csv")
write_csv(jjsegmentedtotalbar_quality, "data/jjsegmentedtotalbar_quality.csv")
write_csv(jjsegmentedtotalbar_temporal, "data/jjsegmentedtotalbar_temporal.csv")
write_csv(jjsegmentedtotalbar_small, "data/jjsegmentedtotalbar_small.csv")

# Save as Excel files
writexl::write_xlsx(jjsegmentedtotalbar_test, "data/jjsegmentedtotalbar_test.xlsx")
writexl::write_xlsx(jjsegmentedtotalbar_demographics, "data/jjsegmentedtotalbar_demographics.xlsx")
writexl::write_xlsx(jjsegmentedtotalbar_biomarker, "data/jjsegmentedtotalbar_biomarker.xlsx")
writexl::write_xlsx(jjsegmentedtotalbar_quality, "data/jjsegmentedtotalbar_quality.xlsx")
writexl::write_xlsx(jjsegmentedtotalbar_temporal, "data/jjsegmentedtotalbar_temporal.xlsx")
writexl::write_xlsx(jjsegmentedtotalbar_small, "data/jjsegmentedtotalbar_small.xlsx")

# Save as jamovi (.omv) files
write_omv(jjsegmentedtotalbar_test, "data/jjsegmentedtotalbar_test.omv")
write_omv(jjsegmentedtotalbar_demographics, "data/jjsegmentedtotalbar_demographics.omv")
write_omv(jjsegmentedtotalbar_biomarker, "data/jjsegmentedtotalbar_biomarker.omv")
write_omv(jjsegmentedtotalbar_quality, "data/jjsegmentedtotalbar_quality.omv")
write_omv(jjsegmentedtotalbar_temporal, "data/jjsegmentedtotalbar_temporal.omv")
write_omv(jjsegmentedtotalbar_small, "data/jjsegmentedtotalbar_small.omv")

# Print summary
cat("\n═══════════════════════════════════════════════════════════\n")
cat("Test data generation completed successfully!\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Datasets created:\n")
cat("1. jjsegmentedtotalbar_test (n=", nrow(jjsegmentedtotalbar_test), ") - Treatment response over time\n")
cat("2. jjsegmentedtotalbar_demographics (n=", nrow(jjsegmentedtotalbar_demographics), ") - Patient demographics\n")
cat("3. jjsegmentedtotalbar_biomarker (n=", nrow(jjsegmentedtotalbar_biomarker), ") - Biomarker categories\n")
cat("4. jjsegmentedtotalbar_quality (n=", nrow(jjsegmentedtotalbar_quality), ") - Quality metrics\n")
cat("5. jjsegmentedtotalbar_temporal (n=", nrow(jjsegmentedtotalbar_temporal), ") - Disease progression\n")
cat("6. jjsegmentedtotalbar_small (n=", nrow(jjsegmentedtotalbar_small), ") - Small sample edge cases\n\n")

cat("Total observations:",
    nrow(jjsegmentedtotalbar_test) +
    nrow(jjsegmentedtotalbar_demographics) +
    nrow(jjsegmentedtotalbar_biomarker) +
    nrow(jjsegmentedtotalbar_quality) +
    nrow(jjsegmentedtotalbar_temporal) +
    nrow(jjsegmentedtotalbar_small), "\n")

cat("\nFiles created per dataset (4 formats each):\n")
cat("- .rda (R data format)\n")
cat("- .csv (comma-separated values)\n")
cat("- .xlsx (Excel format)\n")
cat("- .omv (jamovi format)\n\n")

cat("Total files created: 24 (6 datasets × 4 formats)\n")
cat("═══════════════════════════════════════════════════════════\n")
