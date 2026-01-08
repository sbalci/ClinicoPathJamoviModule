# ═══════════════════════════════════════════════════════════
# Test Data Generation: jwaffle
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the jwaffle
# jamovi function, which creates waffle charts (proportional colored squares)
# for visualizing categorical distributions.
#
# Generated: 2026-01-06
# Function: jwaffle (waffle charts for categorical proportions)

library(tidyverse)
library(jmvReadWrite)

set.seed(20260106)

# ═══════════════════════════════════════════════════════════
# 1. MAIN TEST DATASET: Treatment Response Distribution
# ═══════════════════════════════════════════════════════════
# General dataset for treatment response visualization

n_treatment <- 200

# Generate patient IDs
patient_ids <- paste0("PT", sprintf("%04d", 1:n_treatment))

# Treatment arms
treatment_arms <- sample(c("Chemotherapy", "Immunotherapy", "Targeted Therapy"),
                        n_treatment, replace = TRUE,
                        prob = c(0.35, 0.35, 0.30))

# Response categories with treatment-specific patterns
response_categories <- character(n_treatment)
for (i in 1:n_treatment) {
  if (treatment_arms[i] == "Immunotherapy") {
    response_categories[i] <- sample(
      c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
      1, prob = c(0.25, 0.35, 0.30, 0.10)
    )
  } else if (treatment_arms[i] == "Targeted Therapy") {
    response_categories[i] <- sample(
      c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
      1, prob = c(0.30, 0.40, 0.25, 0.05)
    )
  } else {
    response_categories[i] <- sample(
      c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
      1, prob = c(0.15, 0.35, 0.35, 0.15)
    )
  }
}

# Timepoints
timepoints <- sample(c("Baseline", "Week 12", "Week 24"),
                    n_treatment, replace = TRUE,
                    prob = c(0.33, 0.33, 0.34))

# Performance status
ecog_status <- sample(c("0", "1", "2", "3"),
                     n_treatment, replace = TRUE,
                     prob = c(0.30, 0.40, 0.20, 0.10))

# Count variable (all 1s for individual patient data)
patient_count <- rep(1, n_treatment)

# Add some missing data (~3%)
response_categories[sample(1:n_treatment, round(n_treatment * 0.03))] <- NA

jwaffle_test <- tibble(
  patient_id = patient_ids,
  response_category = factor(response_categories,
                             levels = c("Complete Response", "Partial Response",
                                      "Stable Disease", "Progressive Disease")),
  treatment = factor(treatment_arms),
  timepoint = factor(timepoints, levels = c("Baseline", "Week 12", "Week 24")),
  ecog_status = factor(ecog_status, levels = c("0", "1", "2", "3")),
  patient_count = patient_count
)

# ═══════════════════════════════════════════════════════════
# 2. DISEASE DATASET: Disease Subtypes and Stages
# ═══════════════════════════════════════════════════════════

n_disease <- 180

disease_ids <- paste0("DS", sprintf("%04d", 1:n_disease))

# Disease subtypes (histological types)
disease_subtypes <- sample(
  c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell", "Neuroendocrine"),
  n_disease, replace = TRUE,
  prob = c(0.40, 0.30, 0.15, 0.10, 0.05)
)

# Disease stages
disease_stages <- sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                        n_disease, replace = TRUE,
                        prob = c(0.20, 0.30, 0.30, 0.20))

# Molecular subtypes
molecular_subtypes <- sample(c("EGFR+", "ALK+", "ROS1+", "KRAS+", "Wild Type"),
                            n_disease, replace = TRUE,
                            prob = c(0.15, 0.08, 0.05, 0.25, 0.47))

# Tumor location
tumor_locations <- sample(c("Upper Lobe", "Middle Lobe", "Lower Lobe", "Multiple"),
                         n_disease, replace = TRUE,
                         prob = c(0.40, 0.15, 0.35, 0.10))

# Add some missing data (~3%)
disease_subtypes[sample(1:n_disease, round(n_disease * 0.03))] <- NA
molecular_subtypes[sample(1:n_disease, round(n_disease * 0.03))] <- NA

jwaffle_disease <- tibble(
  sample_id = disease_ids,
  disease_subtype = factor(disease_subtypes),
  disease_stage = factor(disease_stages,
                        levels = c("Stage I", "Stage II", "Stage III", "Stage IV")),
  molecular_subtype = factor(molecular_subtypes),
  tumor_location = factor(tumor_locations),
  case_count = rep(1, n_disease)
)

# ═══════════════════════════════════════════════════════════
# 3. PATHOLOGY DATASET: Grades and Classifications
# ═══════════════════════════════════════════════════════════

n_pathology <- 200

pathology_ids <- paste0("PA", sprintf("%04d", 1:n_pathology))

# Tumor grades
tumor_grades <- sample(c("Grade 1", "Grade 2", "Grade 3"),
                      n_pathology, replace = TRUE,
                      prob = c(0.25, 0.50, 0.25))

# Differentiation status
differentiation <- sample(c("Well Differentiated", "Moderately Differentiated",
                           "Poorly Differentiated", "Undifferentiated"),
                         n_pathology, replace = TRUE,
                         prob = c(0.20, 0.45, 0.30, 0.05))

# Lymphovascular invasion
lvi_status <- sample(c("Absent", "Present", "Suspicious"),
                    n_pathology, replace = TRUE,
                    prob = c(0.60, 0.35, 0.05))

# Perineural invasion
pni_status <- sample(c("Absent", "Present"),
                    n_pathology, replace = TRUE,
                    prob = c(0.70, 0.30))

# Margin status
margin_status <- sample(c("Negative", "Close (<1mm)", "Positive"),
                       n_pathology, replace = TRUE,
                       prob = c(0.75, 0.15, 0.10))

# Hospital site
hospital_sites <- sample(c("Hospital A", "Hospital B", "Hospital C"),
                        n_pathology, replace = TRUE,
                        prob = c(0.40, 0.35, 0.25))

# Add some missing data (~3%)
tumor_grades[sample(1:n_pathology, round(n_pathology * 0.03))] <- NA
differentiation[sample(1:n_pathology, round(n_pathology * 0.03))] <- NA

jwaffle_pathology <- tibble(
  specimen_id = pathology_ids,
  tumor_grade = factor(tumor_grades, levels = c("Grade 1", "Grade 2", "Grade 3")),
  differentiation = factor(differentiation,
                          levels = c("Well Differentiated", "Moderately Differentiated",
                                   "Poorly Differentiated", "Undifferentiated")),
  lvi_status = factor(lvi_status, levels = c("Absent", "Suspicious", "Present")),
  pni_status = factor(pni_status, levels = c("Absent", "Present")),
  margin_status = factor(margin_status,
                        levels = c("Negative", "Close (<1mm)", "Positive")),
  hospital = factor(hospital_sites),
  case_count = rep(1, n_pathology)
)

# ═══════════════════════════════════════════════════════════
# 4. DEMOGRAPHICS DATASET: Patient Demographics
# ═══════════════════════════════════════════════════════════

n_demo <- 150

demo_ids <- paste0("DM", sprintf("%04d", 1:n_demo))

# Age groups
age_groups <- sample(c("18-40", "41-55", "56-65", "66-75", "76+"),
                    n_demo, replace = TRUE,
                    prob = c(0.10, 0.20, 0.30, 0.30, 0.10))

# Gender
gender <- sample(c("Male", "Female"),
                n_demo, replace = TRUE,
                prob = c(0.52, 0.48))

# Ethnicity
ethnicity <- sample(c("Caucasian", "African American", "Asian", "Hispanic", "Other"),
                   n_demo, replace = TRUE,
                   prob = c(0.50, 0.20, 0.15, 0.12, 0.03))

# Smoking status
smoking_status <- sample(c("Never", "Former", "Current"),
                        n_demo, replace = TRUE,
                        prob = c(0.30, 0.45, 0.25))

# Comorbidity count
comorbidity_categories <- sample(c("None", "1-2", "3-4", "5+"),
                                n_demo, replace = TRUE,
                                prob = c(0.25, 0.40, 0.25, 0.10))

# Geographic region
regions <- sample(c("Northeast", "Southeast", "Midwest", "Southwest", "West"),
                 n_demo, replace = TRUE,
                 prob = c(0.20, 0.20, 0.20, 0.20, 0.20))

# Add some missing data (~3%)
ethnicity[sample(1:n_demo, round(n_demo * 0.03))] <- NA
smoking_status[sample(1:n_demo, round(n_demo * 0.03))] <- NA

jwaffle_demographics <- tibble(
  patient_id = demo_ids,
  age_group = factor(age_groups, levels = c("18-40", "41-55", "56-65", "66-75", "76+")),
  gender = factor(gender),
  ethnicity = factor(ethnicity),
  smoking_status = factor(smoking_status, levels = c("Never", "Former", "Current")),
  comorbidities = factor(comorbidity_categories, levels = c("None", "1-2", "3-4", "5+")),
  region = factor(regions),
  patient_count = rep(1, n_demo)
)

# ═══════════════════════════════════════════════════════════
# 5. QUALITY DATASET: Quality Metrics
# ═══════════════════════════════════════════════════════════

n_quality <- 160

quality_ids <- paste0("QA", sprintf("%04d", 1:n_quality))

# Quality grades
quality_grades <- sample(c("Excellent", "Good", "Fair", "Poor"),
                        n_quality, replace = TRUE,
                        prob = c(0.35, 0.40, 0.20, 0.05))

# Compliance levels
compliance_levels <- sample(c("Fully Compliant", "Mostly Compliant",
                             "Partially Compliant", "Non-Compliant"),
                           n_quality, replace = TRUE,
                           prob = c(0.40, 0.35, 0.20, 0.05))

# Risk categories
risk_categories <- sample(c("Low Risk", "Intermediate Risk", "High Risk"),
                         n_quality, replace = TRUE,
                         prob = c(0.50, 0.35, 0.15))

# Accreditation status
accreditation <- sample(c("Accredited", "Provisional", "Not Accredited"),
                       n_quality, replace = TRUE,
                       prob = c(0.75, 0.20, 0.05))

# Institution types
institution_types <- sample(c("Academic Center", "Community Hospital",
                             "Private Practice", "Specialty Clinic"),
                           n_quality, replace = TRUE,
                           prob = c(0.35, 0.30, 0.20, 0.15))

# Audit quarters
audit_quarters <- sample(c("Q1 2025", "Q2 2025", "Q3 2025", "Q4 2025"),
                        n_quality, replace = TRUE,
                        prob = c(0.25, 0.25, 0.25, 0.25))

# Add some missing data (~3%)
quality_grades[sample(1:n_quality, round(n_quality * 0.03))] <- NA
compliance_levels[sample(1:n_quality, round(n_quality * 0.03))] <- NA

jwaffle_quality <- tibble(
  audit_id = quality_ids,
  quality_grade = factor(quality_grades,
                        levels = c("Excellent", "Good", "Fair", "Poor")),
  compliance = factor(compliance_levels,
                     levels = c("Fully Compliant", "Mostly Compliant",
                              "Partially Compliant", "Non-Compliant")),
  risk_category = factor(risk_categories,
                        levels = c("Low Risk", "Intermediate Risk", "High Risk")),
  accreditation = factor(accreditation,
                        levels = c("Accredited", "Provisional", "Not Accredited")),
  institution_type = factor(institution_types),
  quarter = factor(audit_quarters,
                  levels = c("Q1 2025", "Q2 2025", "Q3 2025", "Q4 2025")),
  audit_count = rep(1, n_quality)
)

# ═══════════════════════════════════════════════════════════
# 6. SMALL SAMPLE DATASET: Edge Cases Testing
# ═══════════════════════════════════════════════════════════

n_small <- 30

small_ids <- paste0("SM", sprintf("%03d", 1:n_small))

# Simple categories for small sample
small_categories <- sample(c("Category A", "Category B", "Category C"),
                          n_small, replace = TRUE,
                          prob = c(0.40, 0.35, 0.25))

# Simple faceting variable
small_groups <- sample(c("Group 1", "Group 2"),
                      n_small, replace = TRUE,
                      prob = c(0.50, 0.50))

# Response types
small_response <- sample(c("Positive", "Negative", "Neutral"),
                        n_small, replace = TRUE,
                        prob = c(0.40, 0.35, 0.25))

# Add some missing data (~5% for small sample)
small_categories[sample(1:n_small, round(n_small * 0.05))] <- NA

jwaffle_small <- tibble(
  id = small_ids,
  category = factor(small_categories),
  group = factor(small_groups),
  response = factor(small_response, levels = c("Positive", "Negative", "Neutral")),
  count = rep(1, n_small)
)

# ═══════════════════════════════════════════════════════════
# SAVE ALL DATASETS
# ═══════════════════════════════════════════════════════════

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save as RDA files
save(jwaffle_test, file = "data/jwaffle_test.rda", compress = "xz")
save(jwaffle_disease, file = "data/jwaffle_disease.rda", compress = "xz")
save(jwaffle_pathology, file = "data/jwaffle_pathology.rda", compress = "xz")
save(jwaffle_demographics, file = "data/jwaffle_demographics.rda", compress = "xz")
save(jwaffle_quality, file = "data/jwaffle_quality.rda", compress = "xz")
save(jwaffle_small, file = "data/jwaffle_small.rda", compress = "xz")

# Save as CSV files
write_csv(jwaffle_test, "data/jwaffle_test.csv")
write_csv(jwaffle_disease, "data/jwaffle_disease.csv")
write_csv(jwaffle_pathology, "data/jwaffle_pathology.csv")
write_csv(jwaffle_demographics, "data/jwaffle_demographics.csv")
write_csv(jwaffle_quality, "data/jwaffle_quality.csv")
write_csv(jwaffle_small, "data/jwaffle_small.csv")

# Save as Excel files
writexl::write_xlsx(jwaffle_test, "data/jwaffle_test.xlsx")
writexl::write_xlsx(jwaffle_disease, "data/jwaffle_disease.xlsx")
writexl::write_xlsx(jwaffle_pathology, "data/jwaffle_pathology.xlsx")
writexl::write_xlsx(jwaffle_demographics, "data/jwaffle_demographics.xlsx")
writexl::write_xlsx(jwaffle_quality, "data/jwaffle_quality.xlsx")
writexl::write_xlsx(jwaffle_small, "data/jwaffle_small.xlsx")

# Save as jamovi (.omv) files
write_omv(jwaffle_test, "data/jwaffle_test.omv")
write_omv(jwaffle_disease, "data/jwaffle_disease.omv")
write_omv(jwaffle_pathology, "data/jwaffle_pathology.omv")
write_omv(jwaffle_demographics, "data/jwaffle_demographics.omv")
write_omv(jwaffle_quality, "data/jwaffle_quality.omv")
write_omv(jwaffle_small, "data/jwaffle_small.omv")

# Print summary
cat("\n═══════════════════════════════════════════════════════════\n")
cat("Test data generation completed successfully!\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Datasets created:\n")
cat("1. jwaffle_test (n=", nrow(jwaffle_test), ") - Treatment response distribution\n")
cat("2. jwaffle_disease (n=", nrow(jwaffle_disease), ") - Disease subtypes and stages\n")
cat("3. jwaffle_pathology (n=", nrow(jwaffle_pathology), ") - Pathology grades\n")
cat("4. jwaffle_demographics (n=", nrow(jwaffle_demographics), ") - Patient demographics\n")
cat("5. jwaffle_quality (n=", nrow(jwaffle_quality), ") - Quality metrics\n")
cat("6. jwaffle_small (n=", nrow(jwaffle_small), ") - Small sample edge cases\n\n")

cat("Total observations:",
    nrow(jwaffle_test) +
    nrow(jwaffle_disease) +
    nrow(jwaffle_pathology) +
    nrow(jwaffle_demographics) +
    nrow(jwaffle_quality) +
    nrow(jwaffle_small), "\n")

cat("\nFiles created per dataset (4 formats each):\n")
cat("- .rda (R data format)\n")
cat("- .csv (comma-separated values)\n")
cat("- .xlsx (Excel format)\n")
cat("- .omv (jamovi format)\n\n")

cat("Total files created: 24 (6 datasets × 4 formats)\n")
cat("═══════════════════════════════════════════════════════════\n")
