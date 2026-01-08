# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjpiestats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjpiestats jamovi function
# jjpiestats creates pie charts with statistical analysis for categorical data
#
# Generated: 2026-01-06
# Function: Pie Charts with Statistical Testing
# Package: ClinicoPath

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)  # For reproducibility

# ═══════════════════════════════════════════════════════════
# Dataset 1: jjpiestats_test - Comprehensive clinical data
# ═══════════════════════════════════════════════════════════

n <- 200

jjpiestats_test <- tibble(
  patient_id = 1:n,

  # Treatment response (main categorical variable)
  treatment_response = sample(
    c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
    n, replace = TRUE,
    prob = c(0.25, 0.35, 0.25, 0.15)
  ),

  # Treatment arm
  treatment_arm = sample(
    c("Control", "Treatment A", "Treatment B"),
    n, replace = TRUE,
    prob = c(0.3, 0.35, 0.35)
  ),

  # Disease severity
  disease_severity = sample(
    c("Mild", "Moderate", "Severe"),
    n, replace = TRUE,
    prob = c(0.3, 0.5, 0.2)
  ),

  # Gender
  gender = sample(
    c("Male", "Female"),
    n, replace = TRUE,
    prob = c(0.55, 0.45)
  ),

  # Hospital site (for split/grouped analysis)
  hospital_site = sample(
    c("Site A", "Site B", "Site C"),
    n, replace = TRUE
  ),

  # Age group
  age_group = sample(
    c("18-40", "41-60", "61-80", "80+"),
    n, replace = TRUE,
    prob = c(0.15, 0.35, 0.40, 0.10)
  ),

  # Tumor stage
  tumor_stage = sample(
    c("Stage I", "Stage II", "Stage III", "Stage IV"),
    n, replace = TRUE,
    prob = c(0.20, 0.30, 0.30, 0.20)
  )
)

# Add realistic correlations
# Better treatment outcomes in earlier stages
jjpiestats_test <- jjpiestats_test %>%
  mutate(
    treatment_response = case_when(
      tumor_stage == "Stage I" & runif(n()) < 0.4 ~ "Complete Response",
      tumor_stage == "Stage IV" & runif(n()) < 0.3 ~ "Progressive Disease",
      TRUE ~ treatment_response
    )
  )

# Convert to factors
jjpiestats_test <- jjpiestats_test %>%
  mutate(across(where(is.character), as.factor))

# Save in all formats
save(jjpiestats_test, file = here("data", "jjpiestats_test.rda"))
write.csv(jjpiestats_test, file = here("data", "jjpiestats_test.csv"), row.names = FALSE)
write_xlsx(jjpiestats_test, path = here("data", "jjpiestats_test.xlsx"))
write_omv(jjpiestats_test, here("data", "jjpiestats_test.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 2: jjpiestats_diagnostic - 2×2 Diagnostic test data
# ═══════════════════════════════════════════════════════════

n <- 250

jjpiestats_diagnostic <- tibble(
  patient_id = 1:n,

  # Gold standard diagnosis
  disease_status = sample(
    c("Disease Present", "Disease Absent"),
    n, replace = TRUE,
    prob = c(0.35, 0.65)
  ),

  # Test result (with realistic sensitivity and specificity)
  test_result = NA_character_
)

# Add realistic test results based on disease status
# Sensitivity ~85%, Specificity ~90%
for (i in 1:n) {
  if (jjpiestats_diagnostic$disease_status[i] == "Disease Present") {
    # True positives and false negatives
    jjpiestats_diagnostic$test_result[i] <- sample(
      c("Positive", "Negative"),
      1, prob = c(0.85, 0.15)  # 85% sensitivity
    )
  } else {
    # True negatives and false positives
    jjpiestats_diagnostic$test_result[i] <- sample(
      c("Positive", "Negative"),
      1, prob = c(0.10, 0.90)  # 90% specificity
    )
  }
}

# Additional variables
jjpiestats_diagnostic <- jjpiestats_diagnostic %>%
  mutate(
    age_category = sample(c("< 50 years", "≥ 50 years"), n, replace = TRUE),
    clinical_site = sample(c("Center 1", "Center 2", "Center 3"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))
  ) %>%
  mutate(across(where(is.character), as.factor))

# Save in all formats
save(jjpiestats_diagnostic, file = here("data", "jjpiestats_diagnostic.rda"))
write.csv(jjpiestats_diagnostic, file = here("data", "jjpiestats_diagnostic.csv"), row.names = FALSE)
write_xlsx(jjpiestats_diagnostic, path = here("data", "jjpiestats_diagnostic.xlsx"))
write_omv(jjpiestats_diagnostic, here("data", "jjpiestats_diagnostic.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 3: jjpiestats_treatment - Treatment comparison data
# ═══════════════════════════════════════════════════════════

n <- 180

jjpiestats_treatment <- tibble(
  patient_id = 1:n,

  # Treatment groups
  treatment = rep(
    c("Placebo", "Low Dose", "Medium Dose", "High Dose"),
    each = n/4
  ),

  # Response outcome
  outcome = NA_character_
)

# Add dose-response relationship
for (i in 1:n) {
  outcome_prob <- case_when(
    jjpiestats_treatment$treatment[i] == "Placebo" ~ c(0.10, 0.15, 0.25, 0.50),
    jjpiestats_treatment$treatment[i] == "Low Dose" ~ c(0.20, 0.25, 0.30, 0.25),
    jjpiestats_treatment$treatment[i] == "Medium Dose" ~ c(0.35, 0.30, 0.20, 0.15),
    jjpiestats_treatment$treatment[i] == "High Dose" ~ c(0.45, 0.30, 0.15, 0.10)
  )

  jjpiestats_treatment$outcome[i] <- sample(
    c("Excellent", "Good", "Fair", "Poor"),
    1, prob = outcome_prob
  )
}

# Additional variables
jjpiestats_treatment <- jjpiestats_treatment %>%
  mutate(
    baseline_severity = sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    study_site = sample(c("Site A", "Site B"), n, replace = TRUE),
    adverse_events = sample(c("None", "Mild", "Moderate", "Severe"), n, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05))
  ) %>%
  mutate(across(where(is.character), as.factor))

# Save in all formats
save(jjpiestats_treatment, file = here("data", "jjpiestats_treatment.rda"))
write.csv(jjpiestats_treatment, file = here("data", "jjpiestats_treatment.csv"), row.names = FALSE)
write_xlsx(jjpiestats_treatment, path = here("data", "jjpiestats_treatment.xlsx"))
write_omv(jjpiestats_treatment, here("data", "jjpiestats_treatment.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 4: jjpiestats_biomarker - Biomarker distribution
# ═══════════════════════════════════════════════════════════

n <- 150

jjpiestats_biomarker <- tibble(
  sample_id = 1:n,

  # Biomarker expression level
  expression_level = sample(
    c("Negative", "Low", "Moderate", "High"),
    n, replace = TRUE,
    prob = c(0.25, 0.30, 0.25, 0.20)
  ),

  # Cancer type
  cancer_type = sample(
    c("Type A", "Type B", "Type C"),
    n, replace = TRUE
  ),

  # Receptor status
  receptor_status = sample(
    c("Positive", "Negative"),
    n, replace = TRUE,
    prob = c(0.60, 0.40)
  ),

  # Histological grade
  grade = sample(
    c("Grade 1", "Grade 2", "Grade 3"),
    n, replace = TRUE,
    prob = c(0.30, 0.50, 0.20)
  ),

  # Laboratory
  lab = sample(c("Lab 1", "Lab 2", "Lab 3"), n, replace = TRUE)
) %>%
  mutate(across(where(is.character), as.factor))

# Save in all formats
save(jjpiestats_biomarker, file = here("data", "jjpiestats_biomarker.rda"))
write.csv(jjpiestats_biomarker, file = here("data", "jjpiestats_biomarker.csv"), row.names = FALSE)
write_xlsx(jjpiestats_biomarker, path = here("data", "jjpiestats_biomarker.xlsx"))
write_omv(jjpiestats_biomarker, here("data", "jjpiestats_biomarker.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 5: jjpiestats_aggregated - Pre-aggregated count data
# ═══════════════════════════════════════════════════════════

jjpiestats_aggregated <- tibble(
  category = c("Category A", "Category B", "Category C", "Category D", "Category E"),
  counts = c(45, 78, 32, 95, 50),
  group = rep(c("Group 1", "Group 2"), length.out = 5)
) %>%
  mutate(across(where(is.character), as.factor))

# Save in all formats
save(jjpiestats_aggregated, file = here("data", "jjpiestats_aggregated.rda"))
write.csv(jjpiestats_aggregated, file = here("data", "jjpiestats_aggregated.csv"), row.names = FALSE)
write_xlsx(jjpiestats_aggregated, path = here("data", "jjpiestats_aggregated.xlsx"))
write_omv(jjpiestats_aggregated, here("data", "jjpiestats_aggregated.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 6: jjpiestats_small - Small sample for edge cases
# ═══════════════════════════════════════════════════════════

n <- 20

jjpiestats_small <- tibble(
  id = 1:n,
  category = sample(c("Group A", "Group B", "Group C"), n, replace = TRUE),
  variable2 = sample(c("Type 1", "Type 2"), n, replace = TRUE)
) %>%
  mutate(across(where(is.character), as.factor))

# Save in all formats
save(jjpiestats_small, file = here("data", "jjpiestats_small.rda"))
write.csv(jjpiestats_small, file = here("data", "jjpiestats_small.csv"), row.names = FALSE)
write_xlsx(jjpiestats_small, path = here("data", "jjpiestats_small.xlsx"))
write_omv(jjpiestats_small, here("data", "jjpiestats_small.omv"))

# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
jjpiestats Test Data Generation Complete
═══════════════════════════════════════════════════════════

Datasets Created (24 files total):

1. jjpiestats_test (n=200)
   - Comprehensive clinical response data
   - Variables: treatment_response, treatment_arm, disease_severity, gender,
               hospital_site, age_group, tumor_stage
   - Use case: General pie chart analysis with multiple categorical variables

2. jjpiestats_diagnostic (n=250)
   - 2×2 diagnostic test evaluation
   - Variables: disease_status, test_result, age_category, clinical_site, gender
   - Realistic sensitivity (85%) and specificity (90%)
   - Use case: Diagnostic test performance visualization

3. jjpiestats_treatment (n=180)
   - Treatment dose-response comparison
   - Variables: treatment (4 levels), outcome (4 levels), baseline_severity,
               study_site, adverse_events
   - Dose-dependent outcomes modeled
   - Use case: Treatment efficacy comparison

4. jjpiestats_biomarker (n=150)
   - Biomarker expression distribution
   - Variables: expression_level, cancer_type, receptor_status, grade, lab
   - Use case: Biomarker distribution across cancer types

5. jjpiestats_aggregated (n=5)
   - Pre-aggregated count data
   - Variables: category, counts, group
   - Use case: Testing counts parameter functionality

6. jjpiestats_small (n=20)
   - Small sample test data
   - Variables: category, variable2
   - Use case: Edge case testing with small samples

All datasets saved in 4 formats:
- RDA (native R)
- CSV (universal)
- XLSX (Excel)
- OMV (jamovi)

Next steps:
1. Run: source('data-raw/jjpiestats_test_data.R')
2. Test: data(jjpiestats_test); library(ClinicoPath)
3. Use: jjpiestats(data = jjpiestats_test, dep = 'treatment_response')

═══════════════════════════════════════════════════════════
")
