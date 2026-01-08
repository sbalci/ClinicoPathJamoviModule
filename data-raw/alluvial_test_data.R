# ═══════════════════════════════════════════════════════════
# Test Data Generation: alluvial
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the alluvial jamovi function
#
# Purpose: Generate categorical clinical pathway data for alluvial diagrams
#          showing patient flow through treatment stages, disease progression,
#          and diagnostic pathways
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 200

library(tibble)
library(dplyr)
library(here)

# Ensure reproducibility
set.seed(42)

# Sample size - typical for clinical cohort studies
n <- 200

# ═══════════════════════════════════════════════════════════
# Generate Clinical Categorical Variables for Alluvial Diagrams
# ═══════════════════════════════════════════════════════════

# Helper function to create realistic transitions
create_progression <- function(initial_state, transition_probs) {
  sapply(initial_state, function(state) {
    if (is.na(state)) return(NA)
    sample(names(transition_probs[[state]]), 1, prob = transition_probs[[state]])
  })
}

# ─────────────────────────────────────────────────────────
# 1. Patient Demographics (for stratification)
# ─────────────────────────────────────────────────────────

alluvial_test <- tibble(
  # Patient identifier
  patient_id = 1:n,

  # Age groups (3 categories - optimal for alluvial)
  age_group = sample(
    c("Young (<50)", "Middle (50-70)", "Older (>70)"),
    n, replace = TRUE,
    prob = c(0.2, 0.5, 0.3)
  ),

  # Sex
  sex = sample(c("Male", "Female"), n, replace = TRUE),

  # Tumor location (for oncology pathway)
  tumor_site = sample(
    c("Breast", "Lung", "Colon", "Prostate", "Ovary"),
    n, replace = TRUE,
    prob = c(0.25, 0.25, 0.2, 0.15, 0.15)
  )
)

# ─────────────────────────────────────────────────────────
# 2. Disease Progression Pathway (Temporal Flow)
# ─────────────────────────────────────────────────────────

# Initial tumor grade at diagnosis
alluvial_test$initial_grade <- sample(
  c("Grade 1", "Grade 2", "Grade 3"),
  n, replace = TRUE,
  prob = c(0.3, 0.45, 0.25)
)

# Stage at diagnosis (correlated with grade)
alluvial_test$initial_stage <- sapply(alluvial_test$initial_grade, function(grade) {
  if (grade == "Grade 1") {
    sample(c("Stage I", "Stage II", "Stage III"), 1, prob = c(0.6, 0.3, 0.1))
  } else if (grade == "Grade 2") {
    sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 1, prob = c(0.2, 0.5, 0.2, 0.1))
  } else { # Grade 3
    sample(c("Stage II", "Stage III", "Stage IV"), 1, prob = c(0.1, 0.5, 0.4))
  }
})

# Treatment received (based on stage)
alluvial_test$treatment <- sapply(alluvial_test$initial_stage, function(stage) {
  if (stage %in% c("Stage I", "Stage II")) {
    sample(c("Surgery Only", "Surgery + Chemo", "Surgery + Radio"),
           1, prob = c(0.4, 0.4, 0.2))
  } else if (stage == "Stage III") {
    sample(c("Surgery + Chemo", "Surgery + Radio", "Chemo + Radio", "Trimodal"),
           1, prob = c(0.3, 0.2, 0.3, 0.2))
  } else { # Stage IV
    sample(c("Chemo Only", "Chemo + Radio", "Palliative", "Trimodal"),
           1, prob = c(0.3, 0.3, 0.3, 0.1))
  }
})

# Treatment response (realistic transition probabilities)
response_transitions <- list(
  "Surgery Only" = c("Complete Response" = 0.6, "Partial Response" = 0.3, "Stable Disease" = 0.08, "Progression" = 0.02),
  "Surgery + Chemo" = c("Complete Response" = 0.5, "Partial Response" = 0.35, "Stable Disease" = 0.1, "Progression" = 0.05),
  "Surgery + Radio" = c("Complete Response" = 0.55, "Partial Response" = 0.3, "Stable Disease" = 0.1, "Progression" = 0.05),
  "Chemo + Radio" = c("Complete Response" = 0.3, "Partial Response" = 0.4, "Stable Disease" = 0.2, "Progression" = 0.1),
  "Trimodal" = c("Complete Response" = 0.35, "Partial Response" = 0.4, "Stable Disease" = 0.15, "Progression" = 0.1),
  "Chemo Only" = c("Complete Response" = 0.2, "Partial Response" = 0.4, "Stable Disease" = 0.25, "Progression" = 0.15),
  "Palliative" = c("Partial Response" = 0.2, "Stable Disease" = 0.4, "Progression" = 0.4)
)

alluvial_test$response <- create_progression(alluvial_test$treatment, response_transitions)

# Follow-up status (based on response)
followup_transitions <- list(
  "Complete Response" = c("Disease-Free" = 0.75, "Recurrence" = 0.15, "Second Primary" = 0.1),
  "Partial Response" = c("Disease-Free" = 0.4, "Residual Disease" = 0.35, "Recurrence" = 0.15, "Progression" = 0.1),
  "Stable Disease" = c("Residual Disease" = 0.5, "Disease-Free" = 0.2, "Progression" = 0.3),
  "Progression" = c("Progression" = 0.6, "Residual Disease" = 0.3, "Death" = 0.1)
)

alluvial_test$followup_status <- create_progression(alluvial_test$response, followup_transitions)

# ─────────────────────────────────────────────────────────
# 3. Diagnostic Pathway (Symptom → Test → Diagnosis)
# ─────────────────────────────────────────────────────────

# Initial symptom presentation
alluvial_test$presenting_symptom <- sample(
  c("Asymptomatic", "Pain", "Mass", "Bleeding", "Constitutional"),
  n, replace = TRUE,
  prob = c(0.15, 0.3, 0.25, 0.2, 0.1)
)

# Diagnostic test ordered
diagnostic_test_transitions <- list(
  "Asymptomatic" = c("Screening" = 0.7, "CT" = 0.2, "Ultrasound" = 0.1),
  "Pain" = c("CT" = 0.4, "MRI" = 0.3, "Ultrasound" = 0.2, "X-ray" = 0.1),
  "Mass" = c("Biopsy" = 0.5, "CT" = 0.3, "MRI" = 0.15, "Ultrasound" = 0.05),
  "Bleeding" = c("Endoscopy" = 0.4, "CT" = 0.3, "Colonoscopy" = 0.2, "Ultrasound" = 0.1),
  "Constitutional" = c("CT" = 0.5, "PET" = 0.2, "MRI" = 0.2, "Labs" = 0.1)
)

alluvial_test$diagnostic_test <- create_progression(
  alluvial_test$presenting_symptom,
  diagnostic_test_transitions
)

# Test result
test_result_transitions <- list(
  "Screening" = c("Negative" = 0.85, "Positive" = 0.15),
  "CT" = c("Suspicious" = 0.6, "Negative" = 0.25, "Positive" = 0.15),
  "MRI" = c("Suspicious" = 0.55, "Positive" = 0.3, "Negative" = 0.15),
  "Ultrasound" = c("Suspicious" = 0.5, "Negative" = 0.35, "Positive" = 0.15),
  "X-ray" = c("Negative" = 0.6, "Suspicious" = 0.3, "Positive" = 0.1),
  "Biopsy" = c("Positive" = 0.7, "Negative" = 0.2, "Indeterminate" = 0.1),
  "Endoscopy" = c("Positive" = 0.45, "Negative" = 0.35, "Suspicious" = 0.2),
  "Colonoscopy" = c("Positive" = 0.4, "Negative" = 0.4, "Polyp Found" = 0.2),
  "PET" = c("Positive" = 0.6, "Suspicious" = 0.3, "Negative" = 0.1),
  "Labs" = c("Abnormal" = 0.5, "Normal" = 0.5)
)

alluvial_test$test_result <- create_progression(
  alluvial_test$diagnostic_test,
  test_result_transitions
)

# ─────────────────────────────────────────────────────────
# 4. Risk Stratification Categories
# ─────────────────────────────────────────────────────────

# Comorbidity burden (Charlson-like categories)
alluvial_test$comorbidity <- sample(
  c("None", "Mild", "Moderate", "Severe"),
  n, replace = TRUE,
  prob = c(0.4, 0.3, 0.2, 0.1)
)

# Performance status (ECOG-like)
alluvial_test$performance_status <- sample(
  c("PS 0", "PS 1", "PS 2", "PS 3-4"),
  n, replace = TRUE,
  prob = c(0.35, 0.4, 0.2, 0.05)
)

# Risk group (composite)
alluvial_test$risk_group <- apply(alluvial_test[, c("initial_stage", "initial_grade", "comorbidity")], 1, function(row) {
  stage <- row["initial_stage"]
  grade <- row["initial_grade"]
  comorbid <- row["comorbidity"]

  # Low risk: early stage + low grade + minimal comorbidity
  if (stage %in% c("Stage I") && grade == "Grade 1" && comorbid %in% c("None", "Mild")) {
    return("Low Risk")
  }
  # High risk: advanced stage or high grade or severe comorbidity
  else if (stage == "Stage IV" || grade == "Grade 3" || comorbid == "Severe") {
    return("High Risk")
  }
  # Intermediate: everything else
  else {
    return("Intermediate Risk")
  }
})

# ─────────────────────────────────────────────────────────
# 5. Healthcare Utilization Pathway
# ─────────────────────────────────────────────────────────

# Initial encounter type
alluvial_test$encounter_type <- sample(
  c("Outpatient", "Emergency", "Referral"),
  n, replace = TRUE,
  prob = c(0.5, 0.3, 0.2)
)

# Care setting
care_setting_transitions <- list(
  "Outpatient" = c("Clinic" = 0.7, "Day Hospital" = 0.2, "Admitted" = 0.1),
  "Emergency" = c("Admitted" = 0.6, "Discharged" = 0.25, "ICU" = 0.15),
  "Referral" = c("Clinic" = 0.5, "Admitted" = 0.3, "Day Hospital" = 0.2)
)

alluvial_test$care_setting <- create_progression(
  alluvial_test$encounter_type,
  care_setting_transitions
)

# ─────────────────────────────────────────────────────────
# 6. Weight Variable for Weighted Alluvial Plots
# ─────────────────────────────────────────────────────────

# Frequency weights (for aggregated data scenarios)
# Some combinations occur more frequently
alluvial_test$frequency <- sample(1:10, n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.1, 0.05, 0.04, 0.03, 0.02, 0.005, 0.005))

# Cost weights (for healthcare economics)
# Higher stage/risk → higher costs
alluvial_test$treatment_cost <- sapply(1:n, function(i) {
  base_cost <- switch(alluvial_test$initial_stage[i],
    "Stage I" = 10000,
    "Stage II" = 25000,
    "Stage III" = 50000,
    "Stage IV" = 80000,
    20000
  )

  # Add treatment-specific costs
  treatment_multiplier <- switch(alluvial_test$treatment[i],
    "Surgery Only" = 1.0,
    "Surgery + Chemo" = 1.5,
    "Surgery + Radio" = 1.4,
    "Chemo + Radio" = 1.3,
    "Trimodal" = 2.0,
    "Chemo Only" = 1.2,
    "Palliative" = 0.8,
    1.0
  )

  round(base_cost * treatment_multiplier + rnorm(1, 0, 2000))
})

# ═══════════════════════════════════════════════════════════
# Add Missing Data (~5% realistic pattern)
# ═══════════════════════════════════════════════════════════

# Missing data more common in follow-up variables
n_missing_followup <- round(n * 0.08)
n_missing_test <- round(n * 0.05)
n_missing_general <- round(n * 0.03)

alluvial_test$followup_status[sample(n, n_missing_followup)] <- NA
alluvial_test$test_result[sample(n, n_missing_test)] <- NA
alluvial_test$diagnostic_test[sample(n, n_missing_test)] <- NA
alluvial_test$care_setting[sample(n, n_missing_general)] <- NA
alluvial_test$response[sample(n, n_missing_general)] <- NA

# ═══════════════════════════════════════════════════════════
# Convert to Factors for Proper Alluvial Display
# ═══════════════════════════════════════════════════════════

categorical_vars <- c(
  "age_group", "sex", "tumor_site",
  "initial_grade", "initial_stage", "treatment", "response", "followup_status",
  "presenting_symptom", "diagnostic_test", "test_result",
  "comorbidity", "performance_status", "risk_group",
  "encounter_type", "care_setting"
)

for (var in categorical_vars) {
  alluvial_test[[var]] <- as.factor(alluvial_test[[var]])
}

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R) - fastest loading
save(alluvial_test, file = here::here("data", "alluvial_test.rda"))
message("✓ Created: data/alluvial_test.rda")

# 2. CSV format - universal compatibility
write.csv(alluvial_test,
          file = here::here("data", "alluvial_test.csv"),
          row.names = FALSE)
message("✓ Created: data/alluvial_test.csv")

# 3. Excel format - clinician-friendly
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(alluvial_test,
                      path = here::here("data", "alluvial_test.xlsx"))
  message("✓ Created: data/alluvial_test.xlsx")
} else {
  message("⚠ writexl package not available - skipping Excel format")
}

# 4. Jamovi format (OMV) - native jamovi format
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(alluvial_test,
                          here::here("data", "alluvial_test.omv"))
  message("✓ Created: data/alluvial_test.omv")
} else {
  message("⚠ jmvReadWrite package not available - skipping OMV format")
}

# ═══════════════════════════════════════════════════════════
# Dataset Documentation
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset: alluvial_test\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Observations:", n, "\n")
cat("Variables:", ncol(alluvial_test), "\n\n")

cat("Missing data profile:\n")
cat("  - Follow-up status: ~8% (realistic clinical pattern)\n")
cat("  - Diagnostic tests: ~5%\n")
cat("  - Other pathway variables: ~3%\n\n")

cat("Variable categories (all categorical/factor):\n\n")

cat("1. DEMOGRAPHICS & BASELINE (3 vars):\n")
cat("   - age_group: 3 categories (Young, Middle, Older)\n")
cat("   - sex: 2 categories (Male, Female)\n")
cat("   - tumor_site: 5 categories\n\n")

cat("2. DISEASE PROGRESSION PATHWAY (5 vars):\n")
cat("   - initial_grade: 3 categories (Grade 1-3)\n")
cat("   - initial_stage: 4 categories (Stage I-IV)\n")
cat("   - treatment: 7 categories (various modalities)\n")
cat("   - response: 4 categories (CR, PR, SD, PD)\n")
cat("   - followup_status: 6 categories\n\n")

cat("3. DIAGNOSTIC PATHWAY (3 vars):\n")
cat("   - presenting_symptom: 5 categories\n")
cat("   - diagnostic_test: 9 categories\n")
cat("   - test_result: 6 categories\n\n")

cat("4. RISK STRATIFICATION (3 vars):\n")
cat("   - comorbidity: 4 categories (None to Severe)\n")
cat("   - performance_status: 4 categories (PS 0-4)\n")
cat("   - risk_group: 3 categories (Low, Intermediate, High)\n\n")

cat("5. HEALTHCARE UTILIZATION (2 vars):\n")
cat("   - encounter_type: 3 categories\n")
cat("   - care_setting: 5 categories\n\n")

cat("6. WEIGHT VARIABLES (2 numeric vars):\n")
cat("   - frequency: 1-10 (for frequency-weighted diagrams)\n")
cat("   - treatment_cost: Realistic healthcare costs\n\n")

cat("Realistic transitions:\n")
cat("  - Grade → Stage (correlated)\n")
cat("  - Stage → Treatment (clinically appropriate)\n")
cat("  - Treatment → Response (realistic probabilities)\n")
cat("  - Response → Follow-up status (evidence-based)\n")
cat("  - Symptom → Test → Result (diagnostic pathway logic)\n\n")

cat("Optimal alluvial diagram pathways:\n")
cat("  1. Treatment pathway: initial_grade → initial_stage → treatment → response → followup_status\n")
cat("  2. Diagnostic pathway: presenting_symptom → diagnostic_test → test_result\n")
cat("  3. Risk pathway: comorbidity → performance_status → risk_group → treatment\n")
cat("  4. Utilization pathway: encounter_type → care_setting\n")
cat("  5. Demographic flow: age_group + sex + tumor_site → treatment → response\n\n")

cat("Usage examples:\n\n")

cat("# Load the dataset\n")
cat("data(alluvial_test, package = 'ClinicoPath')\n\n")

cat("# Basic alluvial diagram (treatment pathway)\n")
cat("library(ClinicoPath)\n")
cat("alluvial(\n")
cat("  data = alluvial_test,\n")
cat("  vars = c('initial_grade', 'initial_stage', 'treatment', 'response')\n")
cat(")\n\n")

cat("# Diagnostic pathway with marginal plots\n")
cat("alluvial(\n")
cat("  data = alluvial_test,\n")
cat("  vars = c('presenting_symptom', 'diagnostic_test', 'test_result'),\n")
cat("  marg = TRUE,\n")
cat("  fill = 'first_variable'\n")
cat(")\n\n")

cat("# Full progression pathway with custom settings\n")
cat("alluvial(\n")
cat("  data = alluvial_test,\n")
cat("  vars = c('initial_grade', 'initial_stage', 'treatment', 'response', 'followup_status'),\n")
cat("  colorPalette = 'viridis',\n")
cat("  showCounts = TRUE,\n")
cat("  orient = 'vert'\n")
cat(")\n\n")

cat("# Weighted alluvial with ggalluvial engine\n")
cat("alluvial(\n")
cat("  data = alluvial_test,\n")
cat("  vars = c('initial_stage', 'treatment', 'response'),\n")
cat("  weight = 'frequency',\n")
cat("  engine = 'ggalluvial',\n")
cat("  sankeyStyle = TRUE,\n")
cat("  curveType = 'sigmoid'\n")
cat(")\n\n")

cat("# Condensation plot (focus on one variable)\n")
cat("alluvial(\n")
cat("  data = alluvial_test,\n")
cat("  vars = c('initial_stage', 'treatment', 'response', 'followup_status'),\n")
cat("  condensationvar = 'treatment'\n")
cat(")\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset generation complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
