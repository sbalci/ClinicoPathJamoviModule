# ═══════════════════════════════════════════════════════════
# Ridgeline Test Data Generation Script
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the jjridges function
# jjridges: Ridgeline (joy plot) visualization of distributions across groups
#
# Generated: 2026-01-06
# Function: jjridges (ridgeline distribution plots)
# Parameters: x_var (continuous), y_var (groups), fill_var, facet_var, plot_type
# Datasets: 6 test datasets with varying complexity
# Output: RDA, CSV, XLSX, OMV formats

library(tibble)
library(dplyr)
library(writexl)
library(jmvReadWrite)

set.seed(20260106)

# ═══════════════════════════════════════════════════════════
# Dataset 1: Comprehensive Biomarker Distribution
# ═══════════════════════════════════════════════════════════
# Purpose: Main test dataset with biomarker values across tumor stages
# n = 200
# Features: Clear separation between stages, realistic biomarker ranges

n <- 200

jjridges_test <- tibble(
  patient_id = 1:n,

  # Grouping variables (for y_var - creates ridges)
  tumor_stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.20)),
  tumor_grade = sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE, prob = c(0.30, 0.45, 0.25)),
  receptor_status = sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.65, 0.35)),
  treatment_arm = sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE),

  # Continuous variables (for x_var - distributions)
  # Ki67 proliferation index (0-100%, higher in advanced stages)
  ki67_index = case_when(
    tumor_stage == "Stage I" ~ rnorm(n, mean = 15, sd = 8),
    tumor_stage == "Stage II" ~ rnorm(n, mean = 25, sd = 10),
    tumor_stage == "Stage III" ~ rnorm(n, mean = 40, sd = 12),
    tumor_stage == "Stage IV" ~ rnorm(n, mean = 60, sd = 15)
  ),

  # Tumor size in mm (larger in advanced stages)
  tumor_size = case_when(
    tumor_stage == "Stage I" ~ rnorm(n, mean = 15, sd = 5),
    tumor_stage == "Stage II" ~ rnorm(n, mean = 25, sd = 8),
    tumor_stage == "Stage III" ~ rnorm(n, mean = 40, sd = 10),
    tumor_stage == "Stage IV" ~ rnorm(n, mean = 60, sd = 15)
  ),

  # Protein expression levels (arbitrary units)
  protein_expression = case_when(
    receptor_status == "Positive" ~ rnorm(n, mean = 8.5, sd = 2.0),
    receptor_status == "Negative" ~ rnorm(n, mean = 3.5, sd = 1.5)
  ),

  # Treatment response score (0-100, better with treatment)
  response_score = case_when(
    treatment_arm == "Control" ~ rnorm(n, mean = 45, sd = 15),
    treatment_arm == "Treatment A" ~ rnorm(n, mean = 60, sd = 12),
    treatment_arm == "Treatment B" ~ rnorm(n, mean = 70, sd = 10)
  ),

  # Age (years)
  age = round(rnorm(n, mean = 62, sd = 12)),

  # Lymph node count (right-skewed)
  lymph_nodes = pmax(0, round(rnorm(n, mean = 5, sd = 8)))
)

# Constrain ranges
jjridges_test <- jjridges_test %>%
  mutate(
    ki67_index = pmax(0, pmin(100, ki67_index)),
    tumor_size = pmax(5, pmin(150, tumor_size)),
    protein_expression = pmax(0, pmin(15, protein_expression)),
    response_score = pmax(0, pmin(100, response_score)),
    age = pmax(30, pmin(90, age))
  )

# Add some missing values (3%)
missing_idx <- sample(1:n, size = floor(n * 0.03))
jjridges_test$protein_expression[missing_idx] <- NA

# Convert to factors with ordered levels
jjridges_test <- jjridges_test %>%
  mutate(
    tumor_stage = factor(tumor_stage, levels = c("Stage I", "Stage II", "Stage III", "Stage IV")),
    tumor_grade = factor(tumor_grade, levels = c("Grade 1", "Grade 2", "Grade 3")),
    receptor_status = factor(receptor_status),
    treatment_arm = factor(treatment_arm)
  )

# ═══════════════════════════════════════════════════════════
# Dataset 2: Clinical Laboratory Values
# ═══════════════════════════════════════════════════════════
# Purpose: Lab measurements across diagnostic groups
# n = 180
# Features: Multiple lab parameters with diagnostic associations

n <- 180

jjridges_clinical <- tibble(
  patient_id = 1:n,

  # Diagnostic groups
  diagnosis = sample(c("Healthy", "Pre-diabetes", "Diabetes Type 2", "Metabolic Syndrome"), n, replace = TRUE, prob = c(0.25, 0.25, 0.30, 0.20)),
  bmi_category = sample(c("Normal", "Overweight", "Obese"), n, replace = TRUE, prob = c(0.35, 0.40, 0.25)),
  age_group = sample(c("40-50", "51-60", "61-70", "71-80"), n, replace = TRUE),

  # Lab measurements
  # Fasting glucose (mg/dL)
  glucose = case_when(
    diagnosis == "Healthy" ~ rnorm(n, mean = 92, sd = 8),
    diagnosis == "Pre-diabetes" ~ rnorm(n, mean = 108, sd = 10),
    diagnosis == "Diabetes Type 2" ~ rnorm(n, mean = 145, sd = 30),
    diagnosis == "Metabolic Syndrome" ~ rnorm(n, mean = 115, sd = 20)
  ),

  # HbA1c (%)
  hemoglobin_a1c = case_when(
    diagnosis == "Healthy" ~ rnorm(n, mean = 5.1, sd = 0.3),
    diagnosis == "Pre-diabetes" ~ rnorm(n, mean = 5.9, sd = 0.4),
    diagnosis == "Diabetes Type 2" ~ rnorm(n, mean = 7.8, sd = 1.2),
    diagnosis == "Metabolic Syndrome" ~ rnorm(n, mean = 6.2, sd = 0.8)
  ),

  # Triglycerides (mg/dL)
  triglycerides = case_when(
    bmi_category == "Normal" ~ rlnorm(n, meanlog = log(110), sdlog = 0.4),
    bmi_category == "Overweight" ~ rlnorm(n, meanlog = log(150), sdlog = 0.5),
    bmi_category == "Obese" ~ rlnorm(n, meanlog = log(200), sdlog = 0.6)
  ),

  # LDL cholesterol (mg/dL)
  ldl_cholesterol = rnorm(n, mean = 130, sd = 30),

  # Systolic blood pressure (mmHg)
  systolic_bp = case_when(
    diagnosis == "Healthy" ~ rnorm(n, mean = 118, sd = 10),
    diagnosis == "Pre-diabetes" ~ rnorm(n, mean = 128, sd = 12),
    diagnosis == "Diabetes Type 2" ~ rnorm(n, mean = 138, sd = 15),
    diagnosis == "Metabolic Syndrome" ~ rnorm(n, mean = 145, sd = 18)
  ),

  # C-reactive protein (mg/L, inflammatory marker)
  crp = rlnorm(n, meanlog = log(3), sdlog = 1.2)
)

# Constrain ranges
jjridges_clinical <- jjridges_clinical %>%
  mutate(
    glucose = pmax(60, pmin(350, glucose)),
    hemoglobin_a1c = pmax(4.0, pmin(13.0, hemoglobin_a1c)),
    triglycerides = pmax(40, pmin(500, triglycerides)),
    ldl_cholesterol = pmax(40, pmin(250, ldl_cholesterol)),
    systolic_bp = pmax(90, pmin(200, systolic_bp)),
    crp = pmax(0.1, pmin(50, crp))
  )

# Convert to factors
jjridges_clinical <- jjridges_clinical %>%
  mutate(
    diagnosis = factor(diagnosis, levels = c("Healthy", "Pre-diabetes", "Diabetes Type 2", "Metabolic Syndrome")),
    bmi_category = factor(bmi_category, levels = c("Normal", "Overweight", "Obese")),
    age_group = factor(age_group, levels = c("40-50", "51-60", "61-70", "71-80"))
  )

# ═══════════════════════════════════════════════════════════
# Dataset 3: Treatment Response Over Time
# ═══════════════════════════════════════════════════════════
# Purpose: Symptom/QoL scores across timepoints
# n = 150
# Features: Longitudinal improvement with treatment

n_patients <- 50
timepoints <- c("Baseline", "Week 4", "Week 8", "Week 12")

jjridges_treatment <- tibble(
  patient_id = rep(1:n_patients, each = length(timepoints)),

  timepoint = rep(timepoints, times = n_patients),
  treatment_group = rep(sample(c("Placebo", "Low Dose", "High Dose"), n_patients, replace = TRUE), each = length(timepoints)),
  disease_severity = rep(sample(c("Mild", "Moderate", "Severe"), n_patients, replace = TRUE), each = length(timepoints))
)

# Generate baseline values
baseline_pain <- rnorm(n_patients, mean = 70, sd = 15)
baseline_fatigue <- rnorm(n_patients, mean = 65, sd = 12)

# Create improvement trajectories
jjridges_treatment <- jjridges_treatment %>%
  group_by(patient_id) %>%
  mutate(
    time_numeric = match(timepoint, timepoints) - 1,  # 0, 1, 2, 3

    # Pain score (0-100, higher = worse)
    pain_score = case_when(
      treatment_group == "Placebo" ~ baseline_pain[patient_id] - time_numeric * 3 + rnorm(1, 0, 5),
      treatment_group == "Low Dose" ~ baseline_pain[patient_id] - time_numeric * 8 + rnorm(1, 0, 5),
      treatment_group == "High Dose" ~ baseline_pain[patient_id] - time_numeric * 15 + rnorm(1, 0, 5)
    ),

    # Fatigue score (0-100, higher = worse)
    fatigue_score = case_when(
      treatment_group == "Placebo" ~ baseline_fatigue[patient_id] - time_numeric * 2 + rnorm(1, 0, 5),
      treatment_group == "Low Dose" ~ baseline_fatigue[patient_id] - time_numeric * 6 + rnorm(1, 0, 5),
      treatment_group == "High Dose" ~ baseline_fatigue[patient_id] - time_numeric * 12 + rnorm(1, 0, 5)
    ),

    # Quality of life (0-100, higher = better)
    qol_score = 100 - pain_score * 0.6 - fatigue_score * 0.4 + rnorm(1, 0, 8)
  ) %>%
  ungroup()

# Constrain ranges
jjridges_treatment <- jjridges_treatment %>%
  mutate(
    pain_score = pmax(0, pmin(100, pain_score)),
    fatigue_score = pmax(0, pmin(100, fatigue_score)),
    qol_score = pmax(0, pmin(100, qol_score))
  )

# Convert to factors
jjridges_treatment <- jjridges_treatment %>%
  mutate(
    timepoint = factor(timepoint, levels = timepoints),
    treatment_group = factor(treatment_group, levels = c("Placebo", "Low Dose", "High Dose")),
    disease_severity = factor(disease_severity, levels = c("Mild", "Moderate", "Severe"))
  ) %>%
  select(-time_numeric)

# ═══════════════════════════════════════════════════════════
# Dataset 4: Multiple Biomarkers Across Cancer Types
# ═══════════════════════════════════════════════════════════
# Purpose: Diverse biomarker patterns
# n = 200
# Features: Multiple continuous biomarkers with cancer-type associations

n <- 200

jjridges_biomarker <- tibble(
  sample_id = 1:n,

  # Cancer types
  cancer_type = sample(c("Breast", "Lung", "Colon", "Prostate", "Ovarian"), n, replace = TRUE),
  histology = sample(c("Adenocarcinoma", "Squamous", "Other"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  stage = sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.55, 0.45)),

  # Mutation burden (mutations per megabase)
  mutation_burden = case_when(
    cancer_type == "Lung" ~ rlnorm(n, meanlog = log(12), sdlog = 0.7),
    cancer_type == "Breast" ~ rlnorm(n, meanlog = log(5), sdlog = 0.6),
    cancer_type == "Colon" ~ rlnorm(n, meanlog = log(8), sdlog = 0.7),
    cancer_type == "Prostate" ~ rlnorm(n, meanlog = log(4), sdlog = 0.5),
    cancer_type == "Ovarian" ~ rlnorm(n, meanlog = log(6), sdlog = 0.6)
  ),

  # PD-L1 expression (%)
  pdl1_expression = case_when(
    cancer_type == "Lung" ~ rbeta(n, shape1 = 2, shape2 = 3) * 100,
    cancer_type == "Breast" ~ rbeta(n, shape1 = 1, shape2 = 5) * 100,
    cancer_type == "Colon" ~ rbeta(n, shape1 = 1.5, shape2 = 4) * 100,
    cancer_type == "Prostate" ~ rbeta(n, shape1 = 1, shape2 = 8) * 100,
    cancer_type == "Ovarian" ~ rbeta(n, shape1 = 2, shape2 = 4) * 100
  ),

  # Tumor infiltrating lymphocytes (TILs, %)
  tils_percent = pmax(0, pmin(100, rnorm(n, mean = 20, sd = 15))),

  # Mitotic count (per 10 high-power fields)
  mitotic_count = rpois(n, lambda = 15),

  # Angiogenesis score (0-10)
  angiogenesis_score = pmax(0, pmin(10, rnorm(n, mean = 5, sd = 2)))
)

# Constrain ranges
jjridges_biomarker <- jjridges_biomarker %>%
  mutate(
    mutation_burden = pmax(0.5, pmin(50, mutation_burden)),
    pdl1_expression = pmax(0, pmin(100, pdl1_expression))
  )

# Convert to factors
jjridges_biomarker <- jjridges_biomarker %>%
  mutate(
    cancer_type = factor(cancer_type),
    histology = factor(histology),
    stage = factor(stage, levels = c("Early", "Advanced"))
  )

# ═══════════════════════════════════════════════════════════
# Dataset 5: Survival Time Distribution
# ═══════════════════════════════════════════════════════════
# Purpose: Survival times across treatment/stage groups
# n = 160
# Features: Right-skewed survival data

n <- 160

jjridges_survival <- tibble(
  patient_id = 1:n,

  # Grouping
  treatment = sample(c("Surgery Only", "Surgery + Chemo", "Surgery + Immuno", "Trimodal"), n, replace = TRUE),
  disease_stage = sample(c("Stage I", "Stage II", "Stage III"), n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  age_category = sample(c("< 50", "50-65", "> 65"), n, replace = TRUE, prob = c(0.25, 0.45, 0.30)),

  # Survival time (months, right-skewed)
  survival_months = case_when(
    disease_stage == "Stage I" & treatment == "Trimodal" ~ rweibull(n, shape = 2, scale = 80),
    disease_stage == "Stage I" ~ rweibull(n, shape = 2, scale = 65),
    disease_stage == "Stage II" & treatment == "Trimodal" ~ rweibull(n, shape = 1.8, scale = 50),
    disease_stage == "Stage II" ~ rweibull(n, shape = 1.8, scale = 40),
    disease_stage == "Stage III" & treatment == "Trimodal" ~ rweibull(n, shape = 1.5, scale = 35),
    disease_stage == "Stage III" ~ rweibull(n, shape = 1.5, scale = 25)
  ),

  # Disease-free interval (months)
  dfs_months = survival_months * runif(n, min = 0.6, max = 0.9),

  # Response duration (for responders, months)
  response_duration = pmax(0, rnorm(n, mean = 18, sd = 10))
)

# Constrain ranges
jjridges_survival <- jjridges_survival %>%
  mutate(
    survival_months = pmax(1, pmin(150, survival_months)),
    dfs_months = pmax(0, pmin(survival_months, dfs_months)),
    response_duration = pmax(0, pmin(60, response_duration))
  )

# Convert to factors
jjridges_survival <- jjridges_survival %>%
  mutate(
    treatment = factor(treatment, levels = c("Surgery Only", "Surgery + Chemo", "Surgery + Immuno", "Trimodal")),
    disease_stage = factor(disease_stage, levels = c("Stage I", "Stage II", "Stage III")),
    age_category = factor(age_category, levels = c("< 50", "50-65", "> 65"))
  )

# ═══════════════════════════════════════════════════════════
# Dataset 6: Small Sample Dataset
# ═══════════════════════════════════════════════════════════
# Purpose: Edge case testing with small n
# n = 30
# Features: Minimal sample for valid visualization

n <- 30

jjridges_small <- tibble(
  id = 1:n,

  group = sample(c("Group A", "Group B", "Group C"), n, replace = TRUE),
  category = sample(c("Type 1", "Type 2"), n, replace = TRUE),

  measurement = case_when(
    group == "Group A" ~ rnorm(n, mean = 50, sd = 12),
    group == "Group B" ~ rnorm(n, mean = 65, sd = 10),
    group == "Group C" ~ rnorm(n, mean = 55, sd = 15)
  ),

  value = rnorm(n, mean = 100, sd = 20)
)

# Constrain ranges
jjridges_small <- jjridges_small %>%
  mutate(
    measurement = pmax(10, pmin(100, measurement)),
    value = pmax(50, pmin(150, value))
  )

# Convert to factors
jjridges_small <- jjridges_small %>%
  mutate(
    group = factor(group),
    category = factor(category)
  )

# ═══════════════════════════════════════════════════════════
# Save All Datasets in Multiple Formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  jjridges_test = jjridges_test,
  jjridges_clinical = jjridges_clinical,
  jjridges_treatment = jjridges_treatment,
  jjridges_biomarker = jjridges_biomarker,
  jjridges_survival = jjridges_survival,
  jjridges_small = jjridges_small
)

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]

  # 1. Save as RDA (native R format)
  assign(dataset_name, dataset)
  save(list = dataset_name, file = paste0("data/", dataset_name, ".rda"))

  # 2. Save as CSV
  write.csv(dataset, file = paste0("data/", dataset_name, ".csv"), row.names = FALSE)

  # 3. Save as XLSX
  write_xlsx(dataset, path = paste0("data/", dataset_name, ".xlsx"))

  # 4. Save as OMV (jamovi format)
  write_omv(dataset, paste0("data/", dataset_name, ".omv"))

  cat(sprintf("✓ Generated %s (%d rows, %d columns)\n", dataset_name, nrow(dataset), ncol(dataset)))
}

# ═══════════════════════════════════════════════════════════
# Summary Statistics
# ═══════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════\n")
cat("Ridgeline Test Data Generation Complete\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Generated Datasets:\n")
cat("  1. jjridges_test (n=200)        - Biomarker distribution across tumor stages\n")
cat("  2. jjridges_clinical (n=180)    - Laboratory values across diagnoses\n")
cat("  3. jjridges_treatment (n=150)   - Treatment response over time\n")
cat("  4. jjridges_biomarker (n=200)   - Multiple biomarkers across cancer types\n")
cat("  5. jjridges_survival (n=160)    - Survival time distributions\n")
cat("  6. jjridges_small (n=30)        - Small sample edge cases\n\n")

cat("File Formats Generated:\n")
cat("  - RDA (R native)\n")
cat("  - CSV (universal)\n")
cat("  - XLSX (Excel)\n")
cat("  - OMV (jamovi)\n\n")

cat("Total Files: ", length(datasets) * 4, "\n")
cat("Total Observations: ", sum(sapply(datasets, nrow)), "\n\n")

cat("═══════════════════════════════════════════════════════════\n")
