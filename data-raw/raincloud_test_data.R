# ═══════════════════════════════════════════════════════════
# Raincloud Test Data Generation Script
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the raincloud function
# raincloud: Distribution visualization combining violin + box + dot plots
#
# Generated: 2026-01-06
# Function: raincloud (distribution visualization)
# Parameters: dep_var (continuous), group_var (categorical), facet_var, color_var
# Datasets: 6 test datasets with varying complexity
# Output: RDA, CSV, XLSX, OMV formats

library(tibble)
library(dplyr)
library(writexl)
library(jmvReadWrite)

set.seed(20260106)

# ═══════════════════════════════════════════════════════════
# Dataset 1: Comprehensive Clinical Response Data
# ═══════════════════════════════════════════════════════════
# Purpose: Main test dataset with multiple continuous variables and groupings
# n = 200
# Features: Treatment effects, disease severity, demographic factors

n <- 200

raincloud_test <- tibble(
  patient_id = 1:n,

  # Grouping variables
  treatment_group = sample(c("Control", "Drug A", "Drug B", "Drug C"), n, replace = TRUE),
  disease_severity = sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  gender = sample(c("Male", "Female"), n, replace = TRUE),
  age_group = sample(c("18-40", "41-60", "61-80"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  hospital_site = sample(c("Site A", "Site B", "Site C"), n, replace = TRUE),

  # Continuous outcome variables with treatment effects
  symptom_score = case_when(
    treatment_group == "Control" ~ rnorm(n, mean = 65, sd = 15),
    treatment_group == "Drug A" ~ rnorm(n, mean = 55, sd = 12),
    treatment_group == "Drug B" ~ rnorm(n, mean = 48, sd = 10),
    treatment_group == "Drug C" ~ rnorm(n, mean = 42, sd = 10)
  ),

  quality_of_life = case_when(
    disease_severity == "Mild" ~ rnorm(n, mean = 75, sd = 10),
    disease_severity == "Moderate" ~ rnorm(n, mean = 60, sd = 12),
    disease_severity == "Severe" ~ rnorm(n, mean = 45, sd = 15)
  ),

  pain_intensity = case_when(
    treatment_group == "Control" ~ rnorm(n, mean = 7.5, sd = 1.5),
    treatment_group == "Drug A" ~ rnorm(n, mean = 6.2, sd = 1.3),
    treatment_group == "Drug B" ~ rnorm(n, mean = 5.0, sd = 1.2),
    treatment_group == "Drug C" ~ rnorm(n, mean = 4.0, sd = 1.0)
  ),

  response_time = case_when(
    age_group == "18-40" ~ rnorm(n, mean = 320, sd = 50),
    age_group == "41-60" ~ rnorm(n, mean = 380, sd = 60),
    age_group == "61-80" ~ rnorm(n, mean = 450, sd = 70)
  )
)

# Add some outliers (5%)
outlier_idx <- sample(1:n, size = floor(n * 0.05))
raincloud_test$symptom_score[outlier_idx] <- raincloud_test$symptom_score[outlier_idx] + rnorm(length(outlier_idx), mean = 30, sd = 10)

# Add some missing values (3%)
missing_idx <- sample(1:n, size = floor(n * 0.03))
raincloud_test$quality_of_life[missing_idx] <- NA

# Constrain ranges
raincloud_test <- raincloud_test %>%
  mutate(
    symptom_score = pmax(0, pmin(100, symptom_score)),
    quality_of_life = pmax(0, pmin(100, quality_of_life)),
    pain_intensity = pmax(0, pmin(10, pain_intensity)),
    response_time = pmax(200, pmin(800, response_time))
  )

# Convert to factors
raincloud_test <- raincloud_test %>%
  mutate(
    treatment_group = factor(treatment_group, levels = c("Control", "Drug A", "Drug B", "Drug C")),
    disease_severity = factor(disease_severity, levels = c("Mild", "Moderate", "Severe")),
    gender = factor(gender),
    age_group = factor(age_group, levels = c("18-40", "41-60", "61-80")),
    hospital_site = factor(hospital_site)
  )

# ═══════════════════════════════════════════════════════════
# Dataset 2: Clinical Laboratory Measurements
# ═══════════════════════════════════════════════════════════
# Purpose: Lab values across patient populations
# n = 150
# Features: Realistic clinical ranges, disease associations

n <- 150

raincloud_clinical <- tibble(
  patient_id = 1:n,

  # Grouping variables
  diagnosis = sample(c("Healthy", "Diabetes", "Hypertension", "Heart Disease"), n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2)),
  age_category = sample(c("Young Adult", "Middle Age", "Senior"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  bmi_category = sample(c("Normal", "Overweight", "Obese"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),

  # Lab measurements with diagnostic associations
  glucose = case_when(
    diagnosis == "Healthy" ~ rnorm(n, mean = 95, sd = 10),
    diagnosis == "Diabetes" ~ rnorm(n, mean = 150, sd = 30),
    diagnosis == "Hypertension" ~ rnorm(n, mean = 105, sd = 15),
    diagnosis == "Heart Disease" ~ rnorm(n, mean = 110, sd = 20)
  ),

  hemoglobin_a1c = case_when(
    diagnosis == "Healthy" ~ rnorm(n, mean = 5.2, sd = 0.3),
    diagnosis == "Diabetes" ~ rnorm(n, mean = 7.5, sd = 1.2),
    diagnosis == "Hypertension" ~ rnorm(n, mean = 5.8, sd = 0.5),
    diagnosis == "Heart Disease" ~ rnorm(n, mean = 6.0, sd = 0.6)
  ),

  systolic_bp = case_when(
    diagnosis == "Healthy" ~ rnorm(n, mean = 118, sd = 10),
    diagnosis == "Diabetes" ~ rnorm(n, mean = 135, sd = 15),
    diagnosis == "Hypertension" ~ rnorm(n, mean = 155, sd = 20),
    diagnosis == "Heart Disease" ~ rnorm(n, mean = 145, sd = 18)
  ),

  cholesterol = case_when(
    bmi_category == "Normal" ~ rnorm(n, mean = 180, sd = 25),
    bmi_category == "Overweight" ~ rnorm(n, mean = 210, sd = 30),
    bmi_category == "Obese" ~ rnorm(n, mean = 240, sd = 35)
  ),

  creatinine = case_when(
    age_category == "Young Adult" ~ rnorm(n, mean = 0.9, sd = 0.15),
    age_category == "Middle Age" ~ rnorm(n, mean = 1.0, sd = 0.2),
    age_category == "Senior" ~ rnorm(n, mean = 1.2, sd = 0.25)
  )
)

# Constrain to realistic ranges
raincloud_clinical <- raincloud_clinical %>%
  mutate(
    glucose = pmax(60, pmin(300, glucose)),
    hemoglobin_a1c = pmax(4.0, pmin(12.0, hemoglobin_a1c)),
    systolic_bp = pmax(90, pmin(200, systolic_bp)),
    cholesterol = pmax(120, pmin(350, cholesterol)),
    creatinine = pmax(0.5, pmin(3.0, creatinine))
  )

# Convert to factors
raincloud_clinical <- raincloud_clinical %>%
  mutate(
    diagnosis = factor(diagnosis, levels = c("Healthy", "Diabetes", "Hypertension", "Heart Disease")),
    age_category = factor(age_category, levels = c("Young Adult", "Middle Age", "Senior")),
    bmi_category = factor(bmi_category, levels = c("Normal", "Overweight", "Obese"))
  )

# ═══════════════════════════════════════════════════════════
# Dataset 3: Treatment Effect Over Time
# ═══════════════════════════════════════════════════════════
# Purpose: Baseline vs follow-up measurements
# n = 120 (60 patients, 2 timepoints each)
# Features: Paired measurements, treatment response

n_patients <- 60

raincloud_treatment <- tibble(
  patient_id = rep(1:n_patients, each = 2),

  # Grouping variables
  treatment = rep(sample(c("Standard", "Experimental"), n_patients, replace = TRUE), each = 2),
  timepoint = rep(c("Baseline", "Week 12"), times = n_patients),
  response_category = rep(sample(c("Responder", "Non-responder"), n_patients, replace = TRUE, prob = c(0.6, 0.4)), each = 2)
)

# Generate correlated baseline and follow-up scores
baseline_scores <- rnorm(n_patients, mean = 50, sd = 15)

raincloud_treatment <- raincloud_treatment %>%
  group_by(patient_id) %>%
  mutate(
    tumor_size = if_else(timepoint == "Baseline",
      baseline_scores[patient_id],
      case_when(
        treatment == "Standard" & response_category == "Responder" ~ baseline_scores[patient_id] * 0.7 + rnorm(1, 0, 5),
        treatment == "Standard" & response_category == "Non-responder" ~ baseline_scores[patient_id] * 0.95 + rnorm(1, 0, 5),
        treatment == "Experimental" & response_category == "Responder" ~ baseline_scores[patient_id] * 0.5 + rnorm(1, 0, 5),
        treatment == "Experimental" & response_category == "Non-responder" ~ baseline_scores[patient_id] * 0.85 + rnorm(1, 0, 5)
      )
    ),

    symptom_burden = if_else(timepoint == "Baseline",
      rnorm(1, mean = 65, sd = 12),
      case_when(
        treatment == "Standard" ~ tumor_size * 1.2 + rnorm(1, 0, 8),
        treatment == "Experimental" ~ tumor_size * 1.0 + rnorm(1, 0, 8)
      )
    )
  ) %>%
  ungroup()

# Constrain ranges
raincloud_treatment <- raincloud_treatment %>%
  mutate(
    tumor_size = pmax(5, pmin(150, tumor_size)),
    symptom_burden = pmax(0, pmin(100, symptom_burden))
  )

# Convert to factors
raincloud_treatment <- raincloud_treatment %>%
  mutate(
    treatment = factor(treatment, levels = c("Standard", "Experimental")),
    timepoint = factor(timepoint, levels = c("Baseline", "Week 12")),
    response_category = factor(response_category)
  )

# ═══════════════════════════════════════════════════════════
# Dataset 4: Biomarker Expression Across Cancer Types
# ═══════════════════════════════════════════════════════════
# Purpose: Continuous biomarker data with biological variation
# n = 180
# Features: Multiple cancer types, receptor status, stage

n <- 180

raincloud_biomarker <- tibble(
  sample_id = 1:n,

  # Grouping variables
  cancer_type = sample(c("Breast", "Lung", "Colon", "Prostate"), n, replace = TRUE),
  stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  receptor_status = sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.65, 0.35)),
  grade = sample(c("Low", "Intermediate", "High"), n, replace = TRUE, prob = c(0.25, 0.45, 0.3)),

  # Biomarker measurements (log-scale then transformed)
  ki67_index = case_when(
    grade == "Low" ~ rlnorm(n, meanlog = log(10), sdlog = 0.5),
    grade == "Intermediate" ~ rlnorm(n, meanlog = log(25), sdlog = 0.6),
    grade == "High" ~ rlnorm(n, meanlog = log(50), sdlog = 0.7)
  ),

  protein_expression = case_when(
    receptor_status == "Positive" ~ rnorm(n, mean = 8.5, sd = 2.0),
    receptor_status == "Negative" ~ rnorm(n, mean = 4.0, sd = 1.5)
  ),

  mutation_burden = case_when(
    cancer_type == "Breast" ~ rlnorm(n, meanlog = log(5), sdlog = 0.8),
    cancer_type == "Lung" ~ rlnorm(n, meanlog = log(12), sdlog = 0.9),
    cancer_type == "Colon" ~ rlnorm(n, meanlog = log(8), sdlog = 0.7),
    cancer_type == "Prostate" ~ rlnorm(n, meanlog = log(4), sdlog = 0.6)
  ),

  immune_score = case_when(
    stage == "Stage I" ~ rnorm(n, mean = 75, sd = 12),
    stage == "Stage II" ~ rnorm(n, mean = 65, sd = 15),
    stage == "Stage III" ~ rnorm(n, mean = 50, sd = 18),
    stage == "Stage IV" ~ rnorm(n, mean = 35, sd = 20)
  )
)

# Constrain ranges
raincloud_biomarker <- raincloud_biomarker %>%
  mutate(
    ki67_index = pmax(0, pmin(100, ki67_index)),
    protein_expression = pmax(0, pmin(15, protein_expression)),
    mutation_burden = pmax(0, pmin(50, mutation_burden)),
    immune_score = pmax(0, pmin(100, immune_score))
  )

# Convert to factors
raincloud_biomarker <- raincloud_biomarker %>%
  mutate(
    cancer_type = factor(cancer_type),
    stage = factor(stage, levels = c("Stage I", "Stage II", "Stage III", "Stage IV")),
    receptor_status = factor(receptor_status),
    grade = factor(grade, levels = c("Low", "Intermediate", "High"))
  )

# ═══════════════════════════════════════════════════════════
# Dataset 5: Small Sample Dataset
# ═══════════════════════════════════════════════════════════
# Purpose: Edge case testing with small n
# n = 30
# Features: Minimal sample size for valid statistical testing

n <- 30

raincloud_small <- tibble(
  id = 1:n,

  group = sample(c("Group A", "Group B", "Group C"), n, replace = TRUE),
  category = sample(c("Type 1", "Type 2"), n, replace = TRUE),

  measurement = case_when(
    group == "Group A" ~ rnorm(n, mean = 50, sd = 10),
    group == "Group B" ~ rnorm(n, mean = 60, sd = 12),
    group == "Group C" ~ rnorm(n, mean = 55, sd = 11)
  ),

  score = rnorm(n, mean = 75, sd = 15)
)

# Constrain ranges
raincloud_small <- raincloud_small %>%
  mutate(
    measurement = pmax(20, pmin(100, measurement)),
    score = pmax(0, pmin(100, score))
  )

# Convert to factors
raincloud_small <- raincloud_small %>%
  mutate(
    group = factor(group),
    category = factor(category)
  )

# ═══════════════════════════════════════════════════════════
# Dataset 6: Skewed Distribution Data
# ═══════════════════════════════════════════════════════════
# Purpose: Test normality detection and transformations
# n = 150
# Features: Right-skewed, left-skewed, bimodal distributions

n <- 150

raincloud_skewed <- tibble(
  subject_id = 1:n,

  group = sample(c("Skewed Right", "Skewed Left", "Bimodal", "Normal"), n, replace = TRUE),
  condition = sample(c("Condition A", "Condition B"), n, replace = TRUE),

  # Right-skewed (e.g., reaction time, cost data)
  right_skewed = rexp(n, rate = 0.05),  # Exponential

  # Left-skewed (e.g., test scores, quality ratings)
  left_skewed = 100 - rexp(n, rate = 0.05),

  # Bimodal (e.g., treatment responders vs non-responders)
  bimodal = c(
    rnorm(floor(n * 0.5), mean = 40, sd = 8),
    rnorm(ceiling(n * 0.5), mean = 70, sd = 8)
  )[sample(1:n)],

  # Normal distribution for comparison
  normal = rnorm(n, mean = 55, sd = 12),

  # Log-normal (many biological measurements)
  lognormal = rlnorm(n, meanlog = 3, sdlog = 0.5)
)

# Constrain ranges
raincloud_skewed <- raincloud_skewed %>%
  mutate(
    right_skewed = pmax(0, pmin(100, right_skewed)),
    left_skewed = pmax(0, pmin(100, left_skewed)),
    bimodal = pmax(0, pmin(100, bimodal)),
    normal = pmax(0, pmin(100, normal)),
    lognormal = pmax(1, pmin(200, lognormal))
  )

# Convert to factors
raincloud_skewed <- raincloud_skewed %>%
  mutate(
    group = factor(group, levels = c("Normal", "Skewed Right", "Skewed Left", "Bimodal")),
    condition = factor(condition)
  )

# ═══════════════════════════════════════════════════════════
# Save All Datasets in Multiple Formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  raincloud_test = raincloud_test,
  raincloud_clinical = raincloud_clinical,
  raincloud_treatment = raincloud_treatment,
  raincloud_biomarker = raincloud_biomarker,
  raincloud_small = raincloud_small,
  raincloud_skewed = raincloud_skewed
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
cat("Raincloud Test Data Generation Complete\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Generated Datasets:\n")
cat("  1. raincloud_test (n=200)       - Comprehensive clinical response data\n")
cat("  2. raincloud_clinical (n=150)   - Laboratory measurements across diagnoses\n")
cat("  3. raincloud_treatment (n=120)  - Treatment effect over time (paired)\n")
cat("  4. raincloud_biomarker (n=180)  - Biomarker expression across cancer types\n")
cat("  5. raincloud_small (n=30)       - Small sample edge cases\n")
cat("  6. raincloud_skewed (n=150)     - Non-normal distributions\n\n")

cat("File Formats Generated:\n")
cat("  - RDA (R native)\n")
cat("  - CSV (universal)\n")
cat("  - XLSX (Excel)\n")
cat("  - OMV (jamovi)\n\n")

cat("Total Files: ", length(datasets) * 4, "\n")
cat("Total Observations: ", sum(sapply(datasets, nrow)), "\n\n")

cat("═══════════════════════════════════════════════════════════\n")
