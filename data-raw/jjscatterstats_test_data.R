# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjscatterstats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjscatterstats jamovi function
# (scatter plots with correlation analysis)
#
# Generated: 2026-01-06
# Seed: 42
# Total datasets: 6
# Total observations: 920

library(tibble)
library(dplyr)
library(writexl)
library(jmvReadWrite)
library(MASS)  # For mvrnorm (multivariate normal)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: jjscatterstats_test (n=200)
# Purpose: General biomarker correlations with various strengths
# ═══════════════════════════════════════════════════════════

n <- 200

# Create correlation matrix for multivariate normal
# ki67_index and tumor_size will have strong positive correlation (r ≈ 0.75)
cor_matrix <- matrix(c(
  1.0, 0.75,
  0.75, 1.0
), nrow = 2)

# Generate correlated data
ki67_tumor_data <- mvrnorm(n = n,
                           mu = c(35, 50),
                           Sigma = cor_matrix * matrix(c(225, 180, 180, 400), 2, 2))

jjscatterstats_test <- tibble(
  patient_id = 1:n,

  # Strong positive correlation (r ≈ 0.75)
  ki67_index = pmax(0, pmin(100, ki67_tumor_data[,1])),  # 0-100%, proliferation marker
  tumor_size = pmax(10, pmin(150, ki67_tumor_data[,2])),  # 10-150 mm

  # Moderate positive correlation (r ≈ 0.5) - protein expression and mutation burden
  protein_expression = rnorm(n, mean = 50, sd = 15),
  mutation_burden = 3 + 0.4 * (protein_expression - 50) + rnorm(n, mean = 10, sd = 5),

  # Weak negative correlation (r ≈ -0.3) - immune score and tumor grade
  immune_score = rnorm(n, mean = 60, sd = 20),

  # Zero correlation (r ≈ 0) - age is independent
  age = round(rnorm(n, mean = 62, sd = 12)),

  # Categorical variables
  tumor_stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                       n, replace = TRUE, prob = c(0.25, 0.3, 0.25, 0.2)),
  tumor_grade = sample(c("Grade 1", "Grade 2", "Grade 3"),
                       n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  receptor_status = sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.6, 0.4)),
  treatment_group = sample(c("Control", "Treatment A", "Treatment B"),
                           n, replace = TRUE, prob = c(0.33, 0.33, 0.34))
)

# Add grade-dependent relationships
jjscatterstats_test <- jjscatterstats_test %>%
  mutate(
    # Tumor grade affects immune score (negative relationship)
    immune_score = case_when(
      tumor_grade == "Grade 1" ~ immune_score + 15,
      tumor_grade == "Grade 2" ~ immune_score,
      tumor_grade == "Grade 3" ~ immune_score - 15
    ),

    # Response score related to ki67 and treatment
    response_score = case_when(
      treatment_group == "Control" ~ 40 - 0.2 * ki67_index + rnorm(n, 0, 10),
      treatment_group == "Treatment A" ~ 55 - 0.15 * ki67_index + rnorm(n, 0, 10),
      treatment_group == "Treatment B" ~ 70 - 0.1 * ki67_index + rnorm(n, 0, 10)
    ),
    response_score = pmax(0, pmin(100, response_score)),

    # Lymph node count (right-skewed, correlated with stage)
    lymph_nodes = case_when(
      tumor_stage == "Stage I" ~ rpois(n, lambda = 1),
      tumor_stage == "Stage II" ~ rpois(n, lambda = 3),
      tumor_stage == "Stage III" ~ rpois(n, lambda = 7),
      tumor_stage == "Stage IV" ~ rpois(n, lambda = 12)
    )
  )

# Add some missing data (3%)
n_missing <- round(n * 0.03)
jjscatterstats_test$protein_expression[sample(n, n_missing)] <- NA
jjscatterstats_test$mutation_burden[sample(n, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# Dataset 2: jjscatterstats_clinical (n=180)
# Purpose: Laboratory value correlations (clinical chemistry)
# ═══════════════════════════════════════════════════════════

n <- 180

# Strong correlation between glucose and HbA1c (r ≈ 0.85)
cor_matrix_glu <- matrix(c(1.0, 0.85, 0.85, 1.0), nrow = 2)
glu_hba1c <- mvrnorm(n = n, mu = c(110, 6.0),
                     Sigma = cor_matrix_glu * matrix(c(900, 13.5, 13.5, 1.5), 2, 2))

# Moderate correlation between cholesterol and triglycerides (r ≈ 0.55)
cor_matrix_lipid <- matrix(c(1.0, 0.55, 0.55, 1.0), nrow = 2)
lipid_data <- mvrnorm(n = n, mu = c(200, 150),
                      Sigma = cor_matrix_lipid * matrix(c(1600, 600, 600, 2500), 2, 2))

jjscatterstats_clinical <- tibble(
  patient_id = 1:n,

  # Diagnosis groups
  diagnosis = sample(c("Healthy", "Pre-diabetes", "Diabetes Type 2", "Metabolic Syndrome"),
                    n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2)),

  # Strong positive: Glucose and HbA1c
  glucose = pmax(60, pmin(350, glu_hba1c[,1])),  # mg/dL
  hemoglobin_a1c = pmax(4.0, pmin(12.0, glu_hba1c[,2])),  # %

  # Moderate positive: Cholesterol and Triglycerides
  cholesterol = pmax(120, pmin(350, lipid_data[,1])),  # mg/dL
  triglycerides = pmax(50, pmin(500, lipid_data[,2])),  # mg/dL

  # Weak negative: HDL and triglycerides (r ≈ -0.35)
  hdl_cholesterol = pmax(20, pmin(100, 55 - 0.05 * (triglycerides - 150) + rnorm(n, 0, 8))),

  # Moderate positive: BMI and systolic BP (r ≈ 0.45)
  bmi = rnorm(n, mean = 27, sd = 5),
  systolic_bp = pmax(90, pmin(200, 115 + 1.2 * (bmi - 27) + rnorm(n, 0, 10))),

  # Independent: Age (no correlation with metabolic markers in this dataset)
  age = round(rnorm(n, mean = 58, sd = 14)),

  # Categorical
  age_group = cut(age, breaks = c(0, 45, 65, 100),
                  labels = c("Young (<45)", "Middle (45-65)", "Senior (>65)")),
  bmi_category = cut(bmi, breaks = c(0, 25, 30, 50),
                     labels = c("Normal", "Overweight", "Obese")),
  gender = sample(c("Male", "Female"), n, replace = TRUE)
)

# Adjust by diagnosis
jjscatterstats_clinical <- jjscatterstats_clinical %>%
  mutate(
    glucose = case_when(
      diagnosis == "Healthy" ~ glucose - 20,
      diagnosis == "Pre-diabetes" ~ glucose - 5,
      diagnosis == "Diabetes Type 2" ~ glucose + 40,
      diagnosis == "Metabolic Syndrome" ~ glucose + 10
    ),
    hemoglobin_a1c = case_when(
      diagnosis == "Healthy" ~ hemoglobin_a1c - 1.0,
      diagnosis == "Pre-diabetes" ~ hemoglobin_a1c - 0.3,
      diagnosis == "Diabetes Type 2" ~ hemoglobin_a1c + 1.5,
      diagnosis == "Metabolic Syndrome" ~ hemoglobin_a1c + 0.5
    )
  )

# ═══════════════════════════════════════════════════════════
# Dataset 3: jjscatterstats_treatment (n=150)
# Purpose: Dose-response relationships and paired baseline/follow-up
# ═══════════════════════════════════════════════════════════

n_patients <- 75
n <- n_patients * 2  # 2 timepoints

jjscatterstats_treatment <- tibble(
  patient_id = rep(1:n_patients, each = 2),
  timepoint = rep(c("Baseline", "Week 12"), times = n_patients),

  # Treatment characteristics
  drug_dose = rep(sample(c(0, 50, 100, 200), n_patients, replace = TRUE), each = 2),  # mg
  treatment_group = rep(sample(c("Placebo", "Low Dose", "High Dose"),
                               n_patients, replace = TRUE), each = 2),

  # Baseline characteristics (same for both timepoints)
  age = rep(round(rnorm(n_patients, mean = 55, sd = 10)), each = 2),
  baseline_severity = rep(rnorm(n_patients, mean = 65, sd = 15), each = 2)
)

# Generate dose-response data
jjscatterstats_treatment <- jjscatterstats_treatment %>%
  mutate(
    # Tumor size decreases with dose (negative relationship)
    tumor_size = case_when(
      timepoint == "Baseline" ~ rnorm(n, mean = 45, sd = 15),
      timepoint == "Week 12" ~ rnorm(n, mean = 45 - 0.15 * drug_dose, sd = 12)
    ),
    tumor_size = pmax(5, pmin(100, tumor_size)),

    # Response score increases with dose (positive relationship, sigmoid)
    # Sigmoid: response = 100 / (1 + exp(-k * (dose - IC50)))
    response_score = case_when(
      timepoint == "Baseline" ~ rnorm(n, mean = 20, sd = 10),
      timepoint == "Week 12" ~ 100 / (1 + exp(-0.03 * (drug_dose - 100))) + rnorm(n, 0, 8)
    ),
    response_score = pmax(0, pmin(100, response_score)),

    # Biomarker level (linear dose response)
    biomarker_level = case_when(
      timepoint == "Baseline" ~ rnorm(n, mean = 250, sd = 50),
      timepoint == "Week 12" ~ 250 - 0.8 * drug_dose + rnorm(n, 0, 40)
    ),
    biomarker_level = pmax(50, pmin(400, biomarker_level)),

    # Quality of life (improves with response)
    qol_score = 40 + 0.5 * response_score + rnorm(n, 0, 10),
    qol_score = pmax(0, pmin(100, qol_score)),

    # Adverse event score (increases with dose, non-linear)
    adverse_events = sqrt(drug_dose) * 0.5 + rnorm(n, 0, 2),
    adverse_events = pmax(0, pmin(10, adverse_events))
  )

# ═══════════════════════════════════════════════════════════
# Dataset 4: jjscatterstats_expression (n=200)
# Purpose: Gene/protein expression correlations
# ═══════════════════════════════════════════════════════════

n <- 200

# Strong positive: Gene A and Gene B (co-regulated, r ≈ 0.8)
cor_matrix_genes <- matrix(c(1.0, 0.8, 0.8, 1.0), nrow = 2)
gene_ab <- mvrnorm(n = n, mu = c(8, 7),
                   Sigma = cor_matrix_genes * matrix(c(2.25, 1.8, 1.8, 2.25), 2, 2))

# Moderate negative: Oncogene and Tumor Suppressor (r ≈ -0.6)
oncogene <- rnorm(n, mean = 10, sd = 2.5)
tumor_suppressor <- 8 - 0.6 * (oncogene - 10) + rnorm(n, 0, 1.5)

jjscatterstats_expression <- tibble(
  sample_id = 1:n,

  # Cancer types
  cancer_type = sample(c("Breast", "Lung", "Colorectal", "Prostate", "Melanoma"),
                      n, replace = TRUE),
  stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                n, replace = TRUE, prob = c(0.25, 0.3, 0.25, 0.2)),

  # Strong positive correlation: Co-regulated genes
  gene_a_expression = pmax(2, pmin(15, gene_ab[,1])),  # Log2 scale
  gene_b_expression = pmax(2, pmin(15, gene_ab[,2])),

  # Moderate negative: Oncogene vs tumor suppressor
  oncogene_expression = pmax(4, pmin(16, oncogene)),
  tumor_suppressor_expression = pmax(3, pmin(13, tumor_suppressor)),

  # Weak positive: Proliferation marker (r ≈ 0.35)
  ki67_protein = rnorm(n, mean = 25, sd = 12),
  pcna_protein = pmax(5, pmin(50, 20 + 0.25 * (ki67_protein - 25) + rnorm(n, 0, 8))),

  # Immune markers (moderate positive r ≈ 0.5)
  pdl1_expression = rnorm(n, mean = 15, sd = 10),
  til_score = pmax(0, pmin(100, 30 + 1.2 * (pdl1_expression - 15) + rnorm(n, 0, 15))),

  # Mutation burden (independent of most markers)
  mutation_burden = pmax(0, rnorm(n, mean = 8, sd = 6)),  # mutations/Mb

  # Categorical
  histology = sample(c("Adenocarcinoma", "Squamous", "Other"),
                    n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  grade = sample(c("Low", "Intermediate", "High"),
                n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
)

# Add cancer-type specific patterns
jjscatterstats_expression <- jjscatterstats_expression %>%
  mutate(
    mutation_burden = case_when(
      cancer_type == "Melanoma" ~ mutation_burden + 7,
      cancer_type == "Lung" ~ mutation_burden + 4,
      cancer_type == "Breast" ~ mutation_burden - 2,
      cancer_type == "Prostate" ~ mutation_burden - 3,
      TRUE ~ mutation_burden
    )
  )

# ═══════════════════════════════════════════════════════════
# Dataset 5: jjscatterstats_survival (n=160)
# Purpose: Biomarker vs survival time relationships
# ═══════════════════════════════════════════════════════════

n <- 160

jjscatterstats_survival <- tibble(
  patient_id = 1:n,

  # Disease characteristics
  disease_stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                        n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  treatment = sample(c("Surgery Only", "Surgery+Chemo", "Surgery+Chemo+Immuno"),
                    n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),

  # Biomarkers (will correlate with survival)
  ki67_index = pmax(5, pmin(95, rnorm(n, mean = 35, sd = 20))),
  pdl1_score = pmax(0, pmin(100, rnorm(n, mean = 25, sd = 20))),

  # Age and BMI
  age = round(rnorm(n, mean = 63, sd = 11)),
  bmi = pmax(18, pmin(45, rnorm(n, mean = 26, sd = 5))),

  # Categorical
  age_category = cut(age, breaks = c(0, 50, 70, 100),
                     labels = c("<50", "50-70", ">70")),
  performance_status = sample(c("Good", "Fair", "Poor"),
                             n, replace = TRUE, prob = c(0.5, 0.35, 0.15))
)

# Generate survival times with biomarker relationships
jjscatterstats_survival <- jjscatterstats_survival %>%
  mutate(
    # Survival negatively correlated with Ki67 (r ≈ -0.55)
    # Higher Ki67 → shorter survival
    survival_months = case_when(
      disease_stage == "Stage I" ~ 90 - 0.5 * ki67_index + rnorm(n, 0, 15),
      disease_stage == "Stage II" ~ 65 - 0.4 * ki67_index + rnorm(n, 0, 12),
      disease_stage == "Stage III" ~ 40 - 0.3 * ki67_index + rnorm(n, 0, 10),
      disease_stage == "Stage IV" ~ 20 - 0.2 * ki67_index + rnorm(n, 0, 8)
    ),
    survival_months = pmax(1, pmin(120, survival_months)),

    # PFS (progression-free survival) shorter than OS
    pfs_months = survival_months * 0.6 + rnorm(n, 0, 5),
    pfs_months = pmax(1, pmin(survival_months, pfs_months)),

    # Treatment response score (positively correlated with PD-L1)
    response_score = 30 + 0.5 * pdl1_score + rnorm(n, 0, 15),
    response_score = pmax(0, pmin(100, response_score)),

    # Event indicator (higher ki67 → more events)
    event_probability = 0.3 + 0.005 * ki67_index,
    event = rbinom(n, size = 1, prob = event_probability)
  ) %>%
  dplyr::select(-event_probability)

# Adjust by treatment
jjscatterstats_survival <- jjscatterstats_survival %>%
  mutate(
    survival_months = case_when(
      treatment == "Surgery Only" ~ survival_months * 0.8,
      treatment == "Surgery+Chemo" ~ survival_months * 1.0,
      treatment == "Surgery+Chemo+Immuno" ~ survival_months * 1.25
    )
  )

# ═══════════════════════════════════════════════════════════
# Dataset 6: jjscatterstats_small (n=30)
# Purpose: Small sample edge cases
# ═══════════════════════════════════════════════════════════

n <- 30

jjscatterstats_small <- tibble(
  id = 1:n,

  # Simple correlation
  x_var = rnorm(n, mean = 50, sd = 15),
  y_var = 30 + 0.6 * (x_var - 50) + rnorm(n, 0, 10),

  # Categorical
  group = sample(c("A", "B", "C"), n, replace = TRUE),
  category = sample(c("Type 1", "Type 2"), n, replace = TRUE),

  # Additional variables
  measurement = rnorm(n, mean = 100, sd = 20),
  score = rnorm(n, mean = 50, sd = 15)
)

# ═══════════════════════════════════════════════════════════
# Save all datasets in multiple formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  jjscatterstats_test = jjscatterstats_test,
  jjscatterstats_clinical = jjscatterstats_clinical,
  jjscatterstats_treatment = jjscatterstats_treatment,
  jjscatterstats_expression = jjscatterstats_expression,
  jjscatterstats_survival = jjscatterstats_survival,
  jjscatterstats_small = jjscatterstats_small
)

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]

  # 1. RDA format (native R)
  assign(dataset_name, dataset)
  save(list = dataset_name, file = paste0("data/", dataset_name, ".rda"))

  # 2. CSV format
  write.csv(dataset, file = paste0("data/", dataset_name, ".csv"), row.names = FALSE)

  # 3. Excel format
  write_xlsx(dataset, path = paste0("data/", dataset_name, ".xlsx"))

  # 4. Jamovi format (OMV)
  write_omv(dataset, paste0("data/", dataset_name, ".omv"))

  cat("✓ Generated:", dataset_name, "(n =", nrow(dataset), ")\n")
}

# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete: jjscatterstats\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Generated 6 datasets with realistic scatter plot scenarios:\n\n")

cat("1. jjscatterstats_test (n=200)\n")
cat("   - General biomarker correlations\n")
cat("   - Correlation strengths: strong positive (Ki67 vs tumor size, r≈0.75),\n")
cat("     moderate positive (protein vs mutation, r≈0.5),\n")
cat("     weak negative (immune vs grade, r≈-0.3), zero (age)\n")
cat("   - Variables: ki67_index, tumor_size, protein_expression, mutation_burden,\n")
cat("     immune_score, age, response_score, lymph_nodes\n")
cat("   - Groups: tumor_stage, tumor_grade, receptor_status, treatment_group\n\n")

cat("2. jjscatterstats_clinical (n=180)\n")
cat("   - Laboratory value correlations\n")
cat("   - Strong positive: glucose vs HbA1c (r≈0.85)\n")
cat("   - Moderate positive: cholesterol vs triglycerides (r≈0.55), BMI vs BP (r≈0.45)\n")
cat("   - Weak negative: HDL vs triglycerides (r≈-0.35)\n")
cat("   - Variables: glucose, hemoglobin_a1c, cholesterol, triglycerides, hdl_cholesterol,\n")
cat("     bmi, systolic_bp, age\n")
cat("   - Groups: diagnosis, age_group, bmi_category, gender\n\n")

cat("3. jjscatterstats_treatment (n=150, 75 patients × 2 timepoints)\n")
cat("   - Dose-response relationships\n")
cat("   - Negative: drug_dose vs tumor_size (linear)\n")
cat("   - Positive sigmoid: drug_dose vs response_score (non-linear)\n")
cat("   - Variables: drug_dose, tumor_size, response_score, biomarker_level,\n")
cat("     qol_score, adverse_events\n")
cat("   - Groups: treatment_group, timepoint\n\n")

cat("4. jjscatterstats_expression (n=200)\n")
cat("   - Gene/protein expression correlations\n")
cat("   - Strong positive: gene_a vs gene_b (co-regulated, r≈0.8)\n")
cat("   - Moderate negative: oncogene vs tumor_suppressor (r≈-0.6)\n")
cat("   - Weak positive: ki67 vs pcna (r≈0.35)\n")
cat("   - Moderate positive: pdl1 vs til_score (r≈0.5)\n")
cat("   - Variables: gene_a_expression, gene_b_expression, oncogene_expression,\n")
cat("     tumor_suppressor_expression, ki67_protein, pcna_protein, pdl1_expression,\n")
cat("     til_score, mutation_burden\n")
cat("   - Groups: cancer_type, stage, histology, grade\n\n")

cat("5. jjscatterstats_survival (n=160)\n")
cat("   - Biomarker vs survival relationships\n")
cat("   - Moderate negative: ki67_index vs survival_months (r≈-0.55)\n")
cat("   - Moderate positive: pdl1_score vs response_score (r≈0.5)\n")
cat("   - Variables: ki67_index, pdl1_score, survival_months, pfs_months,\n")
cat("     response_score, age, bmi\n")
cat("   - Groups: disease_stage, treatment, age_category, performance_status\n\n")

cat("6. jjscatterstats_small (n=30)\n")
cat("   - Small sample edge cases\n")
cat("   - Moderate positive: x_var vs y_var (r≈0.6)\n")
cat("   - Variables: x_var, y_var, measurement, score\n")
cat("   - Groups: group, category\n\n")

cat("Total observations across all datasets:",
    sum(sapply(datasets, nrow)), "\n")
cat("Files per dataset: 4 (RDA, CSV, XLSX, OMV)\n")
cat("Total files generated: 24\n\n")

cat("Missing data: ~3% in jjscatterstats_test\n\n")

cat("Correlation types covered:\n")
cat("  - Strong positive (r ≈ 0.75-0.85)\n")
cat("  - Moderate positive (r ≈ 0.45-0.55)\n")
cat("  - Weak positive (r ≈ 0.25-0.35)\n")
cat("  - Zero correlation (r ≈ 0)\n")
cat("  - Weak negative (r ≈ -0.3 to -0.35)\n")
cat("  - Moderate negative (r ≈ -0.55 to -0.6)\n")
cat("  - Non-linear (sigmoid dose-response)\n\n")

cat("Usage examples:\n")
cat('  data(jjscatterstats_test)\n')
cat('  library(ClinicoPath)\n\n')
cat('  # Basic scatter with correlation\n')
cat('  jjscatterstats(data = jjscatterstats_test,\n')
cat('                 dep = "ki67_index",\n')
cat('                 group = "tumor_size")\n\n')
cat('  # With marginal distributions\n')
cat('  jjscatterstats(data = jjscatterstats_clinical,\n')
cat('                 dep = "glucose",\n')
cat('                 group = "hemoglobin_a1c",\n')
cat('                 marginalType = "histogram")\n\n')
cat('  # Grouped by treatment\n')
cat('  jjscatterstats(data = jjscatterstats_test,\n')
cat('                 dep = "ki67_index",\n')
cat('                 group = "tumor_size",\n')
cat('                 grvar = "treatment_group")\n\n')

cat("═══════════════════════════════════════════════════════════\n")
