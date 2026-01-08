# ═══════════════════════════════════════════════════════════
# Test Data Generation: decision
# ═══════════════════════════════════════════════════════════
# Function: Medical Decision Analysis
# Purpose: Evaluate diagnostic test performance against gold standard
# Statistics: Sensitivity, Specificity, PPV, NPV, Likelihood Ratios
# ═══════════════════════════════════════════════════════════

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Helper function to create realistic test results
# ═══════════════════════════════════════════════════════════
create_test_results <- function(n, disease_status, sensitivity, specificity) {
  # For diseased patients
  diseased_tests <- rbinom(sum(disease_status == 1), 1, sensitivity)
  # For healthy patients
  healthy_tests <- rbinom(sum(disease_status == 0), 1, 1 - specificity)

  results <- numeric(n)
  results[disease_status == 1] <- diseased_tests
  results[disease_status == 0] <- healthy_tests

  return(results)
}

# ═══════════════════════════════════════════════════════════
# 1. decision_test: Basic Two-Level Diagnostic Test
# ═══════════════════════════════════════════════════════════
n_basic <- 200
disease_basic <- rbinom(n_basic, 1, 0.30)  # 30% prevalence

decision_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_basic)),
  GoldStandard = factor(disease_basic, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(
    create_test_results(n_basic, disease_basic,
                       sensitivity = 0.85, specificity = 0.90),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_basic, 55, 12)),
  sex = factor(sample(c("Female", "Male"), n_basic, replace = TRUE))
)

# ═══════════════════════════════════════════════════════════
# 2. decision_screening: Cancer Screening Test (Low Prevalence)
# ═══════════════════════════════════════════════════════════
n_screen <- 250
disease_screen <- rbinom(n_screen, 1, 0.05)  # 5% prevalence (screening)

decision_screening <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_screen)),
  Biopsy = factor(disease_screen, levels = c(0, 1),
                  labels = c("Benign", "Malignant")),
  ScreeningTest = factor(
    create_test_results(n_screen, disease_screen,
                       sensitivity = 0.92, specificity = 0.88),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_screen, 62, 10)),
  risk_factor = factor(sample(c("Low", "Medium", "High"), n_screen,
                             replace = TRUE, prob = c(0.6, 0.3, 0.1)))
)

# ═══════════════════════════════════════════════════════════
# 3. decision_diagnostic: Clinical Diagnostic Test (High Prevalence)
# ═══════════════════════════════════════════════════════════
n_diag <- 180
disease_diag <- rbinom(n_diag, 1, 0.60)  # 60% prevalence (clinical setting)

decision_diagnostic <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_diag)),
  GoldStandard = factor(disease_diag, levels = c(0, 1),
                        labels = c("Absent", "Present")),
  ClinicalTest = factor(
    create_test_results(n_diag, disease_diag,
                       sensitivity = 0.88, specificity = 0.85),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  symptom_severity = round(runif(n_diag, 1, 10)),
  duration_days = round(rexp(n_diag, 1/14))
)

# ═══════════════════════════════════════════════════════════
# 4. decision_biomarker: Cardiac Biomarker Test
# ═══════════════════════════════════════════════════════════
n_cardiac <- 220
disease_cardiac <- rbinom(n_cardiac, 1, 0.35)

decision_biomarker <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_cardiac)),
  Angiography = factor(disease_cardiac, levels = c(0, 1),
                       labels = c("No_MI", "MI")),
  Troponin = factor(
    create_test_results(n_cardiac, disease_cardiac,
                       sensitivity = 0.95, specificity = 0.92),
    levels = c(0, 1),
    labels = c("Normal", "Elevated")
  ),
  chest_pain = factor(sample(c("None", "Mild", "Severe"), n_cardiac,
                            replace = TRUE)),
  ecg_changes = factor(sample(c("Normal", "Abnormal"), n_cardiac,
                             replace = TRUE, prob = c(0.6, 0.4)))
)

# ═══════════════════════════════════════════════════════════
# 5. decision_imaging: Imaging Test Evaluation
# ═══════════════════════════════════════════════════════════
n_imaging <- 160
disease_imaging <- rbinom(n_imaging, 1, 0.40)

decision_imaging <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_imaging)),
  Pathology = factor(disease_imaging, levels = c(0, 1),
                     labels = c("Benign", "Malignant")),
  CT_Scan = factor(
    create_test_results(n_imaging, disease_imaging,
                       sensitivity = 0.87, specificity = 0.83),
    levels = c(0, 1),
    labels = c("Normal", "Abnormal")
  ),
  tumor_size_cm = round(runif(n_imaging, 0.5, 8), 1),
  location = factor(sample(c("Lung", "Liver", "Kidney", "Pancreas"),
                          n_imaging, replace = TRUE))
)

# ═══════════════════════════════════════════════════════════
# 6. decision_infectious: Infectious Disease Rapid Test
# ═══════════════════════════════════════════════════════════
n_infect <- 200
disease_infect <- rbinom(n_infect, 1, 0.25)

decision_infectious <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_infect)),
  Culture = factor(disease_infect, levels = c(0, 1),
                   labels = c("Negative", "Positive")),
  RapidTest = factor(
    create_test_results(n_infect, disease_infect,
                       sensitivity = 0.82, specificity = 0.95),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  fever = factor(sample(c("No", "Yes"), n_infect, replace = TRUE)),
  symptom_onset_days = round(rexp(n_infect, 1/3))
)

# ═══════════════════════════════════════════════════════════
# 7. decision_small: Small Sample (Edge Case)
# ═══════════════════════════════════════════════════════════
n_small <- 30
disease_small <- rbinom(n_small, 1, 0.40)

decision_small <- tibble(
  patient_id = paste0("PT", sprintf("%02d", 1:n_small)),
  GoldStandard = factor(disease_small, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(
    create_test_results(n_small, disease_small,
                       sensitivity = 0.85, specificity = 0.85),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  )
)

# ═══════════════════════════════════════════════════════════
# 8. decision_large: Large Sample
# ═══════════════════════════════════════════════════════════
n_large <- 500
disease_large <- rbinom(n_large, 1, 0.30)

decision_large <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_large)),
  GoldStandard = factor(disease_large, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(
    create_test_results(n_large, disease_large,
                       sensitivity = 0.88, specificity = 0.90),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_large, 58, 15)),
  comorbidities = round(rpois(n_large, 1.5))
)

# ═══════════════════════════════════════════════════════════
# 9. decision_perfect: Perfect Test Performance
# ═══════════════════════════════════════════════════════════
n_perfect <- 150
disease_perfect <- rbinom(n_perfect, 1, 0.35)

decision_perfect <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_perfect)),
  GoldStandard = factor(disease_perfect, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  PerfectTest = factor(disease_perfect, levels = c(0, 1),
                       labels = c("Negative", "Positive")),
  age = round(rnorm(n_perfect, 60, 12))
)

# ═══════════════════════════════════════════════════════════
# 10. decision_poor: Poor Test Performance
# ═══════════════════════════════════════════════════════════
n_poor <- 140
disease_poor <- rbinom(n_poor, 1, 0.30)

decision_poor <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_poor)),
  GoldStandard = factor(disease_poor, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  PoorTest = factor(
    create_test_results(n_poor, disease_poor,
                       sensitivity = 0.60, specificity = 0.65),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  )
)

# ═══════════════════════════════════════════════════════════
# 11. decision_rare: Rare Disease (Very Low Prevalence)
# ═══════════════════════════════════════════════════════════
n_rare <- 300
disease_rare <- rbinom(n_rare, 1, 0.02)  # 2% prevalence

decision_rare <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_rare)),
  GoldStandard = factor(disease_rare, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(
    create_test_results(n_rare, disease_rare,
                       sensitivity = 0.90, specificity = 0.95),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  genetic_marker = factor(sample(c("Absent", "Present"), n_rare,
                                replace = TRUE, prob = c(0.9, 0.1)))
)

# ═══════════════════════════════════════════════════════════
# 12. decision_common: Common Disease (Very High Prevalence)
# ═══════════════════════════════════════════════════════════
n_common <- 180
disease_common <- rbinom(n_common, 1, 0.75)  # 75% prevalence

decision_common <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_common)),
  GoldStandard = factor(disease_common, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(
    create_test_results(n_common, disease_common,
                       sensitivity = 0.85, specificity = 0.88),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  )
)

# ═══════════════════════════════════════════════════════════
# 13. decision_missing: Dataset with Missing Values
# ═══════════════════════════════════════════════════════════
n_missing <- 150
disease_missing <- rbinom(n_missing, 1, 0.35)

decision_missing <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_missing)),
  GoldStandard = factor(disease_missing, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(
    create_test_results(n_missing, disease_missing,
                       sensitivity = 0.85, specificity = 0.87),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_missing, 58, 14))
)

# Introduce ~5% missing values
missing_idx_gold <- sample(1:n_missing, round(0.05 * n_missing))
missing_idx_test <- sample(1:n_missing, round(0.05 * n_missing))
decision_missing$GoldStandard[missing_idx_gold] <- NA
decision_missing$NewTest[missing_idx_test] <- NA

# ═══════════════════════════════════════════════════════════
# 14. decision_multilevel: Three-Level Variables (Negative/Indeterminate/Positive)
# ═══════════════════════════════════════════════════════════
n_multi <- 180
disease_multi <- rbinom(n_multi, 1, 0.35)

# Create test with indeterminate results
test_results_multi <- create_test_results(n_multi, disease_multi,
                                         sensitivity = 0.85,
                                         specificity = 0.88)
# Convert some results to indeterminate
indeterminate_idx <- sample(1:n_multi, round(0.10 * n_multi))
test_results_multi[indeterminate_idx] <- 2

decision_multilevel <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_multi)),
  GoldStandard = factor(disease_multi, levels = c(0, 1),
                        labels = c("Negative", "Positive")),
  NewTest = factor(test_results_multi, levels = c(0, 1, 2),
                   labels = c("Negative", "Positive", "Indeterminate"))
)

# ═══════════════════════════════════════════════════════════
# 15. decision_pathology: Pathology Specimen Evaluation
# ═══════════════════════════════════════════════════════════
n_path <- 190
disease_path <- rbinom(n_path, 1, 0.45)

decision_pathology <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_path)),
  Biopsy = factor(disease_path, levels = c(0, 1),
                  labels = c("Benign", "Malignant")),
  FrozenSection = factor(
    create_test_results(n_path, disease_path,
                       sensitivity = 0.93, specificity = 0.96),
    levels = c(0, 1),
    labels = c("Benign", "Malignant")
  ),
  specimen_type = factor(sample(c("Core_Biopsy", "Excision", "FNA"),
                               n_path, replace = TRUE)),
  tumor_grade = factor(sample(c("Low", "Intermediate", "High"),
                             n_path, replace = TRUE))
)

# ═══════════════════════════════════════════════════════════
# 16. decision_pointofcare: Point-of-Care Test
# ═══════════════════════════════════════════════════════════
n_poc <- 210
disease_poc <- rbinom(n_poc, 1, 0.28)

decision_pointofcare <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_poc)),
  LabTest = factor(disease_poc, levels = c(0, 1),
                   labels = c("Negative", "Positive")),
  PointOfCare = factor(
    create_test_results(n_poc, disease_poc,
                       sensitivity = 0.78, specificity = 0.92),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  setting = factor(sample(c("Emergency", "Clinic", "Home"),
                         n_poc, replace = TRUE)),
  urgency = factor(sample(c("Routine", "Urgent", "Critical"),
                         n_poc, replace = TRUE))
)

# ═══════════════════════════════════════════════════════════
# Save datasets in multiple formats
# ═══════════════════════════════════════════════════════════
datasets <- list(
  decision_test = decision_test,
  decision_screening = decision_screening,
  decision_diagnostic = decision_diagnostic,
  decision_biomarker = decision_biomarker,
  decision_imaging = decision_imaging,
  decision_infectious = decision_infectious,
  decision_small = decision_small,
  decision_large = decision_large,
  decision_perfect = decision_perfect,
  decision_poor = decision_poor,
  decision_rare = decision_rare,
  decision_common = decision_common,
  decision_missing = decision_missing,
  decision_multilevel = decision_multilevel,
  decision_pathology = decision_pathology,
  decision_pointofcare = decision_pointofcare
)

# Save as RDA
for (name in names(datasets)) {
  assign(name, datasets[[name]])
  save(list = name, file = here("data", paste0(name, ".rda")), compress = "xz")
}

# Save as CSV
for (name in names(datasets)) {
  write.csv(datasets[[name]],
            file = here("data", paste0(name, ".csv")),
            row.names = FALSE)
}

# Save as XLSX
for (name in names(datasets)) {
  write_xlsx(datasets[[name]],
             path = here("data", paste0(name, ".xlsx")))
}

# Save as OMV (jamovi format)
for (name in names(datasets)) {
  write_omv(datasets[[name]],
            here("data", paste0(name, ".omv")))
}

cat("✓ Generated 16 datasets for decision function\n")
cat("✓ Created", length(datasets) * 4, "files (RDA, CSV, XLSX, OMV formats)\n")
cat("\nDatasets:\n")
for (name in names(datasets)) {
  cat(sprintf("  - %s (n=%d)\n", name, nrow(datasets[[name]])))
}
