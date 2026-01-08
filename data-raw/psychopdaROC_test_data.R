# ═══════════════════════════════════════════════════════════
# Test Data Generation: psychopdaROC
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Advanced ROC Analysis with Optimal Cutpoint Determination

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══ 1. BASIC DIAGNOSTIC TEST ═══
n_basic <- 200
psychopdaROC_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_basic)),
  disease_status = sample(c("Disease", "Healthy"), n_basic, replace = TRUE, prob = c(0.3, 0.7)),
  biomarker = rnorm(n_basic, mean = ifelse(disease_status == "Disease", 75, 50), sd = 15),
  age = round(rnorm(n_basic, 60, 12)),
  sex = sample(c("Male", "Female"), n_basic, replace = TRUE)
)

# ═══ 2. CANCER SCREENING DATA ═══
n_screen <- 250
psychopdaROC_screening <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_screen)),
  cancer = sample(c("Cancer", "No_Cancer"), n_screen, replace = TRUE, prob = c(0.15, 0.85)),
  psa_level = round(exp(rnorm(n_screen, log(ifelse(cancer == "Cancer", 12, 4)), 0.8)), 2),
  ca125 = round(rnorm(n_screen, ifelse(cancer == "Cancer", 65, 25), 18), 1),
  age = round(rnorm(n_screen, 65, 10)),
  risk_factors = sample(c("None", "Family_History", "Multiple"), n_screen, replace = TRUE)
)

# ═══ 3. CARDIAC BIOMARKER DATA ═══
n_cardiac <- 180
psychopdaROC_cardiac <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_cardiac)),
  mi_status = sample(c("MI", "No_MI"), n_cardiac, replace = TRUE, prob = c(0.25, 0.75)),
  troponin = round(rnorm(n_cardiac, ifelse(mi_status == "MI", 2.5, 0.3), 1.2), 2),
  creatinine = round(rnorm(n_cardiac, ifelse(mi_status == "MI", 1.3, 0.9), 0.3), 2),
  bnp = round(rnorm(n_cardiac, ifelse(mi_status == "MI", 850, 200), 300), 0)
)

# ═══ 4. MULTIPLE BIOMARKERS COMPARISON ═══
n_multi <- 220
psychopdaROC_multibiomarker <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_multi)),
  diagnosis = sample(c("Positive", "Negative"), n_multi, replace = TRUE, prob = c(0.35, 0.65)),
  marker1 = rnorm(n_multi, ifelse(diagnosis == "Positive", 100, 70), 20),
  marker2 = rnorm(n_multi, ifelse(diagnosis == "Positive", 85, 55), 18),
  marker3 = rnorm(n_multi, ifelse(diagnosis == "Positive", 90, 65), 22),
  combined_score = NA  # Will be calculated
)
psychopdaROC_multibiomarker$combined_score <-
  (psychopdaROC_multibiomarker$marker1 + psychopdaROC_multibiomarker$marker2 +
   psychopdaROC_multibiomarker$marker3) / 3

# ═══ 5. SUBGROUP ANALYSIS DATA ═══
n_subgroup <- 200
psychopdaROC_subgroup <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_subgroup)),
  disease = sample(c("Disease", "Healthy"), n_subgroup, replace = TRUE, prob = c(0.3, 0.7)),
  test_score = rnorm(n_subgroup, ifelse(disease == "Disease", 70, 45), 18),
  age_group = sample(c("Young", "Middle", "Elderly"), n_subgroup, replace = TRUE),
  sex = sample(c("Male", "Female"), n_subgroup, replace = TRUE)
)

# ═══ 6. PERFECT SEPARATION DATA ═══
n_perfect <- 100
psychopdaROC_perfect <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_perfect)),
  condition = sample(c("Positive", "Negative"), n_perfect, replace = TRUE),
  perfect_test = ifelse(condition == "Positive",
                        runif(n_perfect, 80, 100),
                        runif(n_perfect, 0, 20))
)

# ═══ 7. POOR DISCRIMINATION DATA ═══
n_poor <- 150
psychopdaROC_poor <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_poor)),
  status = sample(c("Case", "Control"), n_poor, replace = TRUE),
  poor_marker = rnorm(n_poor, 50, 15)  # No difference between groups
)

# ═══ 8. OVERLAPPING DISTRIBUTIONS ═══
n_overlap <- 190
psychopdaROC_overlap <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_overlap)),
  diagnosis = sample(c("Diseased", "Non_Diseased"), n_overlap, replace = TRUE, prob = c(0.4, 0.6)),
  test_value = rnorm(n_overlap, ifelse(diagnosis == "Diseased", 60, 55), 18)  # Moderate overlap
)

# ═══ 9. RARE DISEASE DATA ═══
n_rare <- 300
psychopdaROC_rare <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_rare)),
  rare_disease = sample(c("Disease", "No_Disease"), n_rare, replace = TRUE, prob = c(0.05, 0.95)),
  biomarker = rnorm(n_rare, ifelse(rare_disease == "Disease", 80, 45), 15)
)

# ═══ 10. COST-BENEFIT DATA ═══
n_cost <- 160
psychopdaROC_costbenefit <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_cost)),
  outcome = sample(c("Event", "No_Event"), n_cost, replace = TRUE, prob = c(0.28, 0.72)),
  risk_score = round(rnorm(n_cost, ifelse(outcome == "Event", 75, 50), 20), 1),
  false_positive_cost = 100,  # Cost of false positive
  false_negative_cost = 1000  # Cost of false negative (much higher)
)

# ═══ 11. CONTINUOUS SPECTRUM DATA ═══
n_spectrum <- 170
psychopdaROC_spectrum <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_spectrum)),
  severity = sample(c("Mild", "Moderate", "Severe"), n_spectrum, replace = TRUE),
  binary_status = ifelse(severity == "Mild", "Negative", "Positive"),
  continuous_marker = case_when(
    severity == "Mild" ~ rnorm(n_spectrum, 40, 10),
    severity == "Moderate" ~ rnorm(n_spectrum, 60, 12),
    severity == "Severe" ~ rnorm(n_spectrum, 85, 15)
  )
)

# ═══ 12. TIME-DEPENDENT BIOMARKER ═══
n_time <- 140
psychopdaROC_timedep <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_time)),
  outcome = sample(c("Event", "No_Event"), n_time, replace = TRUE, prob = c(0.32, 0.68)),
  baseline_marker = rnorm(n_time, ifelse(outcome == "Event", 70, 50), 15),
  followup_marker = baseline_marker + rnorm(n_time,
                                             ifelse(outcome == "Event", 15, -5), 8),
  time_to_outcome = round(runif(n_time, 1, 36))
)

# ═══ 13. EDGE CASES - SMALL DATASET ═══
psychopdaROC_small <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:30)),
  class = sample(c("Positive", "Negative"), 30, replace = TRUE),
  marker = rnorm(30, ifelse(class == "Positive", 65, 45), 15)
)

# ═══ 14. EDGE CASES - IMBALANCED CLASSES ═══
n_imbalanced <- 200
psychopdaROC_imbalanced <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_imbalanced)),
  rare_outcome = sample(c("Event", "No_Event"), n_imbalanced, replace = TRUE, prob = c(0.02, 0.98)),
  predictor = rnorm(n_imbalanced, ifelse(rare_outcome == "Event", 90, 50), 18)
)

# ═══ 15. EDGE CASES - MISSING DATA ═══
n_missing <- 150
psychopdaROC_missing <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_missing)),
  diagnosis = sample(c("Disease", "Healthy"), n_missing, replace = TRUE),
  test_a = rnorm(n_missing, ifelse(diagnosis == "Disease", 70, 50), 15),
  test_b = rnorm(n_missing, ifelse(diagnosis == "Disease", 65, 45), 18),
  covariate = sample(c("A", "B", "C"), n_missing, replace = TRUE)
)
# Add missing values
psychopdaROC_missing$diagnosis[sample(n_missing, 8)] <- NA
psychopdaROC_missing$test_a[sample(n_missing, 12)] <- NA
psychopdaROC_missing$test_b[sample(n_missing, 10)] <- NA

# ═══ 16. EDGE CASES - CONSTANT PREDICTOR ═══
psychopdaROC_constant <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:80)),
  outcome = sample(c("Positive", "Negative"), 80, replace = TRUE),
  constant_marker = rep(50, 80)  # No variation
)

# ═══ 17. LARGE DATASET ═══
n_large <- 500
psychopdaROC_large <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_large)),
  disease_status = sample(c("Disease", "No_Disease"), n_large, replace = TRUE, prob = c(0.3, 0.7)),
  biomarker1 = rnorm(n_large, ifelse(disease_status == "Disease", 75, 50), 18),
  biomarker2 = rnorm(n_large, ifelse(disease_status == "Disease", 68, 48), 16),
  age = round(rnorm(n_large, 62, 13)),
  sex = sample(c("Male", "Female"), n_large, replace = TRUE),
  site = sample(paste0("Site_", 1:10), n_large, replace = TRUE),
  risk_category = sample(c("Low", "Intermediate", "High"), n_large, replace = TRUE)
)

# ═══ SAVE ALL DATASETS ═══
datasets <- list(
  psychopdaROC_test = psychopdaROC_test,
  psychopdaROC_screening = psychopdaROC_screening,
  psychopdaROC_cardiac = psychopdaROC_cardiac,
  psychopdaROC_multibiomarker = psychopdaROC_multibiomarker,
  psychopdaROC_subgroup = psychopdaROC_subgroup,
  psychopdaROC_perfect = psychopdaROC_perfect,
  psychopdaROC_poor = psychopdaROC_poor,
  psychopdaROC_overlap = psychopdaROC_overlap,
  psychopdaROC_rare = psychopdaROC_rare,
  psychopdaROC_costbenefit = psychopdaROC_costbenefit,
  psychopdaROC_spectrum = psychopdaROC_spectrum,
  psychopdaROC_timedep = psychopdaROC_timedep,
  psychopdaROC_small = psychopdaROC_small,
  psychopdaROC_imbalanced = psychopdaROC_imbalanced,
  psychopdaROC_missing = psychopdaROC_missing,
  psychopdaROC_constant = psychopdaROC_constant,
  psychopdaROC_large = psychopdaROC_large
)

for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  save_name <- dataset_name
  assign(save_name, data)
  save(list = save_name, file = here::here("data", paste0(dataset_name, ".rda")), compress = "xz")
  write.csv(data, file = here::here("data", paste0(dataset_name, ".csv")), row.names = FALSE)
  writexl::write_xlsx(data, path = here::here("data", paste0(dataset_name, ".xlsx")))
  jmvReadWrite::write_omv(dtaFrm = data, fleOut = here::here("data", paste0(dataset_name, ".omv")), frcWrt = TRUE)
}

cat("✅ PsychoPDA ROC test data: 17 datasets × 4 formats = 68 files\n")
