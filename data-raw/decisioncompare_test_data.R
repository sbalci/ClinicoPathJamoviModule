# ═══════════════════════════════════════════════════════════
# Test Data Generation: decisioncompare
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Compare Medical Decision Tests

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══ 1. BASIC TWO-TEST COMPARISON ═══
n_basic <- 200
# True disease status (gold standard)
true_disease <- rbinom(n_basic, 1, 0.30)  # 30% prevalence

decisioncompare_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_basic)),
  # Gold standard (perfect reference)
  GoldStandard = factor(true_disease, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Test 1: Sensitivity 0.85, Specificity 0.90
  Test1 = factor(
    ifelse(true_disease == 1,
           rbinom(n_basic, 1, 0.85),  # True positives
           rbinom(n_basic, 1, 0.10)), # False positives
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 2: Sensitivity 0.80, Specificity 0.85
  Test2 = factor(
    ifelse(true_disease == 1,
           rbinom(n_basic, 1, 0.80),
           rbinom(n_basic, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_basic, 58, 12)),
  sex = sample(c("Male", "Female"), n_basic, replace = TRUE)
)

# ═══ 2. THREE-TEST COMPARISON ═══
n_three <- 180
true_disease_three <- rbinom(n_three, 1, 0.35)

decisioncompare_threetest <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_three)),
  GoldStandard = factor(true_disease_three, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Test 1: High sensitivity (0.90), moderate specificity (0.85)
  Test1 = factor(
    ifelse(true_disease_three == 1,
           rbinom(n_three, 1, 0.90),
           rbinom(n_three, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 2: Balanced (Sens=0.85, Spec=0.88)
  Test2 = factor(
    ifelse(true_disease_three == 1,
           rbinom(n_three, 1, 0.85),
           rbinom(n_three, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 3: High specificity (0.92), moderate sensitivity (0.78)
  Test3 = factor(
    ifelse(true_disease_three == 1,
           rbinom(n_three, 1, 0.78),
           rbinom(n_three, 1, 0.08)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  test_site = sample(c("Site_A", "Site_B", "Site_C"), n_three, replace = TRUE)
)

# ═══ 3. IMAGING VS BIOMARKER COMPARISON ═══
n_imaging <- 220
true_disease_imaging <- rbinom(n_imaging, 1, 0.28)

decisioncompare_imaging <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_imaging)),
  Pathology = factor(true_disease_imaging, levels = c(0, 1), labels = c("Benign", "Malignant")),
  # CT Scan: Sensitivity 0.88, Specificity 0.85
  CT_Scan = factor(
    ifelse(true_disease_imaging == 1,
           rbinom(n_imaging, 1, 0.88),
           rbinom(n_imaging, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Normal", "Abnormal")
  ),
  # MRI: Sensitivity 0.92, Specificity 0.90
  MRI = factor(
    ifelse(true_disease_imaging == 1,
           rbinom(n_imaging, 1, 0.92),
           rbinom(n_imaging, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Normal", "Abnormal")
  ),
  # Biomarker: Sensitivity 0.80, Specificity 0.88
  Biomarker = factor(
    ifelse(true_disease_imaging == 1,
           rbinom(n_imaging, 1, 0.80),
           rbinom(n_imaging, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Normal", "Elevated")
  ),
  tumor_size_mm = round(rnorm(n_imaging, ifelse(true_disease_imaging == 1, 35, 15), 10))
)

# ═══ 4. SCREENING VS DIAGNOSTIC TESTS ═══
n_screen <- 250
true_disease_screen <- rbinom(n_screen, 1, 0.15)  # 15% prevalence (screening)

decisioncompare_screening <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_screen)),
  Biopsy = factor(true_disease_screen, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Screening Test: High sensitivity (0.95), lower specificity (0.80)
  ScreeningTest = factor(
    ifelse(true_disease_screen == 1,
           rbinom(n_screen, 1, 0.95),
           rbinom(n_screen, 1, 0.20)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Diagnostic Test: Lower sensitivity (0.85), high specificity (0.92)
  DiagnosticTest = factor(
    ifelse(true_disease_screen == 1,
           rbinom(n_screen, 1, 0.85),
           rbinom(n_screen, 1, 0.08)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_screen, 62, 8)),
  risk_score = rnorm(n_screen, ifelse(true_disease_screen == 1, 7, 3), 2)
)

# ═══ 5. INTER-RATER COMPARISON ═══
n_rater <- 150
true_disease_rater <- rbinom(n_rater, 1, 0.32)

decisioncompare_raters <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_rater)),
  ConsensusPanel = factor(true_disease_rater, levels = c(0, 1), labels = c("No Disease", "Disease")),
  # Rater 1: Experienced (Sens=0.88, Spec=0.90)
  Rater1 = factor(
    ifelse(true_disease_rater == 1,
           rbinom(n_rater, 1, 0.88),
           rbinom(n_rater, 1, 0.10)),
    levels = c(0, 1),
    labels = c("No Disease", "Disease")
  ),
  # Rater 2: Moderate experience (Sens=0.82, Spec=0.85)
  Rater2 = factor(
    ifelse(true_disease_rater == 1,
           rbinom(n_rater, 1, 0.82),
           rbinom(n_rater, 1, 0.15)),
    levels = c(0, 1),
    labels = c("No Disease", "Disease")
  ),
  # Rater 3: Junior (Sens=0.75, Spec=0.82)
  Rater3 = factor(
    ifelse(true_disease_rater == 1,
           rbinom(n_rater, 1, 0.75),
           rbinom(n_rater, 1, 0.18)),
    levels = c(0, 1),
    labels = c("No Disease", "Disease")
  ),
  case_difficulty = sample(c("Easy", "Moderate", "Difficult"), n_rater, replace = TRUE)
)

# ═══ 6. INDETERMINATE RESULTS DATA ═══
n_indet <- 170
true_disease_indet <- rbinom(n_indet, 1, 0.30)

decisioncompare_indeterminate <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_indet)),
  GoldStandard = factor(true_disease_indet, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Test 1: With some indeterminate results
  Test1 = factor(
    ifelse(true_disease_indet == 1,
           sample(c(0, 1, 2), n_indet, replace = TRUE, prob = c(0.10, 0.80, 0.10)),
           sample(c(0, 1, 2), n_indet, replace = TRUE, prob = c(0.80, 0.10, 0.10))),
    levels = c(0, 1, 2),
    labels = c("Negative", "Positive", "Indeterminate")
  ),
  # Test 2: Standard binary
  Test2 = factor(
    ifelse(true_disease_indet == 1,
           rbinom(n_indet, 1, 0.85),
           rbinom(n_indet, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_indet, 55, 13))
)

# ═══ 7. PERFECT TEST VS IMPERFECT ═══
n_perfect <- 120
true_disease_perfect <- rbinom(n_perfect, 1, 0.30)

decisioncompare_perfect <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_perfect)),
  GoldStandard = factor(true_disease_perfect, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Perfect Test: Sensitivity 1.0, Specificity 1.0
  PerfectTest = factor(true_disease_perfect, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Imperfect Test: Sensitivity 0.85, Specificity 0.88
  ImperfectTest = factor(
    ifelse(true_disease_perfect == 1,
           rbinom(n_perfect, 1, 0.85),
           rbinom(n_perfect, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_perfect, 60, 11))
)

# ═══ 8. POOR TESTS (Low accuracy) ═══
n_poor <- 140
true_disease_poor <- rbinom(n_poor, 1, 0.30)

decisioncompare_poor <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_poor)),
  GoldStandard = factor(true_disease_poor, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Poor Test 1: Sensitivity 0.60, Specificity 0.65
  PoorTest1 = factor(
    ifelse(true_disease_poor == 1,
           rbinom(n_poor, 1, 0.60),
           rbinom(n_poor, 1, 0.35)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Poor Test 2: Sensitivity 0.55, Specificity 0.70
  PoorTest2 = factor(
    ifelse(true_disease_poor == 1,
           rbinom(n_poor, 1, 0.55),
           rbinom(n_poor, 1, 0.30)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_poor, 57, 12))
)

# ═══ 9. RARE DISEASE DATA ═══
n_rare <- 300
true_disease_rare <- rbinom(n_rare, 1, 0.05)  # 5% prevalence

decisioncompare_rare <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_rare)),
  GoldStandard = factor(true_disease_rare, levels = c(0, 1), labels = c("Negative", "Positive")),
  # Test 1: High sensitivity for rare disease detection
  Test1 = factor(
    ifelse(true_disease_rare == 1,
           rbinom(n_rare, 1, 0.90),
           rbinom(n_rare, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 2: High specificity to reduce false alarms
  Test2 = factor(
    ifelse(true_disease_rare == 1,
           rbinom(n_rare, 1, 0.85),
           rbinom(n_rare, 1, 0.05)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  screening_round = sample(1:5, n_rare, replace = TRUE)
)

# ═══ 10. COMMON DISEASE DATA ═══
n_common <- 160
true_disease_common <- rbinom(n_common, 1, 0.60)  # 60% prevalence

decisioncompare_common <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_common)),
  GoldStandard = factor(true_disease_common, levels = c(0, 1), labels = c("Negative", "Positive")),
  Test1 = factor(
    ifelse(true_disease_common == 1,
           rbinom(n_common, 1, 0.85),
           rbinom(n_common, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(true_disease_common == 1,
           rbinom(n_common, 1, 0.82),
           rbinom(n_common, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  clinical_setting = sample(c("Inpatient", "Outpatient"), n_common, replace = TRUE)
)

# ═══ 11. IDENTICAL TESTS ═══
n_identical <- 100
true_disease_identical <- rbinom(n_identical, 1, 0.30)
test_result_identical <- ifelse(true_disease_identical == 1,
                                rbinom(n_identical, 1, 0.85),
                                rbinom(n_identical, 1, 0.12))

decisioncompare_identical <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_identical)),
  GoldStandard = factor(true_disease_identical, levels = c(0, 1), labels = c("Negative", "Positive")),
  Test1 = factor(test_result_identical, levels = c(0, 1), labels = c("Negative", "Positive")),
  Test2 = factor(test_result_identical, levels = c(0, 1), labels = c("Negative", "Positive")),
  age = round(rnorm(n_identical, 58, 10))
)

# ═══ 12. MISSING DATA ═══
n_missing <- 150
true_disease_missing <- rbinom(n_missing, 1, 0.30)

decisioncompare_missing <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_missing)),
  GoldStandard = factor(true_disease_missing, levels = c(0, 1), labels = c("Negative", "Positive")),
  Test1 = factor(
    ifelse(true_disease_missing == 1,
           rbinom(n_missing, 1, 0.85),
           rbinom(n_missing, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(true_disease_missing == 1,
           rbinom(n_missing, 1, 0.80),
           rbinom(n_missing, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test3 = factor(
    ifelse(true_disease_missing == 1,
           rbinom(n_missing, 1, 0.82),
           rbinom(n_missing, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_missing, 58, 12))
)
# Add missing values
decisioncompare_missing$GoldStandard[sample(n_missing, 8)] <- NA
decisioncompare_missing$Test1[sample(n_missing, 10)] <- NA
decisioncompare_missing$Test2[sample(n_missing, 7)] <- NA
decisioncompare_missing$Test3[sample(n_missing, 12)] <- NA

# ═══ 13. SMALL SAMPLE DATA ═══
n_small <- 30
true_disease_small <- rbinom(n_small, 1, 0.30)

decisioncompare_small <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_small)),
  GoldStandard = factor(true_disease_small, levels = c(0, 1), labels = c("Negative", "Positive")),
  Test1 = factor(
    ifelse(true_disease_small == 1,
           rbinom(n_small, 1, 0.85),
           rbinom(n_small, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(true_disease_small == 1,
           rbinom(n_small, 1, 0.80),
           rbinom(n_small, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_small, 55, 10))
)

# ═══ 14. LARGE SAMPLE DATA ═══
n_large <- 500
true_disease_large <- rbinom(n_large, 1, 0.28)

decisioncompare_large <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_large)),
  GoldStandard = factor(true_disease_large, levels = c(0, 1), labels = c("Negative", "Positive")),
  Test1 = factor(
    ifelse(true_disease_large == 1,
           rbinom(n_large, 1, 0.87),
           rbinom(n_large, 1, 0.11)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(true_disease_large == 1,
           rbinom(n_large, 1, 0.84),
           rbinom(n_large, 1, 0.13)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test3 = factor(
    ifelse(true_disease_large == 1,
           rbinom(n_large, 1, 0.81),
           rbinom(n_large, 1, 0.09)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_large, 59, 13)),
  sex = sample(c("Male", "Female"), n_large, replace = TRUE),
  study_center = sample(paste0("Center_", 1:8), n_large, replace = TRUE)
)

# ═══ SAVE ALL DATASETS ═══
datasets <- list(
  decisioncompare_test = decisioncompare_test,
  decisioncompare_threetest = decisioncompare_threetest,
  decisioncompare_imaging = decisioncompare_imaging,
  decisioncompare_screening = decisioncompare_screening,
  decisioncompare_raters = decisioncompare_raters,
  decisioncompare_indeterminate = decisioncompare_indeterminate,
  decisioncompare_perfect = decisioncompare_perfect,
  decisioncompare_poor = decisioncompare_poor,
  decisioncompare_rare = decisioncompare_rare,
  decisioncompare_common = decisioncompare_common,
  decisioncompare_identical = decisioncompare_identical,
  decisioncompare_missing = decisioncompare_missing,
  decisioncompare_small = decisioncompare_small,
  decisioncompare_large = decisioncompare_large
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

cat("✅ Decision Compare test data: 14 datasets × 4 formats = 56 files\n")
