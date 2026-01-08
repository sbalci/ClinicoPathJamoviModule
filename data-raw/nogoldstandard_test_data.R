# ═══════════════════════════════════════════════════════════
# Test Data Generation: nogoldstandard
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Analysis Without Gold Standard - Multiple Test Analysis

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══ 1. BASIC TWO-TEST DATA ═══
n_basic <- 200
# Simulate latent disease status (unknown in practice)
disease_basic <- rbinom(n_basic, 1, 0.3)  # 30% prevalence

nogoldstandard_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_basic)),
  # Test 1: Sensitivity 0.85, Specificity 0.85
  Test1 = factor(
    ifelse(disease_basic == 1,
           rbinom(n_basic, 1, 0.85),  # True positives
           rbinom(n_basic, 1, 0.15)), # False positives
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 2: Sensitivity 0.80, Specificity 0.90
  Test2 = factor(
    ifelse(disease_basic == 1,
           rbinom(n_basic, 1, 0.80),  # True positives
           rbinom(n_basic, 1, 0.10)), # False positives
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_basic, 55, 12)),
  sex = sample(c("Male", "Female"), n_basic, replace = TRUE)
)

# ═══ 2. THREE-TEST PATHOLOGY DATA ═══
n_path <- 180
disease_path <- rbinom(n_path, 1, 0.25)

nogoldstandard_pathology <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_path)),
  # Pathologist 1: Sensitivity 0.88, Specificity 0.92
  Pathologist1 = factor(
    ifelse(disease_path == 1,
           rbinom(n_path, 1, 0.88),
           rbinom(n_path, 1, 0.08)),
    levels = c(0, 1),
    labels = c("Benign", "Malignant")
  ),
  # Pathologist 2: Sensitivity 0.85, Specificity 0.90
  Pathologist2 = factor(
    ifelse(disease_path == 1,
           rbinom(n_path, 1, 0.85),
           rbinom(n_path, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Benign", "Malignant")
  ),
  # Pathologist 3: Sensitivity 0.82, Specificity 0.93
  Pathologist3 = factor(
    ifelse(disease_path == 1,
           rbinom(n_path, 1, 0.82),
           rbinom(n_path, 1, 0.07)),
    levels = c(0, 1),
    labels = c("Benign", "Malignant")
  ),
  tumor_site = sample(c("Lung", "Breast", "Colon", "Prostate"), n_path, replace = TRUE),
  specimen_quality = sample(c("Adequate", "Limited", "Poor"), n_path, replace = TRUE,
                           prob = c(0.7, 0.2, 0.1))
)

# ═══ 3. FOUR-TEST TUMOR MARKER DATA ═══
n_marker <- 220
disease_marker <- rbinom(n_marker, 1, 0.20)  # 20% prevalence (cancer screening)

nogoldstandard_tumormarker <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_marker)),
  # Marker 1: Sensitivity 0.75, Specificity 0.88
  CA125 = factor(
    ifelse(disease_marker == 1,
           rbinom(n_marker, 1, 0.75),
           rbinom(n_marker, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Normal", "Elevated")
  ),
  # Marker 2: Sensitivity 0.70, Specificity 0.85
  HE4 = factor(
    ifelse(disease_marker == 1,
           rbinom(n_marker, 1, 0.70),
           rbinom(n_marker, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Normal", "Elevated")
  ),
  # Marker 3: Sensitivity 0.68, Specificity 0.90
  CEA = factor(
    ifelse(disease_marker == 1,
           rbinom(n_marker, 1, 0.68),
           rbinom(n_marker, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Normal", "Elevated")
  ),
  # Marker 4: Sensitivity 0.72, Specificity 0.87
  AFP = factor(
    ifelse(disease_marker == 1,
           rbinom(n_marker, 1, 0.72),
           rbinom(n_marker, 1, 0.13)),
    levels = c(0, 1),
    labels = c("Normal", "Elevated")
  ),
  age = round(rnorm(n_marker, 62, 10)),
  risk_category = sample(c("Low", "Moderate", "High"), n_marker, replace = TRUE)
)

# ═══ 4. FIVE-TEST SCREENING DATA ═══
n_screen <- 250
disease_screen <- rbinom(n_screen, 1, 0.15)  # 15% prevalence (screening setting)

nogoldstandard_screening <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_screen)),
  # Imaging Test: Sensitivity 0.82, Specificity 0.90
  Imaging = factor(
    ifelse(disease_screen == 1,
           rbinom(n_screen, 1, 0.82),
           rbinom(n_screen, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Normal", "Abnormal")
  ),
  # Clinical Exam: Sensitivity 0.65, Specificity 0.85
  ClinicalExam = factor(
    ifelse(disease_screen == 1,
           rbinom(n_screen, 1, 0.65),
           rbinom(n_screen, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Normal", "Abnormal")
  ),
  # Biomarker: Sensitivity 0.70, Specificity 0.88
  Biomarker = factor(
    ifelse(disease_screen == 1,
           rbinom(n_screen, 1, 0.70),
           rbinom(n_screen, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Normal", "Abnormal")
  ),
  # Questionnaire: Sensitivity 0.60, Specificity 0.75
  Questionnaire = factor(
    ifelse(disease_screen == 1,
           rbinom(n_screen, 1, 0.60),
           rbinom(n_screen, 1, 0.25)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # AI Algorithm: Sensitivity 0.88, Specificity 0.92
  AI_Algorithm = factor(
    ifelse(disease_screen == 1,
           rbinom(n_screen, 1, 0.88),
           rbinom(n_screen, 1, 0.08)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_screen, 58, 15)),
  screening_round = sample(1:5, n_screen, replace = TRUE)
)

# ═══ 5. HIGH AGREEMENT DATA (Tests highly correlated) ═══
n_high_agree <- 150
disease_high <- rbinom(n_high_agree, 1, 0.35)

nogoldstandard_highagreement <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_high_agree)),
  # Tests with high correlation (0.95 conditional independence)
  Test1 = factor(
    ifelse(disease_high == 1,
           rbinom(n_high_agree, 1, 0.90),
           rbinom(n_high_agree, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = NA,  # Will be created with high correlation to Test1
  age = round(rnorm(n_high_agree, 60, 10))
)
# Create highly correlated Test2
base_test2 <- ifelse(disease_high == 1,
                     rbinom(n_high_agree, 1, 0.88),
                     rbinom(n_high_agree, 1, 0.12))
# Make Test2 highly agree with Test1 (95% agreement)
agree_mask <- rbinom(n_high_agree, 1, 0.95)
nogoldstandard_highagreement$Test2 <- factor(
  ifelse(agree_mask == 1, as.numeric(nogoldstandard_highagreement$Test1) - 1, base_test2),
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# ═══ 6. LOW AGREEMENT DATA (Tests poorly correlated) ═══
n_low_agree <- 140
disease_low <- rbinom(n_low_agree, 1, 0.30)

nogoldstandard_lowagreement <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_low_agree)),
  # Test 1: Moderate sensitivity/specificity
  Test1 = factor(
    ifelse(disease_low == 1,
           rbinom(n_low_agree, 1, 0.70),
           rbinom(n_low_agree, 1, 0.20)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 2: Different characteristics, low correlation
  Test2 = factor(
    ifelse(disease_low == 1,
           rbinom(n_low_agree, 1, 0.65),
           rbinom(n_low_agree, 1, 0.25)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_low_agree, 58, 14))
)

# ═══ 7. PERFECT AGREEMENT DATA (All tests identical) ═══
n_perfect <- 100
disease_perfect <- rbinom(n_perfect, 1, 0.30)

nogoldstandard_perfect <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_perfect)),
  Test1 = factor(
    ifelse(disease_perfect == 1,
           rbinom(n_perfect, 1, 0.85),
           rbinom(n_perfect, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = NA,  # Will be identical to Test1
  age = round(rnorm(n_perfect, 55, 12))
)
nogoldstandard_perfect$Test2 <- nogoldstandard_perfect$Test1

# ═══ 8. RARE DISEASE DATA (Very low prevalence) ═══
n_rare <- 300
disease_rare <- rbinom(n_rare, 1, 0.05)  # 5% prevalence

nogoldstandard_rare <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_rare)),
  Test1 = factor(
    ifelse(disease_rare == 1,
           rbinom(n_rare, 1, 0.80),
           rbinom(n_rare, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(disease_rare == 1,
           rbinom(n_rare, 1, 0.75),
           rbinom(n_rare, 1, 0.08)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test3 = factor(
    ifelse(disease_rare == 1,
           rbinom(n_rare, 1, 0.78),
           rbinom(n_rare, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  screening_site = sample(paste0("Site_", 1:10), n_rare, replace = TRUE)
)

# ═══ 9. COMMON DISEASE DATA (High prevalence) ═══
n_common <- 170
disease_common <- rbinom(n_common, 1, 0.60)  # 60% prevalence

nogoldstandard_common <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_common)),
  Test1 = factor(
    ifelse(disease_common == 1,
           rbinom(n_common, 1, 0.85),
           rbinom(n_common, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(disease_common == 1,
           rbinom(n_common, 1, 0.82),
           rbinom(n_common, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  clinical_setting = sample(c("Inpatient", "Outpatient", "Emergency"), n_common, replace = TRUE)
)

# ═══ 10. ALL POSITIVE DATA (All tests positive) ═══
n_all_pos <- 80
nogoldstandard_allpositive <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_all_pos)),
  Test1 = factor(rep("Positive", n_all_pos), levels = c("Negative", "Positive")),
  Test2 = factor(rep("Positive", n_all_pos), levels = c("Negative", "Positive")),
  age = round(rnorm(n_all_pos, 55, 10))
)

# ═══ 11. ALL NEGATIVE DATA (All tests negative) ═══
n_all_neg <- 90
nogoldstandard_allnegative <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_all_neg)),
  Test1 = factor(rep("Negative", n_all_neg), levels = c("Negative", "Positive")),
  Test2 = factor(rep("Negative", n_all_neg), levels = c("Negative", "Positive")),
  age = round(rnorm(n_all_neg, 52, 11))
)

# ═══ 12. IMBALANCED TESTS (One test very sensitive, one very specific) ═══
n_imbal <- 160
disease_imbal <- rbinom(n_imbal, 1, 0.28)

nogoldstandard_imbalanced <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_imbal)),
  # Test 1: Very sensitive (0.95), but low specificity (0.70)
  Sensitive_Test = factor(
    ifelse(disease_imbal == 1,
           rbinom(n_imbal, 1, 0.95),
           rbinom(n_imbal, 1, 0.30)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Test 2: Very specific (0.95), but low sensitivity (0.70)
  Specific_Test = factor(
    ifelse(disease_imbal == 1,
           rbinom(n_imbal, 1, 0.70),
           rbinom(n_imbal, 1, 0.05)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_imbal, 57, 13))
)

# ═══ 13. DIAGNOSTIC VALIDATION DATA ═══
n_valid <- 190
disease_valid <- rbinom(n_valid, 1, 0.32)

nogoldstandard_validation <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_valid)),
  # New Test: Sensitivity 0.88, Specificity 0.90
  New_Test = factor(
    ifelse(disease_valid == 1,
           rbinom(n_valid, 1, 0.88),
           rbinom(n_valid, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Reference Test 1: Sensitivity 0.85, Specificity 0.88
  Reference1 = factor(
    ifelse(disease_valid == 1,
           rbinom(n_valid, 1, 0.85),
           rbinom(n_valid, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  # Reference Test 2: Sensitivity 0.82, Specificity 0.92
  Reference2 = factor(
    ifelse(disease_valid == 1,
           rbinom(n_valid, 1, 0.82),
           rbinom(n_valid, 1, 0.08)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  test_site = sample(c("Academic", "Community", "Private"), n_valid, replace = TRUE)
)

# ═══ 14. MISSING DATA ═══
n_miss <- 150
disease_miss <- rbinom(n_miss, 1, 0.30)

nogoldstandard_missing <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_miss)),
  Test1 = factor(
    ifelse(disease_miss == 1,
           rbinom(n_miss, 1, 0.85),
           rbinom(n_miss, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(disease_miss == 1,
           rbinom(n_miss, 1, 0.80),
           rbinom(n_miss, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test3 = factor(
    ifelse(disease_miss == 1,
           rbinom(n_miss, 1, 0.82),
           rbinom(n_miss, 1, 0.10)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_miss, 58, 12))
)
# Add missing values
nogoldstandard_missing$Test1[sample(n_miss, 10)] <- NA
nogoldstandard_missing$Test2[sample(n_miss, 8)] <- NA
nogoldstandard_missing$Test3[sample(n_miss, 12)] <- NA

# ═══ 15. SMALL SAMPLE DATA ═══
n_small <- 30
disease_small <- rbinom(n_small, 1, 0.30)

nogoldstandard_small <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_small)),
  Test1 = factor(
    ifelse(disease_small == 1,
           rbinom(n_small, 1, 0.85),
           rbinom(n_small, 1, 0.15)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(disease_small == 1,
           rbinom(n_small, 1, 0.80),
           rbinom(n_small, 1, 0.12)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  age = round(rnorm(n_small, 55, 10))
)

# ═══ 16. LARGE SAMPLE DATA ═══
n_large <- 500
disease_large <- rbinom(n_large, 1, 0.28)

nogoldstandard_large <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_large)),
  Test1 = factor(
    ifelse(disease_large == 1,
           rbinom(n_large, 1, 0.87),
           rbinom(n_large, 1, 0.13)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test2 = factor(
    ifelse(disease_large == 1,
           rbinom(n_large, 1, 0.84),
           rbinom(n_large, 1, 0.11)),
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  ),
  Test3 = factor(
    ifelse(disease_large == 1,
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
  nogoldstandard_test = nogoldstandard_test,
  nogoldstandard_pathology = nogoldstandard_pathology,
  nogoldstandard_tumormarker = nogoldstandard_tumormarker,
  nogoldstandard_screening = nogoldstandard_screening,
  nogoldstandard_highagreement = nogoldstandard_highagreement,
  nogoldstandard_lowagreement = nogoldstandard_lowagreement,
  nogoldstandard_perfect = nogoldstandard_perfect,
  nogoldstandard_rare = nogoldstandard_rare,
  nogoldstandard_common = nogoldstandard_common,
  nogoldstandard_allpositive = nogoldstandard_allpositive,
  nogoldstandard_allnegative = nogoldstandard_allnegative,
  nogoldstandard_imbalanced = nogoldstandard_imbalanced,
  nogoldstandard_validation = nogoldstandard_validation,
  nogoldstandard_missing = nogoldstandard_missing,
  nogoldstandard_small = nogoldstandard_small,
  nogoldstandard_large = nogoldstandard_large
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

cat("✅ No Gold Standard test data: 16 datasets × 4 formats = 64 files\n")
