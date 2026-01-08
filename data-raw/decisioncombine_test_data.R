# ═══════════════════════════════════════════════════════════
# Test Data Generation: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the decisioncombine function
# (Combine and Evaluate Diagnostic Test Patterns)
#
# Generated datasets cover:
# 1. Two-test pathology rater agreement
# 2. High concordance scenario
# 3. Discordant tests (one sensitive, one specific)
# 4. Three-test combination
# 5. Screening + confirmatory strategy
# 6. Multi-modal imaging tests
# 7. Serial testing application
# 8. Small dataset for quick testing
#
# Each dataset is saved in 4 formats: RDA, CSV, XLSX, OMV

library(dplyr)
library(here)
library(writexl)

# Set seed for reproducibility
set.seed(42)

# Helper function to generate correlated binary test results
generate_test_results <- function(n, gold_standard, sensitivity, specificity) {
  test_result <- integer(n)

  for (i in 1:n) {
    if (gold_standard[i] == 1) {
      # True positive case
      test_result[i] <- rbinom(1, 1, sensitivity)
    } else {
      # True negative case
      test_result[i] <- rbinom(1, 1, 1 - specificity)  # False positive probability
    }
  }

  factor(test_result, levels = c(0, 1), labels = c("Negative", "Positive"))
}

# ═══════════════════════════════════════════════════════════
# Dataset 1: decisioncombine_pathology
# ═══════════════════════════════════════════════════════════
# Two pathology raters vs gold standard (expert consensus)
# Use case: Inter-rater agreement in diagnostic pathology
# n = 200

n <- 200
prevalence <- 0.40

decisioncombine_pathology <- data.frame(
  patient_id = sprintf("PATH-%03d", 1:n),
  age = round(rnorm(n, 58, 14)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  specimen_type = factor(sample(c("Biopsy", "Resection", "FNA"), n, replace = TRUE,
                                prob = c(0.5, 0.3, 0.2))),
  gold_standard = rbinom(n, 1, prevalence)
)

# Adjust age range
decisioncombine_pathology$age <- pmax(25, pmin(decisioncombine_pathology$age, 85))

# Generate Rater 1 (moderately sensitive and specific)
decisioncombine_pathology$rater1 <- generate_test_results(
  n, decisioncombine_pathology$gold_standard,
  sensitivity = 0.85, specificity = 0.88
)

# Generate Rater 2 (similar performance to Rater 1)
decisioncombine_pathology$rater2 <- generate_test_results(
  n, decisioncombine_pathology$gold_standard,
  sensitivity = 0.82, specificity = 0.90
)

# Convert gold standard to factor
decisioncombine_pathology$gold_standard <- factor(
  decisioncombine_pathology$gold_standard,
  levels = c(0, 1),
  labels = c("Benign", "Malignant")
)

# Save in all formats
save(decisioncombine_pathology, file = here::here("data", "decisioncombine_pathology.rda"))
write.csv(decisioncombine_pathology, file = here::here("data", "decisioncombine_pathology.csv"), row.names = FALSE)
write_xlsx(decisioncombine_pathology, path = here::here("data", "decisioncombine_pathology.xlsx"))
jmvReadWrite::write_omv(decisioncombine_pathology, here::here("data", "decisioncombine_pathology.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 2: decisioncombine_concordant
# ═══════════════════════════════════════════════════════════
# High concordance between two tests
# Use case: Evaluating redundant vs complementary tests
# n = 250

n <- 250
prevalence <- 0.30

decisioncombine_concordant <- data.frame(
  patient_id = sprintf("CONC-%03d", 1:n),
  age = round(rnorm(n, 52, 16)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))),
  clinical_suspicion = factor(sample(c("Low", "Moderate", "High"), n, replace = TRUE,
                                     prob = c(0.3, 0.5, 0.2)),
                              levels = c("Low", "Moderate", "High")),
  gold_standard = rbinom(n, 1, prevalence)
)

decisioncombine_concordant$age <- pmax(20, pmin(decisioncombine_concordant$age, 80))

# Generate Test A (high sensitivity and specificity)
decisioncombine_concordant$test_a <- generate_test_results(
  n, decisioncombine_concordant$gold_standard,
  sensitivity = 0.90, specificity = 0.92
)

# Generate Test B (similar to Test A - highly concordant)
# Add some correlation by copying some results
test_b_base <- generate_test_results(
  n, decisioncombine_concordant$gold_standard,
  sensitivity = 0.88, specificity = 0.90
)

# Make 80% of results match Test A (high concordance)
concordance_mask <- rbinom(n, 1, 0.80)
decisioncombine_concordant$test_b <- ifelse(
  concordance_mask == 1,
  as.character(decisioncombine_concordant$test_a),
  as.character(test_b_base)
)
decisioncombine_concordant$test_b <- factor(decisioncombine_concordant$test_b,
                                            levels = c("Negative", "Positive"))

# Convert gold standard to factor
decisioncombine_concordant$gold_standard <- factor(
  decisioncombine_concordant$gold_standard,
  levels = c(0, 1),
  labels = c("Disease Absent", "Disease Present")
)

# Save in all formats
save(decisioncombine_concordant, file = here::here("data", "decisioncombine_concordant.rda"))
write.csv(decisioncombine_concordant, file = here::here("data", "decisioncombine_concordant.csv"), row.names = FALSE)
write_xlsx(decisioncombine_concordant, path = here::here("data", "decisioncombine_concordant.xlsx"))
jmvReadWrite::write_omv(decisioncombine_concordant, here::here("data", "decisioncombine_concordant.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 3: decisioncombine_discordant
# ═══════════════════════════════════════════════════════════
# Discordant tests: one sensitive, one specific
# Use case: Complementary test combination strategies
# n = 220

n <- 220
prevalence <- 0.35

decisioncombine_discordant <- data.frame(
  patient_id = sprintf("DISC-%03d", 1:n),
  age = round(rnorm(n, 55, 15)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  risk_category = factor(sample(c("Low", "Moderate", "High"), n, replace = TRUE,
                                prob = c(0.4, 0.4, 0.2)),
                        levels = c("Low", "Moderate", "High")),
  gold_standard = rbinom(n, 1, prevalence)
)

decisioncombine_discordant$age <- pmax(30, pmin(decisioncombine_discordant$age, 75))

# Generate Sensitive Test (high sensitivity, lower specificity)
# Good for screening/ruling out
decisioncombine_discordant$sensitive_test <- generate_test_results(
  n, decisioncombine_discordant$gold_standard,
  sensitivity = 0.95, specificity = 0.75
)

# Generate Specific Test (lower sensitivity, high specificity)
# Good for confirmation/ruling in
decisioncombine_discordant$specific_test <- generate_test_results(
  n, decisioncombine_discordant$gold_standard,
  sensitivity = 0.70, specificity = 0.95
)

# Convert gold standard to factor
decisioncombine_discordant$gold_standard <- factor(
  decisioncombine_discordant$gold_standard,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# Save in all formats
save(decisioncombine_discordant, file = here::here("data", "decisioncombine_discordant.rda"))
write.csv(decisioncombine_discordant, file = here::here("data", "decisioncombine_discordant.csv"), row.names = FALSE)
write_xlsx(decisioncombine_discordant, path = here::here("data", "decisioncombine_discordant.xlsx"))
jmvReadWrite::write_omv(decisioncombine_discordant, here::here("data", "decisioncombine_discordant.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 4: decisioncombine_threetest
# ═══════════════════════════════════════════════════════════
# Three-test combination (8 patterns)
# Use case: Multi-modal diagnostic approach
# n = 300

n <- 300
prevalence <- 0.28

decisioncombine_threetest <- data.frame(
  patient_id = sprintf("3TEST-%03d", 1:n),
  age = round(rnorm(n, 60, 13)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))),
  symptoms = factor(sample(c("Asymptomatic", "Mild", "Moderate", "Severe"), n, replace = TRUE,
                           prob = c(0.3, 0.3, 0.3, 0.1)),
                   levels = c("Asymptomatic", "Mild", "Moderate", "Severe")),
  gold_standard = rbinom(n, 1, prevalence)
)

decisioncombine_threetest$age <- pmax(25, pmin(decisioncombine_threetest$age, 85))

# Generate Test 1 (Clinical Exam)
decisioncombine_threetest$clinical_exam <- generate_test_results(
  n, decisioncombine_threetest$gold_standard,
  sensitivity = 0.75, specificity = 0.82
)

# Generate Test 2 (Lab Test)
decisioncombine_threetest$lab_test <- generate_test_results(
  n, decisioncombine_threetest$gold_standard,
  sensitivity = 0.88, specificity = 0.85
)

# Generate Test 3 (Imaging)
decisioncombine_threetest$imaging <- generate_test_results(
  n, decisioncombine_threetest$gold_standard,
  sensitivity = 0.82, specificity = 0.90
)

# Convert gold standard to factor
decisioncombine_threetest$gold_standard <- factor(
  decisioncombine_threetest$gold_standard,
  levels = c(0, 1),
  labels = c("No Disease", "Disease")
)

# Save in all formats
save(decisioncombine_threetest, file = here::here("data", "decisioncombine_threetest.rda"))
write.csv(decisioncombine_threetest, file = here::here("data", "decisioncombine_threetest.csv"), row.names = FALSE)
write_xlsx(decisioncombine_threetest, path = here::here("data", "decisioncombine_threetest.xlsx"))
jmvReadWrite::write_omv(decisioncombine_threetest, here::here("data", "decisioncombine_threetest.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 5: decisioncombine_screening
# ═══════════════════════════════════════════════════════════
# Screening test + confirmatory test strategy
# Use case: Two-stage diagnostic process
# n = 400

n <- 400
prevalence <- 0.10  # Low prevalence (screening population)

decisioncombine_screening <- data.frame(
  patient_id = sprintf("SCR-%03d", 1:n),
  age = round(rnorm(n, 48, 14)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  family_history = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.85, 0.15))),
  screening_indication = factor(sample(c("Routine", "High Risk"), n, replace = TRUE,
                                       prob = c(0.80, 0.20))),
  gold_standard = rbinom(n, 1, prevalence)
)

decisioncombine_screening$age <- pmax(40, pmin(decisioncombine_screening$age, 75))

# Generate Screening Test (very sensitive, moderate specificity)
# Designed to catch all cases
decisioncombine_screening$screening_test <- generate_test_results(
  n, decisioncombine_screening$gold_standard,
  sensitivity = 0.98, specificity = 0.70
)

# Generate Confirmatory Test (moderate sensitivity, very specific)
# Designed to confirm positive screens
decisioncombine_screening$confirmatory_test <- generate_test_results(
  n, decisioncombine_screening$gold_standard,
  sensitivity = 0.75, specificity = 0.98
)

# Convert gold standard to factor
decisioncombine_screening$gold_standard <- factor(
  decisioncombine_screening$gold_standard,
  levels = c(0, 1),
  labels = c("Healthy", "Disease")
)

# Save in all formats
save(decisioncombine_screening, file = here::here("data", "decisioncombine_screening.rda"))
write.csv(decisioncombine_screening, file = here::here("data", "decisioncombine_screening.csv"), row.names = FALSE)
write_xlsx(decisioncombine_screening, path = here::here("data", "decisioncombine_screening.xlsx"))
jmvReadWrite::write_omv(decisioncombine_screening, here::here("data", "decisioncombine_screening.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 6: decisioncombine_imaging
# ═══════════════════════════════════════════════════════════
# Multi-modal imaging: CT + MRI
# Use case: Comparing different imaging modalities
# n = 180

n <- 180
prevalence <- 0.45

decisioncombine_imaging <- data.frame(
  patient_id = sprintf("IMG-%03d", 1:n),
  age = round(rnorm(n, 62, 12)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.60, 0.40))),
  tumor_location = factor(sample(c("Liver", "Lung", "Brain", "Other"), n, replace = TRUE,
                                 prob = c(0.3, 0.3, 0.2, 0.2))),
  lesion_size_cm = round(rnorm(n, 3.5, 1.8), 1),
  gold_standard = rbinom(n, 1, prevalence)
)

decisioncombine_imaging$age <- pmax(35, pmin(decisioncombine_imaging$age, 85))
decisioncombine_imaging$lesion_size_cm <- pmax(0.5, pmin(decisioncombine_imaging$lesion_size_cm, 10))

# Generate CT Scan results
decisioncombine_imaging$ct_scan <- generate_test_results(
  n, decisioncombine_imaging$gold_standard,
  sensitivity = 0.85, specificity = 0.88
)

# Generate MRI results (slightly better than CT)
decisioncombine_imaging$mri_scan <- generate_test_results(
  n, decisioncombine_imaging$gold_standard,
  sensitivity = 0.90, specificity = 0.92
)

# Convert gold standard to factor
decisioncombine_imaging$gold_standard <- factor(
  decisioncombine_imaging$gold_standard,
  levels = c(0, 1),
  labels = c("Benign", "Malignant")
)

# Save in all formats
save(decisioncombine_imaging, file = here::here("data", "decisioncombine_imaging.rda"))
write.csv(decisioncombine_imaging, file = here::here("data", "decisioncombine_imaging.csv"), row.names = FALSE)
write_xlsx(decisioncombine_imaging, path = here::here("data", "decisioncombine_imaging.xlsx"))
jmvReadWrite::write_omv(decisioncombine_imaging, here::here("data", "decisioncombine_imaging.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 7: decisioncombine_serial
# ═══════════════════════════════════════════════════════════
# Serial testing: Initial + Repeat test
# Use case: Serial testing strategy (both must be positive)
# n = 200

n <- 200
prevalence <- 0.25

decisioncombine_serial <- data.frame(
  patient_id = sprintf("SER-%03d", 1:n),
  age = round(rnorm(n, 56, 15)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  time_between_tests_days = round(runif(n, 7, 30)),
  clinical_change = factor(sample(c("Improved", "Stable", "Worsened"), n, replace = TRUE,
                                  prob = c(0.2, 0.6, 0.2)),
                          levels = c("Improved", "Stable", "Worsened")),
  gold_standard = rbinom(n, 1, prevalence)
)

decisioncombine_serial$age <- pmax(18, pmin(decisioncombine_serial$age, 80))

# Generate Initial Test
decisioncombine_serial$initial_test <- generate_test_results(
  n, decisioncombine_serial$gold_standard,
  sensitivity = 0.88, specificity = 0.85
)

# Generate Repeat Test (similar performance)
decisioncombine_serial$repeat_test <- generate_test_results(
  n, decisioncombine_serial$gold_standard,
  sensitivity = 0.85, specificity = 0.88
)

# Convert gold standard to factor
decisioncombine_serial$gold_standard <- factor(
  decisioncombine_serial$gold_standard,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# Save in all formats
save(decisioncombine_serial, file = here::here("data", "decisioncombine_serial.rda"))
write.csv(decisioncombine_serial, file = here::here("data", "decisioncombine_serial.csv"), row.names = FALSE)
write_xlsx(decisioncombine_serial, path = here::here("data", "decisioncombine_serial.xlsx"))
jmvReadWrite::write_omv(decisioncombine_serial, here::here("data", "decisioncombine_serial.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 8: decisioncombine_small
# ═══════════════════════════════════════════════════════════
# Small dataset for quick testing
# Use case: Fast unit tests
# n = 50

n <- 50
prevalence <- 0.30

decisioncombine_small <- data.frame(
  patient_id = sprintf("SM-%02d", 1:n),
  age = round(runif(n, 30, 75)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  gold_standard = rbinom(n, 1, prevalence)
)

# Generate Test 1
decisioncombine_small$test1 <- generate_test_results(
  n, decisioncombine_small$gold_standard,
  sensitivity = 0.85, specificity = 0.85
)

# Generate Test 2
decisioncombine_small$test2 <- generate_test_results(
  n, decisioncombine_small$gold_standard,
  sensitivity = 0.80, specificity = 0.88
)

# Convert gold standard to factor
decisioncombine_small$gold_standard <- factor(
  decisioncombine_small$gold_standard,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# Save in all formats
save(decisioncombine_small, file = here::here("data", "decisioncombine_small.rda"))
write.csv(decisioncombine_small, file = here::here("data", "decisioncombine_small.csv"), row.names = FALSE)
write_xlsx(decisioncombine_small, path = here::here("data", "decisioncombine_small.xlsx"))
jmvReadWrite::write_omv(decisioncombine_small, here::here("data", "decisioncombine_small.omv"))

# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete: decisioncombine\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("Generated 8 datasets with 4 formats each (32 files total):\n")
cat("\n")
cat("1. decisioncombine_pathology    (n=200) - Pathology rater agreement\n")
cat("2. decisioncombine_concordant   (n=250) - High concordance tests\n")
cat("3. decisioncombine_discordant   (n=220) - Sensitive vs specific tests\n")
cat("4. decisioncombine_threetest    (n=300) - Three-test combination (8 patterns)\n")
cat("5. decisioncombine_screening    (n=400) - Screening + confirmatory strategy\n")
cat("6. decisioncombine_imaging      (n=180) - Multi-modal imaging (CT + MRI)\n")
cat("7. decisioncombine_serial       (n=200) - Serial testing strategy\n")
cat("8. decisioncombine_small        (n=50)  - Quick testing dataset\n")
cat("\n")
cat("Formats: RDA, CSV, XLSX, OMV\n")
cat("Total patients across all datasets: 1,800\n")
cat("\n")
cat("Next steps:\n")
cat("1. Create test files (test-decisioncombine-*.R)\n")
cat("2. Create example file (inst/examples/decisioncombine_example.R)\n")
cat("3. Create summary documentation (DECISIONCOMBINE_TEST_DATA_SUMMARY.md)\n")
cat("════════════════════════════════════════════════════════════════\n")
