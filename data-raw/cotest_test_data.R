# ═══════════════════════════════════════════════════════════
# Test Data Generation: cotest
# ═══════════════════════════════════════════════════════════
#
# This script generates comprehensive test datasets for the cotest function
# (Co-Testing Analysis for two concurrent diagnostic tests)
#
# Generated datasets cover:
# 1. Independent tests
# 2. Dependent tests (conditional dependence)
# 3. Clinical scenarios (HPV+Pap, PSA+DRE, Troponin+ECG)
# 4. Different prevalence levels
# 5. Small dataset for quick testing
#
# Each dataset is saved in 4 formats: RDA, CSV, XLSX, OMV

library(dplyr)
library(here)
library(writexl)

# Set seed for reproducibility
set.seed(42)

# Helper function to generate test results based on disease status and test characteristics
generate_test_results <- function(n, prevalence, test1_sens, test1_spec, test2_sens, test2_spec,
                                   independent = TRUE, cond_dep_pos = 0, cond_dep_neg = 0) {
  # Generate disease status
  disease_status <- rbinom(n, 1, prevalence)

  # Initialize test results
  test1_result <- integer(n)
  test2_result <- integer(n)

  if (independent) {
    # Independent tests
    for (i in 1:n) {
      if (disease_status[i] == 1) {
        # Patient has disease
        test1_result[i] <- rbinom(1, 1, test1_sens)
        test2_result[i] <- rbinom(1, 1, test2_sens)
      } else {
        # Patient does not have disease
        test1_result[i] <- rbinom(1, 1, 1 - test1_spec)  # False positive
        test2_result[i] <- rbinom(1, 1, 1 - test2_spec)  # False positive
      }
    }
  } else {
    # Dependent tests (conditional dependence)
    for (i in 1:n) {
      if (disease_status[i] == 1) {
        # Patient has disease
        test1_result[i] <- rbinom(1, 1, test1_sens)

        # Test 2 result depends on Test 1 result
        if (test1_result[i] == 1) {
          # If Test 1 positive, Test 2 more likely positive (positive dependence)
          adjusted_sens <- min(1, test2_sens + cond_dep_pos)
          test2_result[i] <- rbinom(1, 1, adjusted_sens)
        } else {
          # If Test 1 negative, Test 2 less likely positive
          adjusted_sens <- max(0, test2_sens - cond_dep_pos)
          test2_result[i] <- rbinom(1, 1, adjusted_sens)
        }
      } else {
        # Patient does not have disease
        test1_result[i] <- rbinom(1, 1, 1 - test1_spec)

        # Test 2 result depends on Test 1 result
        if (test1_result[i] == 1) {
          # If Test 1 false positive, Test 2 more likely false positive
          adjusted_spec <- max(0, test1_spec - cond_dep_neg)
          test2_result[i] <- rbinom(1, 1, 1 - adjusted_spec)
        } else {
          # If Test 1 true negative, Test 2 more likely true negative
          adjusted_spec <- min(1, test1_spec + cond_dep_neg)
          test2_result[i] <- rbinom(1, 1, 1 - adjusted_spec)
        }
      }
    }
  }

  data.frame(
    disease_status = disease_status,
    test1_result = test1_result,
    test2_result = test2_result
  )
}

# ═══════════════════════════════════════════════════════════
# Dataset 1: cotest_independent
# ═══════════════════════════════════════════════════════════
# Two independent diagnostic tests with moderate characteristics
# Use case: Standard co-testing scenario
# n = 300

cotest_independent <- generate_test_results(
  n = 300,
  prevalence = 0.20,
  test1_sens = 0.85,
  test1_spec = 0.90,
  test2_sens = 0.80,
  test2_spec = 0.92,
  independent = TRUE
)

# Add demographic variables
cotest_independent <- cotest_independent %>%
  mutate(
    patient_id = sprintf("IND-%03d", row_number()),
    age = round(rnorm(n(), 55, 15)),
    age = pmax(18, pmin(age, 90)),
    sex = factor(sample(c("Male", "Female"), n(), replace = TRUE)),
    risk_category = factor(
      sample(c("Low", "Moderate", "High"), n(), replace = TRUE, prob = c(0.4, 0.4, 0.2)),
      levels = c("Low", "Moderate", "High")
    )
  )

# Convert test results to factors
cotest_independent <- cotest_independent %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("Negative", "Positive")),
    test1_result = factor(test1_result, levels = c(0, 1), labels = c("Negative", "Positive")),
    test2_result = factor(test2_result, levels = c(0, 1), labels = c("Negative", "Positive"))
  ) %>%
  select(patient_id, age, sex, risk_category, disease_status, test1_result, test2_result)

# Save in all formats
save(cotest_independent, file = here::here("data", "cotest_independent.rda"))
write.csv(cotest_independent, file = here::here("data", "cotest_independent.csv"), row.names = FALSE)
write_xlsx(cotest_independent, path = here::here("data", "cotest_independent.xlsx"))
jmvReadWrite::write_omv(cotest_independent, here::here("data", "cotest_independent.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 2: cotest_dependent
# ═══════════════════════════════════════════════════════════
# Two dependent diagnostic tests with positive conditional dependence
# Use case: Tests that measure related biomarkers
# n = 300

cotest_dependent <- generate_test_results(
  n = 300,
  prevalence = 0.25,
  test1_sens = 0.85,
  test1_spec = 0.88,
  test2_sens = 0.82,
  test2_spec = 0.90,
  independent = FALSE,
  cond_dep_pos = 0.15,  # Positive dependence
  cond_dep_neg = 0.10   # Negative dependence
)

# Add demographic variables
cotest_dependent <- cotest_dependent %>%
  mutate(
    patient_id = sprintf("DEP-%03d", row_number()),
    age = round(rnorm(n(), 58, 14)),
    age = pmax(20, pmin(age, 85)),
    sex = factor(sample(c("Male", "Female"), n(), replace = TRUE, prob = c(0.55, 0.45))),
    symptom_duration = round(rexp(n(), 1/30)),  # Days
    symptom_severity = factor(
      sample(c("Mild", "Moderate", "Severe"), n(), replace = TRUE, prob = c(0.3, 0.5, 0.2)),
      levels = c("Mild", "Moderate", "Severe")
    )
  )

# Convert test results to factors
cotest_dependent <- cotest_dependent %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("Negative", "Positive")),
    test1_result = factor(test1_result, levels = c(0, 1), labels = c("Negative", "Positive")),
    test2_result = factor(test2_result, levels = c(0, 1), labels = c("Negative", "Positive"))
  ) %>%
  select(patient_id, age, sex, symptom_duration, symptom_severity, disease_status, test1_result, test2_result)

# Save in all formats
save(cotest_dependent, file = here::here("data", "cotest_dependent.rda"))
write.csv(cotest_dependent, file = here::here("data", "cotest_dependent.csv"), row.names = FALSE)
write_xlsx(cotest_dependent, path = here::here("data", "cotest_dependent.xlsx"))
jmvReadWrite::write_omv(cotest_dependent, here::here("data", "cotest_dependent.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 3: cotest_hpv_pap
# ═══════════════════════════════════════════════════════════
# HPV + Pap Smear co-testing for cervical cancer screening
# Use case: Clinical preset scenario
# n = 500

cotest_hpv_pap <- generate_test_results(
  n = 500,
  prevalence = 0.05,  # 5% prevalence of cervical dysplasia
  test1_sens = 0.95,  # HPV test sensitivity
  test1_spec = 0.85,  # HPV test specificity
  test2_sens = 0.55,  # Pap smear sensitivity
  test2_spec = 0.97,  # Pap smear specificity
  independent = FALSE,
  cond_dep_pos = 0.10,
  cond_dep_neg = 0.05
)

# Add clinical variables
cotest_hpv_pap <- cotest_hpv_pap %>%
  mutate(
    patient_id = sprintf("HPV-%03d", row_number()),
    age = round(rnorm(n(), 35, 10)),
    age = pmax(21, pmin(age, 65)),
    previous_abnormal = factor(sample(c("No", "Yes"), n(), replace = TRUE, prob = c(0.85, 0.15))),
    screening_interval = factor(
      sample(c("First Time", "1 Year", "3 Years", "5 Years"), n(), replace = TRUE, prob = c(0.2, 0.1, 0.5, 0.2)),
      levels = c("First Time", "1 Year", "3 Years", "5 Years")
    )
  )

# Rename tests for clarity
cotest_hpv_pap <- cotest_hpv_pap %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("Normal", "Dysplasia")),
    hpv_test = factor(test1_result, levels = c(0, 1), labels = c("Negative", "Positive")),
    pap_smear = factor(test2_result, levels = c(0, 1), labels = c("Normal", "Abnormal"))
  ) %>%
  select(patient_id, age, previous_abnormal, screening_interval, disease_status, hpv_test, pap_smear)

# Save in all formats
save(cotest_hpv_pap, file = here::here("data", "cotest_hpv_pap.rda"))
write.csv(cotest_hpv_pap, file = here::here("data", "cotest_hpv_pap.csv"), row.names = FALSE)
write_xlsx(cotest_hpv_pap, path = here::here("data", "cotest_hpv_pap.xlsx"))
jmvReadWrite::write_omv(cotest_hpv_pap, here::here("data", "cotest_hpv_pap.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 4: cotest_psa_dre
# ═══════════════════════════════════════════════════════════
# PSA + Digital Rectal Exam for prostate cancer screening
# Use case: Clinical preset scenario
# n = 400

cotest_psa_dre <- generate_test_results(
  n = 400,
  prevalence = 0.15,  # 15% prevalence
  test1_sens = 0.85,  # PSA sensitivity
  test1_spec = 0.75,  # PSA specificity
  test2_sens = 0.60,  # DRE sensitivity
  test2_spec = 0.95,  # DRE specificity
  independent = FALSE,
  cond_dep_pos = 0.08,
  cond_dep_neg = 0.05
)

# Add clinical variables (male patients only)
cotest_psa_dre <- cotest_psa_dre %>%
  mutate(
    patient_id = sprintf("PSA-%03d", row_number()),
    age = round(rnorm(n(), 65, 8)),
    age = pmax(50, pmin(age, 85)),
    family_history = factor(sample(c("No", "Yes"), n(), replace = TRUE, prob = c(0.80, 0.20))),
    urinary_symptoms = factor(sample(c("None", "Mild", "Moderate", "Severe"), n(), replace = TRUE,
                                     prob = c(0.4, 0.3, 0.2, 0.1)),
                              levels = c("None", "Mild", "Moderate", "Severe"))
  )

# Rename tests for clarity
cotest_psa_dre <- cotest_psa_dre %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("No Cancer", "Cancer")),
    psa_test = factor(test1_result, levels = c(0, 1), labels = c("Normal", "Elevated")),
    dre_exam = factor(test2_result, levels = c(0, 1), labels = c("Normal", "Abnormal"))
  ) %>%
  select(patient_id, age, family_history, urinary_symptoms, disease_status, psa_test, dre_exam)

# Save in all formats
save(cotest_psa_dre, file = here::here("data", "cotest_psa_dre.rda"))
write.csv(cotest_psa_dre, file = here::here("data", "cotest_psa_dre.csv"), row.names = FALSE)
write_xlsx(cotest_psa_dre, path = here::here("data", "cotest_psa_dre.xlsx"))
jmvReadWrite::write_omv(cotest_psa_dre, here::here("data", "cotest_psa_dre.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 5: cotest_troponin_ecg
# ═══════════════════════════════════════════════════════════
# Troponin + ECG for myocardial infarction diagnosis
# Use case: Clinical preset scenario
# n = 350

cotest_troponin_ecg <- generate_test_results(
  n = 350,
  prevalence = 0.30,  # 30% prevalence in ED chest pain
  test1_sens = 0.95,  # Troponin sensitivity
  test1_spec = 0.88,  # Troponin specificity
  test2_sens = 0.70,  # ECG sensitivity
  test2_spec = 0.92,  # ECG specificity
  independent = FALSE,
  cond_dep_pos = 0.12,
  cond_dep_neg = 0.08
)

# Add clinical variables
cotest_troponin_ecg <- cotest_troponin_ecg %>%
  mutate(
    patient_id = sprintf("TROP-%03d", row_number()),
    age = round(rnorm(n(), 62, 12)),
    age = pmax(30, pmin(age, 90)),
    sex = factor(sample(c("Male", "Female"), n(), replace = TRUE, prob = c(0.60, 0.40))),
    chest_pain_type = factor(
      sample(c("Typical", "Atypical", "Non-cardiac"), n(), replace = TRUE, prob = c(0.4, 0.4, 0.2)),
      levels = c("Non-cardiac", "Atypical", "Typical")
    ),
    time_to_presentation = round(runif(n(), 0.5, 12), 1)  # Hours
  )

# Rename tests for clarity
cotest_troponin_ecg <- cotest_troponin_ecg %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("No MI", "MI")),
    troponin = factor(test1_result, levels = c(0, 1), labels = c("Normal", "Elevated")),
    ecg = factor(test2_result, levels = c(0, 1), labels = c("Normal", "Abnormal"))
  ) %>%
  select(patient_id, age, sex, chest_pain_type, time_to_presentation, disease_status, troponin, ecg)

# Save in all formats
save(cotest_troponin_ecg, file = here::here("data", "cotest_troponin_ecg.rda"))
write.csv(cotest_troponin_ecg, file = here::here("data", "cotest_troponin_ecg.csv"), row.names = FALSE)
write_xlsx(cotest_troponin_ecg, path = here::here("data", "cotest_troponin_ecg.xlsx"))
jmvReadWrite::write_omv(cotest_troponin_ecg, here::here("data", "cotest_troponin_ecg.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 6: cotest_high_prev
# ═══════════════════════════════════════════════════════════
# High prevalence scenario (50%)
# Use case: Testing in high-risk population
# n = 250

cotest_high_prev <- generate_test_results(
  n = 250,
  prevalence = 0.50,  # 50% prevalence
  test1_sens = 0.80,
  test1_spec = 0.85,
  test2_sens = 0.75,
  test2_spec = 0.88,
  independent = TRUE
)

# Add variables
cotest_high_prev <- cotest_high_prev %>%
  mutate(
    patient_id = sprintf("HIGH-%03d", row_number()),
    age = round(rnorm(n(), 60, 12)),
    age = pmax(25, pmin(age, 85)),
    sex = factor(sample(c("Male", "Female"), n(), replace = TRUE)),
    high_risk_exposure = factor(rep("Yes", n())),  # All high risk
    comorbidities = sample(0:3, n(), replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
  )

# Convert test results to factors
cotest_high_prev <- cotest_high_prev %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("Negative", "Positive")),
    test1_result = factor(test1_result, levels = c(0, 1), labels = c("Negative", "Positive")),
    test2_result = factor(test2_result, levels = c(0, 1), labels = c("Negative", "Positive"))
  ) %>%
  select(patient_id, age, sex, high_risk_exposure, comorbidities, disease_status, test1_result, test2_result)

# Save in all formats
save(cotest_high_prev, file = here::here("data", "cotest_high_prev.rda"))
write.csv(cotest_high_prev, file = here::here("data", "cotest_high_prev.csv"), row.names = FALSE)
write_xlsx(cotest_high_prev, path = here::here("data", "cotest_high_prev.xlsx"))
jmvReadWrite::write_omv(cotest_high_prev, here::here("data", "cotest_high_prev.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 7: cotest_low_prev
# ═══════════════════════════════════════════════════════════
# Low prevalence scenario (5%)
# Use case: Screening in general population
# n = 400

cotest_low_prev <- generate_test_results(
  n = 400,
  prevalence = 0.05,  # 5% prevalence
  test1_sens = 0.90,
  test1_spec = 0.95,
  test2_sens = 0.85,
  test2_spec = 0.92,
  independent = TRUE
)

# Add variables
cotest_low_prev <- cotest_low_prev %>%
  mutate(
    patient_id = sprintf("LOW-%03d", row_number()),
    age = round(rnorm(n(), 45, 15)),
    age = pmax(18, pmin(age, 75)),
    sex = factor(sample(c("Male", "Female"), n(), replace = TRUE)),
    screening_type = factor(rep("Routine", n())),
    asymptomatic = factor(rep("Yes", n()))  # All asymptomatic screening
  )

# Convert test results to factors
cotest_low_prev <- cotest_low_prev %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("Negative", "Positive")),
    test1_result = factor(test1_result, levels = c(0, 1), labels = c("Negative", "Positive")),
    test2_result = factor(test2_result, levels = c(0, 1), labels = c("Negative", "Positive"))
  ) %>%
  select(patient_id, age, sex, screening_type, asymptomatic, disease_status, test1_result, test2_result)

# Save in all formats
save(cotest_low_prev, file = here::here("data", "cotest_low_prev.rda"))
write.csv(cotest_low_prev, file = here::here("data", "cotest_low_prev.csv"), row.names = FALSE)
write_xlsx(cotest_low_prev, path = here::here("data", "cotest_low_prev.xlsx"))
jmvReadWrite::write_omv(cotest_low_prev, here::here("data", "cotest_low_prev.omv"))

# ═══════════════════════════════════════════════════════════
# Dataset 8: cotest_small
# ═══════════════════════════════════════════════════════════
# Small dataset for quick testing
# Use case: Fast unit tests
# n = 50

cotest_small <- generate_test_results(
  n = 50,
  prevalence = 0.30,
  test1_sens = 0.85,
  test1_spec = 0.90,
  test2_sens = 0.80,
  test2_spec = 0.88,
  independent = TRUE
)

# Add minimal variables
cotest_small <- cotest_small %>%
  mutate(
    patient_id = sprintf("SM-%02d", row_number()),
    age = round(runif(n(), 30, 70)),
    sex = factor(sample(c("Male", "Female"), n(), replace = TRUE))
  )

# Convert test results to factors
cotest_small <- cotest_small %>%
  mutate(
    disease_status = factor(disease_status, levels = c(0, 1), labels = c("Negative", "Positive")),
    test1_result = factor(test1_result, levels = c(0, 1), labels = c("Negative", "Positive")),
    test2_result = factor(test2_result, levels = c(0, 1), labels = c("Negative", "Positive"))
  ) %>%
  select(patient_id, age, sex, disease_status, test1_result, test2_result)

# Save in all formats
save(cotest_small, file = here::here("data", "cotest_small.rda"))
write.csv(cotest_small, file = here::here("data", "cotest_small.csv"), row.names = FALSE)
write_xlsx(cotest_small, path = here::here("data", "cotest_small.xlsx"))
jmvReadWrite::write_omv(cotest_small, here::here("data", "cotest_small.omv"))

# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete: cotest\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("Generated 8 datasets with 4 formats each (32 files total):\n")
cat("\n")
cat("1. cotest_independent    (n=300) - Independent tests, moderate characteristics\n")
cat("2. cotest_dependent      (n=300) - Dependent tests with conditional dependence\n")
cat("3. cotest_hpv_pap        (n=500) - HPV + Pap smear cervical cancer screening\n")
cat("4. cotest_psa_dre        (n=400) - PSA + DRE prostate cancer screening\n")
cat("5. cotest_troponin_ecg   (n=350) - Troponin + ECG myocardial infarction\n")
cat("6. cotest_high_prev      (n=250) - High prevalence (50%) scenario\n")
cat("7. cotest_low_prev       (n=400) - Low prevalence (5%) screening\n")
cat("8. cotest_small          (n=50)  - Quick testing dataset\n")
cat("\n")
cat("Formats: RDA, CSV, XLSX, OMV\n")
cat("Total patients across all datasets: 2,550\n")
cat("\n")
cat("Next steps:\n")
cat("1. Create test files (test-cotest-*.R)\n")
cat("2. Create example file (inst/examples/cotest_example.R)\n")
cat("3. Create summary documentation (COTEST_TEST_DATA_SUMMARY.md)\n")
cat("════════════════════════════════════════════════════════════════\n")
