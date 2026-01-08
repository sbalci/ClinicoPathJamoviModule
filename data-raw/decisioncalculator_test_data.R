# ═══════════════════════════════════════════════════════════════════════════════
# Test Data Generation: decisioncalculator
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script generates realistic test data for the decisioncalculator jamovi function.
# The decisioncalculator takes 2×2 confusion matrix counts (TP, TN, FP, FN) as input
# to calculate diagnostic test performance metrics.
#
# Generated: 2026-01-07
# Seed: 42 (for reproducibility)

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

cat("══════════════════════════════════════════════════════════\n")
cat("Generating test data for decisioncalculator function\n")
cat("══════════════════════════════════════════════════════════\n\n")

# ═══════════════════════════════════════════════════════════
# Helper function: Calculate diagnostic metrics
# ═══════════════════════════════════════════════════════════
calculate_metrics <- function(TP, TN, FP, FN) {
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  list(
    Sensitivity = round(sensitivity, 3),
    Specificity = round(specificity, 3),
    PPV = round(ppv, 3),
    NPV = round(npv, 3),
    Accuracy = round(accuracy, 3)
  )
}

# ═══════════════════════════════════════════════════════════
# Dataset 1: decisioncalculator_scenarios (Multiple Clinical Scenarios)
# ═══════════════════════════════════════════════════════════
# Purpose: Comprehensive collection of diagnostic test scenarios
# N = 15 scenarios

decisioncalculator_scenarios <- tibble(
  scenario_id = 1:15,

  scenario_name = c(
    "High Sensitivity Screening",
    "High Specificity Confirmatory",
    "Balanced Moderate Performance",
    "Excellent Biomarker",
    "Poor Screening Test",
    "Rare Disease Screening",
    "Common Disease Screening",
    "Perfect Test",
    "Useless Test",
    "High Prevalence Setting",
    "Low Prevalence Setting",
    "Imaging Study",
    "Point-of-Care Test",
    "Laboratory Assay",
    "Clinical Examination"
  ),

  # Clinical context
  test_type = c(
    "Screening", "Confirmatory", "Diagnostic", "Biomarker", "Screening",
    "Screening", "Screening", "Gold Standard", "Poor", "Diagnostic",
    "Screening", "Imaging", "POC", "Laboratory", "Clinical"
  ),

  disease_type = c(
    "Cancer", "Infection", "Cardiac", "Cancer", "Infection",
    "Genetic", "Diabetes", "Any", "Any", "Sepsis",
    "Rare Disease", "Tumor", "COVID-19", "Thyroid", "Appendicitis"
  ),

  # 2×2 confusion matrix counts
  TP = c(95, 85, 70, 98, 50, 19, 85, 100, 50, 180, 18, 88, 92, 78, 65),
  TN = c(150, 195, 160, 192, 140, 980, 210, 200, 150, 120, 970, 180, 185, 175, 155),
  FP = c(50, 5, 40, 8, 60, 20, 90, 0, 50, 80, 30, 20, 15, 25, 45),
  FN = c(5, 15, 30, 2, 50, 1, 15, 0, 50, 20, 2, 12, 8, 22, 35),

  # Total counts
  total_diseased = TP + FN,
  total_healthy = TN + FP,
  total_n = TP + TN + FP + FN,

  # Study prevalence
  prevalence = round((TP + FN) / (TP + TN + FP + FN), 3),

  # Population prevalence (may differ from study)
  population_prevalence = c(
    0.05, 0.15, 0.20, 0.08, 0.25, 0.001, 0.30, 0.10, 0.20, 0.40,
    0.02, 0.12, 0.10, 0.18, 0.25
  ),

  # Clinical notes
  notes = c(
    "Designed to catch all cases, will refer many false positives",
    "High specificity to rule in disease after positive screen",
    "Moderate performance, typical of many clinical tests",
    "Excellent biomarker with high sensitivity and specificity",
    "Poor test performance, not clinically useful",
    "Screening rare genetic condition, PPV will be low",
    "Common disease, better PPV than rare disease screening",
    "Perfect test (theoretical), kappa = 1.0",
    "Random test, sensitivity = specificity = 0.5",
    "High prevalence setting (ICU), affects PPV/NPV",
    "Low prevalence setting, PPV will be low despite good test",
    "CT scan for tumor detection",
    "Rapid antigen test",
    "TSH assay for thyroid disease",
    "McBurney point tenderness for appendicitis"
  )
)

# Add calculated metrics
metrics_list <- mapply(calculate_metrics,
                       decisioncalculator_scenarios$TP,
                       decisioncalculator_scenarios$TN,
                       decisioncalculator_scenarios$FP,
                       decisioncalculator_scenarios$FN,
                       SIMPLIFY = FALSE)

decisioncalculator_scenarios <- decisioncalculator_scenarios %>%
  mutate(
    sensitivity = sapply(metrics_list, function(x) x$Sensitivity),
    specificity = sapply(metrics_list, function(x) x$Specificity),
    ppv = sapply(metrics_list, function(x) x$PPV),
    npv = sapply(metrics_list, function(x) x$NPV),
    accuracy = sapply(metrics_list, function(x) x$Accuracy)
  )

save(decisioncalculator_scenarios, file = here("data", "decisioncalculator_scenarios.rda"))
write.csv(decisioncalculator_scenarios, file = here("data", "decisioncalculator_scenarios.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_scenarios, path = here("data", "decisioncalculator_scenarios.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_scenarios, here("data", "decisioncalculator_scenarios.omv"))

cat("✓ Generated decisioncalculator_scenarios (15 scenarios)\n")

# ═══════════════════════════════════════════════════════════
# Dataset 2: decisioncalculator_screening (High-Sensitivity Screening Test)
# ═══════════════════════════════════════════════════════════
# Purpose: Typical screening test with high sensitivity, moderate specificity
# Scenario: Mammography for breast cancer screening

decisioncalculator_screening <- tibble(
  test_name = "Mammography Screening",
  condition = "Breast Cancer",
  setting = "Community Screening Program",
  sample_size = 1000,

  # 2×2 counts (sensitivity ~90%, specificity ~85%)
  TP = 45,   # 50 cases detected out of 50 with cancer
  TN = 808,  # 950 healthy correctly identified
  FP = 142,  # 142 false positives (recalls)
  FN = 5,    # 5 cancers missed

  # Prevalence
  study_prevalence = 0.05,
  population_prevalence = 0.005,  # Much lower in general population

  # Expected metrics
  expected_sensitivity = 0.90,
  expected_specificity = 0.85,

  notes = "High sensitivity screening test designed to minimize false negatives"
)

save(decisioncalculator_screening, file = here("data", "decisioncalculator_screening.rda"))
write.csv(decisioncalculator_screening, file = here("data", "decisioncalculator_screening.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_screening, path = here("data", "decisioncalculator_screening.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_screening, here("data", "decisioncalculator_screening.omv"))

cat("✓ Generated decisioncalculator_screening\n")

# ═══════════════════════════════════════════════════════════
# Dataset 3: decisioncalculator_confirmatory (High-Specificity Confirmatory Test)
# ═══════════════════════════════════════════════════════════
# Purpose: Confirmatory test with very high specificity
# Scenario: Biopsy after positive screening mammography

decisioncalculator_confirmatory <- tibble(
  test_name = "Tissue Biopsy",
  condition = "Malignancy Confirmation",
  setting = "Diagnostic Following Positive Screen",
  sample_size = 150,

  # 2×2 counts (sensitivity ~85%, specificity ~98%)
  TP = 85,   # 85 of 100 malignancies confirmed
  TN = 49,   # 49 of 50 benign lesions correctly identified
  FP = 1,    # 1 false positive (benign called malignant)
  FN = 15,   # 15 malignancies missed

  study_prevalence = 0.67,  # Enriched sample (post-screening)
  population_prevalence = 0.05,

  expected_sensitivity = 0.85,
  expected_specificity = 0.98,

  notes = "Confirmatory test prioritizes ruling IN disease (high specificity)"
)

save(decisioncalculator_confirmatory, file = here("data", "decisioncalculator_confirmatory.rda"))
write.csv(decisioncalculator_confirmatory, file = here("data", "decisioncalculator_confirmatory.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_confirmatory, path = here("data", "decisioncalculator_confirmatory.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_confirmatory, here("data", "decisioncalculator_confirmatory.omv"))

cat("✓ Generated decisioncalculator_confirmatory\n")

# ═══════════════════════════════════════════════════════════
# Dataset 4: decisioncalculator_biomarker (Excellent Biomarker)
# ═══════════════════════════════════════════════════════════
# Purpose: High-performance biomarker test
# Scenario: Troponin for myocardial infarction

decisioncalculator_biomarker <- tibble(
  test_name = "Cardiac Troponin",
  condition = "Myocardial Infarction",
  setting = "Emergency Department",
  sample_size = 500,

  # 2×2 counts (sensitivity ~95%, specificity ~92%)
  TP = 95,   # 95 of 100 MI cases detected
  TN = 368,  # 368 of 400 non-MI correctly identified
  FP = 32,   # 32 false positives
  FN = 5,    # 5 MI cases missed

  study_prevalence = 0.20,
  population_prevalence = 0.15,

  expected_sensitivity = 0.95,
  expected_specificity = 0.92,

  notes = "Excellent biomarker with high sensitivity and specificity"
)

save(decisioncalculator_biomarker, file = here("data", "decisioncalculator_biomarker.rda"))
write.csv(decisioncalculator_biomarker, file = here("data", "decisioncalculator_biomarker.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_biomarker, path = here("data", "decisioncalculator_biomarker.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_biomarker, here("data", "decisioncalculator_biomarker.omv"))

cat("✓ Generated decisioncalculator_biomarker\n")

# ═══════════════════════════════════════════════════════════
# Dataset 5: decisioncalculator_raredisease (Rare Disease Screening)
# ═══════════════════════════════════════════════════════════
# Purpose: Demonstrates PPV challenge in rare disease screening
# Scenario: Newborn screening for phenylketonuria (PKU)

decisioncalculator_raredisease <- tibble(
  test_name = "PKU Screening Test",
  condition = "Phenylketonuria",
  setting = "Newborn Screening Program",
  sample_size = 10000,

  # 2×2 counts (excellent test, but rare disease)
  # Prevalence 1:10,000 = 0.0001, use 1:1000 for this dataset
  TP = 9,      # 9 of 10 PKU cases detected (sens = 0.90)
  TN = 9890,   # 9890 of 9990 healthy babies (spec = 0.99)
  FP = 100,    # 100 false positives
  FN = 1,      # 1 PKU case missed

  study_prevalence = 0.001,
  population_prevalence = 0.0001,

  expected_sensitivity = 0.90,
  expected_specificity = 0.99,
  expected_ppv = 0.083,  # Low PPV despite excellent test

  notes = "Demonstrates that PPV is low even with excellent test when disease is rare"
)

save(decisioncalculator_raredisease, file = here("data", "decisioncalculator_raredisease.rda"))
write.csv(decisioncalculator_raredisease, file = here("data", "decisioncalculator_raredisease.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_raredisease, path = here("data", "decisioncalculator_raredisease.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_raredisease, here("data", "decisioncalculator_raredisease.omv"))

cat("✓ Generated decisioncalculator_raredisease\n")

# ═══════════════════════════════════════════════════════════
# Dataset 6: decisioncalculator_prevalence (Prevalence Effect Comparison)
# ═══════════════════════════════════════════════════════════
# Purpose: Same test performance in different prevalence settings
# Scenario: Rapid strep test in pediatric vs adult settings

decisioncalculator_prevalence <- tibble(
  scenario = c("High Prevalence (Pediatric)", "Low Prevalence (Adult)"),

  setting = c("Pediatric Clinic", "Adult Primary Care"),

  # Same test characteristics (sens=0.85, spec=0.95)
  # Different prevalence (30% vs 5%)
  TP = c(85, 43),
  TN = c(133, 903),
  FP = c(7, 47),
  FN = c(15, 7),

  total_n = c(240, 1000),
  prevalence = c(0.417, 0.05),

  # Same test, different PPV/NPV due to prevalence
  expected_ppv = c(0.924, 0.478),
  expected_npv = c(0.899, 0.992),

  notes = c(
    "High prevalence: Excellent PPV and NPV",
    "Low prevalence: Poor PPV, excellent NPV"
  )
)

save(decisioncalculator_prevalence, file = here("data", "decisioncalculator_prevalence.rda"))
write.csv(decisioncalculator_prevalence, file = here("data", "decisioncalculator_prevalence.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_prevalence, path = here("data", "decisioncalculator_prevalence.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_prevalence, here("data", "decisioncalculator_prevalence.omv"))

cat("✓ Generated decisioncalculator_prevalence\n")

# ═══════════════════════════════════════════════════════════
# Dataset 7: decisioncalculator_perfect (Perfect Test - Edge Case)
# ═══════════════════════════════════════════════════════════
# Purpose: Perfect diagnostic test (theoretical)

decisioncalculator_perfect <- tibble(
  test_name = "Perfect Test (Theoretical)",
  condition = "Any Disease",

  TP = 50,
  TN = 150,
  FP = 0,
  FN = 0,

  total_n = 200,
  prevalence = 0.25,

  expected_sensitivity = 1.00,
  expected_specificity = 1.00,
  expected_ppv = 1.00,
  expected_npv = 1.00,

  notes = "Perfect test with 100% sensitivity and specificity (theoretical)"
)

save(decisioncalculator_perfect, file = here("data", "decisioncalculator_perfect.rda"))
write.csv(decisioncalculator_perfect, file = here("data", "decisioncalculator_perfect.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_perfect, path = here("data", "decisioncalculator_perfect.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_perfect, here("data", "decisioncalculator_perfect.omv"))

cat("✓ Generated decisioncalculator_perfect\n")

# ═══════════════════════════════════════════════════════════
# Dataset 8: decisioncalculator_useless (Useless Test - Edge Case)
# ═══════════════════════════════════════════════════════════
# Purpose: Test with no diagnostic value (coin flip)

decisioncalculator_useless <- tibble(
  test_name = "Useless Test (Random)",
  condition = "Any Disease",

  # Sensitivity = Specificity = 0.5 (coin flip)
  TP = 25,
  TN = 75,
  FP = 75,
  FN = 25,

  total_n = 200,
  prevalence = 0.25,

  expected_sensitivity = 0.50,
  expected_specificity = 0.50,

  notes = "No diagnostic value, equivalent to random guessing"
)

save(decisioncalculator_useless, file = here("data", "decisioncalculator_useless.rda"))
write.csv(decisioncalculator_useless, file = here("data", "decisioncalculator_useless.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_useless, path = here("data", "decisioncalculator_useless.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_useless, here("data", "decisioncalculator_useless.omv"))

cat("✓ Generated decisioncalculator_useless\n")

# ═══════════════════════════════════════════════════════════
# Dataset 9: decisioncalculator_small (Small Sample)
# ═══════════════════════════════════════════════════════════
# Purpose: Small pilot study with wide confidence intervals

decisioncalculator_small <- tibble(
  test_name = "Pilot Study Test",
  condition = "Pilot Evaluation",
  setting = "Small Validation Study",

  TP = 18,
  TN = 22,
  FP = 3,
  FN = 7,

  total_n = 50,
  prevalence = 0.50,

  notes = "Small sample, wide confidence intervals expected"
)

save(decisioncalculator_small, file = here("data", "decisioncalculator_small.rda"))
write.csv(decisioncalculator_small, file = here("data", "decisioncalculator_small.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_small, path = here("data", "decisioncalculator_small.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_small, here("data", "decisioncalculator_small.omv"))

cat("✓ Generated decisioncalculator_small\n")

# ═══════════════════════════════════════════════════════════
# Dataset 10: decisioncalculator_multiplecuts (Multiple Cut-off Comparison)
# ═══════════════════════════════════════════════════════════
# Purpose: Compare conservative vs aggressive diagnostic thresholds
# Scenario: PSA testing with different cut-offs

decisioncalculator_multiplecuts <- tibble(
  cutoff_scenario = c("Conservative (PSA ≥10)", "Standard (PSA ≥4)", "Aggressive (PSA ≥2.5)"),

  cutoff_value = c(10, 4, 2.5),

  # Conservative: High specificity, lower sensitivity
  # Standard: Balanced
  # Aggressive: High sensitivity, lower specificity
  TP = c(65, 85, 95),
  TN = c(195, 180, 160),
  FP = c(5, 20, 40),
  FN = c(35, 15, 5),

  total_n = 300,
  diseased = 100,
  healthy = 200,

  sensitivity = c(0.65, 0.85, 0.95),
  specificity = c(0.975, 0.90, 0.80),

  clinical_strategy = c(
    "Minimize false positives, accept missing some cases",
    "Balanced approach, standard clinical practice",
    "Maximize case detection, more false positives"
  )
)

save(decisioncalculator_multiplecuts, file = here("data", "decisioncalculator_multiplecuts.rda"))
write.csv(decisioncalculator_multiplecuts, file = here("data", "decisioncalculator_multiplecuts.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_multiplecuts, path = here("data", "decisioncalculator_multiplecuts.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_multiplecuts, here("data", "decisioncalculator_multiplecuts.omv"))

cat("✓ Generated decisioncalculator_multiplecuts\n")

# ═══════════════════════════════════════════════════════════
# Dataset 11: decisioncalculator_imaging (Imaging Study)
# ═══════════════════════════════════════════════════════════
# Purpose: CT scan for appendicitis detection

decisioncalculator_imaging <- tibble(
  test_name = "CT Scan",
  condition = "Appendicitis",
  setting = "Emergency Department",

  TP = 88,
  TN = 180,
  FP = 20,
  FN = 12,

  total_n = 300,
  prevalence = 0.33,

  cost_per_test = 1200,
  radiation_exposure = "5-10 mSv",

  notes = "High-performance imaging with cost and radiation considerations"
)

save(decisioncalculator_imaging, file = here("data", "decisioncalculator_imaging.rda"))
write.csv(decisioncalculator_imaging, file = here("data", "decisioncalculator_imaging.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_imaging, path = here("data", "decisioncalculator_imaging.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_imaging, here("data", "decisioncalculator_imaging.omv"))

cat("✓ Generated decisioncalculator_imaging\n")

# ═══════════════════════════════════════════════════════════
# Dataset 12: decisioncalculator_pointofcare (Rapid Point-of-Care Test)
# ═══════════════════════════════════════════════════════════
# Purpose: COVID-19 rapid antigen test

decisioncalculator_pointofcare <- tibble(
  test_name = "COVID-19 Rapid Antigen Test",
  condition = "SARS-CoV-2 Infection",
  setting = "Outpatient Clinic",

  TP = 92,
  TN = 185,
  FP = 15,
  FN = 8,

  total_n = 300,
  prevalence = 0.33,

  turnaround_time = "15 minutes",
  cost_per_test = 25,

  notes = "Fast, low-cost test with good performance for symptomatic patients"
)

save(decisioncalculator_pointofcare, file = here("data", "decisioncalculator_pointofcare.rda"))
write.csv(decisioncalculator_pointofcare, file = here("data", "decisioncalculator_pointofcare.csv"), row.names = FALSE)
writexl::write_xlsx(decisioncalculator_pointofcare, path = here("data", "decisioncalculator_pointofcare.xlsx"))
jmvReadWrite::write_omv(decisioncalculator_pointofcare, here("data", "decisioncalculator_pointofcare.omv"))

cat("✓ Generated decisioncalculator_pointofcare\n")

# ═══════════════════════════════════════════════════════════
# Summary Report
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("══════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete!\n")
cat("══════════════════════════════════════════════════════════\n")
cat("\nGenerated 12 datasets:\n")
cat("  1. decisioncalculator_scenarios (15 scenarios)\n")
cat("  2. decisioncalculator_screening\n")
cat("  3. decisioncalculator_confirmatory\n")
cat("  4. decisioncalculator_biomarker\n")
cat("  5. decisioncalculator_raredisease\n")
cat("  6. decisioncalculator_prevalence (2 scenarios)\n")
cat("  7. decisioncalculator_perfect\n")
cat("  8. decisioncalculator_useless\n")
cat("  9. decisioncalculator_small\n")
cat(" 10. decisioncalculator_multiplecuts (3 cut-offs)\n")
cat(" 11. decisioncalculator_imaging\n")
cat(" 12. decisioncalculator_pointofcare\n")
cat("\nEach dataset saved in 4 formats:\n")
cat("  - .rda (R native)\n")
cat("  - .csv (universal)\n")
cat("  - .xlsx (Excel)\n")
cat("  - .omv (jamovi)\n")
cat("\nTotal files: 48 (12 datasets × 4 formats)\n")
cat("══════════════════════════════════════════════════════════\n")
