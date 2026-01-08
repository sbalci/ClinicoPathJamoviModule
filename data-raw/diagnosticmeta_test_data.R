# ═══════════════════════════════════════════════════════════
# Test Data Generation: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the diagnosticmeta jamovi function
# Simulates diagnostic test accuracy meta-analysis data from multiple studies
#
# Generated: 2026-01-06
# Seed: 42
# Studies: 20

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# Number of studies
n_studies <- 20

# Generate study characteristics
# Simulate a meta-analysis of AI algorithm for breast cancer detection in pathology
diagnosticmeta_test <- tibble(
  # Study identifier
  study = paste0("Study_", 1:n_studies),

  # Study year (for meta-regression)
  year = sample(2015:2024, n_studies, replace = TRUE),

  # Study quality score (for meta-regression, 1-10)
  quality_score = round(rnorm(n_studies, mean = 7, sd = 1.5)),

  # Sample size per study (varies realistically)
  n_diseased = round(rnorm(n_studies, mean = 80, sd = 30)),
  n_healthy = round(rnorm(n_studies, mean = 100, sd = 40))
) %>%
  mutate(
    # Ensure positive sample sizes
    n_diseased = pmax(20, n_diseased),
    n_healthy = pmax(30, n_healthy),
    quality_score = pmax(1, pmin(10, quality_score))
  )

# Generate diagnostic accuracy data with realistic variation
# True sensitivity and specificity vary across studies (heterogeneity)
diagnosticmeta_test <- diagnosticmeta_test %>%
  mutate(
    # True underlying sensitivity varies by study (mean ~0.85, heterogeneity)
    true_sensitivity = pmin(0.99, pmax(0.60, rbeta(n_studies, 8, 2))),

    # True underlying specificity varies by study (mean ~0.90, heterogeneity)
    true_specificity = pmin(0.99, pmax(0.70, rbeta(n_studies, 9, 1.5))),

    # Generate 2x2 table counts based on these probabilities
    # True positives (among diseased)
    true_positives = rbinom(n_studies, size = n_diseased, prob = true_sensitivity),

    # False negatives (among diseased)
    false_negatives = n_diseased - true_positives,

    # True negatives (among healthy)
    true_negatives = rbinom(n_studies, size = n_healthy, prob = true_specificity),

    # False positives (among healthy)
    false_positives = n_healthy - true_negatives
  ) %>%
  # Add some correlation: higher quality studies tend to have larger samples
  mutate(
    n_diseased = round(n_diseased * (0.7 + 0.3 * quality_score / 10)),
    n_healthy = round(n_healthy * (0.7 + 0.3 * quality_score / 10))
  ) %>%
  # Recalculate to ensure consistency
  mutate(
    total_diseased = true_positives + false_negatives,
    total_healthy = true_negatives + false_positives
  ) %>%
  select(study, true_positives, false_positives, false_negatives, true_negatives,
         year, quality_score)

# Ensure no zero cells in most studies (but keep 1-2 for testing zero-cell handling)
diagnosticmeta_test <- diagnosticmeta_test %>%
  mutate(
    # Add small correction to avoid all zeros (except for 2 studies)
    true_positives = ifelse(row_number() <= 2, true_positives, pmax(1, true_positives)),
    false_positives = pmax(1, false_positives),
    false_negatives = pmax(1, false_negatives),
    true_negatives = ifelse(row_number() <= 2, true_negatives, pmax(1, true_negatives))
  )

# Create variant datasets for different testing scenarios

# 1. Standard test dataset (main)
diagnosticmeta_test_main <- diagnosticmeta_test

# 2. Small dataset (5 studies) for edge case testing
diagnosticmeta_test_small <- diagnosticmeta_test %>%
  slice(1:5)

# 3. Dataset with covariate categories (imaging modality)
diagnosticmeta_test_categorical <- diagnosticmeta_test %>%
  mutate(
    imaging_modality = sample(c("MRI", "CT", "Ultrasound"), n(), replace = TRUE)
  )

# 4. Dataset with zero cells for testing correction methods
diagnosticmeta_test_zeros <- diagnosticmeta_test %>%
  mutate(
    # Force zero cells in some studies
    false_positives = ifelse(row_number() %in% c(1, 5, 10), 0, false_positives),
    false_negatives = ifelse(row_number() %in% c(2, 7), 0, false_negatives)
  )

# 5. Large dataset (50 studies) for performance testing
set.seed(43)
diagnosticmeta_test_large <- tibble(
  study = paste0("Study_", 1:50),
  year = sample(2010:2024, 50, replace = TRUE),
  quality_score = round(rnorm(50, mean = 7, sd = 1.5)),
  n_diseased = round(rnorm(50, mean = 80, sd = 30)),
  n_healthy = round(rnorm(50, mean = 100, sd = 40))
) %>%
  mutate(
    n_diseased = pmax(20, n_diseased),
    n_healthy = pmax(30, n_healthy),
    quality_score = pmax(1, pmin(10, quality_score)),
    true_sensitivity = pmin(0.99, pmax(0.60, rbeta(50, 8, 2))),
    true_specificity = pmin(0.99, pmax(0.70, rbeta(50, 9, 1.5))),
    true_positives = rbinom(50, size = n_diseased, prob = true_sensitivity),
    false_negatives = n_diseased - true_positives,
    true_negatives = rbinom(50, size = n_healthy, prob = true_specificity),
    false_positives = n_healthy - true_negatives
  ) %>%
  mutate(
    true_positives = pmax(1, true_positives),
    false_positives = pmax(1, false_positives),
    false_negatives = pmax(1, false_negatives),
    true_negatives = pmax(1, true_negatives)
  ) %>%
  select(study, true_positives, false_positives, false_negatives, true_negatives,
         year, quality_score)

# Save all datasets in multiple formats

# Main test dataset
save(diagnosticmeta_test, file = here::here("data", "diagnosticmeta_test.rda"))
write.csv(diagnosticmeta_test, file = here::here("data", "diagnosticmeta_test.csv"),
          row.names = FALSE)
writexl::write_xlsx(diagnosticmeta_test,
                    path = here::here("data", "diagnosticmeta_test.xlsx"))
jmvReadWrite::write_omv(diagnosticmeta_test,
                       here::here("data", "diagnosticmeta_test.omv"))

# Small dataset
save(diagnosticmeta_test_small,
     file = here::here("data", "diagnosticmeta_test_small.rda"))
write.csv(diagnosticmeta_test_small,
          file = here::here("data", "diagnosticmeta_test_small.csv"),
          row.names = FALSE)
writexl::write_xlsx(diagnosticmeta_test_small,
                    path = here::here("data", "diagnosticmeta_test_small.xlsx"))
jmvReadWrite::write_omv(diagnosticmeta_test_small,
                       here::here("data", "diagnosticmeta_test_small.omv"))

# Categorical covariate dataset
save(diagnosticmeta_test_categorical,
     file = here::here("data", "diagnosticmeta_test_categorical.rda"))
write.csv(diagnosticmeta_test_categorical,
          file = here::here("data", "diagnosticmeta_test_categorical.csv"),
          row.names = FALSE)
writexl::write_xlsx(diagnosticmeta_test_categorical,
                    path = here::here("data", "diagnosticmeta_test_categorical.xlsx"))
jmvReadWrite::write_omv(diagnosticmeta_test_categorical,
                       here::here("data", "diagnosticmeta_test_categorical.omv"))

# Zero cells dataset
save(diagnosticmeta_test_zeros,
     file = here::here("data", "diagnosticmeta_test_zeros.rda"))
write.csv(diagnosticmeta_test_zeros,
          file = here::here("data", "diagnosticmeta_test_zeros.csv"),
          row.names = FALSE)
writexl::write_xlsx(diagnosticmeta_test_zeros,
                    path = here::here("data", "diagnosticmeta_test_zeros.xlsx"))
jmvReadWrite::write_omv(diagnosticmeta_test_zeros,
                       here::here("data", "diagnosticmeta_test_zeros.omv"))

# Large dataset
save(diagnosticmeta_test_large,
     file = here::here("data", "diagnosticmeta_test_large.rda"))
write.csv(diagnosticmeta_test_large,
          file = here::here("data", "diagnosticmeta_test_large.csv"),
          row.names = FALSE)
writexl::write_xlsx(diagnosticmeta_test_large,
                    path = here::here("data", "diagnosticmeta_test_large.xlsx"))
jmvReadWrite::write_omv(diagnosticmeta_test_large,
                       here::here("data", "diagnosticmeta_test_large.omv"))

# Document the datasets
cat("
═══════════════════════════════════════════════════════════
Diagnostic Meta-Analysis Test Datasets Generated
═══════════════════════════════════════════════════════════

1. diagnosticmeta_test (MAIN)
   - Studies: 20
   - Variables: 7 (study, TP, FP, FN, TN, year, quality_score)
   - Description: Standard diagnostic accuracy meta-analysis data
   - Use case: General testing of all features

2. diagnosticmeta_test_small
   - Studies: 5
   - Variables: 7
   - Description: Small dataset for edge case testing
   - Use case: Testing with minimal studies

3. diagnosticmeta_test_categorical
   - Studies: 20
   - Variables: 8 (includes imaging_modality)
   - Description: Dataset with categorical covariate
   - Use case: Testing categorical meta-regression

4. diagnosticmeta_test_zeros
   - Studies: 20
   - Variables: 7
   - Description: Dataset with intentional zero cells
   - Use case: Testing zero-cell correction methods

5. diagnosticmeta_test_large
   - Studies: 50
   - Variables: 7
   - Description: Large dataset for performance testing
   - Use case: Testing with many studies

Variable descriptions:
- study: Study identifier (character)
- true_positives (TP): Number of true positive results
- false_positives (FP): Number of false positive results
- false_negatives (FN): Number of false negative results
- true_negatives (TN): Number of true negative results
- year: Publication year (2015-2024, for meta-regression)
- quality_score: Study quality score (1-10, for meta-regression)
- imaging_modality: Imaging type (MRI/CT/Ultrasound, categorical covariate)

All datasets saved in 4 formats:
  ✓ .rda (R native)
  ✓ .csv (universal)
  ✓ .xlsx (Excel)
  ✓ .omv (jamovi)

Usage examples:
  # Load main test data
  data(diagnosticmeta_test, package = 'ClinicoPath')

  # Basic meta-analysis
  diagnosticmeta(data = diagnosticmeta_test,
                 study = 'study',
                 true_positives = 'true_positives',
                 false_positives = 'false_positives',
                 false_negatives = 'false_negatives',
                 true_negatives = 'true_negatives')

  # With meta-regression
  diagnosticmeta(data = diagnosticmeta_test,
                 study = 'study',
                 true_positives = 'true_positives',
                 false_positives = 'false_positives',
                 false_negatives = 'false_negatives',
                 true_negatives = 'true_negatives',
                 covariate = 'year',
                 meta_regression = TRUE)

  # Test zero-cell handling
  diagnosticmeta(data = diagnosticmeta_test_zeros,
                 study = 'study',
                 true_positives = 'true_positives',
                 false_positives = 'false_positives',
                 false_negatives = 'false_negatives',
                 true_negatives = 'true_negatives',
                 zero_cell_correction = 'constant')

Generated: ", Sys.time(), "
Seed: 42, 43
═══════════════════════════════════════════════════════════
")
