# ═══════════════════════════════════════════════════════════
# Test Data Generation: benford
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the benford jamovi function
#
# Purpose: Generate numeric data for Benford's Law analysis to detect
#          data quality issues, fabrication, and unusual digit patterns
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 500

library(tibble)
library(dplyr)
library(here)

# Ensure reproducibility
set.seed(42)

# Sample size - adequate for Benford's Law analysis
n <- 500

# ═══════════════════════════════════════════════════════════
# Helper Functions for Benford-Conforming Data Generation
# ═══════════════════════════════════════════════════════════

# Generate data following Benford's Law distribution
# Based on: log-uniform distribution spans multiple orders of magnitude
generate_benford_data <- function(n, min_val = 1, max_val = 10000) {
  # Log-uniform distribution naturally follows Benford's Law
  log_min <- log10(min_val)
  log_max <- log10(max_val)

  log_values <- runif(n, min = log_min, max = log_max)
  values <- 10^log_values

  return(values)
}

# Generate fabricated/uniform data that VIOLATES Benford's Law
# (e.g., data entry errors, fraud, rounding)
generate_fabricated_data <- function(n, min_val = 100, max_val = 999) {
  # Uniform distribution - does NOT follow Benford's Law
  # Each digit 1-9 appears with equal probability (~11%)
  values <- runif(n, min = min_val, max = max_val)
  return(values)
}

# Generate rounded/truncated data (common data quality issue)
generate_rounded_data <- function(n, base = 10) {
  # People tend to round to nice numbers (10, 20, 50, 100, etc.)
  # This violates Benford's Law
  raw_values <- generate_benford_data(n, 10, 1000)
  rounded_values <- round(raw_values / base) * base
  return(rounded_values)
}

# ═══════════════════════════════════════════════════════════
# Generate Main Test Dataset
# ═══════════════════════════════════════════════════════════

benford_test <- tibble(
  # Observation ID
  observation_id = 1:n
)

# ─────────────────────────────────────────────────────────
# 1. Natural Data (CONFORMS to Benford's Law)
# ─────────────────────────────────────────────────────────

# Population sizes (natural Benford data)
benford_test$population_size <- round(generate_benford_data(n, min_val = 100, max_val = 1000000))

# City populations (another natural Benford example)
benford_test$city_population <- round(generate_benford_data(n, min_val = 5000, max_val = 5000000))

# Financial transactions (naturally span orders of magnitude)
benford_test$transaction_amount <- round(generate_benford_data(n, min_val = 10, max_val = 100000), 2)

# ─────────────────────────────────────────────────────────
# 2. Clinical/Laboratory Data (Generally Conforms)
# ─────────────────────────────────────────────────────────

# White Blood Cell count (varies widely, 1000-50000/μL)
# Natural biological variation - follows Benford reasonably well
benford_test$wbc_count <- round(10^rnorm(n, mean = log10(7000), sd = 0.4))
benford_test$wbc_count <- pmax(1000, pmin(50000, benford_test$wbc_count))

# Platelet count (varies 20,000-500,000/μL)
benford_test$platelet_count <- round(10^rnorm(n, mean = log10(250000), sd = 0.3))
benford_test$platelet_count <- pmax(20000, pmin(500000, benford_test$platelet_count))

# Tumor marker CA-125 (highly variable, 1-10000 U/mL)
# Very wide range - excellent Benford example
benford_test$ca125_level <- round(10^rnorm(n, mean = log10(30), sd = 1.2), 1)
benford_test$ca125_level <- pmax(1, pmin(10000, benford_test$ca125_level))

# PSA levels (wide range 0.1-1000 ng/mL)
benford_test$psa_level <- round(10^rnorm(n, mean = log10(4), sd = 1.0), 2)
benford_test$psa_level <- pmax(0.1, pmin(1000, benford_test$psa_level))

# Gene expression values (RNA-seq counts, 1-100000)
benford_test$gene_expression <- round(10^rnorm(n, mean = log10(500), sd = 1.5))
benford_test$gene_expression <- pmax(1, pmin(100000, benford_test$gene_expression))

# ─────────────────────────────────────────────────────────
# 3. Fabricated/Suspicious Data (VIOLATES Benford's Law)
# ─────────────────────────────────────────────────────────

# Uniform random (data entry errors, made-up numbers)
benford_test$uniform_random <- round(runif(n, min = 100, max = 999))

# Rounded values (people round to nice numbers)
benford_test$rounded_values <- generate_rounded_data(n, base = 10)

# Truncated data (artificial cutoffs)
benford_test$truncated_data <- round(runif(n, min = 200, max = 299))

# Repeated patterns (copy-paste errors)
# Create repeating sequences
repeat_pattern <- rep(c(123, 234, 345, 456, 567, 678, 789), length.out = n)
benford_test$repeated_pattern <- repeat_pattern + runif(n, -5, 5)

# ─────────────────────────────────────────────────────────
# 4. Healthcare/Economic Data (Mixed Conformity)
# ─────────────────────────────────────────────────────────

# Hospital charges (spans orders of magnitude)
benford_test$hospital_charges <- round(generate_benford_data(n, min_val = 100, max_val = 500000), 2)

# Length of stay (days) - narrower range, less Benford-like
benford_test$length_of_stay <- round(10^rnorm(n, mean = log10(5), sd = 0.5))
benford_test$length_of_stay <- pmax(1, pmin(90, benford_test$length_of_stay))

# Medicare reimbursement amounts
benford_test$medicare_payment <- round(generate_benford_data(n, min_val = 50, max_val = 50000), 2)

# Prescription costs
benford_test$prescription_cost <- round(generate_benford_data(n, min_val = 5, max_val = 5000), 2)

# ─────────────────────────────────────────────────────────
# 5. Edge Cases for Testing Validation
# ─────────────────────────────────────────────────────────

# Small positive values (tests magnitude range warning)
benford_test$narrow_range <- runif(n, min = 45, max = 55)

# Data with some zeros (tests validation)
benford_test$with_zeros <- c(
  rep(0, 10),  # 10 zeros
  generate_benford_data(n - 10, 10, 1000)
)

# Data with some negatives (tests validation)
benford_test$with_negatives <- c(
  rnorm(50, mean = -100, sd = 50),  # 50 negative values
  generate_benford_data(n - 50, 10, 1000)
)

# Very large values (genomic data)
benford_test$genomic_counts <- round(generate_benford_data(n, min_val = 100, max_val = 10000000))

# ═══════════════════════════════════════════════════════════
# Add Realistic Missing Data (~5%)
# ═══════════════════════════════════════════════════════════

# More missing in clinical biomarkers (not all patients tested)
n_missing_biomarker <- round(n * 0.08)
n_missing_general <- round(n * 0.03)

benford_test$ca125_level[sample(n, n_missing_biomarker)] <- NA
benford_test$psa_level[sample(n, n_missing_biomarker)] <- NA
benford_test$gene_expression[sample(n, n_missing_general)] <- NA
benford_test$hospital_charges[sample(n, n_missing_general)] <- NA

# ═══════════════════════════════════════════════════════════
# Create Smaller Subset Datasets for Edge Case Testing
# ═══════════════════════════════════════════════════════════

# Small sample (n=25, below recommended minimum of 30)
benford_small <- benford_test[1:25, c("observation_id", "population_size")]

# Tiny sample (n=10, very insufficient)
benford_tiny <- benford_test[1:10, c("observation_id", "ca125_level")]

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. Main dataset (RDA format)
save(benford_test, file = here::here("data", "benford_test.rda"))
message("✓ Created: data/benford_test.rda")

# 2. Main dataset (CSV format)
write.csv(benford_test,
          file = here::here("data", "benford_test.csv"),
          row.names = FALSE)
message("✓ Created: data/benford_test.csv")

# 3. Main dataset (Excel format)
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(benford_test,
                      path = here::here("data", "benford_test.xlsx"))
  message("✓ Created: data/benford_test.xlsx")
} else {
  message("⚠ writexl package not available - skipping Excel format")
}

# 4. Main dataset (Jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(benford_test,
                          here::here("data", "benford_test.omv"))
  message("✓ Created: data/benford_test.omv")
} else {
  message("⚠ jmvReadWrite package not available - skipping OMV format")
}

# 5. Small sample datasets (for edge case testing)
save(benford_small, file = here::here("data", "benford_small.rda"))
message("✓ Created: data/benford_small.rda (n=25)")

save(benford_tiny, file = here::here("data", "benford_tiny.rda"))
message("✓ Created: data/benford_tiny.rda (n=10)")

# ═══════════════════════════════════════════════════════════
# Dataset Documentation
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset: benford_test\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Observations:", n, "\n")
cat("Variables:", ncol(benford_test), "\n\n")

cat("Missing data profile:\n")
cat("  - Biomarkers (CA-125, PSA): ~8%\n")
cat("  - Other variables: ~3%\n\n")

cat("Variable categories:\n\n")

cat("1. NATURAL DATA (Conforms to Benford's Law) - 3 vars:\n")
cat("   - population_size: City/region populations (100 - 1,000,000)\n")
cat("   - city_population: Urban population counts (5,000 - 5,000,000)\n")
cat("   - transaction_amount: Financial transactions ($10 - $100,000)\n\n")

cat("2. CLINICAL/LABORATORY DATA (Generally Conforms) - 5 vars:\n")
cat("   - wbc_count: White blood cell count (1,000-50,000/μL)\n")
cat("   - platelet_count: Platelet count (20,000-500,000/μL)\n")
cat("   - ca125_level: Cancer antigen 125 (1-10,000 U/mL)\n")
cat("   - psa_level: Prostate specific antigen (0.1-1,000 ng/mL)\n")
cat("   - gene_expression: RNA-seq counts (1-100,000)\n\n")

cat("3. FABRICATED/SUSPICIOUS DATA (Violates Benford's Law) - 4 vars:\n")
cat("   - uniform_random: Uniform distribution (100-999)\n")
cat("   - rounded_values: Artificially rounded to multiples of 10\n")
cat("   - truncated_data: Narrow uniform range (200-299)\n")
cat("   - repeated_pattern: Repeating sequences (data entry errors)\n\n")

cat("4. HEALTHCARE/ECONOMIC DATA (Mixed Conformity) - 4 vars:\n")
cat("   - hospital_charges: Hospital billing ($100 - $500,000)\n")
cat("   - length_of_stay: Hospital days (1-90)\n")
cat("   - medicare_payment: Medicare reimbursement ($50 - $50,000)\n")
cat("   - prescription_cost: Drug costs ($5 - $5,000)\n\n")

cat("5. EDGE CASES (For Testing Validation) - 4 vars:\n")
cat("   - narrow_range: Limited range (45-55) - triggers warning\n")
cat("   - with_zeros: Includes 10 zero values - triggers error\n")
cat("   - with_negatives: Includes 50 negative values - triggers error\n")
cat("   - genomic_counts: Very large values (100 - 10,000,000)\n\n")

cat("Expected Benford's Law behavior:\n")
cat("  ✓ CONFORMING (MAD < 0.006): population_size, city_population, transaction_amount,\n")
cat("                               wbc_count, ca125_level, psa_level, hospital_charges\n")
cat("  ⚠ MARGINALLY CONFORMING (MAD 0.006-0.012): platelet_count, gene_expression,\n")
cat("                                               medicare_payment, prescription_cost\n")
cat("  ✗ NON-CONFORMING (MAD > 0.012): uniform_random, rounded_values, truncated_data,\n")
cat("                                   repeated_pattern, length_of_stay, narrow_range\n\n")

cat("MAD (Mean Absolute Deviation) interpretation:\n")
cat("  - MAD < 0.006: Close conformity to Benford's Law\n")
cat("  - MAD 0.006-0.012: Acceptable conformity\n")
cat("  - MAD 0.012-0.015: Marginally acceptable\n")
cat("  - MAD > 0.015: Non-conformity (potential data quality issues)\n\n")

cat("Use cases:\n")
cat("  1. Detect fabricated clinical trial data\n")
cat("  2. Identify data entry errors in lab results\n")
cat("  3. Audit financial healthcare data\n")
cat("  4. Quality control for genomic datasets\n")
cat("  5. Validate population health statistics\n\n")

cat("Usage examples:\n\n")

cat("# Load the dataset\n")
cat("data(benford_test, package = 'ClinicoPath')\n\n")

cat("# Analyze natural data (should conform)\n")
cat("library(ClinicoPath)\n")
cat("benford(\n")
cat("  data = benford_test,\n")
cat("  var = 'population_size',\n")
cat("  digits = 2\n")
cat(")\n\n")

cat("# Analyze fabricated data (should NOT conform)\n")
cat("benford(\n")
cat("  data = benford_test,\n")
cat("  var = 'uniform_random',\n")
cat("  digits = 2\n")
cat(")\n\n")

cat("# Analyze clinical biomarker (generally conforms)\n")
cat("benford(\n")
cat("  data = benford_test,\n")
cat("  var = 'ca125_level',\n")
cat("  digits = 2\n")
cat(")\n\n")

cat("# Analyze narrow range data (triggers warning)\n")
cat("benford(\n")
cat("  data = benford_test,\n")
cat("  var = 'narrow_range',\n")
cat("  digits = 1\n")
cat(")\n\n")

cat("# First digit analysis\n")
cat("benford(\n")
cat("  data = benford_test,\n")
cat("  var = 'transaction_amount',\n")
cat("  digits = 1\n")
cat(")\n\n")

cat("# First two digits analysis (more sensitive)\n")
cat("benford(\n")
cat("  data = benford_test,\n")
cat("  var = 'hospital_charges',\n")
cat("  digits = 2\n")
cat(")\n\n")

cat("Additional test datasets:\n")
cat("  - benford_small.rda: n=25 (below minimum, triggers warning)\n")
cat("  - benford_tiny.rda: n=10 (very insufficient)\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset generation complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
