# ═══════════════════════════════════════════════════════════
# Test Data Generation: categorize
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the categorize jamovi function
#
# Purpose: Generate continuous numeric variables suitable for testing various
#          binning/categorization methods (quantile, equal, manual, mean±SD,
#          median split, Jenks natural breaks)
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 200

library(tibble)
library(dplyr)
library(here)

# Ensure reproducibility
set.seed(42)

# Sample size - adequate for categorization testing
n <- 200

# ═══════════════════════════════════════════════════════════
# Generate Main Test Dataset
# ═══════════════════════════════════════════════════════════

categorize_test <- tibble(
  # Patient ID
  patient_id = 1:n
)

# ─────────────────────────────────────────────────────────
# 1. Normal Distributions (Good for all methods)
# ─────────────────────────────────────────────────────────

# Age (years, 20-90, normal distribution centered at 60)
categorize_test$age <- round(rnorm(n, mean = 60, sd = 15))
categorize_test$age <- pmax(20, pmin(90, categorize_test$age))

# BMI (kg/m², 15-45, normal distribution centered at 25)
categorize_test$bmi <- round(rnorm(n, mean = 25, sd = 5), 1)
categorize_test$bmi <- pmax(15, pmin(45, categorize_test$bmi))

# Systolic BP (mmHg, 90-200, normal distribution centered at 130)
categorize_test$systolic_bp <- round(rnorm(n, mean = 130, sd = 20))
categorize_test$systolic_bp <- pmax(90, pmin(200, categorize_test$systolic_bp))

# ─────────────────────────────────────────────────────────
# 2. Skewed Distributions (Test robustness)
# ─────────────────────────────────────────────────────────

# PSA (ng/mL, 0.1-100, right-skewed - log-normal)
categorize_test$psa_level <- round(exp(rnorm(n, mean = log(4), sd = 1.2)), 2)
categorize_test$psa_level <- pmax(0.1, pmin(100, categorize_test$psa_level))

# Tumor size (mm, 5-150, right-skewed)
categorize_test$tumor_size <- round(exp(rnorm(n, mean = log(30), sd = 0.8)))
categorize_test$tumor_size <- pmax(5, pmin(150, categorize_test$tumor_size))

# C-reactive protein (mg/L, 0.1-200, highly right-skewed)
categorize_test$crp <- round(exp(rnorm(n, mean = log(5), sd = 1.5)), 1)
categorize_test$crp <- pmax(0.1, pmin(200, categorize_test$crp))

# ─────────────────────────────────────────────────────────
# 3. Wide Range Variables (Good for equal intervals)
# ─────────────────────────────────────────────────────────

# White blood cell count (cells/μL, 1000-30000)
categorize_test$wbc_count <- round(runif(n, min = 1000, max = 30000))

# CA-125 tumor marker (U/mL, 1-1000, very wide range)
categorize_test$ca125 <- round(exp(rnorm(n, mean = log(30), sd = 1.5)), 1)
categorize_test$ca125 <- pmax(1, pmin(1000, categorize_test$ca125))

# Hospital charges (USD, 1000-100000)
categorize_test$hospital_charges <- round(runif(n, min = 1000, max = 100000), 2)

# ─────────────────────────────────────────────────────────
# 4. Narrow Range Variables (Edge case testing)
# ─────────────────────────────────────────────────────────

# Temperature (°C, 36-40, narrow range)
categorize_test$temperature <- round(rnorm(n, mean = 37, sd = 0.8), 1)
categorize_test$temperature <- pmax(36, pmin(40, categorize_test$temperature))

# Oxygen saturation (%, 85-100, narrow range)
categorize_test$oxygen_sat <- round(rnorm(n, mean = 96, sd = 3))
categorize_test$oxygen_sat <- pmax(85, pmin(100, categorize_test$oxygen_sat))

# pH (7.0-7.8, very narrow range)
categorize_test$blood_ph <- round(rnorm(n, mean = 7.4, sd = 0.1), 2)
categorize_test$blood_ph <- pmax(7.0, pmin(7.8, categorize_test$blood_ph))

# ─────────────────────────────────────────────────────────
# 5. Variables with Outliers (Test method behavior)
# ─────────────────────────────────────────────────────────

# Creatinine (mg/dL, mostly 0.5-1.5, some high outliers)
categorize_test$creatinine <- rnorm(n, mean = 1.0, sd = 0.3)
categorize_test$creatinine <- pmax(0.5, categorize_test$creatinine)
# Add 10 outliers (renal failure patients)
outlier_indices <- sample(n, 10)
categorize_test$creatinine[outlier_indices] <- runif(10, min = 3, max = 8)

# Hemoglobin (g/dL, mostly 10-16, some low outliers)
categorize_test$hemoglobin <- rnorm(n, mean = 13, sd = 1.5)
categorize_test$hemoglobin <- pmax(6, pmin(18, categorize_test$hemoglobin))
# Add 8 outliers (severe anemia)
outlier_indices2 <- sample(n, 8)
categorize_test$hemoglobin[outlier_indices2] <- runif(8, min = 6, max = 8)

# ─────────────────────────────────────────────────────────
# 6. Variables with Few Unique Values (Test quantile ties)
# ─────────────────────────────────────────────────────────

# ECOG Performance Status (0-4, only 5 possible values)
categorize_test$ecog_score <- sample(0:4, n, replace = TRUE,
                                     prob = c(0.3, 0.35, 0.2, 0.1, 0.05))

# Pain score (0-10, 11 possible values)
categorize_test$pain_score <- sample(0:10, n, replace = TRUE,
                                     prob = c(0.15, 0.15, 0.12, 0.1, 0.1,
                                             0.1, 0.08, 0.08, 0.06, 0.04, 0.02))

# ─────────────────────────────────────────────────────────
# 7. Edge Cases for Validation Testing
# ─────────────────────────────────────────────────────────

# Zero variance variable (constant, should fail median/meansd methods)
categorize_test$constant_value <- rep(100, n)

# Near-zero variance (very small SD)
categorize_test$minimal_variance <- rnorm(n, mean = 50, sd = 0.01)

# Bimodal distribution (good for Jenks natural breaks)
mode1 <- rnorm(n/2, mean = 30, sd = 5)
mode2 <- rnorm(n/2, mean = 70, sd = 5)
categorize_test$bimodal_dist <- c(mode1, mode2)
categorize_test$bimodal_dist <- sample(categorize_test$bimodal_dist) # shuffle

# ─────────────────────────────────────────────────────────
# 8. Clinical Scores and Indices
# ─────────────────────────────────────────────────────────

# Framingham Risk Score (0-30, cardiovascular risk)
categorize_test$framingham_score <- round(rnorm(n, mean = 12, sd = 5))
categorize_test$framingham_score <- pmax(0, pmin(30, categorize_test$framingham_score))

# Gleason score (6-10, prostate cancer grading)
# Only 5 values: 6, 7, 8, 9, 10
categorize_test$gleason_score <- sample(6:10, n, replace = TRUE,
                                        prob = c(0.25, 0.35, 0.2, 0.15, 0.05))

# Ki-67 proliferation index (%, 0-100)
categorize_test$ki67_index <- round(rbeta(n, shape1 = 2, shape2 = 5) * 100, 1)

# ═══════════════════════════════════════════════════════════
# Add Realistic Missing Data (~5%)
# ═══════════════════════════════════════════════════════════

n_missing_biomarker <- round(n * 0.08)  # 8% for lab tests
n_missing_general <- round(n * 0.03)    # 3% for routine measures

# More missing in specialized tests
categorize_test$psa_level[sample(n, n_missing_biomarker)] <- NA
categorize_test$ca125[sample(n, n_missing_biomarker)] <- NA
categorize_test$ki67_index[sample(n, n_missing_biomarker)] <- NA

# Less missing in routine measures
categorize_test$bmi[sample(n, n_missing_general)] <- NA
categorize_test$systolic_bp[sample(n, n_missing_general)] <- NA
categorize_test$creatinine[sample(n, n_missing_general)] <- NA

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. Main dataset (RDA format)
save(categorize_test, file = here::here("data", "categorize_test.rda"))
message("✓ Created: data/categorize_test.rda")

# 2. Main dataset (CSV format)
write.csv(categorize_test,
          file = here::here("data", "categorize_test.csv"),
          row.names = FALSE)
message("✓ Created: data/categorize_test.csv")

# 3. Main dataset (Excel format)
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(categorize_test,
                      path = here::here("data", "categorize_test.xlsx"))
  message("✓ Created: data/categorize_test.xlsx")
} else {
  message("⚠ writexl package not available - skipping Excel format")
}

# 4. Main dataset (Jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(categorize_test,
                          here::here("data", "categorize_test.omv"))
  message("✓ Created: data/categorize_test.omv")
} else {
  message("⚠ jmvReadWrite package not available - skipping OMV format")
}

# ═══════════════════════════════════════════════════════════
# Dataset Documentation
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset: categorize_test\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Observations:", n, "\n")
cat("Variables:", ncol(categorize_test), "\n\n")

cat("Missing data profile:\n")
cat("  - Biomarkers (PSA, CA-125, Ki-67): ~8%\n")
cat("  - Routine measures (BMI, BP, creatinine): ~3%\n\n")

cat("Variable categories:\n\n")

cat("1. NORMAL DISTRIBUTIONS (Good for all methods) - 3 vars:\n")
cat("   - age: Patient age in years (20-90)\n")
cat("   - bmi: Body Mass Index kg/m² (15-45)\n")
cat("   - systolic_bp: Systolic blood pressure mmHg (90-200)\n\n")

cat("2. SKEWED DISTRIBUTIONS (Test robustness) - 3 vars:\n")
cat("   - psa_level: PSA ng/mL (0.1-100, right-skewed)\n")
cat("   - tumor_size: Tumor size mm (5-150, right-skewed)\n")
cat("   - crp: C-reactive protein mg/L (0.1-200, highly skewed)\n\n")

cat("3. WIDE RANGE VARIABLES (Good for equal intervals) - 3 vars:\n")
cat("   - wbc_count: White blood cell count cells/μL (1000-30000)\n")
cat("   - ca125: CA-125 tumor marker U/mL (1-1000)\n")
cat("   - hospital_charges: Hospital charges USD (1000-100000)\n\n")

cat("4. NARROW RANGE VARIABLES (Edge case testing) - 3 vars:\n")
cat("   - temperature: Body temperature °C (36-40)\n")
cat("   - oxygen_sat: Oxygen saturation % (85-100)\n")
cat("   - blood_ph: Blood pH (7.0-7.8)\n\n")

cat("5. VARIABLES WITH OUTLIERS (Test method behavior) - 2 vars:\n")
cat("   - creatinine: Serum creatinine mg/dL (0.5-8, 10 outliers)\n")
cat("   - hemoglobin: Hemoglobin g/dL (6-18, 8 outliers)\n\n")

cat("6. FEW UNIQUE VALUES (Test quantile ties) - 2 vars:\n")
cat("   - ecog_score: ECOG Performance Status (0-4, only 5 values)\n")
cat("   - pain_score: Pain score (0-10, 11 values)\n\n")

cat("7. EDGE CASES (For testing validation) - 3 vars:\n")
cat("   - constant_value: Constant (all 100) - fails median/meansd\n")
cat("   - minimal_variance: Near-zero variance (SD=0.01)\n")
cat("   - bimodal_dist: Bimodal distribution (peaks at 30 and 70)\n\n")

cat("8. CLINICAL SCORES (Realistic medical data) - 3 vars:\n")
cat("   - framingham_score: Framingham cardiovascular risk (0-30)\n")
cat("   - gleason_score: Gleason prostate cancer grade (6-10)\n")
cat("   - ki67_index: Ki-67 proliferation index % (0-100)\n\n")

cat("Optimal binning method by variable type:\n")
cat("  ✓ QUANTILE (default): age, bmi, systolic_bp, wbc_count, hospital_charges\n")
cat("  ✓ EQUAL INTERVALS: temperature, oxygen_sat, blood_ph, framingham_score\n")
cat("  ✓ MEAN±SD: age, bmi, systolic_bp (normally distributed)\n")
cat("  ✓ MEDIAN SPLIT: Any continuous variable for binary categorization\n")
cat("  ✓ JENKS NATURAL BREAKS: bimodal_dist, crp, tumor_size (irregular distributions)\n")
cat("  ✓ MANUAL: PSA (0, 4, 10, 20, 100), BMI (18.5, 25, 30), Age (18, 40, 65)\n\n")

cat("Expected behavior:\n")
cat("  ✅ Should work: All normal/skewed/wide/narrow range variables\n")
cat("  ⚠ Warning expected: ecog_score, pain_score (few unique values with quantile)\n")
cat("  ❌ Should fail: constant_value (zero variance with median/meansd methods)\n\n")

cat("Use cases:\n")
cat("  1. Age categorization (Young/Middle/Older) - use quantile or manual\n")
cat("  2. BMI classification (Underweight/Normal/Overweight/Obese) - manual breaks\n")
cat("  3. Risk stratification (Low/Medium/High) - use quantile or mean±SD\n")
cat("  4. PSA interpretation (Normal/Borderline/Elevated/High) - manual breaks\n")
cat("  5. Biomarker tertiles/quartiles for survival analysis - quantile method\n\n")

cat("Usage examples:\n\n")

cat("# Load the dataset\n")
cat("data(categorize_test, package = 'ClinicoPath')\n\n")

cat("# Example 1: Age into quartiles\n")
cat("library(ClinicoPath)\n")
cat("categorize(\n")
cat("  data = categorize_test,\n")
cat("  var = 'age',\n")
cat("  method = 'quantile',\n")
cat("  nbins = 4,\n")
cat("  labels = 'semantic'\n")
cat(")  # Labels: Low, Medium-Low, Medium-High, High\n\n")

cat("# Example 2: BMI with WHO categories (manual breaks)\n")
cat("categorize(\n")
cat("  data = categorize_test,\n")
cat("  var = 'bmi',\n")
cat("  method = 'manual',\n")
cat("  breaks = '15, 18.5, 25, 30, 45',\n")
cat("  labels = 'custom',\n")
cat("  customlabels = 'Underweight, Normal, Overweight, Obese'\n")
cat(")  # WHO classification\n\n")

cat("# Example 3: PSA risk categories (manual breaks)\n")
cat("categorize(\n")
cat("  data = categorize_test,\n")
cat("  var = 'psa_level',\n")
cat("  method = 'manual',\n")
cat("  breaks = '0, 4, 10, 20, 100',\n")
cat("  labels = 'custom',\n")
cat("  customlabels = 'Normal, Borderline, Elevated, Very High'\n")
cat(")  # Clinical PSA interpretation\n\n")

cat("# Example 4: Mean ± SD categorization (3 categories)\n")
cat("categorize(\n")
cat("  data = categorize_test,\n")
cat("  var = 'systolic_bp',\n")
cat("  method = 'meansd',\n")
cat("  sdmult = 1,\n")
cat("  labels = 'custom',\n")
cat("  customlabels = 'Low, Normal, High'\n")
cat(")  # Low (<M-SD), Normal (M±SD), High (>M+SD)\n\n")

cat("# Example 5: Median split (binary categorization)\n")
cat("categorize(\n")
cat("  data = categorize_test,\n")
cat("  var = 'tumor_size',\n")
cat("  method = 'median',\n")
cat("  labels = 'custom',\n")
cat("  customlabels = 'Small, Large'\n")
cat(")  # Dichotomization at median\n\n")

cat("# Example 6: Jenks natural breaks (optimal clustering)\n")
cat("categorize(\n")
cat("  data = categorize_test,\n")
cat("  var = 'bimodal_dist',\n")
cat("  method = 'jenks',\n")
cat("  nbins = 2,\n")
cat("  labels = 'semantic'\n")
cat(")  # Finds natural groupings\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset generation complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
