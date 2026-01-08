# ═══════════════════════════════════════════════════════════
# Test Data Generation: checkdata
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the checkdata jamovi function
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 200
#
# Single Variable Quality Check Scenarios Tested:
# 1. Perfect quality (no issues)
# 2. Variables with outliers (mild, extreme, both tails)
# 3. Variables with missing data (low, moderate, high, all missing)
# 4. Variables with duplicates
# 5. Variables with rare categories
# 6. Clinical variables (age, height, weight, lab values)
# 7. Different distributions (normal, skewed, bimodal)
# 8. Variables needing transformation (log-normal)
# 9. Unit system issues (metric vs imperial mix)
# 10. Edge cases (constant, single value, infinite values)

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# Sample size
n <- 200

# ═══════════════════════════════════════════════════════════
# PATIENT IDENTIFIERS
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PT%04d", 1:n)
study_site <- sample(c("Site A", "Site B", "Site C"), n, replace = TRUE)

# ═══════════════════════════════════════════════════════════
# 1. PERFECT QUALITY VARIABLES (No Issues)
# ═══════════════════════════════════════════════════════════

# Perfect continuous - normal distribution, no outliers, no missing
age_perfect <- round(rnorm(n, mean = 55, sd = 12))
age_perfect <- pmin(pmax(age_perfect, 18), 95)  # Realistic bounds

# Perfect categorical - balanced categories, no rare categories
treatment_perfect <- sample(c("Control", "Treatment A", "Treatment B", "Treatment C"),
                            n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))

# Perfect binary - balanced
sex_perfect <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))

# ═══════════════════════════════════════════════════════════
# 2. VARIABLES WITH OUTLIERS
# ═══════════════════════════════════════════════════════════

# Mild outliers (z-score 3-4)
glucose_mild_outliers <- c(
  rnorm(round(n * 0.94), mean = 100, sd = 15),      # 94% normal
  rnorm(round(n * 0.03), mean = 160, sd = 10),      # 3% high (z~4)
  rnorm(round(n * 0.03), mean = 40, sd = 5)         # 3% low (z~-4)
)
glucose_mild_outliers <- pmax(glucose_mild_outliers[1:n], 20)
glucose_mild_outliers <- round(glucose_mild_outliers, 1)

# Extreme outliers (z-score > 5)
creatinine_extreme_outliers <- c(
  rnorm(round(n * 0.97), mean = 1.0, sd = 0.2),     # 97% normal
  rnorm(round(n * 0.03), mean = 12.0, sd = 2.0)     # 3% extreme (renal failure, z>50)
)
creatinine_extreme_outliers <- pmax(creatinine_extreme_outliers[1:n], 0.3)
creatinine_extreme_outliers <- round(creatinine_extreme_outliers, 2)

# Only high-tail outliers (asymmetric)
alt_high_outliers <- c(
  rlnorm(round(n * 0.95), meanlog = log(25), sdlog = 0.4),   # 95% normal
  rlnorm(round(n * 0.05), meanlog = log(350), sdlog = 0.6)   # 5% elevated (hepatitis)
)
alt_high_outliers <- round(pmin(alt_high_outliers[1:n], 2000))

# Only low-tail outliers (asymmetric)
hemoglobin_low_outliers <- c(
  rnorm(round(n * 0.94), mean = 13.5, sd = 1.5),    # 94% normal
  rnorm(round(n * 0.06), mean = 7.0, sd = 1.0)      # 6% anemia
)
hemoglobin_low_outliers <- pmax(hemoglobin_low_outliers[1:n], 4)
hemoglobin_low_outliers <- round(hemoglobin_low_outliers, 1)

# ═══════════════════════════════════════════════════════════
# 3. VARIABLES WITH MISSING DATA
# ═══════════════════════════════════════════════════════════

# Low missing (3%)
bmi_low_missing <- rnorm(n, mean = 26, sd = 5)
bmi_low_missing <- pmin(pmax(bmi_low_missing, 15), 50)
bmi_low_missing <- round(bmi_low_missing, 1)
bmi_low_missing[sample(n, round(n * 0.03))] <- NA

# Moderate missing (15%)
cholesterol_moderate_missing <- rnorm(n, mean = 200, sd = 40)
cholesterol_moderate_missing <- pmax(cholesterol_moderate_missing, 100)
cholesterol_moderate_missing <- round(cholesterol_moderate_missing)
cholesterol_moderate_missing[sample(n, round(n * 0.15))] <- NA

# High missing (40%)
genetic_test_high_missing <- sample(c("Positive", "Negative", "Uncertain"),
                                    n, replace = TRUE, prob = c(0.15, 0.70, 0.15))
genetic_test_high_missing[sample(n, round(n * 0.40))] <- NA

# All missing (100%)
future_biomarker_all_missing <- rep(NA_real_, n)

# ═══════════════════════════════════════════════════════════
# 4. VARIABLES WITH DUPLICATES
# ═══════════════════════════════════════════════════════════

# High duplicates - only a few unique values
tumor_grade_duplicates <- sample(c("Grade 1", "Grade 2", "Grade 3"),
                                 n, replace = TRUE, prob = c(0.3, 0.5, 0.2))

# Many duplicates - repeated measurements
visit_number_duplicates <- sample(1:5, n, replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.12, 0.08))

# Few duplicates - mostly unique
patient_code_few_duplicates <- sprintf("PAT%03d", sample(1:180, n, replace = TRUE))

# ═══════════════════════════════════════════════════════════
# 5. VARIABLES WITH RARE CATEGORIES
# ═══════════════════════════════════════════════════════════

# One rare category (<5%)
histology_rare_category <- sample(
  c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell", "Rare Subtype"),
  n, replace = TRUE,
  prob = c(0.45, 0.35, 0.12, 0.05, 0.03)  # Rare Subtype is 3% (rare)
)

# Multiple rare categories
diagnosis_multiple_rare <- sample(
  c("Healthy", "Diabetes", "Hypertension", "Cancer", "Rare Disease A", "Rare Disease B"),
  n, replace = TRUE,
  prob = c(0.50, 0.25, 0.15, 0.06, 0.02, 0.02)  # Two rare diseases
)

# ═══════════════════════════════════════════════════════════
# 6. CLINICAL VARIABLES (For Clinical Validation)
# ═══════════════════════════════════════════════════════════

# Age - realistic clinical range
age_clinical <- round(rnorm(n, mean = 62, sd = 15))
age_clinical <- pmin(pmax(age_clinical, 18), 100)

# Height - metric (cm)
height_metric <- round(rnorm(n, mean = 170, sd = 10))
height_metric <- pmin(pmax(height_metric, 140), 210)

# Height - with imperial mix (data entry errors)
height_mixed_units <- height_metric
# Some entered in inches instead of cm (outliers)
mixed_idx <- sample(n, round(n * 0.04))
height_mixed_units[mixed_idx] <- round(rnorm(length(mixed_idx), mean = 67, sd = 4))

# Weight - metric (kg)
weight_metric <- round(rnorm(n, mean = 75, sd = 15), 1)
weight_metric <- pmin(pmax(weight_metric, 40), 150)

# Systolic BP - realistic range
systolic_bp_clinical <- round(rnorm(n, mean = 128, sd = 18))
systolic_bp_clinical <- pmin(pmax(systolic_bp_clinical, 80), 200)

# Heart rate - realistic range
heart_rate_clinical <- round(rnorm(n, mean = 72, sd = 12))
heart_rate_clinical <- pmin(pmax(heart_rate_clinical, 45), 150)

# Temperature - realistic range (°C)
temperature_clinical <- round(rnorm(n, mean = 37.0, sd = 0.5), 1)
temperature_clinical <- pmin(pmax(temperature_clinical, 35.0), 40.0)

# Hemoglobin - realistic clinical range
hemoglobin_clinical <- round(rnorm(n, mean = 13.5, sd = 1.8), 1)
hemoglobin_clinical <- pmin(pmax(hemoglobin_clinical, 7.0), 18.0)

# WBC count - realistic range
wbc_clinical <- round(rnorm(n, mean = 7.5, sd = 2.5), 1)
wbc_clinical <- pmax(wbc_clinical, 2.0)

# ═══════════════════════════════════════════════════════════
# 7. DIFFERENT DISTRIBUTIONS
# ═══════════════════════════════════════════════════════════

# Highly right-skewed (log-normal) - needs transformation
psa_right_skewed <- rlnorm(n, meanlog = log(4), sdlog = 1.2)
psa_right_skewed <- round(pmin(psa_right_skewed, 200), 2)

# Highly left-skewed
time_to_event_left_skewed <- 100 - rlnorm(n, meanlog = log(10), sdlog = 0.8)
time_to_event_left_skewed <- pmax(time_to_event_left_skewed, 0)
time_to_event_left_skewed <- round(time_to_event_left_skewed, 1)

# Bimodal distribution (two populations)
biomarker_bimodal <- c(
  rnorm(round(n * 0.6), mean = 50, sd = 10),   # Population 1
  rnorm(round(n * 0.4), mean = 120, sd = 15)   # Population 2
)
biomarker_bimodal <- round(biomarker_bimodal[1:n], 1)

# Uniform distribution
random_uniform <- round(runif(n, min = 0, max = 100), 1)

# Heavy-tailed (t-distribution)
heavy_tailed_var <- rt(n, df = 3) * 15 + 100
heavy_tailed_var <- round(heavy_tailed_var, 1)

# ═══════════════════════════════════════════════════════════
# 8. VARIABLES NEEDING TRANSFORMATION
# ═══════════════════════════════════════════════════════════

# Log transformation needed (multiplicative errors, right skew)
crp_log_transform <- rlnorm(n, meanlog = log(5), sdlog = 1.5)
crp_log_transform <- round(pmin(crp_log_transform, 500), 1)

# Square root transformation needed (count data, variance proportional to mean)
platelet_count_sqrt <- rpois(n, lambda = 250)
platelet_count_sqrt[platelet_count_sqrt < 50] <- platelet_count_sqrt[platelet_count_sqrt < 50] + 50

# ═══════════════════════════════════════════════════════════
# 9. UNIT SYSTEM ISSUES
# ═══════════════════════════════════════════════════════════

# Weight with mixed units (kg and lb)
weight_mixed_units <- weight_metric
# Some entered in lb instead of kg
mixed_wt_idx <- sample(n, round(n * 0.05))
weight_mixed_units[mixed_wt_idx] <- round(weight_mixed_units[mixed_wt_idx] * 2.2, 1)  # Convert to lb (outliers)

# Temperature mixed (Celsius and Fahrenheit)
temperature_mixed_units <- temperature_clinical
# Some entered in Fahrenheit instead of Celsius
mixed_temp_idx <- sample(n, round(n * 0.03))
temperature_mixed_units[mixed_temp_idx] <- round(temperature_mixed_units[mixed_temp_idx] * 1.8 + 32, 1)

# ═══════════════════════════════════════════════════════════
# 10. EDGE CASES
# ═══════════════════════════════════════════════════════════

# Constant variable (all same value)
constant_var <- rep("Site A", n)

# Nearly constant (99% same value)
nearly_constant <- c(rep(1, round(n * 0.99)), rep(2, n - round(n * 0.99)))

# Single unique value (after removing missing)
single_value_var <- rep(42, n)
single_value_var[1:10] <- NA  # Add some missing

# Very small variance
tiny_variance <- rnorm(n, mean = 37.0, sd = 0.001)
tiny_variance <- round(tiny_variance, 3)

# Infinite values (computational errors)
with_infinite <- rnorm(n, mean = 100, sd = 20)
with_infinite[c(5, 15, 25)] <- Inf
with_infinite[c(10, 20, 30)] <- -Inf

# Extreme range (possible data entry errors)
extreme_range <- c(
  rnorm(round(n * 0.95), mean = 50, sd = 5),       # Normal range
  c(0.001, 50000, -10000, 99999, 0.0001)           # Extreme outliers
)
extreme_range <- extreme_range[1:n]

# ═══════════════════════════════════════════════════════════
# DATE VARIABLES
# ═══════════════════════════════════════════════════════════

# Date of birth
date_of_birth <- as.Date("2024-01-01") - round(age_clinical * 365.25)

# Study enrollment date
enrollment_date <- as.Date("2023-01-01") + sample(0:730, n, replace = TRUE)

# ═══════════════════════════════════════════════════════════
# COMBINE INTO DATASET
# ═══════════════════════════════════════════════════════════

checkdata_test <- tibble(
  # Identifiers
  patient_id = patient_id,
  study_site = study_site,

  # 1. Perfect quality
  age_perfect = age_perfect,
  treatment_perfect = treatment_perfect,
  sex_perfect = sex_perfect,

  # 2. Variables with outliers
  glucose_mild_outliers = glucose_mild_outliers,
  creatinine_extreme_outliers = creatinine_extreme_outliers,
  alt_high_outliers = alt_high_outliers,
  hemoglobin_low_outliers = hemoglobin_low_outliers,

  # 3. Variables with missing data
  bmi_low_missing = bmi_low_missing,
  cholesterol_moderate_missing = cholesterol_moderate_missing,
  genetic_test_high_missing = genetic_test_high_missing,
  future_biomarker_all_missing = future_biomarker_all_missing,

  # 4. Variables with duplicates
  tumor_grade_duplicates = tumor_grade_duplicates,
  visit_number_duplicates = visit_number_duplicates,
  patient_code_few_duplicates = patient_code_few_duplicates,

  # 5. Variables with rare categories
  histology_rare_category = histology_rare_category,
  diagnosis_multiple_rare = diagnosis_multiple_rare,

  # 6. Clinical variables
  age_clinical = age_clinical,
  height_metric = height_metric,
  height_mixed_units = height_mixed_units,
  weight_metric = weight_metric,
  systolic_bp_clinical = systolic_bp_clinical,
  heart_rate_clinical = heart_rate_clinical,
  temperature_clinical = temperature_clinical,
  hemoglobin_clinical = hemoglobin_clinical,
  wbc_clinical = wbc_clinical,

  # 7. Different distributions
  psa_right_skewed = psa_right_skewed,
  time_to_event_left_skewed = time_to_event_left_skewed,
  biomarker_bimodal = biomarker_bimodal,
  random_uniform = random_uniform,
  heavy_tailed_var = heavy_tailed_var,

  # 8. Variables needing transformation
  crp_log_transform = crp_log_transform,
  platelet_count_sqrt = platelet_count_sqrt,

  # 9. Unit system issues
  weight_mixed_units = weight_mixed_units,
  temperature_mixed_units = temperature_mixed_units,

  # 10. Edge cases
  constant_var = constant_var,
  nearly_constant = nearly_constant,
  single_value_var = single_value_var,
  tiny_variance = tiny_variance,
  with_infinite = with_infinite,
  extreme_range = extreme_range,

  # Dates
  date_of_birth = date_of_birth,
  enrollment_date = enrollment_date
)

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(checkdata_test, file = here::here("data", "checkdata_test.rda"))

# 2. CSV format
write.csv(checkdata_test, file = here::here("data", "checkdata_test.csv"), row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(checkdata_test, path = here::here("data", "checkdata_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(checkdata_test, here::here("data", "checkdata_test.omv"))

# ═══════════════════════════════════════════════════════════
# DATASET DOCUMENTATION
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
CHECKDATA TEST DATASET
═══════════════════════════════════════════════════════════

Dataset: checkdata_test
Total Observations: ", n, "
Variables: ", ncol(checkdata_test), "
Generated: ", Sys.Date(), "
Seed: 42

═══════════════════════════════════════════════════════════
VARIABLE DESCRIPTIONS BY QUALITY CATEGORY
═══════════════════════════════════════════════════════════

IDENTIFIERS (2 variables):
  1. patient_id              - Unique patient identifier
  2. study_site              - Study site (A/B/C)

1. PERFECT QUALITY (No Issues) - 3 variables:
  3. age_perfect             - Age in years, normal dist, no outliers/missing
  4. treatment_perfect       - Treatment group, balanced categories
  5. sex_perfect             - Patient sex, balanced binary

2. VARIABLES WITH OUTLIERS - 4 variables:
  6. glucose_mild_outliers          - 6% mild outliers (z~4, both tails)
  7. creatinine_extreme_outliers    - 3% extreme outliers (z>50, renal failure)
  8. alt_high_outliers              - 5% high outliers (log-normal, hepatitis)
  9. hemoglobin_low_outliers        - 6% low outliers (anemia)

3. VARIABLES WITH MISSING DATA - 4 variables:
 10. bmi_low_missing                - 3% missing (low)
 11. cholesterol_moderate_missing   - 15% missing (moderate)
 12. genetic_test_high_missing      - 40% missing (high)
 13. future_biomarker_all_missing   - 100% missing (all)

4. VARIABLES WITH DUPLICATES - 3 variables:
 14. tumor_grade_duplicates         - High duplicates (3 unique values)
 15. visit_number_duplicates        - Many duplicates (1-5 visits)
 16. patient_code_few_duplicates    - Few duplicates (~90% unique)

5. VARIABLES WITH RARE CATEGORIES - 2 variables:
 17. histology_rare_category        - One rare category (3% < 5% threshold)
 18. diagnosis_multiple_rare        - Two rare categories (2% each)

6. CLINICAL VARIABLES - 9 variables:
 19. age_clinical                   - Age (18-100 years)
 20. height_metric                  - Height in cm (140-210)
 21. height_mixed_units             - Height with 4% in inches (unit errors)
 22. weight_metric                  - Weight in kg (40-150)
 23. systolic_bp_clinical           - Systolic BP (80-200 mmHg)
 24. heart_rate_clinical            - Heart rate (45-150 bpm)
 25. temperature_clinical           - Temperature (35-40 °C)
 26. hemoglobin_clinical            - Hemoglobin (7-18 g/dL)
 27. wbc_clinical                   - WBC count (>2 ×10⁹/L)

7. DIFFERENT DISTRIBUTIONS - 5 variables:
 28. psa_right_skewed               - Highly right-skewed (log-normal)
 29. time_to_event_left_skewed      - Left-skewed distribution
 30. biomarker_bimodal              - Bimodal (two populations)
 31. random_uniform                 - Uniform distribution
 32. heavy_tailed_var               - Heavy-tailed (t-distribution)

8. VARIABLES NEEDING TRANSFORMATION - 2 variables:
 33. crp_log_transform              - Log transformation needed (right skew)
 34. platelet_count_sqrt            - Square root transform needed (count data)

9. UNIT SYSTEM ISSUES - 2 variables:
 35. weight_mixed_units             - 5% in lb instead of kg (outliers)
 36. temperature_mixed_units        - 3% in °F instead of °C (outliers)

10. EDGE CASES - 6 variables:
 37. constant_var                   - Constant (all 'Site A')
 38. nearly_constant                - 99% same value
 39. single_value_var               - One unique value (with missing)
 40. tiny_variance                  - Very small variance (SD=0.001)
 41. with_infinite                  - Contains Inf and -Inf values
 42. extreme_range                  - Extreme range (possible data errors)

DATE VARIABLES - 2 variables:
 43. date_of_birth                  - Calculated from age
 44. enrollment_date                - Study enrollment (2023-2025)

═══════════════════════════════════════════════════════════
TESTING SCENARIOS
═══════════════════════════════════════════════════════════

OUTLIER DETECTION:
✓ Mild outliers (z-score 3-4)
✓ Extreme outliers (z-score >5)
✓ Asymmetric outliers (high-tail only)
✓ Asymmetric outliers (low-tail only)
✓ Outlier transformation (log, sqrt)

MISSING DATA:
✓ No missing (perfect variables)
✓ Low missing (3%)
✓ Moderate missing (15%)
✓ High missing (40%)
✓ All missing (100%)

DUPLICATES:
✓ High duplicates (few unique values)
✓ Many duplicates (repeated measurements)
✓ Few duplicates (mostly unique)

RARE CATEGORIES:
✓ One rare category (<5%)
✓ Multiple rare categories

CLINICAL VALIDATION:
✓ Age range validation
✓ Height/weight plausibility
✓ Vital signs ranges
✓ Lab values ranges
✓ Unit system detection (metric/imperial)

DISTRIBUTIONS:
✓ Normal distribution
✓ Right-skewed (log-normal)
✓ Left-skewed
✓ Bimodal
✓ Uniform
✓ Heavy-tailed

EDGE CASES:
✓ Constant variable
✓ Nearly constant
✓ Single unique value
✓ Tiny variance
✓ Infinite values
✓ Extreme range

═══════════════════════════════════════════════════════════
USAGE EXAMPLES
═══════════════════════════════════════════════════════════

# Load data
data(checkdata_test)

# Example 1: Perfect quality variable
checkdata(
  data = checkdata_test,
  var = 'age_perfect',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 2: Variable with outliers
checkdata(
  data = checkdata_test,
  var = 'creatinine_extreme_outliers',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'log'  # Transform for better detection
)

# Example 3: Variable with missing data
checkdata(
  data = checkdata_test,
  var = 'cholesterol_moderate_missing',
  showOutliers = TRUE,
  showPatterns = TRUE,
  mcarTest = TRUE  # Test if missing completely at random
)

# Example 4: Variable with rare categories
checkdata(
  data = checkdata_test,
  var = 'histology_rare_category',
  showDuplicates = TRUE,
  rareCategoryThreshold = 5  # Flag categories <5%
)

# Example 5: Clinical variable validation
checkdata(
  data = checkdata_test,
  var = 'height_mixed_units',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'auto'  # Detect unit issues
)

# Example 6: Skewed distribution needing transformation
checkdata(
  data = checkdata_test,
  var = 'crp_log_transform',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'log',  # Apply log transform
  showSummary = TRUE
)

═══════════════════════════════════════════════════════════
FILES GENERATED
═══════════════════════════════════════════════════════════

✓ data/checkdata_test.rda      - R data format
✓ data/checkdata_test.csv      - CSV format
✓ data/checkdata_test.xlsx     - Excel format
✓ data/checkdata_test.omv      - Jamovi format

═══════════════════════════════════════════════════════════
")
