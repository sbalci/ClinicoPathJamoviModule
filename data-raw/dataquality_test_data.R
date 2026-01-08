# ═══════════════════════════════════════════════════════════
# Test Data Generation: dataquality
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the dataquality jamovi function
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 200
#
# Data Quality Scenarios Tested:
# 1. Perfect quality variables (no missing, no duplicates)
# 2. Variables with different missing percentages (5%, 15%, 30%, 60%)
# 3. Variables with duplicates
# 4. Variables with near-zero variance
# 5. Variables with high cardinality
# 6. Variables with outliers
# 7. Different data types (numeric, integer, character, factor, date)
# 8. Complete duplicate rows

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# Sample size
n <- 200

# ═══════════════════════════════════════════════════════════
# PERFECT QUALITY VARIABLES (no issues)
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PT%04d", 1:n)
age_perfect <- round(rnorm(n, mean = 65, sd = 12))
sex_perfect <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))

# ═══════════════════════════════════════════════════════════
# MISSING VALUE VARIABLES (different percentages)
# ═══════════════════════════════════════════════════════════

# Low missing (5%)
tumor_size_low_missing <- round(rnorm(n, mean = 3.5, sd = 1.8), 1)
tumor_size_low_missing[sample(n, round(n * 0.05))] <- NA

# Moderate missing (15%)
psa_moderate_missing <- round(rnorm(n, mean = 8.5, sd = 5.2), 1)
psa_moderate_missing[sample(n, round(n * 0.15))] <- NA

# High missing (30%)
biomarker_high_missing <- round(rnorm(n, mean = 120, sd = 35), 1)
biomarker_high_missing[sample(n, round(n * 0.30))] <- NA

# Very high missing (60%) - should trigger strong warning
genetic_test_very_high_missing <- sample(c("Positive", "Negative", "Uncertain"), n, replace = TRUE)
genetic_test_very_high_missing[sample(n, round(n * 0.60))] <- NA

# ═══════════════════════════════════════════════════════════
# DUPLICATE VALUE VARIABLES
# ═══════════════════════════════════════════════════════════

# High duplicates - only a few unique values
tumor_grade_duplicates <- sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE,
                                  prob = c(0.3, 0.5, 0.2))

# Categorical with moderate duplicates
stage_duplicates <- sample(c("I", "II", "III", "IV"), n, replace = TRUE,
                           prob = c(0.2, 0.35, 0.3, 0.15))

# Numeric with many duplicates (treatment cycles)
treatment_cycles_duplicates <- sample(1:6, n, replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.1, 0.07, 0.03))

# ═══════════════════════════════════════════════════════════
# NEAR-ZERO VARIANCE VARIABLES
# ═══════════════════════════════════════════════════════════

# Constant variable (all same value)
hospital_site_constant <- rep("Site A", n)

# Near-zero variance numeric (99% same value)
surgery_type_near_zero <- c(rep(1, round(n * 0.99)), rep(2, n - round(n * 0.99)))

# Near-zero variance continuous (very small SD)
temperature_near_zero <- rnorm(n, mean = 37.0, sd = 0.001)

# ═══════════════════════════════════════════════════════════
# HIGH CARDINALITY VARIABLES
# ═══════════════════════════════════════════════════════════

# High cardinality numeric (many unique values)
biomarker_continuous_high_card <- round(rnorm(n, mean = 500, sd = 150), 2)

# High cardinality categorical (unique IDs, free text)
pathology_report_id_high_card <- sprintf("PATH-%s-%04d",
                                         sample(c("A", "B", "C", "D"), n, replace = TRUE),
                                         sample(1:9999, n, replace = TRUE))

# Free text comments (high cardinality categorical)
physician_notes_high_card <- sprintf("Patient presents with %s symptoms, %s progression",
                                     sample(c("mild", "moderate", "severe", "no"), n, replace = TRUE),
                                     sample(c("stable", "improving", "worsening", "variable"), n, replace = TRUE))

# ═══════════════════════════════════════════════════════════
# VARIABLES WITH OUTLIERS
# ═══════════════════════════════════════════════════════════

# Numeric with outliers
wbc_count_with_outliers <- c(
  rnorm(round(n * 0.9), mean = 7.5, sd = 2),  # 90% normal range
  rnorm(round(n * 0.1), mean = 25, sd = 5)    # 10% outliers (elevated WBC)
)
wbc_count_with_outliers <- round(wbc_count_with_outliers[1:n], 1)

# Continuous with extreme outliers
hospital_stay_with_outliers <- c(
  rpois(round(n * 0.85), lambda = 5),       # 85% typical stays
  rpois(round(n * 0.1), lambda = 15),       # 10% longer stays
  rpois(round(n * 0.05), lambda = 45)       # 5% very long stays (outliers)
)
hospital_stay_with_outliers <- hospital_stay_with_outliers[1:n]

# Biomarker with measurement errors (outliers)
creatinine_with_outliers <- c(
  rnorm(round(n * 0.92), mean = 1.0, sd = 0.3),  # 92% normal
  rnorm(round(n * 0.08), mean = 5.5, sd = 1.2)   # 8% renal impairment (outliers)
)
creatinine_with_outliers <- pmax(0.1, round(creatinine_with_outliers[1:n], 2))

# ═══════════════════════════════════════════════════════════
# DIFFERENT DATA TYPES
# ═══════════════════════════════════════════════════════════

# Integer
num_biopsies_integer <- sample(1:5, n, replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.1, 0.05))

# Numeric/double
hemoglobin_numeric <- round(rnorm(n, mean = 13.5, sd = 1.8), 1)

# Character
diagnosis_character <- sample(c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell"),
                              n, replace = TRUE)

# Factor (ordered)
ecog_status_factor <- factor(
  sample(0:4, n, replace = TRUE, prob = c(0.3, 0.35, 0.2, 0.1, 0.05)),
  levels = 0:4,
  ordered = TRUE
)

# Date
diagnosis_date <- as.Date("2020-01-01") + sample(0:1460, n, replace = TRUE)  # 4 years range

# Logical
smoking_history_logical <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.4, 0.6))

# ═══════════════════════════════════════════════════════════
# MIXED QUALITY ISSUES (multiple problems)
# ═══════════════════════════════════════════════════════════

# Variable with BOTH missing AND outliers
platelets_mixed_issues <- c(
  rnorm(round(n * 0.75), mean = 250, sd = 50),  # 75% normal
  rnorm(round(n * 0.05), mean = 600, sd = 100)  # 5% outliers
)
platelets_mixed_issues <- round(platelets_mixed_issues[1:n], 0)
platelets_mixed_issues[sample(n, round(n * 0.20))] <- NA  # 20% missing

# Variable with high duplicates AND missing
comorbidity_mixed <- sample(c("None", "Diabetes", "Hypertension", "Both"), n, replace = TRUE,
                            prob = c(0.3, 0.25, 0.25, 0.2))
comorbidity_mixed[sample(n, round(n * 0.12))] <- NA  # 12% missing

# ═══════════════════════════════════════════════════════════
# CREATE DUPLICATE ROWS (for complete_cases_only testing)
# ═══════════════════════════════════════════════════════════

# Combine all variables into data frame
dataquality_test <- tibble(
  patient_id = patient_id,
  age_perfect = age_perfect,
  sex_perfect = sex_perfect,
  tumor_size_low_missing = tumor_size_low_missing,
  psa_moderate_missing = psa_moderate_missing,
  biomarker_high_missing = biomarker_high_missing,
  genetic_test_very_high_missing = genetic_test_very_high_missing,
  tumor_grade_duplicates = tumor_grade_duplicates,
  stage_duplicates = stage_duplicates,
  treatment_cycles_duplicates = treatment_cycles_duplicates,
  hospital_site_constant = hospital_site_constant,
  surgery_type_near_zero = surgery_type_near_zero,
  temperature_near_zero = temperature_near_zero,
  biomarker_continuous_high_card = biomarker_continuous_high_card,
  pathology_report_id_high_card = pathology_report_id_high_card,
  physician_notes_high_card = physician_notes_high_card,
  wbc_count_with_outliers = wbc_count_with_outliers,
  hospital_stay_with_outliers = hospital_stay_with_outliers,
  creatinine_with_outliers = creatinine_with_outliers,
  num_biopsies_integer = num_biopsies_integer,
  hemoglobin_numeric = hemoglobin_numeric,
  diagnosis_character = diagnosis_character,
  ecog_status_factor = ecog_status_factor,
  diagnosis_date = diagnosis_date,
  smoking_history_logical = smoking_history_logical,
  platelets_mixed_issues = platelets_mixed_issues,
  comorbidity_mixed = comorbidity_mixed
)

# Add 5 complete duplicate rows (copy rows 10, 25, 50, 75, 100)
duplicate_indices <- c(10, 25, 50, 75, 100)
duplicate_rows <- dataquality_test[duplicate_indices, ]

# Append duplicates to end
dataquality_test <- bind_rows(dataquality_test, duplicate_rows)

# Shuffle to make duplicates less obvious
set.seed(42)
dataquality_test <- dataquality_test[sample(nrow(dataquality_test)), ]

# Reset row names
rownames(dataquality_test) <- NULL

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(dataquality_test, file = here::here("data", "dataquality_test.rda"))

# 2. CSV format
write.csv(dataquality_test, file = here::here("data", "dataquality_test.csv"), row.names = FALSE)

# 3. Excel format (single sheet)
writexl::write_xlsx(dataquality_test, path = here::here("data", "dataquality_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(dataquality_test, here::here("data", "dataquality_test.omv"))

# ═══════════════════════════════════════════════════════════
# DATASET DOCUMENTATION
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
DATA QUALITY TEST DATASET
═══════════════════════════════════════════════════════════

Dataset: dataquality_test
Total Observations: ", nrow(dataquality_test), " (includes 5 duplicate rows)
Variables: ", ncol(dataquality_test), "
Generated: ", Sys.Date(), "
Seed: 42

═══════════════════════════════════════════════════════════
VARIABLE DESCRIPTIONS BY QUALITY CATEGORY
═══════════════════════════════════════════════════════════

PERFECT QUALITY (no issues):
  1. patient_id                   - Unique patient identifier
  2. age_perfect                  - Patient age (years), no missing
  3. sex_perfect                  - Patient sex (Male/Female), no missing

MISSING VALUE VARIABLES:
  4. tumor_size_low_missing       - Tumor size (cm), 5% missing
  5. psa_moderate_missing         - PSA level (ng/mL), 15% missing
  6. biomarker_high_missing       - Biomarker value, 30% missing
  7. genetic_test_very_high_missing - Genetic test result, 60% missing (triggers warning!)

DUPLICATE VALUE VARIABLES:
  8. tumor_grade_duplicates       - Tumor grade (1-3), high duplicates
  9. stage_duplicates             - Cancer stage (I-IV), moderate duplicates
 10. treatment_cycles_duplicates  - Number of treatment cycles (1-6)

NEAR-ZERO VARIANCE VARIABLES:
 11. hospital_site_constant       - Hospital site (all 'Site A')
 12. surgery_type_near_zero       - Surgery type (99% type 1)
 13. temperature_near_zero        - Body temperature (very small SD)

HIGH CARDINALITY VARIABLES:
 14. biomarker_continuous_high_card    - Continuous biomarker (many unique values)
 15. pathology_report_id_high_card     - Pathology report ID (unique IDs)
 16. physician_notes_high_card         - Physician notes (free text)

VARIABLES WITH OUTLIERS:
 17. wbc_count_with_outliers      - WBC count (10% outliers)
 18. hospital_stay_with_outliers  - Hospital stay days (5% extreme outliers)
 19. creatinine_with_outliers     - Creatinine level (8% renal impairment outliers)

DIFFERENT DATA TYPES:
 20. num_biopsies_integer         - Number of biopsies (integer)
 21. hemoglobin_numeric           - Hemoglobin level (numeric/double)
 22. diagnosis_character          - Diagnosis type (character)
 23. ecog_status_factor           - ECOG performance status (ordered factor 0-4)
 24. diagnosis_date               - Date of diagnosis (Date)
 25. smoking_history_logical      - Smoking history (logical TRUE/FALSE)

MIXED QUALITY ISSUES:
 26. platelets_mixed_issues       - Platelet count (20% missing + 5% outliers)
 27. comorbidity_mixed            - Comorbidity status (12% missing + high duplicates)

═══════════════════════════════════════════════════════════
DUPLICATE ROWS
═══════════════════════════════════════════════════════════

Number of complete duplicate rows: 5
Original row indices: 10, 25, 50, 75, 100
Note: Rows have been shuffled to make duplicates less obvious

═══════════════════════════════════════════════════════════
TESTING CHECKLIST
═══════════════════════════════════════════════════════════

✓ Missing value analysis (4 levels: 5%, 15%, 30%, 60%)
✓ Duplicate detection (value-level and row-level)
✓ Near-zero variance detection (3 types)
✓ High cardinality detection (numeric and categorical)
✓ Outlier detection (3 variables with outliers)
✓ Data type diversity (6 types: integer, numeric, character, factor, date, logical)
✓ Mixed quality issues (variables with multiple problems)
✓ Complete duplicate rows (for complete_cases_only option)
✓ High missing threshold warning (>50% missing)

═══════════════════════════════════════════════════════════
USAGE EXAMPLES
═══════════════════════════════════════════════════════════

# Load data
data(dataquality_test)

# Example 1: Analyze all variables
dataquality(data = dataquality_test)

# Example 2: Check specific variables for missing values
dataquality(
  data = dataquality_test,
  vars = c('tumor_size_low_missing', 'psa_moderate_missing',
           'biomarker_high_missing', 'genetic_test_very_high_missing'),
  check_missing = TRUE,
  plot_missing_patterns = TRUE
)

# Example 3: Check for duplicate rows
dataquality(
  data = dataquality_test,
  vars = c('age_perfect', 'sex_perfect', 'stage_duplicates'),
  check_duplicates = TRUE,
  complete_cases_only = TRUE
)

# Example 4: Comprehensive analysis with all plots
dataquality(
  data = dataquality_test,
  vars = c('age_perfect', 'tumor_size_low_missing', 'wbc_count_with_outliers',
           'biomarker_continuous_high_card', 'stage_duplicates'),
  check_duplicates = TRUE,
  check_missing = TRUE,
  plot_data_overview = TRUE,
  plot_missing_patterns = TRUE,
  plot_data_types = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE
)

═══════════════════════════════════════════════════════════
FILES GENERATED
═══════════════════════════════════════════════════════════

✓ data/dataquality_test.rda      - R data format
✓ data/dataquality_test.csv      - CSV format
✓ data/dataquality_test.xlsx     - Excel format
✓ data/dataquality_test.omv      - Jamovi format

═══════════════════════════════════════════════════════════
")
