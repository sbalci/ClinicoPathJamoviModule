# ═══════════════════════════════════════════════════════════
# Test Data Generation: reportcat
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the reportcat jamovi function
# (Summary of Categorical Variables)
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 300

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# Sample size
n <- 300

# ═══════════════════════════════════════════════════════════
# 1. BINARY CATEGORICAL VARIABLES (2 Levels)
# ═══════════════════════════════════════════════════════════

# Sex (balanced)
sex_balanced <- factor(
  sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.5, 0.5)),
  levels = c("Male", "Female")
)

# Vital status (slightly imbalanced)
vital_status <- factor(
  sample(c("Alive", "Deceased"), n, replace = TRUE, prob = c(0.75, 0.25)),
  levels = c("Alive", "Deceased")
)

# Test result (positive/negative, skewed)
test_result <- factor(
  sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.85, 0.15)),
  levels = c("Negative", "Positive")
)

# Response (yes/no, balanced with missing)
response_clean <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.6, 0.4))
response_clean[sample(n, round(n * 0.05))] <- NA  # 5% missing
response <- factor(response_clean, levels = c("No", "Yes"))

# ═══════════════════════════════════════════════════════════
# 2. ORDINAL CATEGORICAL VARIABLES (Ordered Levels)
# ═══════════════════════════════════════════════════════════

# Tumor stage (I-IV, ordered)
stage_ordered <- factor(
  sample(c("I", "II", "III", "IV"), n, replace = TRUE,
         prob = c(0.25, 0.35, 0.25, 0.15)),
  levels = c("I", "II", "III", "IV"),
  ordered = TRUE
)

# Tumor grade (1-3, ordered)
grade_ordered <- factor(
  sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE,
         prob = c(0.3, 0.5, 0.2)),
  levels = c("Grade 1", "Grade 2", "Grade 3"),
  ordered = TRUE
)

# Pain severity (ordered, 5 levels)
pain_severity <- factor(
  sample(c("None", "Mild", "Moderate", "Severe", "Very Severe"), n, replace = TRUE,
         prob = c(0.25, 0.30, 0.25, 0.15, 0.05)),
  levels = c("None", "Mild", "Moderate", "Severe", "Very Severe"),
  ordered = TRUE
)

# Performance status (ECOG 0-4, ordered)
performance_status <- factor(
  sample(0:4, n, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.10, 0.05)),
  levels = 0:4,
  ordered = TRUE,
  labels = c("0: Fully Active", "1: Restricted", "2: Ambulatory",
             "3: Limited Self-Care", "4: Disabled")
)

# ═══════════════════════════════════════════════════════════
# 3. NOMINAL CATEGORICAL VARIABLES (Unordered, 3-5 Levels)
# ═══════════════════════════════════════════════════════════

# Treatment type (3 categories)
treatment_type <- factor(
  sample(c("Surgery", "Chemotherapy", "Radiation"), n, replace = TRUE,
         prob = c(0.4, 0.35, 0.25)),
  levels = c("Surgery", "Chemotherapy", "Radiation")
)

# Tumor location (4 categories)
tumor_location <- factor(
  sample(c("Upper", "Middle", "Lower", "Diffuse"), n, replace = TRUE,
         prob = c(0.3, 0.35, 0.25, 0.1)),
  levels = c("Upper", "Middle", "Lower", "Diffuse")
)

# Ethnicity (5 categories, reflecting diversity)
ethnicity <- factor(
  sample(c("Caucasian", "African American", "Hispanic", "Asian", "Other"),
         n, replace = TRUE,
         prob = c(0.45, 0.25, 0.15, 0.10, 0.05)),
  levels = c("Caucasian", "African American", "Hispanic", "Asian", "Other")
)

# Marital status (4 categories)
marital_status <- factor(
  sample(c("Single", "Married", "Divorced", "Widowed"), n, replace = TRUE,
         prob = c(0.25, 0.50, 0.15, 0.10)),
  levels = c("Single", "Married", "Divorced", "Widowed")
)

# ═══════════════════════════════════════════════════════════
# 4. NOMINAL VARIABLES WITH MANY LEVELS (6-10 Levels)
# ═══════════════════════════════════════════════════════════

# Histology type (8 categories, clinical pathology)
histology <- factor(
  sample(c("Adenocarcinoma", "Squamous Cell", "Large Cell", "Small Cell",
           "Adenosquamous", "Carcinoid", "Sarcoma", "Other"),
         n, replace = TRUE,
         prob = c(0.35, 0.25, 0.15, 0.10, 0.05, 0.04, 0.03, 0.03)),
  levels = c("Adenocarcinoma", "Squamous Cell", "Large Cell", "Small Cell",
             "Adenosquamous", "Carcinoid", "Sarcoma", "Other")
)

# Primary site (10 anatomical locations)
primary_site <- factor(
  sample(c("Lung", "Breast", "Colon", "Prostate", "Stomach",
           "Liver", "Pancreas", "Kidney", "Bladder", "Other"),
         n, replace = TRUE,
         prob = c(0.20, 0.18, 0.15, 0.12, 0.10,
                  0.08, 0.06, 0.05, 0.03, 0.03)),
  levels = c("Lung", "Breast", "Colon", "Prostate", "Stomach",
             "Liver", "Pancreas", "Kidney", "Bladder", "Other")
)

# Comorbidity (7 common conditions)
comorbidity <- factor(
  sample(c("None", "Diabetes", "Hypertension", "COPD", "Heart Disease",
           "Kidney Disease", "Multiple"),
         n, replace = TRUE,
         prob = c(0.30, 0.15, 0.20, 0.12, 0.10, 0.08, 0.05)),
  levels = c("None", "Diabetes", "Hypertension", "COPD", "Heart Disease",
             "Kidney Disease", "Multiple")
)

# ═══════════════════════════════════════════════════════════
# 5. BALANCED DISTRIBUTION VARIABLES
# ═══════════════════════════════════════════════════════════

# Perfectly balanced 3 categories
treatment_arm <- factor(
  sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE,
         prob = c(1/3, 1/3, 1/3)),
  levels = c("Control", "Treatment A", "Treatment B")
)

# Perfectly balanced 4 categories
blood_type <- factor(
  sample(c("A", "B", "AB", "O"), n, replace = TRUE,
         prob = c(0.25, 0.25, 0.25, 0.25)),
  levels = c("A", "B", "AB", "O")
)

# ═══════════════════════════════════════════════════════════
# 6. SKEWED DISTRIBUTION VARIABLES (Rare Categories)
# ═══════════════════════════════════════════════════════════

# One very rare category (<2%)
mutation_status <- factor(
  sample(c("Wild Type", "EGFR", "KRAS", "ALK", "ROS1"),
         n, replace = TRUE,
         prob = c(0.60, 0.25, 0.10, 0.04, 0.01)),
  levels = c("Wild Type", "EGFR", "KRAS", "ALK", "ROS1")
)

# Multiple rare categories
diagnosis_rare <- factor(
  sample(c("Common Cancer", "Rare Type 1", "Rare Type 2", "Rare Type 3",
           "Very Rare 1", "Very Rare 2"),
         n, replace = TRUE,
         prob = c(0.75, 0.10, 0.08, 0.04, 0.02, 0.01)),
  levels = c("Common Cancer", "Rare Type 1", "Rare Type 2", "Rare Type 3",
             "Very Rare 1", "Very Rare 2")
)

# ═══════════════════════════════════════════════════════════
# 7. VARIABLES WITH DIFFERENT MISSING DATA PERCENTAGES
# ═══════════════════════════════════════════════════════════

# Low missing data (3%)
smoking_status_clean <- sample(c("Never", "Former", "Current"), n, replace = TRUE,
                               prob = c(0.45, 0.35, 0.20))
smoking_status_clean[sample(n, round(n * 0.03))] <- NA
smoking_status <- factor(smoking_status_clean,
                        levels = c("Never", "Former", "Current"))

# Moderate missing data (15%)
alcohol_use_clean <- sample(c("None", "Social", "Moderate", "Heavy"),
                            n, replace = TRUE,
                            prob = c(0.35, 0.40, 0.20, 0.05))
alcohol_use_clean[sample(n, round(n * 0.15))] <- NA
alcohol_use <- factor(alcohol_use_clean,
                     levels = c("None", "Social", "Moderate", "Heavy"))

# High missing data (35%)
genetic_marker_clean <- sample(c("Positive", "Negative", "Inconclusive"),
                              n, replace = TRUE,
                              prob = c(0.25, 0.60, 0.15))
genetic_marker_clean[sample(n, round(n * 0.35))] <- NA
genetic_marker <- factor(genetic_marker_clean,
                        levels = c("Negative", "Positive", "Inconclusive"))

# Very high missing data (60%)
biomarker_optional_clean <- sample(c("High", "Low", "Normal"),
                                   n, replace = TRUE,
                                   prob = c(0.30, 0.25, 0.45))
biomarker_optional_clean[sample(n, round(n * 0.60))] <- NA
biomarker_optional <- factor(biomarker_optional_clean,
                            levels = c("Low", "Normal", "High"))

# ═══════════════════════════════════════════════════════════
# 8. VARIABLES WITH SPECIAL CHARACTERS IN LEVELS
# ═══════════════════════════════════════════════════════════

# Levels with spaces and special characters
diagnosis_detailed <- factor(
  sample(c("Adenocarcinoma, NOS", "Squamous Cell Carcinoma",
           "Large Cell Carcinoma", "Small Cell Carcinoma",
           "Mixed Type (Adenosquamous)", "Carcinoid Tumor",
           "Not Otherwise Specified"),
         n, replace = TRUE,
         prob = c(0.30, 0.25, 0.15, 0.12, 0.08, 0.05, 0.05)),
  levels = c("Adenocarcinoma, NOS", "Squamous Cell Carcinoma",
             "Large Cell Carcinoma", "Small Cell Carcinoma",
             "Mixed Type (Adenosquamous)", "Carcinoid Tumor",
             "Not Otherwise Specified")
)

# Levels with numbers and units
tumor_size_category <- factor(
  sample(c("<2 cm", "2-5 cm", "5-7 cm", ">7 cm"),
         n, replace = TRUE,
         prob = c(0.25, 0.45, 0.20, 0.10)),
  levels = c("<2 cm", "2-5 cm", "5-7 cm", ">7 cm"),
  ordered = TRUE
)

# Levels with accents and international characters
country_origin <- factor(
  sample(c("United States", "México", "Canadá", "España",
           "Brasil", "Other"),
         n, replace = TRUE,
         prob = c(0.50, 0.15, 0.10, 0.10, 0.08, 0.07)),
  levels = c("United States", "México", "Canadá", "España",
             "Brasil", "Other")
)

# ═══════════════════════════════════════════════════════════
# 9. EDGE CASES
# ═══════════════════════════════════════════════════════════

# All missing data (100%)
future_test <- factor(rep(NA_character_, n), levels = c("Positive", "Negative"))

# Single level variable (constant)
study_site <- factor(rep("Main Hospital", n), levels = c("Main Hospital"))

# Two levels but one is very rare (1 observation)
rare_mutation_clean <- c(rep("Wild Type", n - 1), "Rare Variant")
rare_mutation <- factor(rare_mutation_clean,
                       levels = c("Wild Type", "Rare Variant"))

# Empty factor (zero observations after filtering, but levels exist)
# This will have levels but all NA
filter_fail <- factor(rep(NA_character_, n),
                     levels = c("Pass", "Fail", "Indeterminate"))

# ═══════════════════════════════════════════════════════════
# 10. CHARACTER VECTORS (Will be converted to factors)
# ═══════════════════════════════════════════════════════════

# Character vector (not factor) - should still work
protocol_char <- sample(c("Protocol A", "Protocol B", "Protocol C"),
                       n, replace = TRUE, prob = c(0.5, 0.3, 0.2))

# Character with missing values
region_char <- sample(c("North", "South", "East", "West", NA),
                     n, replace = TRUE,
                     prob = c(0.25, 0.25, 0.25, 0.15, 0.10))

# ═══════════════════════════════════════════════════════════
# PATIENT CHARACTERISTICS (for identification)
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PAT%05d", 1:n)
age_group <- factor(
  cut(sample(18:90, n, replace = TRUE),
      breaks = c(0, 40, 60, 75, 100),
      labels = c("18-40", "41-60", "61-75", ">75")),
  ordered = TRUE
)

# ═══════════════════════════════════════════════════════════
# CREATE TIBBLE WITH ALL VARIABLES
# ═══════════════════════════════════════════════════════════

reportcat_test <- tibble(
  # Identifiers
  patient_id = patient_id,

  # Binary variables (2 levels)
  sex = sex_balanced,
  vital_status = vital_status,
  test_result = test_result,
  response = response,

  # Ordinal variables (ordered)
  age_group = age_group,
  stage = stage_ordered,
  grade = grade_ordered,
  pain_severity = pain_severity,
  performance_status = performance_status,
  tumor_size_category = tumor_size_category,

  # Nominal variables (3-5 levels)
  treatment_type = treatment_type,
  tumor_location = tumor_location,
  ethnicity = ethnicity,
  marital_status = marital_status,

  # Nominal variables (6-10 levels)
  histology = histology,
  primary_site = primary_site,
  comorbidity = comorbidity,

  # Balanced distribution
  treatment_arm = treatment_arm,
  blood_type = blood_type,

  # Skewed distribution (rare categories)
  mutation_status = mutation_status,
  diagnosis_rare = diagnosis_rare,

  # Different missing data percentages
  smoking_status = smoking_status,           # 3% missing
  alcohol_use = alcohol_use,                 # 15% missing
  genetic_marker = genetic_marker,           # 35% missing
  biomarker_optional = biomarker_optional,   # 60% missing

  # Special characters in levels
  diagnosis_detailed = diagnosis_detailed,
  country_origin = country_origin,

  # Edge cases
  future_test = future_test,                 # 100% missing
  study_site = study_site,                   # constant (1 level)
  rare_mutation = rare_mutation,             # 1 rare observation
  filter_fail = filter_fail,                 # all NA but has levels

  # Character vectors
  protocol = protocol_char,
  region = region_char
)

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(reportcat_test, file = here::here("data", "reportcat_test.rda"))

# 2. CSV format
write.csv(reportcat_test,
          file = here::here("data", "reportcat_test.csv"),
          row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(reportcat_test,
                    path = here::here("data", "reportcat_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(reportcat_test, here::here("data", "reportcat_test.omv"))

# ═══════════════════════════════════════════════════════════
# DATASET SUMMARY
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
Dataset: reportcat_test
═══════════════════════════════════════════════════════════

Observations:", n, "
Variables:", ncol(reportcat_test), "

VARIABLE CATEGORIES:
═══════════════════════════════════════════════════════════

1. BINARY VARIABLES (2 Levels): 4 variables
   - sex: Male/Female (balanced 50/50)
   - vital_status: Alive/Deceased (75/25)
   - test_result: Negative/Positive (85/15)
   - response: Yes/No (60/40, 5% missing)

2. ORDINAL VARIABLES (Ordered): 6 variables
   - age_group: 18-40, 41-60, 61-75, >75
   - stage: I-IV (tumor staging)
   - grade: Grade 1-3
   - pain_severity: None to Very Severe (5 levels)
   - performance_status: ECOG 0-4
   - tumor_size_category: <2 cm to >7 cm

3. NOMINAL VARIABLES (3-5 Levels): 4 variables
   - treatment_type: Surgery/Chemo/Radiation
   - tumor_location: Upper/Middle/Lower/Diffuse
   - ethnicity: 5 ethnic groups
   - marital_status: 4 categories

4. NOMINAL VARIABLES (6-10 Levels): 3 variables
   - histology: 8 histological types
   - primary_site: 10 anatomical sites
   - comorbidity: 7 comorbidity types

5. BALANCED DISTRIBUTION: 2 variables
   - treatment_arm: 3 arms (33% each)
   - blood_type: A/B/AB/O (25% each)

6. SKEWED DISTRIBUTION (Rare Categories): 2 variables
   - mutation_status: 1 category <2%
   - diagnosis_rare: Multiple rare categories

7. MISSING DATA VARIABLES: 4 variables
   - smoking_status: 3% missing
   - alcohol_use: 15% missing
   - genetic_marker: 35% missing
   - biomarker_optional: 60% missing

8. SPECIAL CHARACTERS: 2 variables
   - diagnosis_detailed: Parentheses, commas, NOS
   - country_origin: Accented characters (México, España)

9. EDGE CASES: 4 variables
   - future_test: 100% missing
   - study_site: Constant (1 level only)
   - rare_mutation: 1 rare observation
   - filter_fail: All NA but has levels

10. CHARACTER VECTORS: 2 variables
    - protocol: Character (not factor)
    - region: Character with NA

═══════════════════════════════════════════════════════════
USAGE EXAMPLES:
═══════════════════════════════════════════════════════════

# Load data in R
data(reportcat_test, package = 'ClinicoPath')

# Basic usage
reportcat(data = reportcat_test, vars = c('sex', 'stage', 'histology'))

# Multiple variables
reportcat(data = reportcat_test,
          vars = c('treatment_type', 'tumor_location', 'ethnicity'))

# Variables with missing data
reportcat(data = reportcat_test,
          vars = c('smoking_status', 'alcohol_use', 'genetic_marker'))

═══════════════════════════════════════════════════════════
FILES CREATED:
═══════════════════════════════════════════════════════════

data/reportcat_test.rda    (R native format)
data/reportcat_test.csv    (CSV format)
data/reportcat_test.xlsx   (Excel format)
data/reportcat_test.omv    (Jamovi format)

═══════════════════════════════════════════════════════════
")
