# ═══════════════════════════════════════════════════════════
# Test Data Generation: vartree
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the vartree jamovi function
# (Variable Tree - Hierarchical Visualization of Categorical Variables)
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
# CATEGORICAL VARIABLES FOR TREE CONSTRUCTION
# ═══════════════════════════════════════════════════════════

# Treatment (3 levels)
treatment <- factor(
  sample(c("Surgery", "Chemotherapy", "Radiation"),
         n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
  levels = c("Surgery", "Chemotherapy", "Radiation")
)

# Response (2 levels)
response <- factor(
  sample(c("Complete Response", "Partial Response"),
         n, replace = TRUE, prob = c(0.6, 0.4)),
  levels = c("Complete Response", "Partial Response")
)

# Stage (4 levels)
stage <- factor(
  sample(c("I", "II", "III", "IV"),
         n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.20)),
  levels = c("I", "II", "III", "IV"),
  ordered = TRUE
)

# Grade (3 levels)
grade <- factor(
  sample(c("Low", "Intermediate", "High"),
         n, replace = TRUE, prob = c(0.30, 0.45, 0.25)),
  levels = c("Low", "Intermediate", "High"),
  ordered = TRUE
)

# Histology (4 types)
histology <- factor(
  sample(c("Adenocarcinoma", "Squamous", "Large Cell", "Small Cell"),
         n, replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10)),
  levels = c("Adenocarcinoma", "Squamous", "Large Cell", "Small Cell")
)

# Metastasis (2 levels)
metastasis <- factor(
  sample(c("No", "Yes"),
         n, replace = TRUE, prob = c(0.65, 0.35)),
  levels = c("No", "Yes")
)

# Performance Status - Generated later with smoking correlation (see line ~170)
# Commented out initial generation to avoid overwriting
# performance_status <- factor(
#   sample(c("Good", "Moderate", "Poor"),
#          n, replace = TRUE, prob = c(0.50, 0.35, 0.15)),
#   levels = c("Good", "Moderate", "Poor"),
#   ordered = TRUE
# )

# Smoking Status (3 levels)
smoking_status <- factor(
  sample(c("Never", "Former", "Current"),
         n, replace = TRUE, prob = c(0.30, 0.45, 0.25)),
  levels = c("Never", "Former", "Current")
)

# ═══════════════════════════════════════════════════════════
# BINARY VARIABLES
# ═══════════════════════════════════════════════════════════

# Sex
sex <- factor(
  sample(c("Male", "Female"),
         n, replace = TRUE, prob = c(0.55, 0.45)),
  levels = c("Male", "Female")
)

# Comorbidity
comorbidity <- factor(
  sample(c("No", "Yes"),
         n, replace = TRUE, prob = c(0.70, 0.30)),
  levels = c("No", "Yes")
)

# Recurrence
recurrence <- factor(
  sample(c("No", "Yes"),
         n, replace = TRUE, prob = c(0.75, 0.25)),
  levels = c("No", "Yes")
)

# Vital Status
vital_status <- factor(
  sample(c("Alive", "Deceased"),
         n, replace = TRUE, prob = c(0.70, 0.30)),
  levels = c("Alive", "Deceased")
)

# ═══════════════════════════════════════════════════════════
# CONTINUOUS VARIABLES FOR SUMMARIES
# ═══════════════════════════════════════════════════════════

# Age (continuous)
age <- round(rnorm(n, mean = 65, sd = 12))
age <- pmax(pmin(age, 95), 30)  # Age 30-95

# Tumor Size (continuous, mm)
tumor_size <- rnorm(n, mean = 35, sd = 15)
tumor_size <- round(pmax(tumor_size, 5), 1)

# Survival Time (months)
survival_months <- rgamma(n, shape = 5, scale = 6)
survival_months <- round(pmax(survival_months, 1))

# BMI (continuous)
bmi <- rnorm(n, mean = 26, sd = 5)
bmi <- round(pmax(pmin(bmi, 45), 15), 1)

# Biomarker Level (continuous)
biomarker <- rlnorm(n, meanlog = log(50), sdlog = 0.8)
biomarker <- round(pmin(biomarker, 500), 1)

# Quality of Life Score (0-100)
qol_score <- rnorm(n, mean = 70, sd = 20)
qol_score <- round(pmax(pmin(qol_score, 100), 0))

# ═══════════════════════════════════════════════════════════
# CORRELATIONS AND REALISTIC PATTERNS
# ═══════════════════════════════════════════════════════════

# Higher stage → worse response
stage_numeric <- as.numeric(stage)
response_prob <- 0.8 - (stage_numeric - 1) * 0.15
response <- factor(
  ifelse(runif(n) < response_prob, "Complete Response", "Partial Response"),
  levels = c("Complete Response", "Partial Response")
)

# Metastasis more likely with higher stage
metastasis_prob <- 0.1 + (stage_numeric - 1) * 0.15
metastasis <- factor(
  ifelse(runif(n) < metastasis_prob, "Yes", "No"),
  levels = c("No", "Yes")
)

# Higher grade → lower survival
grade_numeric <- as.numeric(grade)
survival_months <- survival_months * (1.5 - grade_numeric * 0.2)
survival_months <- round(pmax(survival_months, 1))

# Smoking → worse performance status (regenerate with correlation)
performance_status <- sapply(1:n, function(i) {
  if (smoking_status[i] == "Current") {
    sample(c("Good", "Moderate", "Poor"), 1, prob = c(0.3, 0.45, 0.25))
  } else if (smoking_status[i] == "Former") {
    sample(c("Good", "Moderate", "Poor"), 1, prob = c(0.5, 0.35, 0.15))
  } else {  # Never
    sample(c("Good", "Moderate", "Poor"), 1, prob = c(0.65, 0.30, 0.05))
  }
})
performance_status <- factor(performance_status,
                             levels = c("Good", "Moderate", "Poor"),
                             ordered = TRUE)

# ═══════════════════════════════════════════════════════════
# VARIABLES WITH MISSING DATA
# ═══════════════════════════════════════════════════════════

# Add missing data to some variables
biomarker[sample(n, round(n * 0.10))] <- NA  # 10% missing
tumor_size[sample(n, round(n * 0.05))] <- NA  # 5% missing
qol_score[sample(n, round(n * 0.15))] <- NA  # 15% missing

# Some patients missing grade info
grade_missing <- grade
grade_missing[sample(n, round(n * 0.08))] <- NA  # 8% missing

# ═══════════════════════════════════════════════════════════
# RARE CATEGORIES (for pruning tests)
# ═══════════════════════════════════════════════════════════

# Mutation Status (one very rare category)
mutation_status <- factor(
  sample(c("Wild Type", "EGFR", "KRAS", "ALK", "ROS1"),
         n, replace = TRUE,
         prob = c(0.60, 0.25, 0.10, 0.04, 0.01)),
  levels = c("Wild Type", "EGFR", "KRAS", "ALK", "ROS1")
)

# Subtype (multiple rare categories for pruning)
subtype <- factor(
  sample(c("Common", "Rare A", "Rare B", "Very Rare"),
         n, replace = TRUE,
         prob = c(0.80, 0.10, 0.07, 0.03)),
  levels = c("Common", "Rare A", "Rare B", "Very Rare")
)

# ═══════════════════════════════════════════════════════════
# PATTERN/SEQUENCE VARIABLES
# ═══════════════════════════════════════════════════════════

# Treatment Sequence (for sequence tree)
treatment_sequence <- factor(
  sample(c("First Line", "Second Line", "Third Line"),
         n, replace = TRUE, prob = c(0.60, 0.30, 0.10)),
  levels = c("First Line", "Second Line", "Third Line"),
  ordered = TRUE
)

# Response Pattern (for pattern tree)
response_pattern <- factor(
  sample(c("Early Response", "Late Response", "No Response"),
         n, replace = TRUE, prob = c(0.50, 0.30, 0.20)),
  levels = c("Early Response", "Late Response", "No Response")
)

# ═══════════════════════════════════════════════════════════
# DEMOGRAPHIC VARIABLES
# ═══════════════════════════════════════════════════════════

# Ethnicity (5 categories)
ethnicity <- factor(
  sample(c("Caucasian", "African American", "Hispanic", "Asian", "Other"),
         n, replace = TRUE,
         prob = c(0.50, 0.20, 0.15, 0.10, 0.05)),
  levels = c("Caucasian", "African American", "Hispanic", "Asian", "Other")
)

# Marital Status (4 categories)
marital_status <- factor(
  sample(c("Single", "Married", "Divorced", "Widowed"),
         n, replace = TRUE,
         prob = c(0.20, 0.55, 0.15, 0.10)),
  levels = c("Single", "Married", "Divorced", "Widowed")
)

# Insurance (3 categories)
insurance <- factor(
  sample(c("Private", "Medicare", "Medicaid"),
         n, replace = TRUE,
         prob = c(0.50, 0.35, 0.15)),
  levels = c("Private", "Medicare", "Medicaid")
)

# ═══════════════════════════════════════════════════════════
# EDGE CASES
# ═══════════════════════════════════════════════════════════

# Constant variable (all one category)
constant_category <- factor(rep("Category A", n), levels = c("Category A"))

# Nearly constant (99% one category)
nearly_constant <- factor(
  c(rep("Dominant", round(n * 0.99)), rep("Rare", n - round(n * 0.99))),
  levels = c("Dominant", "Rare")
)

# All missing
all_missing_cat <- factor(rep(NA_character_, n), levels = c("Level1", "Level2"))

# Many categories (for visualization stress test)
many_categories <- factor(
  sample(paste0("Cat", 1:12), n, replace = TRUE),
  levels = paste0("Cat", 1:12)
)

# ═══════════════════════════════════════════════════════════
# IDENTIFIERS
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PAT%05d", 1:n)

# ═══════════════════════════════════════════════════════════
# CREATE TIBBLE
# ═══════════════════════════════════════════════════════════

vartree_test <- tibble(
  # Identifier
  patient_id = patient_id,
  
  # Primary categorical variables (for tree construction)
  treatment = treatment,
  response = response,
  stage = stage,
  grade = grade,
  histology = histology,
  metastasis = metastasis,
  performance_status = performance_status,
  smoking_status = smoking_status,
  
  # Binary variables
  sex = sex,
  comorbidity = comorbidity,
  recurrence = recurrence,
  vital_status = vital_status,
  
  # Continuous variables (for summaries)
  age = age,
  tumor_size = tumor_size,
  survival_months = survival_months,
  bmi = bmi,
  biomarker = biomarker,
  qol_score = qol_score,
  
  # Variables with missing data
  grade_missing = grade_missing,
  
  # Rare categories
  mutation_status = mutation_status,
  subtype = subtype,
  
  # Pattern/Sequence
  treatment_sequence = treatment_sequence,
  response_pattern = response_pattern,
  
  # Demographics
  ethnicity = ethnicity,
  marital_status = marital_status,
  insurance = insurance,
  
  # Edge cases
  constant_category = constant_category,
  nearly_constant = nearly_constant,
  all_missing_cat = all_missing_cat,
  many_categories = many_categories
)

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(vartree_test, file = here::here("data", "vartree_test.rda"))

# 2. CSV format
write.csv(vartree_test,
          file = here::here("data", "vartree_test.csv"),
          row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(vartree_test,
                    path = here::here("data", "vartree_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(vartree_test, here::here("data", "vartree_test.omv"))

cat("Dataset vartree_test created successfully!\n")
