# ═══════════════════════════════════════════════════════════
# Test Data Generation: crosstable
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the crosstable jamovi function
#
# Purpose: Generate mixed categorical and continuous variables for testing
#          cross table generation with multiple table styles (arsenal, finalfit,
#          gtsummary, NEJM, Lancet, hmisc) and automatic statistical test selection
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 250

library(tibble)
library(dplyr)
library(here)

# Ensure reproducibility
set.seed(42)

# Sample size - adequate for cross table analysis
n <- 250

# ═══════════════════════════════════════════════════════════
# Generate Main Test Dataset
# ═══════════════════════════════════════════════════════════

crosstable_test <- tibble(
  # Patient ID
  patient_id = 1:n
)

# ─────────────────────────────────────────────────────────
# PRIMARY GROUPING VARIABLES (Columns in Cross Table)
# ─────────────────────────────────────────────────────────

# Main grouping variable: Treatment groups (3 levels, balanced)
crosstable_test$treatment <- sample(
  c("Control", "Treatment A", "Treatment B"),
  n,
  replace = TRUE,
  prob = c(0.33, 0.34, 0.33)
)
crosstable_test$treatment <- factor(crosstable_test$treatment,
                                    levels = c("Control", "Treatment A", "Treatment B"))

# Alternative grouping: Tumor stage (4 levels, representative of clinical distribution)
crosstable_test$stage <- sample(
  c("Stage I", "Stage II", "Stage III", "Stage IV"),
  n,
  replace = TRUE,
  prob = c(0.25, 0.35, 0.25, 0.15)
)
crosstable_test$stage <- factor(crosstable_test$stage,
                                levels = c("Stage I", "Stage II", "Stage III", "Stage IV"))

# Binary grouping: Recurrence status
crosstable_test$recurrence <- sample(
  c("No", "Yes"),
  n,
  replace = TRUE,
  prob = c(0.65, 0.35)
)
crosstable_test$recurrence <- factor(crosstable_test$recurrence,
                                     levels = c("No", "Yes"))

# ─────────────────────────────────────────────────────────
# CATEGORICAL DEPENDENT VARIABLES (Rows in Cross Table)
# ─────────────────────────────────────────────────────────

# Response category (4 levels, associated with treatment)
# Treatment A should have better response
response_probs <- list(
  "Control" = c(0.15, 0.25, 0.35, 0.25),  # Complete, Partial, Stable, Progression
  "Treatment A" = c(0.40, 0.35, 0.20, 0.05),  # Better response
  "Treatment B" = c(0.25, 0.35, 0.25, 0.15)   # Moderate response
)

crosstable_test$response <- sapply(1:n, function(i) {
  tx <- as.character(crosstable_test$treatment[i])
  probs <- response_probs[[tx]]
  sample(c("Complete Response", "Partial Response", "Stable Disease", "Progression"),
         1, prob = probs)
})
crosstable_test$response <- factor(crosstable_test$response,
                                   levels = c("Complete Response", "Partial Response",
                                            "Stable Disease", "Progression"))

# Tumor grade (3 levels)
crosstable_test$grade <- sample(
  c("Grade 1", "Grade 2", "Grade 3"),
  n,
  replace = TRUE,
  prob = c(0.30, 0.45, 0.25)
)
crosstable_test$grade <- factor(crosstable_test$grade,
                                levels = c("Grade 1", "Grade 2", "Grade 3"))

# Sex (2 levels, balanced)
crosstable_test$sex <- sample(
  c("Male", "Female"),
  n,
  replace = TRUE,
  prob = c(0.48, 0.52)
)
crosstable_test$sex <- factor(crosstable_test$sex)

# Smoking status (3 levels)
crosstable_test$smoking <- sample(
  c("Never", "Former", "Current"),
  n,
  replace = TRUE,
  prob = c(0.45, 0.30, 0.25)
)
crosstable_test$smoking <- factor(crosstable_test$smoking,
                                  levels = c("Never", "Former", "Current"))

# ECOG Performance Status (5 levels, ordinal)
crosstable_test$ecog <- sample(
  c("0", "1", "2", "3", "4"),
  n,
  replace = TRUE,
  prob = c(0.25, 0.35, 0.25, 0.10, 0.05)
)
crosstable_test$ecog <- factor(crosstable_test$ecog,
                               levels = c("0", "1", "2", "3", "4"),
                               ordered = TRUE)

# Histology type (5 levels)
crosstable_test$histology <- sample(
  c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell", "Other"),
  n,
  replace = TRUE,
  prob = c(0.40, 0.25, 0.15, 0.10, 0.10)
)
crosstable_test$histology <- factor(crosstable_test$histology)

# Mutation status (2 levels, binary)
crosstable_test$mutation <- sample(
  c("Wild-type", "Mutated"),
  n,
  replace = TRUE,
  prob = c(0.60, 0.40)
)
crosstable_test$mutation <- factor(crosstable_test$mutation,
                                   levels = c("Wild-type", "Mutated"))

# Complications (2 levels, associated with treatment)
complication_risk <- ifelse(crosstable_test$treatment == "Treatment B", 0.30,
                           ifelse(crosstable_test$treatment == "Treatment A", 0.15, 0.20))
crosstable_test$complications <- sapply(complication_risk, function(p) {
  sample(c("No", "Yes"), 1, prob = c(1-p, p))
})
crosstable_test$complications <- factor(crosstable_test$complications,
                                       levels = c("No", "Yes"))

# ─────────────────────────────────────────────────────────
# CONTINUOUS DEPENDENT VARIABLES (For t-test/ANOVA)
# ─────────────────────────────────────────────────────────

# Age (years, 30-85, normal distribution)
crosstable_test$age <- round(rnorm(n, mean = 62, sd = 12))
crosstable_test$age <- pmax(30, pmin(85, crosstable_test$age))

# BMI (kg/m², 18-45, right-skewed)
crosstable_test$bmi <- round(rnorm(n, mean = 27, sd = 5), 1)
crosstable_test$bmi <- pmax(18, pmin(45, crosstable_test$bmi))

# Tumor size (mm, 10-120, associated with stage)
# Higher stage → larger tumor
size_mean <- ifelse(crosstable_test$stage == "Stage I", 25,
                    ifelse(crosstable_test$stage == "Stage II", 35,
                          ifelse(crosstable_test$stage == "Stage III", 50, 70)))
crosstable_test$tumor_size <- round(rnorm(n, mean = size_mean, sd = 15))
crosstable_test$tumor_size <- pmax(10, pmin(120, crosstable_test$tumor_size))

# Ki-67 proliferation index (%, 0-100, associated with grade)
ki67_mean <- ifelse(crosstable_test$grade == "Grade 1", 10,
                    ifelse(crosstable_test$grade == "Grade 2", 25, 45))
crosstable_test$ki67 <- round(rnorm(n, mean = ki67_mean, sd = 12), 1)
crosstable_test$ki67 <- pmax(0, pmin(100, crosstable_test$ki67))

# PSA level (ng/mL, 0.5-100, log-normal, males only make sense but include all for testing)
crosstable_test$psa <- round(exp(rnorm(n, mean = log(8), sd = 1.2)), 2)
crosstable_test$psa <- pmax(0.5, pmin(100, crosstable_test$psa))

# Hemoglobin (g/dL, 8-18)
crosstable_test$hemoglobin <- round(rnorm(n, mean = 13.5, sd = 2.0), 1)
crosstable_test$hemoglobin <- pmax(8, pmin(18, crosstable_test$hemoglobin))

# Creatinine (mg/dL, 0.5-3.0)
crosstable_test$creatinine <- round(rnorm(n, mean = 1.0, sd = 0.4), 2)
crosstable_test$creatinine <- pmax(0.5, pmin(3.0, crosstable_test$creatinine))

# White blood cell count (×10³/μL, 2-20)
crosstable_test$wbc <- round(rnorm(n, mean = 7.5, sd = 2.5), 1)
crosstable_test$wbc <- pmax(2, pmin(20, crosstable_test$wbc))

# Survival time (months, 1-120, associated with stage and response)
# Better stage and response → longer survival
survival_mean <- 60
if_stage_iv <- crosstable_test$stage == "Stage IV"
if_progression <- crosstable_test$response == "Progression"
survival_mean <- ifelse(if_stage_iv, 12, ifelse(if_progression, 24, 60))

crosstable_test$survival_months <- round(pmax(1, rnorm(n, mean = survival_mean, sd = 20)))
crosstable_test$survival_months <- pmin(120, crosstable_test$survival_months)

# QOL score (quality of life, 0-100, associated with treatment and complications)
qol_base <- 70
qol_treatment_effect <- ifelse(crosstable_test$treatment == "Treatment A", 10,
                               ifelse(crosstable_test$treatment == "Treatment B", 5, 0))
qol_complication_effect <- ifelse(crosstable_test$complications == "Yes", -15, 0)

crosstable_test$qol_score <- round(rnorm(n, mean = qol_base + qol_treatment_effect + qol_complication_effect, sd = 15))
crosstable_test$qol_score <- pmax(0, pmin(100, crosstable_test$qol_score))

# ─────────────────────────────────────────────────────────
# VARIABLES WITH SPECIAL CHARACTERISTICS
# ─────────────────────────────────────────────────────────

# Variable with spaces in name (tests name handling)
crosstable_test$`Treatment Response` <- crosstable_test$response

# Variable with special characters (tests escaping)
crosstable_test$`Age (years)` <- crosstable_test$age

# Variable with very few observations in one category (tests Fisher's exact)
crosstable_test$rare_event <- sample(
  c("No", "Yes"),
  n,
  replace = TRUE,
  prob = c(0.95, 0.05)  # Only 5% in "Yes" category
)
crosstable_test$rare_event <- factor(crosstable_test$rare_event)

# Constant variable (edge case - no variation)
crosstable_test$constant_var <- rep("Same Value", n)
crosstable_test$constant_var <- factor(crosstable_test$constant_var)

# ═══════════════════════════════════════════════════════════
# Add Realistic Missing Data
# ═══════════════════════════════════════════════════════════

n_missing_high <- round(n * 0.10)  # 10% for optional tests
n_missing_moderate <- round(n * 0.05)  # 5% for routine measures
n_missing_low <- round(n * 0.02)  # 2% for key variables

# More missing in optional biomarkers
crosstable_test$ki67[sample(n, n_missing_high)] <- NA
crosstable_test$psa[sample(n, n_missing_high)] <- NA
crosstable_test$mutation[sample(n, n_missing_moderate)] <- NA

# Less missing in routine clinical variables
crosstable_test$response[sample(n, n_missing_low)] <- NA
crosstable_test$complications[sample(n, n_missing_low)] <- NA
crosstable_test$bmi[sample(n, n_missing_moderate)] <- NA
crosstable_test$tumor_size[sample(n, n_missing_moderate)] <- NA

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. Main dataset (RDA format)
save(crosstable_test, file = here::here("data", "crosstable_test.rda"))
message("✓ Created: data/crosstable_test.rda")

# 2. Main dataset (CSV format)
write.csv(crosstable_test,
          file = here::here("data", "crosstable_test.csv"),
          row.names = FALSE)
message("✓ Created: data/crosstable_test.csv")

# 3. Main dataset (Excel format)
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(crosstable_test,
                      path = here::here("data", "crosstable_test.xlsx"))
  message("✓ Created: data/crosstable_test.xlsx")
} else {
  message("⚠ writexl package not available - skipping Excel format")
}

# 4. Main dataset (Jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(crosstable_test,
                          here::here("data", "crosstable_test.omv"))
  message("✓ Created: data/crosstable_test.omv")
} else {
  message("⚠ jmvReadWrite package not available - skipping OMV format")
}

# ═══════════════════════════════════════════════════════════
# Dataset Documentation
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset: crosstable_test\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Observations:", n, "\n")
cat("Variables:", ncol(crosstable_test), "\n\n")

cat("Missing data profile:\n")
cat("  - Optional biomarkers (Ki-67, PSA): ~10%\n")
cat("  - Mutation status: ~5%\n")
cat("  - Clinical measures (BMI, tumor size): ~5%\n")
cat("  - Key outcomes (response, complications): ~2%\n\n")

cat("Variable categories:\n\n")

cat("1. GROUPING VARIABLES (Columns in cross table) - 3 vars:\n")
cat("   - treatment: 3 levels (Control, Treatment A, Treatment B)\n")
cat("   - stage: 4 levels (Stage I-IV)\n")
cat("   - recurrence: 2 levels (No, Yes)\n\n")

cat("2. CATEGORICAL DEPENDENT VARIABLES (Chi-sq/Fisher) - 10 vars:\n")
cat("   - response: 4 levels (Complete, Partial, Stable, Progression)\n")
cat("   - grade: 3 levels (Grade 1, 2, 3)\n")
cat("   - sex: 2 levels (Male, Female)\n")
cat("   - smoking: 3 levels (Never, Former, Current)\n")
cat("   - ecog: 5 levels (0-4, ordered)\n")
cat("   - histology: 5 levels (Adenocarcinoma, Squamous, Small Cell, Large, Other)\n")
cat("   - mutation: 2 levels (Wild-type, Mutated)\n")
cat("   - complications: 2 levels (No, Yes)\n")
cat("   - rare_event: 2 levels (95% No, 5% Yes - for Fisher's exact)\n")
cat("   - constant_var: 1 level (edge case - no variation)\n\n")

cat("3. CONTINUOUS DEPENDENT VARIABLES (t-test/ANOVA) - 10 vars:\n")
cat("   - age: Years (30-85)\n")
cat("   - bmi: Body Mass Index kg/m² (18-45)\n")
cat("   - tumor_size: Tumor size mm (10-120, correlated with stage)\n")
cat("   - ki67: Ki-67 proliferation index % (0-100, correlated with grade)\n")
cat("   - psa: PSA level ng/mL (0.5-100, log-normal)\n")
cat("   - hemoglobin: Hemoglobin g/dL (8-18)\n")
cat("   - creatinine: Creatinine mg/dL (0.5-3.0)\n")
cat("   - wbc: White blood cell count ×10³/μL (2-20)\n")
cat("   - survival_months: Survival time months (1-120, correlated with stage/response)\n")
cat("   - qol_score: Quality of life 0-100 (correlated with treatment/complications)\n\n")

cat("4. SPECIAL TESTING VARIABLES - 2 vars:\n")
cat("   - `Treatment Response`: Same as response (tests spaces in names)\n")
cat("   - `Age (years)`: Same as age (tests special characters)\n\n")

cat("Designed associations for testing:\n")
cat("  ✓ treatment × response: STRONG (Treatment A better)\n")
cat("  ✓ stage × tumor_size: STRONG (higher stage → larger tumor)\n")
cat("  ✓ grade × ki67: STRONG (higher grade → higher Ki-67)\n")
cat("  ✓ treatment × complications: MODERATE (Treatment B more complications)\n")
cat("  ✓ treatment × qol_score: MODERATE (Treatment A better QOL)\n")
cat("  ✓ stage × survival_months: STRONG (higher stage → shorter survival)\n")
cat("  ✓ sex × histology: INDEPENDENT (no association)\n\n")

cat("Table styles testable:\n")
cat("  • arsenal: Comprehensive clinical tables\n")
cat("  • finalfit: Clean publication-ready format\n")
cat("  • gtsummary: Modern gt-based tables with p-value adjustment\n")
cat("  • nejm: New England Journal of Medicine style\n")
cat("  • lancet: The Lancet journal style\n")
cat("  • hmisc: Harrell's Hmisc package style\n\n")

cat("Statistical tests automatically selected:\n")
cat("  • Categorical variables:\n")
cat("    - Chi-square test (default, adequate cell frequencies)\n")
cat("    - Fisher's exact test (rare_event: small expected frequencies)\n")
cat("  • Continuous variables:\n")
cat("    - t-test (2 groups: recurrence Yes/No)\n")
cat("    - ANOVA (3+ groups: treatment, stage)\n\n")

cat("Usage examples:\n\n")

cat("# Load the dataset\n")
cat("data(crosstable_test, package = 'ClinicoPath')\n\n")

cat("# Example 1: Basic cross table (treatment groups × response)\n")
cat("library(ClinicoPath)\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(response, grade, sex),\n")
cat("  group = 'treatment',\n")
cat("  sty = 'nejm'\n")
cat(")  # NEJM style table\n\n")

cat("# Example 2: Continuous variables by groups (ANOVA)\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(age, tumor_size, ki67),\n")
cat("  group = 'treatment',\n")
cat("  sty = 'finalfit',\n")
cat("  cont = 'mean'\n")
cat(")  # Show means\n\n")

cat("# Example 3: Mixed categorical and continuous\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(response, age, tumor_size, complications),\n")
cat("  group = 'stage',\n")
cat("  sty = 'gtsummary'\n")
cat(")  # gtsummary style\n\n")

cat("# Example 4: Binary grouping with Fisher's exact\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(rare_event, mutation),\n")
cat("  group = 'recurrence',\n")
cat("  pcat = 'fisher'\n")
cat(")  # Force Fisher's exact test\n\n")

cat("# Example 5: Median instead of mean\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(psa, survival_months),\n")
cat("  group = 'treatment',\n")
cat("  cont = 'median'\n")
cat(")  # Median for skewed data\n\n")

cat("# Example 6: Multiple testing correction (gtsummary only)\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(response, grade, mutation, complications),\n")
cat("  group = 'treatment',\n")
cat("  sty = 'gtsummary',\n")
cat("  p_adjust = 'BH'\n")
cat(")  # Benjamini-Hochberg FDR correction\n\n")

cat("# Example 7: Variables with special characters\n")
cat("crosstable(\n")
cat("  data = crosstable_test,\n")
cat("  vars = vars(`Treatment Response`, `Age (years)`),\n")
cat("  group = 'treatment',\n")
cat("  sty = 'lancet'\n")
cat(")  # Tests name handling\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset generation complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
