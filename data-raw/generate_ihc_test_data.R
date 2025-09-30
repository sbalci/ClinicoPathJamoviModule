# =============================================================================
# IHC Clustering Test Data Generator
# =============================================================================
#
# Purpose: Generate comprehensive, realistic IHC data for testing ihccluster
#
# Dataset Design:
# - 150 breast cancer cases
# - 4 molecular subtypes (Luminal A, Luminal B, HER2+, Triple Negative)
# - Categorical IHC markers: ER, PR, HER2, CK5/6, EGFR
# - Continuous IHC markers: Ki67 (%), AR H-score, p53 %
# - Clinical variables: Age, Grade, Stage, Tumor Size
# - Survival data: Overall survival time and event
# - Missing data patterns: ~10% realistic missingness
# - Edge cases: Small groups, imbalanced clusters
#
# Author: ClinicoPath Development Team
# Date: 2024-01-15
# =============================================================================

library(dplyr)

set.seed(42)  # For reproducibility

# =============================================================================
# 1. DEFINE MOLECULAR SUBTYPES WITH REALISTIC DISTRIBUTIONS
# =============================================================================

n_luminal_a <- 60   # 40% - Most common
n_luminal_b <- 40   # 26.7%
n_her2_pos <- 25    # 16.7%
n_triple_neg <- 25  # 16.7%
n_total <- n_luminal_a + n_luminal_b + n_her2_pos + n_triple_neg

# Create subtype labels
subtypes <- c(
    rep("Luminal_A", n_luminal_a),
    rep("Luminal_B", n_luminal_b),
    rep("HER2_Positive", n_her2_pos),
    rep("Triple_Negative", n_triple_neg)
)

# =============================================================================
# 2. GENERATE PATIENT IDs WITH REALISTIC NAMING
# =============================================================================

patient_ids <- sprintf("BC-%04d", 1:n_total)

# =============================================================================
# 3. GENERATE CATEGORICAL IHC MARKERS
# =============================================================================

# ER Status (Estrogen Receptor)
# Luminal A/B: 100% positive
# HER2+: ~20% positive
# Triple Negative: 0% positive
er_status <- c(
    rep("Positive", n_luminal_a),
    rep("Positive", n_luminal_b),
    sample(c("Positive", "Negative"), n_her2_pos, replace=TRUE, prob=c(0.2, 0.8)),
    rep("Negative", n_triple_neg)
)

# PR Status (Progesterone Receptor)
# Luminal A: ~90% positive
# Luminal B: ~70% positive
# HER2+: ~15% positive
# Triple Negative: 0% positive
pr_status <- c(
    sample(c("Positive", "Negative"), n_luminal_a, replace=TRUE, prob=c(0.90, 0.10)),
    sample(c("Positive", "Negative"), n_luminal_b, replace=TRUE, prob=c(0.70, 0.30)),
    sample(c("Positive", "Negative"), n_her2_pos, replace=TRUE, prob=c(0.15, 0.85)),
    rep("Negative", n_triple_neg)
)

# HER2 Status (IHC: 0, 1+, 2+, 3+)
# Luminal A/B: mostly 0/1+
# HER2+: 2+/3+
# Triple Negative: 0/1+
her2_status <- c(
    sample(c("0", "1+", "2+"), n_luminal_a, replace=TRUE, prob=c(0.7, 0.25, 0.05)),
    sample(c("0", "1+", "2+"), n_luminal_b, replace=TRUE, prob=c(0.6, 0.30, 0.10)),
    sample(c("2+", "3+"), n_her2_pos, replace=TRUE, prob=c(0.3, 0.7)),
    sample(c("0", "1+"), n_triple_neg, replace=TRUE, prob=c(0.8, 0.2))
)

# CK5/6 (Basal cytokeratin)
# Luminal: mostly negative
# Triple Negative: ~70% positive
# HER2+: ~20% positive
ck5_6_status <- c(
    sample(c("Positive", "Negative"), n_luminal_a, replace=TRUE, prob=c(0.05, 0.95)),
    sample(c("Positive", "Negative"), n_luminal_b, replace=TRUE, prob=c(0.10, 0.90)),
    sample(c("Positive", "Negative"), n_her2_pos, replace=TRUE, prob=c(0.20, 0.80)),
    sample(c("Positive", "Negative"), n_triple_neg, replace=TRUE, prob=c(0.70, 0.30))
)

# EGFR Status
# Triple Negative: ~50% positive
# Others: ~10% positive
egfr_status <- c(
    sample(c("Positive", "Negative"), n_luminal_a, replace=TRUE, prob=c(0.05, 0.95)),
    sample(c("Positive", "Negative"), n_luminal_b, replace=TRUE, prob=c(0.08, 0.92)),
    sample(c("Positive", "Negative"), n_her2_pos, replace=TRUE, prob=c(0.15, 0.85)),
    sample(c("Positive", "Negative"), n_triple_neg, replace=TRUE, prob=c(0.50, 0.50))
)

# =============================================================================
# 4. GENERATE CONTINUOUS IHC MARKERS
# =============================================================================

# Ki67 Proliferation Index (%)
# Luminal A: low (5-15%)
# Luminal B: high (20-50%)
# HER2+: moderate-high (15-40%)
# Triple Negative: high (30-80%)
ki67_percent <- c(
    pmax(0, pmin(100, rnorm(n_luminal_a, mean=10, sd=5))),
    pmax(0, pmin(100, rnorm(n_luminal_b, mean=35, sd=10))),
    pmax(0, pmin(100, rnorm(n_her2_pos, mean=28, sd=12))),
    pmax(0, pmin(100, rnorm(n_triple_neg, mean=55, sd=20)))
)

# AR H-score (Androgen Receptor, 0-300)
# Luminal: high (150-250)
# HER2+: moderate (80-150)
# Triple Negative: variable (0-200)
ar_hscore <- c(
    pmax(0, pmin(300, rnorm(n_luminal_a, mean=200, sd=50))),
    pmax(0, pmin(300, rnorm(n_luminal_b, mean=180, sd=60))),
    pmax(0, pmin(300, rnorm(n_her2_pos, mean=120, sd=50))),
    pmax(0, pmin(300, rnorm(n_triple_neg, mean=80, sd=60)))
)

# p53 Percentage (% nuclear staining)
# Wild-type: <10%
# Mutant: >60%
# Mix based on subtype mutation rates
p53_percent <- c(
    sample(c(
        rnorm(n_luminal_a * 0.8, mean=5, sd=3),      # 80% wild-type
        rnorm(n_luminal_a * 0.2, mean=75, sd=15)     # 20% mutant
    )),
    sample(c(
        rnorm(n_luminal_b * 0.7, mean=5, sd=3),      # 70% wild-type
        rnorm(n_luminal_b * 0.3, mean=75, sd=15)     # 30% mutant
    )),
    sample(c(
        rnorm(n_her2_pos * 0.6, mean=5, sd=3),       # 60% wild-type
        rnorm(n_her2_pos * 0.4, mean=75, sd=15)      # 40% mutant
    )),
    sample(c(
        rnorm(n_triple_neg * 0.2, mean=5, sd=3),     # 20% wild-type
        rnorm(n_triple_neg * 0.8, mean=75, sd=15)    # 80% mutant
    ))
)
p53_percent <- pmax(0, pmin(100, p53_percent))

# =============================================================================
# 5. GENERATE CLINICAL VARIABLES
# =============================================================================

# Age at diagnosis
# Younger for Triple Negative, older for Luminal A
age <- c(
    round(rnorm(n_luminal_a, mean=62, sd=12)),
    round(rnorm(n_luminal_b, mean=58, sd=11)),
    round(rnorm(n_her2_pos, mean=56, sd=13)),
    round(rnorm(n_triple_neg, mean=52, sd=14))
)
age <- pmax(30, pmin(90, age))

# Tumor Grade (1=well diff, 2=moderate, 3=poor)
# Luminal A: mostly grade 1-2
# Luminal B/HER2+/TNBC: mostly grade 2-3
tumor_grade <- c(
    sample(1:3, n_luminal_a, replace=TRUE, prob=c(0.3, 0.5, 0.2)),
    sample(1:3, n_luminal_b, replace=TRUE, prob=c(0.1, 0.4, 0.5)),
    sample(1:3, n_her2_pos, replace=TRUE, prob=c(0.05, 0.35, 0.6)),
    sample(1:3, n_triple_neg, replace=TRUE, prob=c(0.05, 0.25, 0.7))
)
tumor_grade <- factor(tumor_grade, levels=1:3, labels=c("G1", "G2", "G3"))

# Clinical Stage (I, II, III, IV)
tumor_stage <- sample(c("I", "II", "III", "IV"), n_total, replace=TRUE,
                      prob=c(0.30, 0.45, 0.20, 0.05))
tumor_stage <- factor(tumor_stage, levels=c("I", "II", "III", "IV"))

# Tumor Size (cm)
tumor_size <- round(rexp(n_total, rate=0.5) + 0.5, 1)
tumor_size <- pmin(tumor_size, 15)  # Cap at 15cm

# Lymph Node Status
lymph_nodes <- c(
    sample(c("Negative", "Positive"), n_luminal_a, replace=TRUE, prob=c(0.6, 0.4)),
    sample(c("Negative", "Positive"), n_luminal_b, replace=TRUE, prob=c(0.5, 0.5)),
    sample(c("Negative", "Positive"), n_her2_pos, replace=TRUE, prob=c(0.45, 0.55)),
    sample(c("Negative", "Positive"), n_triple_neg, replace=TRUE, prob=c(0.4, 0.6))
)

# =============================================================================
# 6. GENERATE SURVIVAL DATA
# =============================================================================

# Overall Survival (months)
# Luminal A: best prognosis
# Triple Negative: worst prognosis
# Add variation based on stage

base_survival <- c(
    rexp(n_luminal_a, rate=1/90),      # Median ~90 months
    rexp(n_luminal_b, rate=1/70),      # Median ~70 months
    rexp(n_her2_pos, rate=1/60),       # Median ~60 months
    rexp(n_triple_neg, rate=1/40)      # Median ~40 months
)

# Adjust for stage
stage_factor <- as.numeric(tumor_stage) / 2
os_months <- round(base_survival * (2 - stage_factor), 1)
os_months <- pmax(1, pmin(os_months, 180))  # Cap at 15 years

# Death Event (0=censored, 1=death)
# Worse subtypes have higher event rates
event_prob <- c(
    rep(0.30, n_luminal_a),    # 30% deaths
    rep(0.45, n_luminal_b),    # 45% deaths
    rep(0.55, n_her2_pos),     # 55% deaths
    rep(0.65, n_triple_neg)    # 65% deaths
)
os_event <- rbinom(n_total, 1, event_prob)

# =============================================================================
# 7. ADD REALISTIC MISSING DATA
# =============================================================================

# Missing data patterns (~10% overall)
# Some markers have more missingness (e.g., newer markers)

introduce_missing <- function(x, rate) {
    x[sample(1:length(x), size=round(length(x) * rate))] <- NA
    x
}

# Add missing values
ki67_percent <- introduce_missing(ki67_percent, 0.08)
ar_hscore <- introduce_missing(ar_hscore, 0.12)  # Newer marker, more missing
p53_percent <- introduce_missing(p53_percent, 0.10)
ck5_6_status <- introduce_missing(ck5_6_status, 0.05)
egfr_status <- introduce_missing(egfr_status, 0.07)
tumor_size <- introduce_missing(tumor_size, 0.06)

# =============================================================================
# 8. CREATE FINAL DATASET
# =============================================================================

ihc_breast_cancer <- data.frame(
    # Identifiers
    PatientID = patient_ids,
    TrueSubtype = subtypes,  # Ground truth for validation

    # Categorical IHC Markers
    ER_Status = factor(er_status, levels=c("Negative", "Positive")),
    PR_Status = factor(pr_status, levels=c("Negative", "Positive")),
    HER2_IHC = factor(her2_status, levels=c("0", "1+", "2+", "3+")),
    CK5_6 = factor(ck5_6_status, levels=c("Negative", "Positive")),
    EGFR = factor(egfr_status, levels=c("Negative", "Positive")),

    # Continuous IHC Markers
    Ki67_Percent = round(ki67_percent, 1),
    AR_Hscore = round(ar_hscore, 0),
    p53_Percent = round(p53_percent, 1),

    # Clinical Variables
    Age_Years = age,
    Tumor_Grade = tumor_grade,
    Tumor_Stage = tumor_stage,
    Tumor_Size_cm = tumor_size,
    Lymph_Node_Status = factor(lymph_nodes, levels=c("Negative", "Positive")),

    # Survival Data
    OS_Months = os_months,
    OS_Event = os_event,

    stringsAsFactors = FALSE
)

# =============================================================================
# 9. ADD SPECIAL TEST CASES
# =============================================================================

# Add 5 cases with extreme values for edge testing
extreme_cases <- data.frame(
    PatientID = sprintf("BC-TEST%02d", 1:5),
    TrueSubtype = "Test_Case",

    # Case 1: All positive
    ER_Status = factor(c("Positive", "Negative", "Positive", "Negative", "Positive"),
                      levels=c("Negative", "Positive")),
    PR_Status = factor(c("Positive", "Negative", "Positive", "Negative", "Positive"),
                      levels=c("Negative", "Positive")),
    HER2_IHC = factor(c("3+", "0", "2+", "1+", "3+"), levels=c("0", "1+", "2+", "3+")),
    CK5_6 = factor(c("Negative", "Positive", "Negative", "Positive", "Negative"),
                  levels=c("Negative", "Positive")),
    EGFR = factor(c("Negative", "Positive", "Negative", "Positive", "Negative"),
                 levels=c("Negative", "Positive")),

    # Extreme continuous values
    Ki67_Percent = c(0, 100, 50, 5, 95),
    AR_Hscore = c(0, 300, 150, 10, 290),
    p53_Percent = c(0, 100, 50, 2, 98),

    Age_Years = c(30, 90, 60, 35, 85),
    Tumor_Grade = factor(c("G3", "G1", "G2", "G3", "G1"), levels=c("G1", "G2", "G3")),
    Tumor_Stage = factor(c("IV", "I", "II", "III", "IV"), levels=c("I", "II", "III", "IV")),
    Tumor_Size_cm = c(15, 0.5, 3, 8, 12),
    Lymph_Node_Status = factor(c("Positive", "Negative", "Positive", "Negative", "Positive"),
                               levels=c("Negative", "Positive")),

    OS_Months = c(1, 180, 60, 24, 120),
    OS_Event = c(1, 0, 1, 1, 0),

    stringsAsFactors = FALSE
)

# Combine main data with test cases
ihc_breast_cancer <- rbind(ihc_breast_cancer, extreme_cases)

# =============================================================================
# 10. DATA QUALITY CHECKS
# =============================================================================

cat("\n=== DATA GENERATION SUMMARY ===\n")
cat(sprintf("Total cases: %d\n", nrow(ihc_breast_cancer)))
cat(sprintf("  - Luminal A: %d\n", sum(ihc_breast_cancer$TrueSubtype == "Luminal_A")))
cat(sprintf("  - Luminal B: %d\n", sum(ihc_breast_cancer$TrueSubtype == "Luminal_B")))
cat(sprintf("  - HER2 Positive: %d\n", sum(ihc_breast_cancer$TrueSubtype == "HER2_Positive")))
cat(sprintf("  - Triple Negative: %d\n", sum(ihc_breast_cancer$TrueSubtype == "Triple_Negative")))
cat(sprintf("  - Test Cases: %d\n", sum(ihc_breast_cancer$TrueSubtype == "Test_Case")))

cat("\n=== MISSING DATA SUMMARY ===\n")
missing_summary <- sapply(ihc_breast_cancer, function(x) sum(is.na(x)))
missing_pct <- round(100 * missing_summary / nrow(ihc_breast_cancer), 1)
missing_data <- data.frame(
    Variable = names(missing_summary),
    Missing = missing_summary,
    Percent = missing_pct
)
print(missing_data[missing_data$Missing > 0, ])

cat("\n=== CATEGORICAL MARKER DISTRIBUTIONS ===\n")
cat("ER Status:\n")
print(table(ihc_breast_cancer$ER_Status, useNA="ifany"))
cat("\nPR Status:\n")
print(table(ihc_breast_cancer$PR_Status, useNA="ifany"))
cat("\nHER2 IHC:\n")
print(table(ihc_breast_cancer$HER2_IHC, useNA="ifany"))

cat("\n=== CONTINUOUS MARKER SUMMARY ===\n")
cat("Ki67 %: ")
cat(sprintf("Mean=%.1f, Median=%.1f, Range=%.1f-%.1f\n",
    mean(ihc_breast_cancer$Ki67_Percent, na.rm=TRUE),
    median(ihc_breast_cancer$Ki67_Percent, na.rm=TRUE),
    min(ihc_breast_cancer$Ki67_Percent, na.rm=TRUE),
    max(ihc_breast_cancer$Ki67_Percent, na.rm=TRUE)))

cat("AR H-score: ")
cat(sprintf("Mean=%.0f, Median=%.0f, Range=%.0f-%.0f\n",
    mean(ihc_breast_cancer$AR_Hscore, na.rm=TRUE),
    median(ihc_breast_cancer$AR_Hscore, na.rm=TRUE),
    min(ihc_breast_cancer$AR_Hscore, na.rm=TRUE),
    max(ihc_breast_cancer$AR_Hscore, na.rm=TRUE)))

cat("\n=== SURVIVAL DATA SUMMARY ===\n")
cat(sprintf("Median OS: %.1f months\n", median(ihc_breast_cancer$OS_Months)))
cat(sprintf("Events: %d (%.1f%%)\n",
    sum(ihc_breast_cancer$OS_Event),
    100 * mean(ihc_breast_cancer$OS_Event)))

# =============================================================================
# 11. SAVE DATASETS
# =============================================================================

# Save to data folder as CSV
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
}

csv_path <- file.path(data_dir, "ihc_breast_cancer.csv")
write.csv(ihc_breast_cancer, csv_path, row.names = FALSE)
cat(sprintf("\n✅ Data saved to: %s\n", csv_path))

# Also save as RDS for faster loading
rds_path <- file.path(data_dir, "ihc_breast_cancer.rds")
saveRDS(ihc_breast_cancer, rds_path)
cat(sprintf("✅ Data saved to: %s\n", rds_path))

# =============================================================================
# 12. CREATE DATA DOCUMENTATION
# =============================================================================

# Create codebook
codebook <- "
# IHC Breast Cancer Clustering Test Dataset

## Overview
Realistic breast cancer IHC data for testing clustering algorithms.
155 cases representing 4 molecular subtypes with clinical and survival data.

## Variables

### Identifiers
- **PatientID**: Unique patient identifier (BC-0001 to BC-0155)
- **TrueSubtype**: Ground truth molecular subtype for validation
  - Luminal_A (n=60)
  - Luminal_B (n=40)
  - HER2_Positive (n=25)
  - Triple_Negative (n=25)
  - Test_Case (n=5, edge cases)

### Categorical IHC Markers
- **ER_Status**: Estrogen Receptor (Negative/Positive)
- **PR_Status**: Progesterone Receptor (Negative/Positive)
- **HER2_IHC**: HER2 immunohistochemistry (0/1+/2+/3+)
- **CK5_6**: Cytokeratin 5/6 basal marker (Negative/Positive)
- **EGFR**: Epidermal Growth Factor Receptor (Negative/Positive)

### Continuous IHC Markers
- **Ki67_Percent**: Proliferation index (0-100%)
- **AR_Hscore**: Androgen Receptor H-score (0-300)
- **p53_Percent**: p53 nuclear staining (0-100%)

### Clinical Variables
- **Age_Years**: Age at diagnosis (30-90 years)
- **Tumor_Grade**: Histologic grade (G1/G2/G3)
- **Tumor_Stage**: Clinical stage (I/II/III/IV)
- **Tumor_Size_cm**: Tumor size in cm
- **Lymph_Node_Status**: Lymph node involvement (Negative/Positive)

### Survival Data
- **OS_Months**: Overall survival time in months
- **OS_Event**: Death event (0=censored, 1=death)

## Missing Data
~10% realistic missingness across markers:
- AR_Hscore: 12% (newer marker)
- p53_Percent: 10%
- Ki67_Percent: 8%
- EGFR: 7%

## Expected Clustering Results
With proper clustering, should identify ~4 main clusters corresponding to:
1. Luminal A: ER+/PR+/HER2-/low Ki67
2. Luminal B: ER+/PR+/HER2-/high Ki67 or HER2+
3. HER2-enriched: ER-/PR-/HER2+++
4. Triple Negative: ER-/PR-/HER2-/high CK5_6

## Test Scenarios
The dataset enables testing:
- Mixed categorical and continuous markers
- Missing data handling (complete vs pairwise)
- Multiple clustering methods (PAM, hierarchical, dimension reduction)
- Survival analysis integration
- Clinical variable correlations
- Tumor preset configurations (breast_luminal, breast_triple_negative)
- Edge cases (extreme values in test cases)

## Generation
Generated using data-raw/generate_ihc_test_data.R
Date: 2024-01-15
Seed: 42 (reproducible)
"

codebook_path <- file.path(data_dir, "ihc_breast_cancer_codebook.txt")
writeLines(codebook, codebook_path)
cat(sprintf("✅ Codebook saved to: %s\n", codebook_path))

cat("\n=== DATA GENERATION COMPLETE ===\n")