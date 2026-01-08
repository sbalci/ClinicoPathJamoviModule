# ═══════════════════════════════════════════════════════════
# Test Data Generation: chisqposttest
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the chisqposttest jamovi function
#
# Purpose: Generate categorical contingency table data for testing chi-square
#          post-hoc pairwise comparisons with various adjustment methods
#          (Bonferroni, Holm, FDR)
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 300

library(tibble)
library(dplyr)
library(here)

# Ensure reproducibility
set.seed(42)

# Sample size - adequate for chi-square testing
n <- 300

# ═══════════════════════════════════════════════════════════
# Helper Function: Generate Dependent Categorical Variables
# ═══════════════════════════════════════════════════════════

# Create categorical variable B dependent on categorical variable A
# association_strength: 0 = independent, 1 = perfect association
generate_dependent_categories <- function(var_a, categories_b, association_strength = 0.5) {
  n <- length(var_a)
  levels_a <- unique(var_a)
  n_levels_a <- length(levels_a)
  n_levels_b <- length(categories_b)

  # Create base probabilities for B
  base_probs <- rep(1 / n_levels_b, n_levels_b)

  var_b <- character(n)

  for (i in 1:n) {
    # Get level index for current observation
    a_level_idx <- which(levels_a == var_a[i])

    # Modify probabilities based on association strength
    # Shift probabilities toward certain categories based on A's level
    adjusted_probs <- base_probs

    # Create dependency: level i of A favors level i of B (circular if needed)
    favored_b_idx <- ((a_level_idx - 1) %% n_levels_b) + 1

    # Strengthen probability for favored category
    adjusted_probs[favored_b_idx] <- base_probs[favored_b_idx] + association_strength
    adjusted_probs <- adjusted_probs / sum(adjusted_probs)

    # Sample from adjusted distribution
    var_b[i] <- sample(categories_b, size = 1, prob = adjusted_probs)
  }

  return(factor(var_b, levels = categories_b))
}

# ═══════════════════════════════════════════════════════════
# Generate Main Test Dataset
# ═══════════════════════════════════════════════════════════

chisqposttest_test <- tibble(
  # Patient ID
  patient_id = 1:n
)

# ─────────────────────────────────────────────────────────
# 1. Treatment Response Analysis (3x4 table, STRONG association)
# ─────────────────────────────────────────────────────────

# Treatment groups (3 levels)
chisqposttest_test$treatment <- sample(
  c("Control", "Drug A", "Drug B"),
  n,
  replace = TRUE,
  prob = c(0.35, 0.35, 0.3)
)

# Response (4 levels, dependent on treatment with strong association)
chisqposttest_test$response <- generate_dependent_categories(
  chisqposttest_test$treatment,
  c("Complete Response", "Partial Response", "Stable Disease", "Progression"),
  association_strength = 0.6  # Strong association
)

# ─────────────────────────────────────────────────────────
# 2. Tumor Grade vs Stage (3x4 table, MODERATE association)
# ─────────────────────────────────────────────────────────

# Tumor grade (3 levels)
chisqposttest_test$grade <- sample(
  c("Grade 1", "Grade 2", "Grade 3"),
  n,
  replace = TRUE,
  prob = c(0.3, 0.45, 0.25)
)

# Tumor stage (4 levels, dependent on grade)
chisqposttest_test$stage <- generate_dependent_categories(
  chisqposttest_test$grade,
  c("Stage I", "Stage II", "Stage III", "Stage IV"),
  association_strength = 0.4  # Moderate association
)

# ─────────────────────────────────────────────────────────
# 3. Complications vs Risk Group (2x3 table, MODERATE association)
# ─────────────────────────────────────────────────────────

# Risk group (3 levels)
chisqposttest_test$risk_group <- sample(
  c("Low Risk", "Moderate Risk", "High Risk"),
  n,
  replace = TRUE,
  prob = c(0.4, 0.35, 0.25)
)

# Complications (2 levels, dependent on risk)
chisqposttest_test$complications <- generate_dependent_categories(
  chisqposttest_test$risk_group,
  c("No Complications", "Complications"),
  association_strength = 0.5
)

# ─────────────────────────────────────────────────────────
# 4. Smoking Status vs Cancer Type (3x5 table, WEAK association)
# ─────────────────────────────────────────────────────────

# Smoking status (3 levels)
chisqposttest_test$smoking <- sample(
  c("Never Smoker", "Former Smoker", "Current Smoker"),
  n,
  replace = TRUE,
  prob = c(0.4, 0.35, 0.25)
)

# Cancer type (5 levels, weak association with smoking)
chisqposttest_test$cancer_type <- generate_dependent_categories(
  chisqposttest_test$smoking,
  c("Lung", "Breast", "Colon", "Prostate", "Other"),
  association_strength = 0.2  # Weak association
)

# ─────────────────────────────────────────────────────────
# 5. Mutation Status vs Treatment Outcome (2x2 table, STRONG)
# ─────────────────────────────────────────────────────────

# Mutation status (2 levels)
chisqposttest_test$mutation <- sample(
  c("Wild-type", "Mutated"),
  n,
  replace = TRUE,
  prob = c(0.65, 0.35)
)

# Treatment outcome (2 levels, strong dependency)
chisqposttest_test$outcome <- generate_dependent_categories(
  chisqposttest_test$mutation,
  c("Responder", "Non-responder"),
  association_strength = 0.7
)

# ─────────────────────────────────────────────────────────
# 6. Performance Status vs Survival (4x2 table)
# ─────────────────────────────────────────────────────────

# ECOG Performance Status (4 levels)
chisqposttest_test$ecog <- sample(
  c("PS 0", "PS 1", "PS 2", "PS 3-4"),
  n,
  replace = TRUE,
  prob = c(0.25, 0.35, 0.25, 0.15)
)

# Survival status (2 levels, dependent on performance)
chisqposttest_test$survival <- generate_dependent_categories(
  chisqposttest_test$ecog,
  c("Alive", "Deceased"),
  association_strength = 0.6
)

# ─────────────────────────────────────────────────────────
# 7. Age Group vs Disease Severity (3x3 table, MODERATE)
# ─────────────────────────────────────────────────────────

# Age group (3 levels)
chisqposttest_test$age_group <- sample(
  c("Young (<50)", "Middle (50-70)", "Older (>70)"),
  n,
  replace = TRUE,
  prob = c(0.25, 0.45, 0.3)
)

# Disease severity (3 levels)
chisqposttest_test$severity <- generate_dependent_categories(
  chisqposttest_test$age_group,
  c("Mild", "Moderate", "Severe"),
  association_strength = 0.35
)

# ─────────────────────────────────────────────────────────
# 8. Independent Variables (NO association) - NULL hypothesis
# ─────────────────────────────────────────────────────────

# Sex (2 levels, independent)
chisqposttest_test$sex <- sample(
  c("Male", "Female"),
  n,
  replace = TRUE,
  prob = c(0.5, 0.5)
)

# Tumor site (5 levels, independent of sex)
chisqposttest_test$tumor_site <- sample(
  c("Breast", "Lung", "Colon", "Prostate", "Other"),
  n,
  replace = TRUE,
  prob = c(0.22, 0.20, 0.20, 0.18, 0.20)
)

# ─────────────────────────────────────────────────────────
# 9. Small Expected Frequencies (Test Fisher's exact test)
# ─────────────────────────────────────────────────────────

# Rare mutation (2 levels, imbalanced)
chisqposttest_test$rare_mutation <- sample(
  c("Negative", "Positive"),
  n,
  replace = TRUE,
  prob = c(0.92, 0.08)  # Only 8% positive - creates small cells
)

# Adverse event (2 levels)
chisqposttest_test$adverse_event <- generate_dependent_categories(
  chisqposttest_test$rare_mutation,
  c("No", "Yes"),
  association_strength = 0.5
)

# ─────────────────────────────────────────────────────────
# 10. Ordinal Categories (Testing ordered chi-square)
# ─────────────────────────────────────────────────────────

# Pain level (5 levels, ordered)
chisqposttest_test$pain_level <- sample(
  c("None", "Mild", "Moderate", "Severe", "Very Severe"),
  n,
  replace = TRUE,
  prob = c(0.25, 0.30, 0.25, 0.15, 0.05)
)
chisqposttest_test$pain_level <- factor(
  chisqposttest_test$pain_level,
  levels = c("None", "Mild", "Moderate", "Severe", "Very Severe"),
  ordered = TRUE
)

# Intervention type (3 levels)
chisqposttest_test$intervention <- sample(
  c("Placebo", "Low Dose", "High Dose"),
  n,
  replace = TRUE
)

# ─────────────────────────────────────────────────────────
# 11. Clinical Trial Scenarios
# ─────────────────────────────────────────────────────────

# Histology type (4 levels)
chisqposttest_test$histology <- sample(
  c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Other"),
  n,
  replace = TRUE,
  prob = c(0.45, 0.30, 0.15, 0.10)
)

# Biomarker expression (3 levels, dependent on histology)
chisqposttest_test$biomarker_expression <- generate_dependent_categories(
  chisqposttest_test$histology,
  c("Negative", "Low", "High"),
  association_strength = 0.45
)

# ═══════════════════════════════════════════════════════════
# Add Missing Data (~5%)
# ═══════════════════════════════════════════════════════════

n_missing <- round(n * 0.05)

# More missing in biomarker/mutation data (not always tested)
chisqposttest_test$biomarker_expression[sample(n, round(n * 0.08))] <- NA
chisqposttest_test$mutation[sample(n, round(n * 0.06))] <- NA
chisqposttest_test$rare_mutation[sample(n, round(n * 0.07))] <- NA

# Less missing in routine clinical variables
chisqposttest_test$response[sample(n, n_missing)] <- NA
chisqposttest_test$complications[sample(n, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# Create Aggregated Dataset with Counts
# ═══════════════════════════════════════════════════════════

# Aggregate treatment vs response with counts
chisqposttest_aggregated <- chisqposttest_test %>%
  filter(!is.na(treatment) & !is.na(response)) %>%
  group_by(treatment, response) %>%
  summarise(count = n(), .groups = "drop")

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. Main dataset (RDA format)
save(chisqposttest_test, file = here::here("data", "chisqposttest_test.rda"))
message("✓ Created: data/chisqposttest_test.rda")

# 2. Aggregated dataset (RDA format)
save(chisqposttest_aggregated, file = here::here("data", "chisqposttest_aggregated.rda"))
message("✓ Created: data/chisqposttest_aggregated.rda")

# 3. Main dataset (CSV format)
write.csv(chisqposttest_test,
          file = here::here("data", "chisqposttest_test.csv"),
          row.names = FALSE)
message("✓ Created: data/chisqposttest_test.csv")

# 4. Aggregated dataset (CSV format)
write.csv(chisqposttest_aggregated,
          file = here::here("data", "chisqposttest_aggregated.csv"),
          row.names = FALSE)
message("✓ Created: data/chisqposttest_aggregated.csv")

# 5. Main dataset (Excel format)
if (requireNamespace("writexl", quietly = TRUE)) {
  # Create multi-sheet workbook
  excel_data <- list(
    "Raw Data" = chisqposttest_test,
    "Aggregated Data" = chisqposttest_aggregated
  )
  writexl::write_xlsx(excel_data,
                      path = here::here("data", "chisqposttest_test.xlsx"))
  message("✓ Created: data/chisqposttest_test.xlsx (multi-sheet)")
} else {
  message("⚠ writexl package not available - skipping Excel format")
}

# 6. Main dataset (Jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(chisqposttest_test,
                          here::here("data", "chisqposttest_test.omv"))
  message("✓ Created: data/chisqposttest_test.omv")
} else {
  message("⚠ jmvReadWrite package not available - skipping OMV format")
}

# ═══════════════════════════════════════════════════════════
# Dataset Documentation
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset: chisqposttest_test\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Observations:", n, "\n")
cat("Variables:", ncol(chisqposttest_test), "\n\n")

cat("Missing data profile:\n")
cat("  - Biomarker/mutation data: ~6-8%\n")
cat("  - Clinical response data: ~5%\n\n")

cat("Variable categories:\n\n")

cat("1. STRONG ASSOCIATIONS (Post-hoc tests expected) - 4 pairs:\n")
cat("   - treatment × response: 3x4 table (strong association = 0.6)\n")
cat("   - mutation × outcome: 2x2 table (strong association = 0.7)\n")
cat("   - ecog × survival: 4x2 table (strong association = 0.6)\n")
cat("   - grade × stage: 3x4 table (moderate-strong = 0.4)\n\n")

cat("2. MODERATE ASSOCIATIONS - 3 pairs:\n")
cat("   - risk_group × complications: 3x2 table (moderate = 0.5)\n")
cat("   - histology × biomarker_expression: 4x3 table (moderate = 0.45)\n")
cat("   - age_group × severity: 3x3 table (moderate = 0.35)\n\n")

cat("3. WEAK ASSOCIATIONS - 1 pair:\n")
cat("   - smoking × cancer_type: 3x5 table (weak association = 0.2)\n\n")

cat("4. INDEPENDENT VARIABLES (NULL hypothesis) - 1 pair:\n")
cat("   - sex × tumor_site: 2x5 table (no association)\n\n")

cat("5. SMALL EXPECTED FREQUENCIES (Fisher's exact) - 1 pair:\n")
cat("   - rare_mutation × adverse_event: 2x2 table (8% positive)\n\n")

cat("6. ORDINAL CATEGORIES - 1 pair:\n")
cat("   - intervention × pain_level: 3x5 table (ordered outcome)\n\n")

cat("Variable details:\n\n")

cat("Row variables (suitable for rows in contingency table):\n")
cat("  - treatment: 3 levels (Control, Drug A, Drug B)\n")
cat("  - grade: 3 levels (Grade 1, 2, 3)\n")
cat("  - risk_group: 3 levels (Low, Moderate, High)\n")
cat("  - smoking: 3 levels (Never, Former, Current)\n")
cat("  - mutation: 2 levels (Wild-type, Mutated)\n")
cat("  - ecog: 4 levels (PS 0, 1, 2, 3-4)\n")
cat("  - age_group: 3 levels (Young, Middle, Older)\n")
cat("  - sex: 2 levels (Male, Female)\n")
cat("  - rare_mutation: 2 levels (Negative, Positive - 8%)\n")
cat("  - histology: 4 levels (Adenocarcinoma, Squamous, Small Cell, Other)\n")
cat("  - intervention: 3 levels (Placebo, Low Dose, High Dose)\n\n")

cat("Column variables (suitable for columns in contingency table):\n")
cat("  - response: 4 levels (Complete, Partial, Stable, Progression)\n")
cat("  - stage: 4 levels (Stage I-IV)\n")
cat("  - complications: 2 levels (No, Yes)\n")
cat("  - cancer_type: 5 levels (Lung, Breast, Colon, Prostate, Other)\n")
cat("  - outcome: 2 levels (Responder, Non-responder)\n")
cat("  - survival: 2 levels (Alive, Deceased)\n")
cat("  - severity: 3 levels (Mild, Moderate, Severe)\n")
cat("  - tumor_site: 5 levels (Breast, Lung, Colon, Prostate, Other)\n")
cat("  - adverse_event: 2 levels (No, Yes)\n")
cat("  - biomarker_expression: 3 levels (Negative, Low, High)\n")
cat("  - pain_level: 5 levels (None to Very Severe, ordered)\n\n")

cat("Aggregated dataset (chisqposttest_aggregated):\n")
cat("  - Pre-aggregated treatment × response table\n")
cat("  - 3 columns: treatment, response, count\n")
cat("  - Use 'count' as weights variable\n\n")

cat("Expected behavior by scenario:\n\n")

cat("STRONG associations (overall χ² p < 0.001):\n")
cat("  ✓ Post-hoc tests should run\n")
cat("  ✓ Multiple significant pairwise comparisons expected\n")
cat("  ✓ Bonferroni < Holm < FDR p-values\n\n")

cat("MODERATE associations (overall χ² p < 0.05):\n")
cat("  ✓ Post-hoc tests should run\n")
cat("  ✓ Some significant pairwise comparisons\n")
cat("  ✓ Adjustment method affects significance\n\n")

cat("WEAK/NO associations (overall χ² p > 0.05):\n")
cat("  ✗ Post-hoc tests SHOULD NOT run\n")
cat("  ℹ Message: \"Overall chi-square not significant\"\n\n")

cat("SMALL expected frequencies (rare_mutation):\n")
cat("  ⚠ Warning expected: \"Small expected frequencies\"\n")
cat("  ✓ Fisher's exact test recommended/auto-selected\n\n")

cat("Usage examples:\n\n")

cat("# Load the dataset\n")
cat("data(chisqposttest_test, package = 'ClinicoPath')\n\n")

cat("# Example 1: Strong association (post-hoc tests run)\n")
cat("library(ClinicoPath)\n")
cat("chisqposttest(\n")
cat("  data = chisqposttest_test,\n")
cat("  rows = 'treatment',\n")
cat("  cols = 'response',\n")
cat("  posthoc = 'bonferroni'\n")
cat(")  # Expect significant overall χ² and multiple pairwise differences\n\n")

cat("# Example 2: Using aggregated data with counts\n")
cat("data(chisqposttest_aggregated, package = 'ClinicoPath')\n")
cat("chisqposttest(\n")
cat("  data = chisqposttest_aggregated,\n")
cat("  rows = 'treatment',\n")
cat("  cols = 'response',\n")
cat("  counts = 'count',\n")
cat("  posthoc = 'bonferroni'\n")
cat(")  # Same results as raw data\n\n")

cat("# Example 3: 2x2 table with strong effect\n")
cat("chisqposttest(\n")
cat("  data = chisqposttest_test,\n")
cat("  rows = 'mutation',\n")
cat("  cols = 'outcome',\n")
cat("  posthoc = 'bonferroni'\n")
cat(")  # 2x2 table - post-hoc not applicable but overall χ² significant\n\n")

cat("# Example 4: Small expected frequencies (Fisher's exact)\n")
cat("chisqposttest(\n")
cat("  data = chisqposttest_test,\n")
cat("  rows = 'rare_mutation',\n")
cat("  cols = 'adverse_event',\n")
cat("  testSelection = 'auto'\n")
cat(")  # Should auto-select Fisher's exact test\n\n")

cat("# Example 5: No association (post-hoc should NOT run)\n")
cat("chisqposttest(\n")
cat("  data = chisqposttest_test,\n")
cat("  rows = 'sex',\n")
cat("  cols = 'tumor_site',\n")
cat("  posthoc = 'bonferroni'\n")
cat(")  # Overall χ² non-significant → no post-hoc tests\n\n")

cat("# Example 6: Comparing adjustment methods\n")
cat("# Bonferroni (most conservative)\n")
cat("chisqposttest(data = chisqposttest_test, rows = 'grade', cols = 'stage',\n")
cat("             posthoc = 'bonferroni')\n\n")

cat("# Holm (less conservative)\n")
cat("chisqposttest(data = chisqposttest_test, rows = 'grade', cols = 'stage',\n")
cat("             posthoc = 'holm')\n\n")

cat("# FDR (controls false discovery rate)\n")
cat("chisqposttest(data = chisqposttest_test, rows = 'grade', cols = 'stage',\n")
cat("             posthoc = 'fdr')\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset generation complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
