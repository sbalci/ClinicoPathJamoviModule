# Create comprehensive test dataset for chisqposttest function
# This script generates multiple datasets testing all features
# Author: Auto-generated for comprehensive testing
# Date: 2025-12-23

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

set.seed(123)  # For reproducibility

# ============================================================================
# Dataset 1: Main Test Data (Individual Observations)
# Tests: Post-hoc comparisons, various association strengths, missing data
# ============================================================================

n <- 400

chisqposttest_comprehensive <- data.frame(

  # ID
  ID = 1:n,

  # ===== SCENARIO 1: Strong Association (Post-hoc WILL run) =====
  # Treatment groups (4 levels) vs Outcome (2 levels)
  # This should produce significant omnibus test and significant pairwise comparisons
  TreatmentGroup = factor(
    sample(c("Placebo", "Drug A", "Drug B", "Drug C"), n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
    levels = c("Placebo", "Drug A", "Drug B", "Drug C")
  ),

  # Outcome strongly associated with treatment
  ClinicalResponse = factor(rep(NA, n), levels = c("No Response", "Response")),

  # ===== SCENARIO 2: Weak Association (Post-hoc may NOT run) =====
  # Disease Type (3 levels) vs Marker (2 levels) - weak/no association
  DiseaseType = factor(
    sample(c("Type 1", "Type 2", "Type 3"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    levels = c("Type 1", "Type 2", "Type 3")
  ),

  Biomarker = factor(
    sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.55, 0.45)),
    levels = c("Negative", "Positive")
  ),

  # ===== SCENARIO 3: Moderate Association (Good for post-hoc) =====
  # Tumor Grade (3 levels) vs Lymph Node Status (3 levels)
  TumorGrade = factor(
    sample(c("Well Diff", "Moderate Diff", "Poor Diff"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    levels = c("Well Diff", "Moderate Diff", "Poor Diff")
  ),

  LymphNodeStatus = factor(rep(NA, n), levels = c("N0", "N1", "N2")),

  # ===== SCENARIO 4: Variables with Small Expected Counts =====
  # (Will trigger Fisher's exact test in pairwise comparisons)
  RareMutation = factor(
    sample(c("Wild Type", "Variant A", "Variant B", "Variant C"), n, replace = TRUE,
           prob = c(0.70, 0.15, 0.10, 0.05)),
    levels = c("Wild Type", "Variant A", "Variant B", "Variant C")
  ),

  DrugSensitivity = factor(rep(NA, n), levels = c("Resistant", "Sensitive")),

  # ===== SCENARIO 5: Balanced 2x2 Table =====
  Gender = factor(
    sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("Male", "Female")
  ),

  SideEffect = factor(
    sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("No", "Yes")
  ),

  # ===== SCENARIO 6: Highly Unbalanced Categories =====
  RareDisease = factor(
    sample(c("Common", "Uncommon", "Rare", "Very Rare"), n, replace = TRUE,
           prob = c(0.65, 0.20, 0.10, 0.05)),
    levels = c("Common", "Uncommon", "Rare", "Very Rare")
  ),

  Complication = factor(rep(NA, n), levels = c("None", "Mild", "Severe")),

  # ===== Additional Variables for Testing =====
  AgeGroup = factor(
    sample(c("< 40", "40-60", "> 60"), n, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    levels = c("< 40", "40-60", "> 60")
  ),

  SmokingStatus = factor(
    sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    levels = c("Never", "Former", "Current")
  )
)

# Create known associations

# 1. Strong: TreatmentGroup -> ClinicalResponse
# Placebo: 20% response, Drug A: 40%, Drug B: 65%, Drug C: 80%
for (i in 1:n) {
  response_prob <- switch(as.character(chisqposttest_comprehensive$TreatmentGroup[i]),
                         "Placebo" = 0.20,
                         "Drug A" = 0.40,
                         "Drug B" = 0.65,
                         "Drug C" = 0.80)
  chisqposttest_comprehensive$ClinicalResponse[i] <-
    ifelse(runif(1) < response_prob, "Response", "No Response")
}
chisqposttest_comprehensive$ClinicalResponse <-
  factor(chisqposttest_comprehensive$ClinicalResponse, levels = c("No Response", "Response"))

# 2. Moderate: TumorGrade -> LymphNodeStatus
for (i in 1:n) {
  ln_probs <- switch(as.character(chisqposttest_comprehensive$TumorGrade[i]),
                    "Well Diff" = c(0.65, 0.25, 0.10),
                    "Moderate Diff" = c(0.40, 0.40, 0.20),
                    "Poor Diff" = c(0.20, 0.35, 0.45))
  chisqposttest_comprehensive$LymphNodeStatus[i] <-
    sample(c("N0", "N1", "N2"), 1, prob = ln_probs)
}
chisqposttest_comprehensive$LymphNodeStatus <-
  factor(chisqposttest_comprehensive$LymphNodeStatus, levels = c("N0", "N1", "N2"))

# 3. Small counts: RareMutation -> DrugSensitivity
for (i in 1:n) {
  sens_prob <- switch(as.character(chisqposttest_comprehensive$RareMutation[i]),
                     "Wild Type" = 0.40,
                     "Variant A" = 0.60,
                     "Variant B" = 0.75,
                     "Variant C" = 0.85)
  chisqposttest_comprehensive$DrugSensitivity[i] <-
    ifelse(runif(1) < sens_prob, "Sensitive", "Resistant")
}
chisqposttest_comprehensive$DrugSensitivity <-
  factor(chisqposttest_comprehensive$DrugSensitivity, levels = c("Resistant", "Sensitive"))

# 4. Unbalanced: RareDisease -> Complication
for (i in 1:n) {
  comp_probs <- switch(as.character(chisqposttest_comprehensive$RareDisease[i]),
                      "Common" = c(0.70, 0.20, 0.10),
                      "Uncommon" = c(0.50, 0.30, 0.20),
                      "Rare" = c(0.30, 0.40, 0.30),
                      "Very Rare" = c(0.20, 0.30, 0.50))
  chisqposttest_comprehensive$Complication[i] <-
    sample(c("None", "Mild", "Severe"), 1, prob = comp_probs)
}
chisqposttest_comprehensive$Complication <-
  factor(chisqposttest_comprehensive$Complication, levels = c("None", "Mild", "Severe"))

# Add missing data (5%)
missing_idx <- sample(1:n, floor(n * 0.05))
chisqposttest_comprehensive$TreatmentGroup[missing_idx[1:5]] <- NA
chisqposttest_comprehensive$TumorGrade[missing_idx[6:10]] <- NA
chisqposttest_comprehensive$Gender[missing_idx[11:15]] <- NA


# ============================================================================
# Dataset 2: Weighted/Summarized Data (For counts variable testing)
# ============================================================================

chisqposttest_weighted <- data.frame(
  PathologyType = rep(c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell"), each = 6),
  TreatmentArm = rep(c("Surgery Only", "Surgery + Chemo", "Surgery + Radio",
                      "Chemo Only", "Radio Only", "Best Supportive"), times = 4),
  Count = c(
    # Adenocarcinoma
    45, 62, 38, 28, 22, 15,
    # Squamous Cell
    38, 51, 44, 33, 29, 18,
    # Small Cell
    12, 28, 15, 35, 22, 8,
    # Large Cell
    18, 24, 20, 19, 16, 11
  )
)

chisqposttest_weighted$PathologyType <- factor(chisqposttest_weighted$PathologyType,
  levels = c("Adenocarcinoma", "Squamous Cell", "Small Cell", "Large Cell"))
chisqposttest_weighted$TreatmentArm <- factor(chisqposttest_weighted$TreatmentArm,
  levels = c("Surgery Only", "Surgery + Chemo", "Surgery + Radio",
            "Chemo Only", "Radio Only", "Best Supportive"))


# ============================================================================
# Dataset 3: Small Sample Size (Triggers Fisher's exact, low power)
# ============================================================================

chisqposttest_small <- data.frame(
  Drug = factor(rep(c("Placebo", "Low Dose", "High Dose"), each = 8),
               levels = c("Placebo", "Low Dose", "High Dose")),
  Toxicity = factor(c(
    # Placebo
    rep("None", 6), rep("Mild", 2),
    # Low Dose
    rep("None", 4), rep("Mild", 3), rep("Severe", 1),
    # High Dose
    rep("None", 2), rep("Mild", 4), rep("Severe", 2)
  ), levels = c("None", "Mild", "Severe"))
)


# ============================================================================
# Dataset 4: Edge Cases
# ============================================================================

# 4a: Perfectly balanced (no association)
chisqposttest_null <- data.frame(
  GroupA = factor(rep(c("Cat 1", "Cat 2", "Cat 3"), each = 60),
                 levels = c("Cat 1", "Cat 2", "Cat 3")),
  GroupB = factor(rep(c("Type X", "Type Y", "Type Z"), times = 60),
                 levels = c("Type X", "Type Y", "Type Z"))
)

# 4b: Perfect association (all in diagonal)
chisqposttest_perfect <- data.frame(
  Stage = factor(c(rep("Early", 30), rep("Intermediate", 30), rep("Advanced", 30)),
                levels = c("Early", "Intermediate", "Advanced")),
  Risk = factor(c(rep("Low", 30), rep("Medium", 30), rep("High", 30)),
               levels = c("Low", "Medium", "High"))
)


# ============================================================================
# Save all datasets as CSV files
# ============================================================================

# Main comprehensive dataset
write.csv(chisqposttest_comprehensive,
          "data/chisqposttest_comprehensive.csv",
          row.names = FALSE)

# Weighted data
write.csv(chisqposttest_weighted,
          "data/chisqposttest_weighted.csv",
          row.names = FALSE)

# Small sample
write.csv(chisqposttest_small,
          "data/chisqposttest_small.csv",
          row.names = FALSE)

# Null association
write.csv(chisqposttest_null,
          "data/chisqposttest_null.csv",
          row.names = FALSE)

# Perfect association
write.csv(chisqposttest_perfect,
          "data/chisqposttest_perfect.csv",
          row.names = FALSE)


# ============================================================================
# Create combined dataset with all scenarios
# ============================================================================

# Add scenario identifier to each dataset
chisqposttest_comprehensive$Dataset <- "Main"
chisqposttest_small$Dataset <- "Small"
chisqposttest_null$Dataset <- "Null"
chisqposttest_perfect$Dataset <- "Perfect"

# Save combined dataset for easy access
write.csv(chisqposttest_comprehensive,
          "data/chisqposttest_all_features.csv",
          row.names = FALSE)


# ============================================================================
# Print Summary
# ============================================================================

cat("\n=== Chi-Square Post-Hoc Test Data Generation Complete ===\n\n")

cat("Files created:\n")
cat("1. chisqposttest_comprehensive.csv (n=400)\n")
cat("   - Strong associations: TreatmentGroup -> ClinicalResponse\n")
cat("   - Moderate associations: TumorGrade -> LymphNodeStatus\n")
cat("   - Weak/null associations: DiseaseType -> Biomarker\n")
cat("   - Small expected counts: RareMutation -> DrugSensitivity\n")
cat("   - Unbalanced categories: RareDisease -> Complication\n")
cat("   - Missing data: ~5%\n\n")

cat("2. chisqposttest_weighted.csv (n=24 summarized rows)\n")
cat("   - Tests counts variable feature\n")
cat("   - PathologyType x TreatmentArm with frequency counts\n\n")

cat("3. chisqposttest_small.csv (n=24)\n")
cat("   - Small sample size\n")
cat("   - Will trigger Fisher's exact test warnings\n")
cat("   - Drug x Toxicity with sparse cells\n\n")

cat("4. chisqposttest_null.csv (n=180)\n")
cat("   - Perfectly balanced, no association\n")
cat("   - Tests non-significant omnibus test path\n\n")

cat("5. chisqposttest_perfect.csv (n=90)\n")
cat("   - Perfect association (diagonal pattern)\n")
cat("   - Tests extreme association scenario\n\n")

cat("6. chisqposttest_all_features.csv\n")
cat("   - Combined main dataset for comprehensive testing\n\n")

cat("=== Test Scenarios ===\n\n")

cat("Use these to test:\n")
cat("✓ Post-hoc methods: Bonferroni, Holm, FDR, None\n")
cat("✓ Significance levels: 0.05, 0.01, 0.1\n")
cat("✓ Exclude missing data: TRUE/FALSE\n")
cat("✓ Show residuals analysis\n")
cat("✓ Show educational panels\n")
cat("✓ Test selection: auto, chisquare, fisher\n")
cat("✓ Residuals cutoff: 2.0, 3.0\n")
cat("✓ Export results\n")
cat("✓ Clinical summary\n")
cat("✓ Example interpretations\n")
cat("✓ Weighted data (counts variable)\n\n")

# Display example crosstabs
cat("=== Example: Strong Association (should trigger post-hoc) ===\n")
print(table(chisqposttest_comprehensive$TreatmentGroup,
            chisqposttest_comprehensive$ClinicalResponse,
            useNA = "no"))
cat("\n")

cat("=== Example: Null Association (should NOT trigger post-hoc) ===\n")
print(table(chisqposttest_null$GroupA, chisqposttest_null$GroupB, useNA = "no"))
cat("\n")

cat("=== Example: Weighted Data ===\n")
cat("First 12 rows of weighted dataset:\n")
print(head(chisqposttest_weighted, 12))
cat("\n")

cat("All datasets saved to data/ folder!\n")
