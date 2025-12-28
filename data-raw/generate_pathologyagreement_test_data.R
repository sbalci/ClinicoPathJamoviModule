# =============================================================================
# Pathology Agreement Test Data Generator
# =============================================================================
#
# Purpose: Generate comprehensive test data for pathologyagreement analysis
#
# Dataset Design:
# - Multiple scenarios covering all agreement levels (perfect to poor)
# - Realistic digital pathology contexts (Ki67, ER, PR, HER2 measurements)
# - Multi-platform comparisons (HALO, Aiforia, ImageJ, Manual pathologist)
# - Systematic and proportional bias scenarios
# - Multi-method analysis (3+ platforms)
# - Edge cases: small samples, missing data, biomarker range violations
#
# Author: ClinicoPath Development Team
# Date: 2024-12-28
# =============================================================================

library(dplyr)

set.seed(123)  # For reproducibility

# =============================================================================
# SCENARIO 1: PERFECT AGREEMENT (ICC ~1.0)
# =============================================================================
# Use case: Same measurements repeated (test-retest reliability)

n_perfect <- 50

perfect_agreement <- data.frame(
    SampleID = sprintf("PERFECT-%03d", 1:n_perfect),
    Scenario = "Perfect Agreement",

    # Ki67 measurements - virtually identical
    Ki67_HALO = round(runif(n_perfect, 5, 95), 1),
    Ki67_Aiforia = NA  # Will fill with tiny noise
)

# Add tiny measurement noise (<0.5%)
perfect_agreement$Ki67_Aiforia <- perfect_agreement$Ki67_HALO +
    rnorm(n_perfect, mean = 0, sd = 0.3)
perfect_agreement$Ki67_Aiforia <- pmax(0, pmin(100,
    round(perfect_agreement$Ki67_Aiforia, 1)))

# =============================================================================
# SCENARIO 2: EXCELLENT AGREEMENT (ICC 0.90-0.99)
# =============================================================================
# Use case: HALO vs Aiforia digital pathology platforms

n_excellent <- 80

# Generate base Ki67 values
base_ki67 <- round(c(
    rnorm(n_excellent * 0.3, mean = 10, sd = 5),    # Low Ki67 (Luminal A)
    rnorm(n_excellent * 0.4, mean = 35, sd = 12),   # Moderate Ki67 (Luminal B)
    rnorm(n_excellent * 0.3, mean = 65, sd = 15)    # High Ki67 (Triple negative)
), 1)

excellent_agreement <- data.frame(
    SampleID = sprintf("EXCELLENT-%03d", 1:n_excellent),
    Scenario = "Excellent Agreement",

    # HALO platform (reference)
    Ki67_HALO = pmax(0, pmin(100, base_ki67)),

    # Aiforia platform (excellent correlation, minimal bias)
    Ki67_Aiforia = pmax(0, pmin(100,
        base_ki67 + rnorm(n_excellent, mean = 0.5, sd = 2.5)))
)

excellent_agreement$Ki67_HALO <- round(excellent_agreement$Ki67_HALO, 1)
excellent_agreement$Ki67_Aiforia <- round(excellent_agreement$Ki67_Aiforia, 1)

# =============================================================================
# SCENARIO 3: GOOD AGREEMENT (ICC 0.75-0.89)
# =============================================================================
# Use case: Digital pathology vs manual pathologist scoring

n_good <- 60

base_values <- c(
    rnorm(n_good * 0.4, mean = 15, sd = 8),
    rnorm(n_good * 0.3, mean = 40, sd = 12),
    rnorm(n_good * 0.3, mean = 70, sd = 15)
)

good_agreement <- data.frame(
    SampleID = sprintf("GOOD-%03d", 1:n_good),
    Scenario = "Good Agreement",

    # HALO automated scoring
    Ki67_HALO = pmax(0, pmin(100, round(base_values, 1))),

    # Manual pathologist scoring (more variability)
    Ki67_Manual = pmax(0, pmin(100,
        round(base_values + rnorm(n_good, mean = 1, sd = 5), 1)))
)

# =============================================================================
# SCENARIO 4: MODERATE AGREEMENT (ICC 0.50-0.74)
# =============================================================================
# Use case: Two different staining protocols or antibodies

n_moderate <- 50

base_moderate <- runif(n_moderate, 5, 90)

moderate_agreement <- data.frame(
    SampleID = sprintf("MODERATE-%03d", 1:n_moderate),
    Scenario = "Moderate Agreement",

    # Protocol A (MIB-1 antibody)
    Ki67_ProtocolA = pmax(0, pmin(100,
        round(base_moderate, 1))),

    # Protocol B (Ki-67 antibody, different clone)
    Ki67_ProtocolB = pmax(0, pmin(100,
        round(base_moderate * 0.85 + rnorm(n_moderate, mean = 3, sd = 8), 1)))
)

# =============================================================================
# SCENARIO 5: POOR AGREEMENT (ICC <0.50)
# =============================================================================
# Use case: Incompatible methods or severe technical issues

n_poor <- 40

poor_agreement <- data.frame(
    SampleID = sprintf("POOR-%03d", 1:n_poor),
    Scenario = "Poor Agreement",

    # Well-performing platform
    Ki67_Platform1 = round(runif(n_poor, 10, 80), 1),

    # Poorly calibrated platform (random relationship)
    Ki67_Platform2 = round(runif(n_poor, 10, 80), 1)
)

# =============================================================================
# SCENARIO 6: SYSTEMATIC BIAS (Good correlation, constant offset)
# =============================================================================
# Use case: Platforms differ by constant amount but rank similarly

n_bias <- 60

base_bias <- c(
    rnorm(n_bias * 0.3, mean = 12, sd = 6),
    rnorm(n_bias * 0.4, mean = 35, sd = 10),
    rnorm(n_bias * 0.3, mean = 65, sd = 12)
)

systematic_bias <- data.frame(
    SampleID = sprintf("SYSTEMATIC-%03d", 1:n_bias),
    Scenario = "Systematic Bias",

    # Platform A
    Ki67_PlatformA = pmax(0, pmin(100, round(base_bias, 1))),

    # Platform B (constant +8% bias)
    Ki67_PlatformB = pmax(0, pmin(100,
        round(base_bias + 8 + rnorm(n_bias, 0, 2), 1)))
)

# =============================================================================
# SCENARIO 7: PROPORTIONAL BIAS (Bias increases with magnitude)
# =============================================================================
# Use case: Platforms diverge at higher values

n_prop <- 50

base_prop <- seq(5, 95, length.out = n_prop)

proportional_bias <- data.frame(
    SampleID = sprintf("PROPORTIONAL-%03d", 1:n_prop),
    Scenario = "Proportional Bias",

    # Method 1
    Method1 = round(base_prop + rnorm(n_prop, 0, 3), 1),

    # Method 2 (bias = 0.15 * value, increases proportionally)
    Method2 = round(base_prop * 1.15 + rnorm(n_prop, 0, 3), 1)
)

proportional_bias$Method1 <- pmax(0, pmin(100, proportional_bias$Method1))
proportional_bias$Method2 <- pmax(0, pmin(100, proportional_bias$Method2))

# =============================================================================
# SCENARIO 8: MULTI-METHOD ANALYSIS (4 platforms)
# =============================================================================
# Use case: Compare HALO, Aiforia, ImageJ, and Manual scoring

n_multi <- 100

# Generate base truth
base_multi <- c(
    rnorm(n_multi * 0.25, mean = 8, sd = 4),
    rnorm(n_multi * 0.35, mean = 30, sd = 10),
    rnorm(n_multi * 0.25, mean = 55, sd = 12),
    rnorm(n_multi * 0.15, mean = 80, sd = 8)
)

multimethod_data <- data.frame(
    SampleID = sprintf("MULTI-%03d", 1:n_multi),
    Scenario = "Multi-Method",

    # HALO (reference platform, most precise)
    Ki67_HALO = pmax(0, pmin(100,
        round(base_multi + rnorm(n_multi, 0, 2), 1))),

    # Aiforia (excellent agreement with HALO)
    Ki67_Aiforia = pmax(0, pmin(100,
        round(base_multi + rnorm(n_multi, 0.5, 2.5), 1))),

    # ImageJ (good agreement, slightly more variable)
    Ki67_ImageJ = pmax(0, pmin(100,
        round(base_multi + rnorm(n_multi, 1, 4), 1))),

    # Manual scoring (good correlation, more subjective)
    Ki67_Manual = pmax(0, pmin(100,
        round(base_multi + rnorm(n_multi, 2, 5), 1)))
)

# =============================================================================
# SCENARIO 9: SMALL SAMPLE (n<10) - Triggers warnings
# =============================================================================

n_small <- 8

small_sample <- data.frame(
    SampleID = sprintf("SMALL-%02d", 1:n_small),
    Scenario = "Small Sample",

    Method1 = round(c(5, 12, 25, 38, 45, 62, 78, 88), 1),
    Method2 = round(c(6, 13, 27, 40, 43, 60, 75, 85), 1)
)

# =============================================================================
# SCENARIO 10: MISSING DATA
# =============================================================================

n_missing <- 50

missing_data <- data.frame(
    SampleID = sprintf("MISSING-%03d", 1:n_missing),
    Scenario = "Missing Data",

    HALO_Score = round(runif(n_missing, 5, 90), 1),
    Aiforia_Score = round(runif(n_missing, 5, 90), 1),
    ImageJ_Score = round(runif(n_missing, 5, 90), 1)
)

# Introduce realistic missing patterns (~20% missing)
set.seed(456)
missing_data$HALO_Score[sample(1:n_missing, 8)] <- NA
missing_data$Aiforia_Score[sample(1:n_missing, 12)] <- NA
missing_data$ImageJ_Score[sample(1:n_missing, 10)] <- NA

# =============================================================================
# SCENARIO 11: BIOMARKER RANGE VALIDATION
# =============================================================================
# Test biomarker_platforms preset with some out-of-range values

n_range <- 40

biomarker_range <- data.frame(
    SampleID = sprintf("RANGE-%03d", 1:n_range),
    Scenario = "Biomarker Range",

    ER_Percent_HALO = round(c(
        runif(n_range * 0.9, 0, 100),    # Normal range
        runif(n_range * 0.1, 100, 120)   # Out of range (should trigger warning)
    ), 1),

    ER_Percent_Manual = round(c(
        runif(n_range * 0.9, 0, 100),
        runif(n_range * 0.1, 100, 115)
    ), 1)
)

# =============================================================================
# SCENARIO 12: AI vs PATHOLOGIST
# =============================================================================
# Real-world use case: Validating AI algorithm against pathologist

n_ai <- 70

base_ai <- c(
    rnorm(n_ai * 0.3, mean = 10, sd = 5),
    rnorm(n_ai * 0.4, mean = 35, sd = 12),
    rnorm(n_ai * 0.3, mean = 70, sd = 15)
)

ai_validation <- data.frame(
    SampleID = sprintf("AI-%03d", 1:n_ai),
    Scenario = "AI Validation",
    TumorType = sample(c("Luminal_A", "Luminal_B", "Triple_Negative"),
                       n_ai, replace = TRUE),

    # AI algorithm (very consistent)
    Ki67_AI = pmax(0, pmin(100,
        round(base_ai + rnorm(n_ai, 0, 2), 1))),

    # Expert pathologist (more variable, slight positive bias)
    Ki67_Pathologist = pmax(0, pmin(100,
        round(base_ai + 2 + rnorm(n_ai, 0, 4), 1)))
)

# =============================================================================
# SCENARIO 13: MULTISITE VALIDATION
# =============================================================================
# Use case: Same platform across different institutions

n_sites <- 90

base_site <- runif(n_sites, 5, 90)

multisite_data <- data.frame(
    SampleID = sprintf("SITE-%03d", 1:n_sites),
    Scenario = "Multisite Validation",
    Institution = sample(c("Site_A", "Site_B", "Site_C"),
                        n_sites, replace = TRUE),

    # Site A (reference site)
    Ki67_SiteA = pmax(0, pmin(100,
        round(base_site + rnorm(n_sites, 0, 3), 1))),

    # Site B (excellent agreement)
    Ki67_SiteB = pmax(0, pmin(100,
        round(base_site + rnorm(n_sites, 1, 3.5), 1)))
)

# =============================================================================
# COMBINE ALL SCENARIOS
# =============================================================================

# Dataset 1: Primary test dataset (most common scenarios)
pathology_agreement_main <- bind_rows(
    excellent_agreement,
    good_agreement,
    moderate_agreement,
    systematic_bias
)

# Dataset 2: Edge cases and special scenarios
pathology_agreement_edge <- bind_rows(
    perfect_agreement,
    poor_agreement,
    proportional_bias,
    small_sample,
    biomarker_range
)

# Dataset 3: Multi-method analysis
pathology_agreement_multimethod <- multimethod_data

# Dataset 4: Clinical presets
pathology_agreement_ai <- ai_validation
pathology_agreement_multisite <- multisite_data

# Dataset 5: Missing data
pathology_agreement_missing <- missing_data

# =============================================================================
# SAVE DATASETS
# =============================================================================

data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
}

# Main test dataset
write.csv(pathology_agreement_main,
          file.path(data_dir, "pathology_agreement_main.csv"),
          row.names = FALSE)
cat("✅ Main dataset saved: pathology_agreement_main.csv\n")

# Edge cases
write.csv(pathology_agreement_edge,
          file.path(data_dir, "pathology_agreement_edge.csv"),
          row.names = FALSE)
cat("✅ Edge cases saved: pathology_agreement_edge.csv\n")

# Multi-method
write.csv(pathology_agreement_multimethod,
          file.path(data_dir, "pathology_agreement_multimethod.csv"),
          row.names = FALSE)
cat("✅ Multi-method dataset saved: pathology_agreement_multimethod.csv\n")

# AI validation
write.csv(pathology_agreement_ai,
          file.path(data_dir, "pathology_agreement_ai.csv"),
          row.names = FALSE)
cat("✅ AI validation dataset saved: pathology_agreement_ai.csv\n")

# Multisite
write.csv(pathology_agreement_multisite,
          file.path(data_dir, "pathology_agreement_multisite.csv"),
          row.names = FALSE)
cat("✅ Multisite dataset saved: pathology_agreement_multisite.csv\n")

# Missing data
write.csv(pathology_agreement_missing,
          file.path(data_dir, "pathology_agreement_missing.csv"),
          row.names = FALSE)
cat("✅ Missing data dataset saved: pathology_agreement_missing.csv\n")

# =============================================================================
# PRINT SUMMARY STATISTICS
# =============================================================================

cat("\n=== DATASET SUMMARY ===\n\n")

print_agreement_stats <- function(df, name, method1, method2) {
    complete_cases <- complete.cases(df[[method1]], df[[method2]])
    m1 <- df[[method1]][complete_cases]
    m2 <- df[[method2]][complete_cases]

    cat(sprintf("--- %s ---\n", name))
    cat(sprintf("N: %d complete pairs\n", length(m1)))
    cat(sprintf("Correlation: %.3f\n", cor(m1, m2, method = "spearman")))
    cat(sprintf("Mean difference (bias): %.2f\n", mean(m1 - m2)))
    cat(sprintf("SD of differences: %.2f\n", sd(m1 - m2)))

    if (requireNamespace('psych', quietly = TRUE)) {
        icc_data <- data.frame(M1 = m1, M2 = m2)
        icc_result <- psych::ICC(icc_data)
        cat(sprintf("ICC(3,1): %.3f\n", icc_result$results$ICC[6]))
    }
    cat("\n")
}

# Print stats for each scenario
print_agreement_stats(excellent_agreement, "Excellent Agreement",
                     "Ki67_HALO", "Ki67_Aiforia")
print_agreement_stats(good_agreement, "Good Agreement",
                     "Ki67_HALO", "Ki67_Manual")
print_agreement_stats(moderate_agreement, "Moderate Agreement",
                     "Ki67_ProtocolA", "Ki67_ProtocolB")
print_agreement_stats(systematic_bias, "Systematic Bias",
                     "Ki67_PlatformA", "Ki67_PlatformB")
print_agreement_stats(proportional_bias, "Proportional Bias",
                     "Method1", "Method2")

cat("=== DATA GENERATION COMPLETE ===\n")
