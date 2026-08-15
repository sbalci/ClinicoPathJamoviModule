#!/usr/bin/env Rscript

# Test Data Generator for ihcheterogeneity Function
# Purpose: Create realistic IHC heterogeneity datasets for manual and automated testing
# Author: Generated for ClinicoPath Module
# Date: 2025-12-28

# Required packages
if (!requireNamespace("mvtnorm", quietly = TRUE)) {
  message("Installing mvtnorm package for multivariate normal data generation...")
  install.packages("mvtnorm")
}
library(mvtnorm)

# Set seed for reproducibility
set.seed(20251228)

# ============================================================================
# FUNCTION: Generate Realistic IHC Heterogeneity Data
# ============================================================================

generate_ihc_heterogeneity_data <- function(
    n_cases = 100,
    biomarker = "Ki67",
    mean_value = 25,
    heterogeneity_level = "moderate",
    include_spatial = TRUE,
    include_missing = TRUE,
    missing_rate = 0.05) {

  # Define heterogeneity parameters
  heterogeneity_params <- list(
    low = list(cv = 0.10, icc = 0.95, bias = 0.02),
    moderate = list(cv = 0.20, icc = 0.80, bias = 0.05),
    high = list(cv = 0.35, icc = 0.60, bias = 0.10),
    very_high = list(cv = 0.50, icc = 0.40, bias = 0.15)
  )

  params <- heterogeneity_params[[heterogeneity_level]]

  # Calculate standard deviation from CV
  sd_value <- mean_value * params$cv

  # Generate whole section (reference) measurements
  whole_section <- rnorm(n_cases, mean = mean_value, sd = sd_value)

  # Constrain to valid range (0-100 for percentages, 0-300 for H-scores)
  max_value <- ifelse(grepl("score|HScore", biomarker, ignore.case = TRUE), 300, 100)
  whole_section <- pmax(0, pmin(max_value, whole_section))

  # Generate correlated regional measurements with specified ICC
  # Using compound symmetry covariance structure
  n_regions <- 6  # biopsy1-4 + 2 additional

  # Calculate variance components for ICC
  var_between <- sd_value^2 * params$icc
  var_within <- sd_value^2 * (1 - params$icc)

  # Create covariance matrix
  sigma <- matrix(var_between, nrow = n_regions, ncol = n_regions)
  diag(sigma) <- var_between + var_within

  # Generate correlated measurements
  regional_data <- matrix(NA, nrow = n_cases, ncol = n_regions)

  for (i in 1:n_cases) {
    # Generate correlated deviations from reference
    deviations <- as.vector(rmvnorm(1, mean = rep(0, n_regions), sigma = sigma))

    # Add systematic bias to some regions
    bias_vector <- c(
      -params$bias * mean_value,  # biopsy1: slight underestimation
      params$bias * mean_value,   # biopsy2: slight overestimation
      0,                          # biopsy3: no bias
      params$bias * mean_value * 0.5,  # biopsy4: mild overestimation
      -params$bias * mean_value * 0.5, # additional1: mild underestimation
      0                           # additional2: no bias
    )

    regional_data[i, ] <- whole_section[i] + deviations + bias_vector
  }

  # Constrain regional measurements to valid range (preserve matrix structure)
  regional_data <- pmax(0, pmin(max_value, regional_data))
  regional_data <- matrix(regional_data, nrow = n_cases, ncol = n_regions)

  # Create spatial regions if requested
  if (include_spatial) {
    spatial_region <- sample(
      c("central", "peripheral", "invasive_front", "preinvasive"),
      n_cases,
      replace = TRUE,
      prob = c(0.4, 0.4, 0.15, 0.05)
    )
  } else {
    spatial_region <- rep(NA, n_cases)
  }

  # Ensure regional_data is a matrix
  regional_data <- as.matrix(regional_data)

  # Create data frame
  df <- data.frame(
    case_id = 1:n_cases,
    whole_section = round(whole_section, 2),
    biopsy1 = round(regional_data[, 1], 2),
    biopsy2 = round(regional_data[, 2], 2),
    biopsy3 = round(regional_data[, 3], 2),
    biopsy4 = round(regional_data[, 4], 2),
    additional_biopsy1 = round(regional_data[, 5], 2),
    additional_biopsy2 = round(regional_data[, 6], 2),
    spatial_region = as.factor(spatial_region),
    stringsAsFactors = FALSE
  )

  # Add biomarker-specific column names
  biomarker_clean <- gsub("[^A-Za-z0-9_]", "_", tolower(biomarker))
  colnames(df)[2:8] <- paste0(biomarker_clean, "_", c(
    "wholesection", "region1", "region2", "region3",
    "region4", "region5", "region6"
  ))

  # Introduce missing data if requested
  if (include_missing && missing_rate > 0) {
    n_missing <- round(n_cases * n_regions * missing_rate)
    if (n_missing > 0) {
      # Randomly select cells to set as NA (excluding case_id and spatial_region)
      missing_indices <- sample(1:(n_cases * n_regions), n_missing)
      measurement_cols <- 2:8

      for (idx in missing_indices) {
        row <- ((idx - 1) %% n_cases) + 1
        col <- measurement_cols[((idx - 1) %/% n_cases) + 1]
        df[row, col] <- NA
      }
    }
  }

  return(df)
}

# ============================================================================
# GENERATE TEST DATASETS
# ============================================================================

cat("Generating IHC heterogeneity test datasets...\n\n")

# Dataset 1: Ki67 with moderate heterogeneity (standard clinical scenario)
cat("1. Ki67 moderate heterogeneity (n=100)...\n")
ki67_moderate <- generate_ihc_heterogeneity_data(
  n_cases = 100,
  biomarker = "Ki67",
  mean_value = 25,
  heterogeneity_level = "moderate",
  include_spatial = TRUE,
  include_missing = TRUE,
  missing_rate = 0.05
)

# Dataset 2: ER H-score with low heterogeneity (excellent reproducibility)
cat("2. ER H-score low heterogeneity (n=80)...\n")
er_hscore_low <- generate_ihc_heterogeneity_data(
  n_cases = 80,
  biomarker = "ER_HScore",
  mean_value = 180,
  heterogeneity_level = "low",
  include_spatial = TRUE,
  include_missing = TRUE,
  missing_rate = 0.03
)

# Dataset 3: PDL1 with high heterogeneity (challenging biomarker)
cat("3. PDL1 high heterogeneity (n=60)...\n")
pdl1_high <- generate_ihc_heterogeneity_data(
  n_cases = 60,
  biomarker = "PDL1",
  mean_value = 15,
  heterogeneity_level = "high",
  include_spatial = TRUE,
  include_missing = TRUE,
  missing_rate = 0.08
)

# Dataset 4: Inter-regional comparison (no reference measurement)
cat("4. Inter-regional comparison dataset (n=50)...\n")
interregional <- generate_ihc_heterogeneity_data(
  n_cases = 50,
  biomarker = "HER2",
  mean_value = 35,
  heterogeneity_level = "moderate",
  include_spatial = TRUE,
  include_missing = FALSE
)
# Remove whole section measurement for inter-regional study
interregional$her2_wholesection <- NULL

# Dataset 5: Small sample with edge cases
cat("5. Small sample with edge cases (n=15)...\n")
small_sample <- generate_ihc_heterogeneity_data(
  n_cases = 15,
  biomarker = "p53",
  mean_value = 40,
  heterogeneity_level = "very_high",
  include_spatial = FALSE,
  include_missing = TRUE,
  missing_rate = 0.10
)

# Dataset 6: Minimal data (minimal viable dataset)
cat("6. Minimal viable dataset (n=20)...\n")
minimal <- data.frame(
  case_id = 1:20,
  ki67_region1 = rnorm(20, mean = 25, sd = 8),
  ki67_region2 = rnorm(20, mean = 27, sd = 9),
  stringsAsFactors = FALSE
)
minimal$ki67_region1 <- round(pmax(0, pmin(100, minimal$ki67_region1)), 2)
minimal$ki67_region2 <- round(pmax(0, pmin(100, minimal$ki67_region2)), 2)

# Dataset 7: Extreme outliers test
cat("7. Dataset with outliers (n=40)...\n")
outliers_data <- generate_ihc_heterogeneity_data(
  n_cases = 40,
  biomarker = "Ki67",
  mean_value = 30,
  heterogeneity_level = "moderate",
  include_spatial = TRUE,
  include_missing = FALSE
)
# Introduce outliers in 10% of cases
outlier_cases <- sample(1:40, 4)
outliers_data$ki67_region1[outlier_cases] <- c(95, 2, 98, 1)
outliers_data$ki67_region2[outlier_cases] <- c(92, 5, 96, 3)

# Dataset 8: Spatial compartment comparison
cat("8. Spatial compartment comparison (n=80)...\n")
spatial_comparison <- generate_ihc_heterogeneity_data(
  n_cases = 80,
  biomarker = "Ki67",
  mean_value = 28,
  heterogeneity_level = "moderate",
  include_spatial = TRUE,
  include_missing = FALSE
)

# Dataset 9: Comprehensive validation dataset
cat("9. Comprehensive validation dataset (n=150)...\n")
comprehensive <- generate_ihc_heterogeneity_data(
  n_cases = 150,
  biomarker = "Ki67",
  mean_value = 35,
  heterogeneity_level = "moderate",
  include_spatial = TRUE,
  include_missing = TRUE,
  missing_rate = 0.06
)
# Add tumor grade and additional clinical variables
comprehensive$tumor_grade <- sample(
  c("grade1", "grade2", "grade3"),
  150,
  replace = TRUE,
  prob = c(0.2, 0.5, 0.3)
)
comprehensive$tumor_size_cm <- round(rnorm(150, mean = 3.5, sd = 1.5), 1)
comprehensive$tumor_size_cm <- pmax(0.5, pmin(15, comprehensive$tumor_size_cm))

# ============================================================================
# SAVE DATASETS
# ============================================================================

output_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data"
cat("\nSaving datasets to:", output_dir, "\n\n")

# Main test dataset (backward compatible)
write.csv(ki67_moderate,
          file.path(output_dir, "ihc_heterogeneity.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity.csv (main test dataset)\n")

# Additional test datasets
write.csv(er_hscore_low,
          file.path(output_dir, "ihc_heterogeneity_er_low.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_er_low.csv\n")

write.csv(pdl1_high,
          file.path(output_dir, "ihc_heterogeneity_pdl1_high.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_pdl1_high.csv\n")

write.csv(interregional,
          file.path(output_dir, "ihc_heterogeneity_interregional.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_interregional.csv\n")

write.csv(small_sample,
          file.path(output_dir, "ihc_heterogeneity_small.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_small.csv\n")

write.csv(minimal,
          file.path(output_dir, "ihc_heterogeneity_minimal.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_minimal.csv\n")

write.csv(outliers_data,
          file.path(output_dir, "ihc_heterogeneity_outliers.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_outliers.csv\n")

write.csv(spatial_comparison,
          file.path(output_dir, "ihc_heterogeneity_spatial.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_spatial.csv\n")

write.csv(comprehensive,
          file.path(output_dir, "ihc_heterogeneity_comprehensive.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_comprehensive.csv\n")

# ============================================================================
# GENERATE DATASET SUMMARY
# ============================================================================

summary_data <- data.frame(
  dataset = c(
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity_er_low.csv",
    "ihc_heterogeneity_pdl1_high.csv",
    "ihc_heterogeneity_interregional.csv",
    "ihc_heterogeneity_small.csv",
    "ihc_heterogeneity_minimal.csv",
    "ihc_heterogeneity_outliers.csv",
    "ihc_heterogeneity_spatial.csv",
    "ihc_heterogeneity_comprehensive.csv"
  ),
  n_cases = c(100, 80, 60, 50, 15, 20, 40, 80, 150),
  biomarker = c("Ki67", "ER_HScore", "PDL1", "HER2", "p53", "Ki67", "Ki67", "Ki67", "Ki67"),
  heterogeneity = c("moderate", "low", "high", "moderate", "very_high", "moderate", "moderate", "moderate", "moderate"),
  has_reference = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
  has_spatial = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
  has_missing = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
  use_case = c(
    "Standard clinical scenario",
    "Excellent reproducibility",
    "Challenging biomarker",
    "Inter-regional comparison",
    "Small sample edge cases",
    "Minimal viable dataset",
    "Outlier detection",
    "Spatial compartment analysis",
    "Comprehensive validation"
  ),
  stringsAsFactors = FALSE
)

write.csv(summary_data,
          file.path(output_dir, "ihc_heterogeneity_datasets_summary.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_datasets_summary.csv\n")

# ============================================================================
# GENERATE TEST SCENARIOS GUIDE
# ============================================================================

test_scenarios <- data.frame(
  scenario_id = 1:20,
  test_name = c(
    "Basic reference-based analysis",
    "Inter-regional comparison",
    "High heterogeneity detection",
    "Low heterogeneity validation",
    "Missing data handling",
    "Small sample warnings",
    "Outlier detection",
    "Spatial compartment comparison",
    "Variance component analysis",
    "Power analysis",
    "Plain language summary",
    "Statistical glossary",
    "CV threshold sensitivity",
    "ICC interpretation",
    "Comprehensive analysis mode",
    "Reproducibility focus",
    "Bias analysis focus",
    "Variability focus",
    "Different sampling strategies",
    "Clinical recommendations"
  ),
  dataset_to_use = c(
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity_interregional.csv",
    "ihc_heterogeneity_pdl1_high.csv",
    "ihc_heterogeneity_er_low.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity_small.csv",
    "ihc_heterogeneity_outliers.csv",
    "ihc_heterogeneity_spatial.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity_er_low.csv",
    "ihc_heterogeneity_comprehensive.csv",
    "ihc_heterogeneity_er_low.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity_pdl1_high.csv",
    "ihc_heterogeneity.csv",
    "ihc_heterogeneity_comprehensive.csv"
  ),
  expected_result = c(
    "ICC ~0.80, CV ~20%",
    "No bias table, inter-regional ICC only",
    "ICC <0.60, CV >35%, high variability warnings",
    "ICC >0.95, CV <10%, excellent agreement",
    "Analysis completes with reduced N",
    "Small sample size warning in interpretation",
    "Outlier warnings, high CV cases flagged",
    "Compartment comparison tests, spatial plot",
    "Between/within variance decomposition",
    "Sample size recommendations for studies",
    "Natural language interpretation",
    "Definitions of ICC, CV, correlation",
    "Interpretation changes with threshold",
    "Excellent reliability interpretation",
    "All tables and plots generated",
    "Focus on ICC and correlation metrics",
    "Bias table with systematic differences",
    "Variance components and CV emphasis",
    "Strategy-specific recommendations",
    "Clinical action recommendations"
  ),
  stringsAsFactors = FALSE
)

write.csv(test_scenarios,
          file.path(output_dir, "ihc_heterogeneity_test_scenarios.csv"),
          row.names = FALSE)
cat("✓ Saved: ihc_heterogeneity_test_scenarios.csv\n")

# ============================================================================
# PRINT SUMMARY
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("  IHC HETEROGENEITY TEST DATA GENERATION COMPLETE\n")
cat("================================================================================\n\n")
cat("Generated 9 datasets with varying characteristics:\n")
cat("  • Ki67 moderate heterogeneity (standard)\n")
cat("  • ER H-score low heterogeneity (excellent reproducibility)\n")
cat("  • PDL1 high heterogeneity (challenging biomarker)\n")
cat("  • Inter-regional comparison (no reference)\n")
cat("  • Small sample with edge cases\n")
cat("  • Minimal viable dataset\n")
cat("  • Dataset with outliers\n")
cat("  • Spatial compartment comparison\n")
cat("  • Comprehensive validation dataset\n\n")
cat("Also created:\n")
cat("  • Dataset summary guide\n")
cat("  • Test scenarios guide (20 scenarios)\n\n")
cat("All files saved to: data/\n")
cat("================================================================================\n\n")

cat("Next steps:\n")
cat("1. Review generated datasets\n")
cat("2. Run ihcheterogeneity function with different datasets\n")
cat("3. Use test scenarios guide for comprehensive testing\n")
cat("4. Update testthat tests to use new datasets\n\n")
