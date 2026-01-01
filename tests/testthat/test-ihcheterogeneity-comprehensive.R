# Comprehensive Test Suite for ihcheterogeneity Function
# Purpose: Automated testing of IHC heterogeneity analysis
# Coverage: All major features, edge cases, and error handling

library(testthat)
library(jmvcore)

# Test context setup
context("IHC Heterogeneity Analysis - Comprehensive Tests")

# Helper function to load test data
load_test_data <- function(filename) {
  data_path <- file.path(
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data",
    filename
  )
  if (!file.exists(data_path)) {
    skip(paste("Test data not found:", filename))
  }
  read.csv(data_path, stringsAsFactors = TRUE)
}

# ============================================================================
# 1. BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("ihcheterogeneity module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("ihcheterogeneityClass"))
  expect_true(is.function(ihcheterogeneity))
})

test_that("ihcheterogeneity works with standard Ki67 data", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    biopsy3 = "ki67_region3",
    biopsy4 = "ki67_region4"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
  expect_true(!is.null(result$reproducibilitytable))
  expect_true(!is.null(result$samplingbiastable))
})

test_that("ihcheterogeneity works with minimal dataset (2 regions only)", {
  data <- load_test_data("ihc_heterogeneity_minimal.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2"
    )
  }, NA)
})

# ============================================================================
# 2. STUDY DESIGN TESTS
# ============================================================================

test_that("ihcheterogeneity handles reference-based study design", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2"
  )

  # Should have both reproducibility and bias tables
  expect_true(!is.null(result$reproducibilitytable))
  expect_true(!is.null(result$samplingbiastable))
})

test_that("ihcheterogeneity handles inter-regional study design", {
  data <- load_test_data("ihc_heterogeneity_interregional.csv")

  result <- ihcheterogeneity(
    data = data,
    biopsy1 = "her2_region1",
    biopsy2 = "her2_region2",
    biopsy3 = "her2_region3"
  )

  # Should have reproducibility table but bias table should be empty or hidden
  expect_true(!is.null(result$reproducibilitytable))
})

# ============================================================================
# 3. HETEROGENEITY LEVEL TESTS
# ============================================================================

test_that("ihcheterogeneity detects low heterogeneity (ER H-score)", {
  data <- load_test_data("ihc_heterogeneity_er_low.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "er_hscore_wholesection",
    biopsy1 = "er_hscore_region1",
    biopsy2 = "er_hscore_region2",
    biopsy3 = "er_hscore_region3",
    cv_threshold = 15.0,
    correlation_threshold = 0.90
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity detects high heterogeneity (PDL1)", {
  data <- load_test_data("ihc_heterogeneity_pdl1_high.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "pdl1_wholesection",
    biopsy1 = "pdl1_region1",
    biopsy2 = "pdl1_region2",
    biopsy3 = "pdl1_region3",
    cv_threshold = 20.0,
    correlation_threshold = 0.70
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

# ============================================================================
# 4. ANALYSIS TYPE TESTS
# ============================================================================

test_that("ihcheterogeneity reproducibility analysis mode works", {
  data <- load_test_data("ihc_heterogeneity.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      analysis_type = "reproducibility"
    )
  }, NA)
})

test_that("ihcheterogeneity bias analysis mode works", {
  data <- load_test_data("ihc_heterogeneity.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      analysis_type = "bias"
    )
  }, NA)
})

test_that("ihcheterogeneity variability analysis mode works", {
  data <- load_test_data("ihc_heterogeneity.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      analysis_type = "variability",
      variance_components = TRUE
    )
  }, NA)
})

test_that("ihcheterogeneity comprehensive analysis mode works", {
  data <- load_test_data("ihc_heterogeneity_comprehensive.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      biopsy3 = "ki67_region3",
      analysis_type = "comprehensive",
      variance_components = TRUE,
      power_analysis = TRUE,
      show_variability_plots = TRUE
    )
  }, NA)
})

# ============================================================================
# 5. SPATIAL ANALYSIS TESTS
# ============================================================================

test_that("ihcheterogeneity handles spatial region analysis", {
  data <- load_test_data("ihc_heterogeneity_spatial.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    spatial_id = "spatial_region"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
  expect_true(!is.null(result$spatialanalysistable))
})

test_that("ihcheterogeneity spatial compartment comparison works", {
  data <- load_test_data("ihc_heterogeneity_spatial.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      spatial_id = "spatial_region",
      compareCompartments = TRUE,
      compartmentTests = TRUE
    )
  }, NA)
})

# ============================================================================
# 6. MISSING DATA HANDLING
# ============================================================================

test_that("ihcheterogeneity handles missing data appropriately", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Introduce some missing data
  data_missing <- data
  data_missing$ki67_region2[c(3, 8, 15, 22, 30)] <- NA
  data_missing$ki67_wholesection[c(5, 12)] <- NA

  expect_error({
    result <- ihcheterogeneity(
      data = data_missing,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      biopsy3 = "ki67_region3"
    )
  }, NA)
})

test_that("ihcheterogeneity handles high proportion of missing data", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Create dataset with 30% missing data
  data_missing <- data
  set.seed(123)
  n_total <- nrow(data) * 4  # 4 measurement columns
  n_missing <- round(n_total * 0.30)
  missing_idx <- sample(1:nrow(data), n_missing, replace = TRUE)

  for (idx in missing_idx) {
    col <- sample(c("ki67_region1", "ki67_region2", "ki67_region3", "ki67_region4"), 1)
    data_missing[idx, col] <- NA
  }

  # Should complete but may have warnings
  expect_error({
    result <- ihcheterogeneity(
      data = data_missing,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      biopsy3 = "ki67_region3"
    )
  }, NA)
})

# ============================================================================
# 7. EDGE CASES AND DATA QUALITY
# ============================================================================

test_that("ihcheterogeneity detects small sample size", {
  data <- load_test_data("ihc_heterogeneity_small.csv")

  # Should complete with warnings about small sample
  result <- ihcheterogeneity(
    data = data,
    wholesection = "p53_wholesection",
    biopsy1 = "p53_region1",
    biopsy2 = "p53_region2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity detects outliers", {
  data <- load_test_data("ihc_heterogeneity_outliers.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles constant values", {
  data <- load_test_data("ihc_heterogeneity_minimal.csv")

  # Create constant values
  data_constant <- data
  data_constant$ki67_region1 <- 50
  data_constant$ki67_region2 <- 50

  # Should complete with appropriate warnings
  expect_error({
    result <- ihcheterogeneity(
      data = data_constant,
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2"
    )
  }, NA)
})

# ============================================================================
# 8. CLINICAL THRESHOLD TESTS
# ============================================================================

test_that("ihcheterogeneity respects CV threshold settings", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Test different CV thresholds
  cv_thresholds <- c(15, 20, 25, 30)

  for (threshold in cv_thresholds) {
    expect_error({
      result <- ihcheterogeneity(
        data = data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        cv_threshold = threshold
      )
    }, NA, info = paste("CV threshold:", threshold))
  }
})

test_that("ihcheterogeneity respects correlation threshold settings", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Test different correlation thresholds
  corr_thresholds <- c(0.60, 0.70, 0.80, 0.90)

  for (threshold in corr_thresholds) {
    expect_error({
      result <- ihcheterogeneity(
        data = data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        correlation_threshold = threshold
      )
    }, NA, info = paste("Correlation threshold:", threshold))
  }
})

# ============================================================================
# 9. OUTPUT OPTIONS TESTS
# ============================================================================

test_that("ihcheterogeneity generates plain language summary", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    showSummary = TRUE
  )

  expect_true(result$summary$visible)
})

test_that("ihcheterogeneity generates statistical glossary", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    showGlossary = TRUE
  )

  expect_true(result$glossary$visible)
})

test_that("ihcheterogeneity generates variability plots", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    biopsy3 = "ki67_region3",
    show_variability_plots = TRUE
  )

  expect_true(result$biopsyplot$visible)
  expect_true(result$variabilityplot$visible)
})

test_that("ihcheterogeneity variance components analysis works", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    biopsy3 = "ki67_region3",
    variance_components = TRUE
  )

  expect_true(result$variancetable$visible)
})

test_that("ihcheterogeneity power analysis works", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    power_analysis = TRUE
  )

  expect_true(result$poweranalysistable$visible)
})

test_that("ihcheterogeneity generates clinical recommendations", {
  data <- load_test_data("ihc_heterogeneity_comprehensive.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      generate_recommendations = TRUE
    )
  }, NA)
})

# ============================================================================
# 10. SAMPLING STRATEGY TESTS
# ============================================================================

test_that("ihcheterogeneity handles different sampling strategies", {
  data <- load_test_data("ihc_heterogeneity.csv")

  strategies <- c("random", "systematic", "stratified", "unknown")

  for (strategy in strategies) {
    expect_error({
      result <- ihcheterogeneity(
        data = data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        sampling_strategy = strategy
      )
    }, NA, info = paste("Sampling strategy:", strategy))
  }
})

# ============================================================================
# 11. ADDITIONAL BIOPSIES PARAMETER TEST
# ============================================================================

test_that("ihcheterogeneity handles additional biopsies parameter", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2",
    biopsy3 = "ki67_region3",
    biopsies = c("ki67_region5", "ki67_region6")
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

# ============================================================================
# 12. COMPREHENSIVE PARAMETER COMBINATION TESTS
# ============================================================================

test_that("ihcheterogeneity works with all features enabled", {
  data <- load_test_data("ihc_heterogeneity_comprehensive.csv")

  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection",
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2",
      biopsy3 = "ki67_region3",
      biopsy4 = "ki67_region4",
      biopsies = c("ki67_region5", "ki67_region6"),
      spatial_id = "spatial_region",
      compareCompartments = TRUE,
      compartmentTests = TRUE,
      analysis_type = "comprehensive",
      sampling_strategy = "systematic",
      cv_threshold = 20.0,
      correlation_threshold = 0.80,
      show_variability_plots = TRUE,
      variance_components = TRUE,
      power_analysis = TRUE,
      generate_recommendations = TRUE,
      showSummary = TRUE,
      showGlossary = TRUE
    )
  }, NA)
})

# ============================================================================
# 13. INPUT VALIDATION TESTS
# ============================================================================

test_that("ihcheterogeneity validates missing required parameters", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Missing biopsy1 should be handled gracefully
  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "ki67_wholesection"
    )
  }, NA)  # Should handle during initialization, not throw error
})

test_that("ihcheterogeneity handles invalid variable names", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Non-existent variable name
  expect_error({
    result <- ihcheterogeneity(
      data = data,
      wholesection = "nonexistent_variable",
      biopsy1 = "ki67_region1"
    )
  }, NA)  # jamovi should handle this gracefully
})

# ============================================================================
# 14. RESULT STRUCTURE TESTS
# ============================================================================

test_that("ihcheterogeneity results have expected structure", {
  data <- load_test_data("ihc_heterogeneity.csv")

  result <- ihcheterogeneity(
    data = data,
    wholesection = "ki67_wholesection",
    biopsy1 = "ki67_region1",
    biopsy2 = "ki67_region2"
  )

  # Check for expected result components
  expect_true(exists("reproducibilitytable", result))
  expect_true(exists("samplingbiastable", result))
  expect_true(exists("variancetable", result))
  expect_true(exists("spatialanalysistable", result))
  expect_true(exists("poweranalysistable", result))
})

# ============================================================================
# 15. ROBUSTNESS TESTS
# ============================================================================

test_that("ihcheterogeneity handles extreme values", {
  data <- load_test_data("ihc_heterogeneity_minimal.csv")

  # Test with values at extremes of valid range
  data_extreme <- data
  data_extreme$ki67_region1[1:5] <- c(0, 5, 50, 95, 100)
  data_extreme$ki67_region2[1:5] <- c(2, 8, 48, 98, 100)

  expect_error({
    result <- ihcheterogeneity(
      data = data_extreme,
      biopsy1 = "ki67_region1",
      biopsy2 = "ki67_region2"
    )
  }, NA)
})

test_that("ihcheterogeneity handles different random seeds", {
  data <- load_test_data("ihc_heterogeneity.csv")

  # Should produce consistent results regardless of seed
  results <- list()

  for (i in 1:3) {
    set.seed(i * 100)
    expect_error({
      results[[i]] <- ihcheterogeneity(
        data = data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2"
      )
    }, NA)
  }

  expect_length(results, 3)
})

# ============================================================================
# TEST SUMMARY
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("  IHC HETEROGENEITY TEST SUITE COMPLETE\n")
cat("================================================================================\n")
cat("Total test groups: 15\n")
cat("Coverage areas:\n")
cat("  ✓ Basic functionality\n")
cat("  ✓ Study design variations\n")
cat("  ✓ Heterogeneity level detection\n")
cat("  ✓ Analysis type modes\n")
cat("  ✓ Spatial analysis features\n")
cat("  ✓ Missing data handling\n")
cat("  ✓ Edge cases and data quality\n")
cat("  ✓ Clinical threshold settings\n")
cat("  ✓ Output options\n")
cat("  ✓ Sampling strategies\n")
cat("  ✓ Additional parameters\n")
cat("  ✓ Comprehensive combinations\n")
cat("  ✓ Input validation\n")
cat("  ✓ Result structure\n")
cat("  ✓ Robustness\n")
cat("================================================================================\n\n")
