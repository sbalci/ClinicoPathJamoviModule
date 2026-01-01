context("Time-Dependent ROC Analysis")

library(testthat)
library(timeROC)

# Load test datasets directly from files
cancer_data_path <- file.path("..", "..", "data", "timeroc_cancer_biomarker.rda")
cv_data_path <- file.path("..", "..", "data", "timeroc_cardiovascular_risk.rda")
multi_data_path <- file.path("..", "..", "data", "timeroc_multi_biomarker.rda")
edge_data_path <- file.path("..", "..", "data", "timeroc_edge_cases.rda")

if (file.exists(cancer_data_path)) load(cancer_data_path)
if (file.exists(cv_data_path)) load(cv_data_path)
if (file.exists(multi_data_path)) load(multi_data_path) 
if (file.exists(edge_data_path)) load(edge_data_path)

# Source the timeroc function directly
source("../../R/timeroc.b.R")
source("../../R/timeroc.h.R")

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("timeroc basic functionality works with cancer biomarker data", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test basic analysis
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event", 
    marker = "tumor_biomarker",
    timepoints = "12, 36, 60"
  )
  
  # Check that result object exists and has expected structure
  expect_true(!is.null(result))
  expect_true("aucTable" %in% names(result))
  expect_true("rocPlot" %in% names(result))
  expect_true("aucPlot" %in% names(result))
  
  # Check AUC table structure
  auc_df <- result$aucTable$asDF
  expect_equal(nrow(auc_df), 3)  # 3 timepoints
  expect_true("timepoint" %in% names(auc_df))
  expect_true("auc" %in% names(auc_df))
  
  # Check AUC values are reasonable (between 0 and 1)
  expect_true(all(auc_df$auc >= 0 & auc_df$auc <= 1))
  
  # Check timepoints match input
  expect_equal(sort(auc_df$timepoint), c(12, 36, 60))
})

test_that("timeroc works with cardiovascular risk data", {
  
  result <- timeroc(
    data = timeroc_cardiovascular_risk,
    elapsedtime = "follow_up_months",
    outcome = "cv_event",
    marker = "troponin_level",
    timepoints = "6, 18, 36"
  )
  
  # Check basic structure
  expect_true(!is.null(result))
  auc_df <- result$aucTable$asDF
  expect_equal(nrow(auc_df), 3)
  expect_equal(sort(auc_df$timepoint), c(6, 18, 36))
  
  # AUC values should be reasonable for this dataset
  expect_true(all(auc_df$auc >= 0.4 & auc_df$auc <= 1))
})

# =============================================================================
# Method Comparison Tests  
# =============================================================================

test_that("different ROC methods produce different results", {
  
  # Test different methods
  incident_result <- timeroc(
    data = timeroc_multi_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "primary_event",
    marker = "biomarker_alpha",
    timepoints = "6, 12",
    method = "incident"
  )
  
  cumulative_result <- timeroc(
    data = timeroc_multi_biomarker,
    elapsedtime = "follow_up_months", 
    outcome = "primary_event",
    marker = "biomarker_alpha",
    timepoints = "6, 12",
    method = "cumulative"
  )
  
  static_result <- timeroc(
    data = timeroc_multi_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "primary_event", 
    marker = "biomarker_alpha",
    timepoints = "6, 12",
    method = "static"
  )
  
  # Extract AUC values
  incident_auc <- incident_result$aucTable$asDF$auc
  cumulative_auc <- cumulative_result$aucTable$asDF$auc
  static_auc <- static_result$aucTable$asDF$auc
  
  # Methods should generally produce different AUC values
  # (allowing for some cases where they might be very similar)
  expect_true(any(abs(incident_auc - cumulative_auc) > 0.01) ||
              any(abs(incident_auc - static_auc) > 0.01) ||
              any(abs(cumulative_auc - static_auc) > 0.01))
})

# =============================================================================
# Multi-Biomarker Comparison Tests
# =============================================================================

test_that("multi-biomarker comparison shows expected ranking", {
  
  # Test all three biomarkers with known performance ranking
  markers <- c("biomarker_alpha", "biomarker_beta", "biomarker_gamma")
  results <- list()
  
  for(marker in markers) {
    results[[marker]] <- timeroc(
      data = timeroc_multi_biomarker,
      elapsedtime = "follow_up_months",
      outcome = "primary_event",
      marker = marker,
      timepoints = "12"
    )
  }
  
  # Extract AUC values for the 12-month timepoint
  auc_alpha <- results[["biomarker_alpha"]]$aucTable$asDF$auc[1]
  auc_beta <- results[["biomarker_beta"]]$aucTable$asDF$auc[1]
  auc_gamma <- results[["biomarker_gamma"]]$aucTable$asDF$auc[1]
  
  # biomarker_alpha should be best, gamma should be worst
  # (allowing for some sampling variation)
  expect_true(auc_alpha >= auc_beta)
  expect_true(auc_beta >= auc_gamma - 0.05)  # Small tolerance for sampling variation
  
  # Alpha should be clearly better than gamma
  expect_true(auc_alpha > auc_gamma + 0.05)
})

# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("timeroc handles invalid inputs gracefully", {
  
  # Test missing required variables
  expect_error(
    timeroc(
      data = timeroc_cancer_biomarker,
      # elapsedtime missing
      outcome = "death_event",
      marker = "tumor_biomarker"
    ),
    "elapsedtime"
  )
  
  # Test invalid variable names
  expect_error(
    timeroc(
      data = timeroc_cancer_biomarker,
      elapsedtime = "nonexistent_time",
      outcome = "death_event", 
      marker = "tumor_biomarker"
    )
  )
  
  # Test invalid timepoints
  expect_warning(
    timeroc(
      data = timeroc_cancer_biomarker,
      elapsedtime = "follow_up_months",
      outcome = "death_event",
      marker = "tumor_biomarker",
      timepoints = "100, 200, 300"  # Beyond follow-up range
    )
  )
})

test_that("timeroc handles edge cases in data", {
  
  # Test with edge cases dataset
  result <- timeroc(
    data = timeroc_edge_cases,
    elapsedtime = "time_months", 
    outcome = "event_status",
    marker = "biomarker",
    timepoints = "6, 12"
  )
  
  # Should complete without error despite missing values and outliers
  expect_true(!is.null(result))
  auc_df <- result$aucTable$asDF
  expect_true(nrow(auc_df) >= 1)  # At least some results
  
  # AUC values should still be valid
  expect_true(all(auc_df$auc >= 0 & auc_df$auc <= 1, na.rm = TRUE))
})

# =============================================================================
# Bootstrap Confidence Interval Tests
# =============================================================================

test_that("bootstrap confidence intervals work", {
  
  # Test with bootstrap CI enabled (small number for speed)
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker", 
    timepoints = "36",
    bootstrapCI = TRUE,
    nboot = 10  # Small number for testing speed
  )
  
  expect_true(!is.null(result))
  auc_df <- result$aucTable$asDF
  
  # Should have confidence interval columns
  expect_true("ci_lower" %in% names(auc_df))
  expect_true("ci_upper" %in% names(auc_df))
  
  # CI should be reasonable (lower < AUC < upper)
  expect_true(auc_df$ci_lower[1] <= auc_df$auc[1])
  expect_true(auc_df$auc[1] <= auc_df$ci_upper[1])
})

# =============================================================================
# Optional Features Tests
# =============================================================================

test_that("optimal cutoff calculation works", {
  
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "36",
    showOptimalCutoff = TRUE
  )
  
  # Check cutoff table exists
  expect_true("cutoffTable" %in% names(result))
  
  # Should have at least one row if calculation succeeds
  cutoff_df <- result$cutoffTable$asDF
  if(nrow(cutoff_df) > 0) {
    expect_true("cutoff" %in% names(cutoff_df))
    expect_true("sensitivity" %in% names(cutoff_df))
    expect_true("specificity" %in% names(cutoff_df))
    expect_true("youden" %in% names(cutoff_df))
    
    # Sensitivity and specificity should be between 0 and 1
    expect_true(all(cutoff_df$sensitivity >= 0 & cutoff_df$sensitivity <= 1))
    expect_true(all(cutoff_df$specificity >= 0 & cutoff_df$specificity <= 1))
  }
})

test_that("marker statistics display works", {
  
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "36",
    showMarkerStats = TRUE
  )
  
  # Check marker stats table exists
  expect_true("markerStats" %in% names(result))
  
  # Should have statistical summary rows
  stats_df <- result$markerStats$asDF
  expect_true(nrow(stats_df) >= 5)  # At least basic stats
  expect_true("statistic" %in% names(stats_df))
  expect_true("value" %in% names(stats_df))
})

test_that("baseline comparison works", {
  
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months", 
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "36",
    compareBaseline = TRUE
  )
  
  # Check model comparison exists
  expect_true("modelComparison" %in% names(result))
  
  # Should contain comparison content
  comparison_content <- result$modelComparison$content
  expect_true(nchar(comparison_content) > 0)
})

# =============================================================================
# Plot Generation Tests
# =============================================================================

test_that("ROC and AUC plots can be generated", {
  
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "12, 36",
    plotROC = TRUE,
    plotAUC = TRUE
  )
  
  # Check plot objects exist
  expect_true("rocPlot" %in% names(result))
  expect_true("aucPlot" %in% names(result))
  
  # Plots should have render functions
  expect_true(!is.null(result$rocPlot$renderFun))
  expect_true(!is.null(result$aucPlot$renderFun))
})

# =============================================================================
# Output Validation Tests
# =============================================================================

test_that("timeroc output structure is consistent", {
  
  result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "12, 24, 36"
  )
  
  # Check all expected components exist
  expected_components <- c(
    "text", "aucTable", "rocPlot", "aucPlot", 
    "markerStats", "cutoffTable", "modelComparison", 
    "clinicalInterpretation"
  )
  
  for(component in expected_components) {
    expect_true(component %in% names(result))
  }
  
  # Check AUC table completeness
  auc_df <- result$aucTable$asDF
  expect_equal(nrow(auc_df), 3)  # 3 timepoints
  expect_true(all(c("timepoint", "auc", "se", "ci_lower", "ci_upper") %in% names(auc_df)))
  
  # Check clinical interpretation exists
  clinical_content <- result$clinicalInterpretation$content
  expect_true(nchar(clinical_content) > 0)
  expect_true(grepl("Clinical Interpretation", clinical_content))
})

# =============================================================================
# Performance and Stress Tests
# =============================================================================

test_that("timeroc handles larger datasets efficiently", {
  
  # Test with cardiovascular dataset (400 observations)
  start_time <- Sys.time()
  
  result <- timeroc(
    data = timeroc_cardiovascular_risk,
    elapsedtime = "follow_up_months",
    outcome = "cv_event",
    marker = "risk_score",
    timepoints = "6, 12, 18, 24, 30, 36"
  )
  
  end_time <- Sys.time()
  runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within reasonable time (< 30 seconds)
  expect_true(runtime < 30)
  
  # Results should still be valid
  expect_true(!is.null(result))
  auc_df <- result$aucTable$asDF
  expect_equal(nrow(auc_df), 6)  # 6 timepoints
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("timeroc integrates with timeROC package correctly", {
  
  # Test that we can call timeROC directly with same data and get consistent results
  data_clean <- na.omit(timeroc_cancer_biomarker[c("follow_up_months", "death_event", "tumor_biomarker")])
  
  # Direct timeROC call
  direct_result <- timeROC::timeROC(
    T = data_clean$follow_up_months,
    delta = data_clean$death_event,
    marker = data_clean$tumor_biomarker,
    cause = 1,
    times = c(36),
    ROC = TRUE
  )
  
  # timeroc wrapper call
  wrapper_result <- timeroc(
    data = timeroc_cancer_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    marker = "tumor_biomarker",
    timepoints = "36"
  )
  
  # AUC values should be very similar (allowing for small numerical differences)
  direct_auc <- direct_result$AUC[1]
  wrapper_auc <- wrapper_result$aucTable$asDF$auc[1]
  
  expect_true(abs(direct_auc - wrapper_auc) < 0.01)
})

# =============================================================================
# Clean up
# =============================================================================

# Clean up any objects created during testing
rm(list = ls(pattern = "^(result|auc_|direct_|wrapper_|start_|end_|runtime)"))
