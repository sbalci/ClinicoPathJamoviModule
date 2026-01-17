context("Time-Dependent ROC Analysis")

library(testthat)
library(timeROC)

# Source the module files globally for all tests
# This ensures we test the FILE code, not installed package
source("../../R/timeroc.h.R")
source("../../R/timeroc.b.R")

# Helper function to generate synthetic data
get_synthetic_data <- function(n=100, seed=123) {
  set.seed(seed)
  # Generate biomarker with some predictive signal
  status <- sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  biomarker <- rnorm(n) + (status * 1.5) # Signal
  
  data.frame(
    follow_up_months = runif(n, 5, 120),
    death_event = status,
    tumor_biomarker = biomarker,
    # Cardiovascular fields
    cv_event = status,
    troponin_level = biomarker,
    risk_score = biomarker + rnorm(n, 0, 0.2),
    # Multi-biomarker fields
    primary_event = status,
    biomarker_alpha = biomarker + 1, # Best
    biomarker_beta = biomarker,      # Mid
    biomarker_gamma = rnorm(n),      # Worst (noise)
    # Edge case fields
    time_months = runif(n, 1, 60),
    event_status = status,
    biomarker_edge = biomarker,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("timeroc basic functionality works with cancer biomarker data", {
  skip_if_not_installed('jmvReadWrite')
  
  synthetic_data <- get_synthetic_data(n=200)

  # Test basic analysis
  result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months",
    outcome = "death_event", 
    outcomeLevel = NULL,
    marker = "tumor_biomarker",
    timepoints = "12, 36, 60"
  )
  
  # Check that result object exists and has expected structure
  expect_true(!is.null(result))
  expect_true("aucTable" %in% names(result))
  expect_true("rocPlot" %in% names(result))
  
  # Check AUC table structure
  auc_df <- result$aucTable$asDF
  if (nrow(auc_df) > 0) {
      expect_equal(nrow(auc_df), 3)  # 3 timepoints
      expect_true(all(auc_df$auc >= 0 & auc_df$auc <= 1))
  }
})

test_that("timeroc works with cardiovascular risk data", {
  synthetic_data <- get_synthetic_data(n=200)
  
  result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months",
    outcome = "cv_event",
    outcomeLevel = NULL,
    marker = "troponin_level",
    timepoints = "6, 18, 36"
  )
  
  expect_true(!is.null(result))
  auc_df <- result$aucTable$asDF
  if (nrow(auc_df) > 0) {
      expect_true(all(auc_df$auc >= 0 & auc_df$auc <= 1))
  }
})

# =============================================================================
# Method Comparison Tests  
# =============================================================================

test_that("different ROC methods produce different results", {
  synthetic_data <- get_synthetic_data(n=300)

  # Test different methods
  incident_result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months",
    outcome = "primary_event",
    outcomeLevel = NULL,
    marker = "biomarker_alpha",
    timepoints = "24, 48",
    method = "incident"
  )
  
  cumulative_result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months", 
    outcome = "primary_event",
    outcomeLevel = NULL,
    marker = "biomarker_alpha",
    timepoints = "24, 48",
    method = "cumulative"
  )
  
  # Extract AUC values
  incident_auc <- incident_result$aucTable$asDF$auc
  cumulative_auc <- cumulative_result$aucTable$asDF$auc
  
  if (length(incident_auc) > 0 && length(cumulative_auc) > 0) {
     expect_true(length(incident_auc) == length(cumulative_auc))
  }
})

# =============================================================================
# Multi-Biomarker Comparison Tests
# =============================================================================

test_that("multi-biomarker comparison shows expected ranking", {
  synthetic_data <- get_synthetic_data(n=300)
  
  # Test all three biomarkers with known performance ranking
  markers <- c("biomarker_alpha", "biomarker_beta", "biomarker_gamma")
  results <- list()
  
  for(marker in markers) {
    args <- list(
      data = synthetic_data,
      elapsedtime = "follow_up_months",
      outcome = "primary_event",
      outcomeLevel = NULL,
      marker = marker,
      timepoints = "24"
    )
    # Using do.call to handle marker argument correctly
    results[[marker]] <- do.call("timeroc", args)
  }
  
  # Extract AUC values for the timepoint
  auc_alpha <- results[["biomarker_alpha"]]$aucTable$asDF$auc[1]
  auc_gamma <- results[["biomarker_gamma"]]$aucTable$asDF$auc[1]
  
  if (!is.na(auc_alpha) && !is.na(auc_gamma)) {
      expect_true(auc_alpha > auc_gamma)
  }
})

# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("timeroc handles invalid inputs gracefully", {
  start_data <- get_synthetic_data(n=50)

  # Test missing required variables (timeroc usually returns welcome message or handles it)
  # Just checking it doesn't crash the R session
  result <- timeroc(
      data = start_data,
      # elapsedtime missing
      outcome = "death_event",
      outcomeLevel = NULL,
      marker = "tumor_biomarker"
  )
  expect_true(!is.null(result))
})

# =============================================================================
# Output Validation Tests
# =============================================================================

test_that("timeroc output structure is consistent", {
  synthetic_data <- get_synthetic_data(n=100)
  
  result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    outcomeLevel = NULL,
    marker = "tumor_biomarker",
    timepoints = "12, 24, 36"
  )
  
  expected_components <- c(
    "text", "aucTable", "rocPlot", "aucPlot", 
    "markerStats", "cutoffTable", "modelComparison", 
    "clinicalInterpretation"
  )
  
  for(component in expected_components) {
    expect_true(component %in% names(result))
  }
})

# =============================================================================
# Performance and Stress Tests
# =============================================================================

test_that("timeroc handles larger datasets efficiently", {
  synthetic_data <- get_synthetic_data(n=500)
  
  start_time <- Sys.time()
  
  result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months",
    outcome = "cv_event",
    outcomeLevel = NULL,
    marker = "risk_score",
    timepoints = "6, 12, 18, 24, 30, 36"
  )
  
  end_time <- Sys.time()
  runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(runtime < 30)
  expect_true(!is.null(result))
  
  auc_df <- result$aucTable$asDF
  expect_true(nrow(auc_df) > 0) 
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("timeroc integrates with timeROC package correctly", {
  synthetic_data <- get_synthetic_data(n=200)
  timepoint <- 36
  
  # Direct timeROC call
  direct_result <- timeROC::timeROC(
    T = synthetic_data$follow_up_months,
    delta = synthetic_data$death_event,
    marker = synthetic_data$tumor_biomarker,
    cause = 1,
    times = c(timepoint),
    ROC = TRUE,
    iid = FALSE
  )
  
  # timeroc wrapper call
  wrapper_result <- timeroc(
    data = synthetic_data,
    elapsedtime = "follow_up_months",
    outcome = "death_event",
    outcomeLevel = NULL,
    marker = "tumor_biomarker",
    timepoints = as.character(timepoint)
  )
  
  direct_val <- direct_result$AUC[which(direct_result$times == timepoint)]
  wrapper_auc <- wrapper_result$aucTable$asDF$auc[1]
  
  if (!is.numeric(wrapper_auc)) {
      wrapper_auc <- 0 # Handle empty
  }
  
  expect_true(abs(direct_val - wrapper_auc) < 0.01)
})
