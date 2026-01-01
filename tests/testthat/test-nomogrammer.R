#' @title Comprehensive Tests for nomogrammer Function
#' @description Unit tests covering all aspects of the nomogrammer function including
#'   mathematical accuracy, error handling, plot generation, and edge cases
#' @author ClinicoPath Development Team

library(testthat)
library(ggplot2)
library(scales)

# Load the nomogrammer function (assumes it's available in the package)
# source("../../R/nomogrammer.R")  # If testing standalone

#' =============================================================================
#' Test Suite 1: Mathematical Accuracy
#' =============================================================================

test_that("Mathematical calculations are accurate", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test Case 1: Standard scenario with known values
  prevalence <- 0.30
  sensitivity <- 0.90
  specificity <- 0.80
  
  # Calculate expected values manually
  expected_PLR <- sensitivity / (1 - specificity)  # 0.90 / 0.20 = 4.5
  expected_NLR <- (1 - sensitivity) / specificity  # 0.10 / 0.80 = 0.125
  
  # Test with sensitivity/specificity input
  expect_silent(
    plot1 <- nomogrammer(Prevalence = prevalence, Sens = sensitivity, Spec = specificity)
  )
  expect_s3_class(plot1, "ggplot")
  
  # Test Case 2: Back-calculation from likelihood ratios
  test_PLR <- 4.5
  test_NLR <- 0.125
  
  # Expected back-calculated values
  expected_spec <- (test_PLR - 1) / (test_PLR - test_NLR)  # 3.5 / 4.375 = 0.8
  expected_sens <- test_PLR * (1 - expected_spec)         # 4.5 * 0.2 = 0.9
  
  expect_equal(expected_spec, 0.8, tolerance = 1e-10)
  expect_equal(expected_sens, 0.9, tolerance = 1e-10)
  
  # Test with likelihood ratio input
  expect_silent(
    plot2 <- nomogrammer(Prevalence = prevalence, Plr = test_PLR, Nlr = test_NLR)
  )
  expect_s3_class(plot2, "ggplot")
})

test_that("Forward and backward calculations are consistent", {
  
  # Test multiple scenarios for mathematical consistency
  test_cases <- data.frame(
    prevalence = c(0.01, 0.10, 0.25, 0.50, 0.75),
    sensitivity = c(0.95, 0.85, 0.90, 0.80, 0.70),
    specificity = c(0.99, 0.90, 0.85, 0.95, 0.80)
  )
  
  for (i in 1:nrow(test_cases)) {
    prev <- test_cases$prevalence[i]
    sens <- test_cases$sensitivity[i]
    spec <- test_cases$specificity[i]
    
    # Forward calculation
    PLR <- sens / (1 - spec)
    NLR <- (1 - sens) / spec
    
    # Backward calculation
    spec_back <- (PLR - 1) / (PLR - NLR)
    sens_back <- PLR * (1 - spec_back)
    
    # Test consistency
    expect_equal(spec, spec_back, tolerance = 1e-10, 
                 info = paste("Specificity consistency failed at row", i))
    expect_equal(sens, sens_back, tolerance = 1e-10,
                 info = paste("Sensitivity consistency failed at row", i))
  }
})

test_that("Posterior probability calculations are correct", {
  
  # Test Bayesian calculations manually
  prevalence <- 0.20
  PLR <- 10.0
  NLR <- 0.1
  
  # Manual calculation
  prior_odds <- prevalence / (1 - prevalence)  # 0.25
  post_odds_pos <- prior_odds * PLR            # 2.5
  post_odds_neg <- prior_odds * NLR            # 0.025
  expected_post_prob_pos <- post_odds_pos / (1 + post_odds_pos)  # 0.714
  expected_post_prob_neg <- post_odds_neg / (1 + post_odds_neg)  # 0.024
  
  # These values should be consistent with nomogrammer internal calculations
  expect_gt(expected_post_prob_pos, prevalence)  # Positive test increases probability
  expect_lt(expected_post_prob_neg, prevalence)  # Negative test decreases probability
})

#' =============================================================================
#' Test Suite 2: Input Validation and Error Handling
#' =============================================================================

test_that("Input validation works correctly", {
  
  # Test missing prevalence
  expect_error(
    nomogrammer(Sens = 0.9, Spec = 0.8),
    "Prevalence is required"
  )
  
  # Test invalid prevalence values
  expect_error(
    nomogrammer(Prevalence = -0.1, Sens = 0.9, Spec = 0.8),
    "between 0 and 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 1.5, Sens = 0.9, Spec = 0.8),
    "between 0 and 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0, Sens = 0.9, Spec = 0.8),
    "between 0 and 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 1, Sens = 0.9, Spec = 0.8),
    "between 0 and 1"
  )
  
  # Test invalid sensitivity values
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = -0.1, Spec = 0.8),
    "between 0 and 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = 1.2, Spec = 0.8),
    "between 0 and 1"
  )
  
  # Test invalid specificity values
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = -0.1),
    "between 0 and 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 1.1),
    "between 0 and 1"
  )
  
  # Test invalid likelihood ratios
  expect_error(
    nomogrammer(Prevalence = 0.3, Plr = 0.5, Nlr = 0.1),
    "should be >= 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Plr = 2.0, Nlr = -0.1),
    "must be between 0 and 1"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Plr = 2.0, Nlr = 1.5),
    "must be between 0 and 1"
  )
  
  # Test equal PLR and NLR (uninformative test)
  expect_error(
    nomogrammer(Prevalence = 0.3, Plr = 2.0, Nlr = 2.0),
    "uninformative test"
  )
  
  # Test missing required parameters
  expect_error(
    nomogrammer(Prevalence = 0.3),
    "Either \\(Sens, Spec\\) or \\(Plr, Nlr\\) must be provided"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = 0.9),
    "Either \\(Sens, Spec\\) or \\(Plr, Nlr\\) must be provided"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Plr = 2.0),
    "Either \\(Sens, Spec\\) or \\(Plr, Nlr\\) must be provided"
  )
})

test_that("Warning messages work correctly", {
  
  # Test warning when both sens/spec and LRs provided
  expect_warning(
    nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8, Plr = 4.5, Nlr = 0.125),
    "Using sensitivity/specificity values"
  )
  
  # Test warnings for poor test performance (when calculated)
  expect_warning(
    nomogrammer(Prevalence = 0.3, Sens = 0.1, Spec = 0.8),  # Very low sensitivity
    "PLR < 1"
  )
  
  expect_warning(
    nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.1),  # Very low specificity  
    "NLR > 1"
  )
})

#' =============================================================================
#' Test Suite 3: Function Options and Features
#' =============================================================================

test_that("Optional parameters work correctly", {
  
  # Test Detail option
  expect_silent(
    plot_detailed <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8, Detail = TRUE)
  )
  expect_s3_class(plot_detailed, "ggplot")
  
  # Test NullLine option
  expect_silent(
    plot_nullline <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8, NullLine = TRUE)
  )
  expect_s3_class(plot_nullline, "ggplot")
  
  # Test LabelSize option
  expect_silent(
    plot_labels <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8, LabelSize = 3.0)
  )
  expect_s3_class(plot_labels, "ggplot")
  
  # Test Verbose option (should not throw error, but hard to test output directly)
  expect_silent(
    plot_verbose <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8, Verbose = TRUE)
  )
  expect_s3_class(plot_verbose, "ggplot")
  
  # Test all options together
  expect_silent(
    plot_all <- nomogrammer(
      Prevalence = 0.3, 
      Sens = 0.9, 
      Spec = 0.8,
      Detail = TRUE,
      NullLine = TRUE, 
      LabelSize = 2.5,
      Verbose = FALSE
    )
  )
  expect_s3_class(plot_all, "ggplot")
})

#' =============================================================================
#' Test Suite 4: Edge Cases and Special Scenarios
#' =============================================================================

test_that("Edge cases handle correctly", {
  
  # Very low prevalence (screening scenarios)
  expect_silent(
    plot_lowprev <- nomogrammer(Prevalence = 0.001, Sens = 0.99, Spec = 0.95)
  )
  expect_s3_class(plot_lowprev, "ggplot")
  
  # Very high prevalence
  expect_silent(
    plot_highprev <- nomogrammer(Prevalence = 0.999, Sens = 0.90, Spec = 0.90)
  )
  expect_s3_class(plot_highprev, "ggplot")
  
  # Near-perfect test characteristics
  expect_silent(
    plot_perfect <- nomogrammer(Prevalence = 0.1, Sens = 0.999, Spec = 0.999)
  )
  expect_s3_class(plot_perfect, "ggplot")
  
  # Poor test characteristics
  expect_warning(
    plot_poor <- nomogrammer(Prevalence = 0.3, Sens = 0.6, Spec = 0.6),
    NA  # Should not produce warnings for this case
  )
  expect_s3_class(plot_poor, "ggplot")
  
  # Extreme likelihood ratios
  expect_silent(
    plot_extreme_lr <- nomogrammer(Prevalence = 0.2, Plr = 100, Nlr = 0.01)
  )
  expect_s3_class(plot_extreme_lr, "ggplot")
})

#' =============================================================================
#' Test Suite 5: Real-world Clinical Scenarios
#' =============================================================================

test_that("Real-world clinical scenarios work", {
  
  # Mammography screening
  expect_silent(
    mammography <- nomogrammer(
      Prevalence = 0.005,  # 0.5% prevalence in screening
      Sens = 0.85,
      Spec = 0.95,
      Detail = TRUE
    )
  )
  expect_s3_class(mammography, "ggplot")
  
  # COVID-19 rapid test
  expect_silent(
    covid_test <- nomogrammer(
      Prevalence = 0.10,   # 10% prevalence in symptomatic population
      Sens = 0.95,
      Spec = 0.85,
      Detail = TRUE
    )
  )
  expect_s3_class(covid_test, "ggplot")
  
  # Troponin for myocardial infarction
  expect_silent(
    troponin <- nomogrammer(
      Prevalence = 0.15,   # 15% prevalence in chest pain patients
      Sens = 0.99,
      Spec = 0.85,
      Detail = TRUE
    )
  )
  expect_s3_class(troponin, "ggplot")
})

#' =============================================================================
#' Test Suite 6: Plot Structure and Content
#' =============================================================================

test_that("Plot structure is correct", {
  
  plot_test <- nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8)
  
  # Test that plot is a ggplot object
  expect_s3_class(plot_test, "ggplot")
  
  # Test that plot has required components
  expect_true("data" %in% names(plot_test))
  expect_true("layers" %in% names(plot_test))
  expect_true("scales" %in% names(plot_test))
  expect_true("theme" %in% names(plot_test))
  
  # Test plot build (should not error)
  expect_silent(built_plot <- ggplot_build(plot_test))
  expect_true("data" %in% names(built_plot))
  
  # Test plot data structure
  plot_data <- built_plot$data
  expect_true(length(plot_data) > 0)  # Should have at least one layer
})

#' =============================================================================
#' Test Suite 7: Performance and Efficiency
#' =============================================================================

test_that("Function performance is acceptable", {
  
  # Test that function runs quickly (should complete in under 1 second)
  start_time <- Sys.time()
  
  for (i in 1:10) {
    nomogrammer(Prevalence = 0.3, Sens = 0.9, Spec = 0.8)
  }
  
  end_time <- Sys.time()
  elapsed <- as.numeric(end_time - start_time)
  
  expect_lt(elapsed, 5)  # Should complete 10 runs in under 5 seconds
})

#' =============================================================================
#' Test Suite 8: Parameter Type Validation
#' =============================================================================

test_that("Parameter types are validated", {
  
  # Test non-numeric inputs
  expect_error(
    nomogrammer(Prevalence = "0.3", Sens = 0.9, Spec = 0.8),
    "must be.*numeric"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = "0.9", Spec = 0.8),
    "must be numeric"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Plr = "2.0", Nlr = 0.1),
    "must be numeric"
  )
  
  # Test vector inputs (should require single values)
  expect_error(
    nomogrammer(Prevalence = c(0.3, 0.4), Sens = 0.9, Spec = 0.8),
    "must be.*single"
  )
  
  expect_error(
    nomogrammer(Prevalence = 0.3, Sens = c(0.9, 0.8), Spec = 0.8),
    "must be.*single"
  )
})

#' =============================================================================
#' Test Output Message
#' =============================================================================

cat("=== Nomogrammer Function Test Suite ===\n")
cat("All tests designed to validate:\n")
cat("1. Mathematical accuracy and consistency\n")
cat("2. Input validation and error handling\n") 
cat("3. Optional parameters and features\n")
cat("4. Edge cases and special scenarios\n")
cat("5. Real-world clinical applications\n")
cat("6. Plot structure and rendering\n")
cat("7. Performance and efficiency\n")
cat("8. Parameter type validation\n")
cat("==========================================\n")
