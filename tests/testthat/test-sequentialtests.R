# Test Suite for Sequential Testing Analysis Module
# Testing mathematical accuracy, edge cases, and clinical scenarios

context("Sequential Testing Analysis - Mathematical Accuracy")

# Source the implementation files
if (file.exists("../../R/sequentialtests.h.R")) {
  source("../../R/sequentialtests.h.R")
  source("../../R/sequentialtests.b.R")
}

# Helper functions to calculate expected values manually
calculate_serial_positive_sens <- function(sens1, sens2) {
  sens1 * sens2
}

calculate_serial_positive_spec <- function(spec1, spec2) {
  spec1 + (1 - spec1) * spec2
}

calculate_serial_negative_sens <- function(sens1, sens2) {
  sens1 + (1 - sens1) * sens2
}

calculate_serial_negative_spec <- function(spec1, spec2) {
  spec1 * spec2
}

calculate_parallel_sens <- function(sens1, sens2) {
  sens1 + sens2 - (sens1 * sens2)
}

calculate_parallel_spec <- function(spec1, spec2) {
  spec1 * spec2
}

calculate_ppv <- function(sens, spec, prev) {
  (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
}

calculate_npv <- function(sens, spec, prev) {
  (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
}

test_that("Serial testing (positive strategy) calculations are mathematically correct", {
  # Test case 1: Standard clinical scenario
  sens1 <- 0.95
  spec1 <- 0.70
  sens2 <- 0.80
  spec2 <- 0.98
  prev <- 0.10
  
  expected_combined_sens <- calculate_serial_positive_sens(sens1, sens2)
  expected_combined_spec <- calculate_serial_positive_spec(spec1, spec2)
  expected_ppv <- calculate_ppv(expected_combined_sens, expected_combined_spec, prev)
  expected_npv <- calculate_npv(expected_combined_sens, expected_combined_spec, prev)
  
  # Expected values for verification
  expect_equal(round(expected_combined_sens, 4), 0.7600, tolerance = 0.0001)
  expect_equal(round(expected_combined_spec, 4), 0.9940, tolerance = 0.0001)
  expect_equal(round(expected_ppv, 4), 0.9337, tolerance = 0.0001)
  expect_equal(round(expected_npv, 4), 0.9739, tolerance = 0.0001)
})

test_that("Serial testing (negative strategy) calculations are mathematically correct", {
  # Test case: High sensitivity strategy
  sens1 <- 0.85
  spec1 <- 0.90
  sens2 <- 0.75
  spec2 <- 0.85
  prev <- 0.05
  
  expected_combined_sens <- calculate_serial_negative_sens(sens1, sens2)
  expected_combined_spec <- calculate_serial_negative_spec(spec1, spec2)
  expected_ppv <- calculate_ppv(expected_combined_sens, expected_combined_spec, prev)
  expected_npv <- calculate_npv(expected_combined_sens, expected_combined_spec, prev)
  
  # Expected values
  expect_equal(round(expected_combined_sens, 4), 0.9625, tolerance = 0.0001)
  expect_equal(round(expected_combined_spec, 4), 0.7650, tolerance = 0.0001)
  expect_equal(round(expected_ppv, 4), 0.1773, tolerance = 0.0001)
  expect_equal(round(expected_npv, 4), 0.9974, tolerance = 0.0001)
})

test_that("Parallel testing calculations are mathematically correct", {
  # Test case: Complementary tests
  sens1 <- 0.80
  spec1 <- 0.95
  sens2 <- 0.70
  spec2 <- 0.90
  prev <- 0.15
  
  expected_combined_sens <- calculate_parallel_sens(sens1, sens2)
  expected_combined_spec <- calculate_parallel_spec(spec1, spec2)
  expected_ppv <- calculate_ppv(expected_combined_sens, expected_combined_spec, prev)
  expected_npv <- calculate_npv(expected_combined_sens, expected_combined_spec, prev)
  
  # Expected values
  expect_equal(round(expected_combined_sens, 4), 0.9400, tolerance = 0.0001)
  expect_equal(round(expected_combined_spec, 4), 0.8550, tolerance = 0.0001)
  expect_equal(round(expected_ppv, 4), 0.5336, tolerance = 0.0001)
  expect_equal(round(expected_npv, 4), 0.9878, tolerance = 0.0001)
})

test_that("Edge case calculations work correctly", {
  # Test case 1: Perfect sensitivity, imperfect specificity
  sens1 <- 0.99
  spec1 <- 0.50
  sens2 <- 0.80
  spec2 <- 0.99
  prev <- 0.01
  
  # Serial positive strategy should maximize specificity
  expected_combined_sens <- calculate_serial_positive_sens(sens1, sens2)
  expected_combined_spec <- calculate_serial_positive_spec(spec1, spec2)
  
  expect_equal(round(expected_combined_sens, 4), 0.7920, tolerance = 0.0001)
  expect_equal(round(expected_combined_spec, 4), 0.9950, tolerance = 0.0001)
  
  # Test case 2: Perfect specificity, imperfect sensitivity
  sens1 <- 0.60
  spec1 <- 0.99
  sens2 <- 0.90
  spec2 <- 0.80
  prev <- 0.50
  
  # Serial negative strategy should maximize sensitivity
  expected_combined_sens <- calculate_serial_negative_sens(sens1, sens2)
  expected_combined_spec <- calculate_serial_negative_spec(spec1, spec2)
  
  expect_equal(round(expected_combined_sens, 4), 0.9600, tolerance = 0.0001)
  expect_equal(round(expected_combined_spec, 4), 0.7920, tolerance = 0.0001)
})

test_that("Extreme prevalence scenarios are handled correctly", {
  # Very low prevalence (rare disease screening)
  sens1 <- 0.90
  spec1 <- 0.95
  sens2 <- 0.85
  spec2 <- 0.99
  prev <- 0.001
  
  combined_sens <- calculate_serial_positive_sens(sens1, sens2)
  combined_spec <- calculate_serial_positive_spec(spec1, spec2)
  ppv <- calculate_ppv(combined_sens, combined_spec, prev)
  npv <- calculate_npv(combined_sens, combined_spec, prev)
  
  expect_true(ppv > 0 && ppv < 1)
  expect_true(npv > 0 && npv < 1)
  expect_true(npv > 0.999) # Should be very high in low prevalence
  
  # Very high prevalence (confirmatory testing)
  prev <- 0.90
  ppv <- calculate_ppv(combined_sens, combined_spec, prev)
  npv <- calculate_npv(combined_sens, combined_spec, prev)
  
  expect_true(ppv > 0.95) # Should be very high in high prevalence
  expect_true(npv > 0 && npv < 1)
})

context("Sequential Testing Analysis - Clinical Scenarios")

test_that("COVID-19 testing scenario produces sensible results", {
  # Rapid test followed by RT-PCR confirmation
  rapid_sens <- 0.85
  rapid_spec <- 0.95
  pcr_sens <- 0.95
  pcr_spec <- 0.99
  
  # Community prevalence
  community_prev <- 0.02
  
  # Serial positive strategy (confirmation)
  combined_sens <- calculate_serial_positive_sens(rapid_sens, pcr_sens)
  combined_spec <- calculate_serial_positive_spec(rapid_spec, pcr_spec)
  ppv <- calculate_ppv(combined_sens, combined_spec, community_prev)
  npv <- calculate_npv(combined_sens, combined_spec, community_prev)
  
  expect_true(combined_sens < rapid_sens) # Should decrease sensitivity
  expect_true(combined_spec > rapid_spec) # Should increase specificity
  expect_true(ppv > 0.70) # Should have good PPV after confirmation
  expect_true(npv > 0.99) # Should have excellent NPV
})

test_that("Cancer screening scenario produces realistic results", {
  # Mammography followed by biopsy
  mammo_sens <- 0.80
  mammo_spec <- 0.90
  biopsy_sens <- 0.95
  biopsy_spec <- 0.98
  
  # Cancer prevalence in screening population
  cancer_prev <- 0.008
  
  # Serial positive strategy
  combined_sens <- calculate_serial_positive_sens(mammo_sens, biopsy_sens)
  combined_spec <- calculate_serial_positive_spec(mammo_spec, biopsy_spec)
  ppv <- calculate_ppv(combined_sens, combined_spec, cancer_prev)
  
  expect_true(combined_sens < mammo_sens) # Lower sensitivity due to serial testing
  expect_true(combined_spec > mammo_spec) # Higher specificity
  expect_true(ppv > 0.50) # Should have reasonable PPV after biopsy
})

test_that("Cardiac testing scenario works correctly", {
  # Stress test followed by catheterization
  stress_sens <- 0.85
  stress_spec <- 0.75
  cath_sens <- 0.95
  cath_spec <- 0.98
  
  # CAD prevalence in chest pain patients
  cad_prev <- 0.25
  
  # Serial positive strategy
  combined_sens <- calculate_serial_positive_sens(stress_sens, cath_sens)
  combined_spec <- calculate_serial_positive_spec(stress_spec, cath_spec)
  ppv <- calculate_ppv(combined_sens, combined_spec, cad_prev)
  npv <- calculate_npv(combined_sens, combined_spec, cad_prev)
  
  expect_true(ppv > 0.80) # Should have high confidence after catheterization
  expect_true(npv > 0.90) # Good negative predictive value
})

test_that("sequentialtests returns finite combined metrics for typical inputs", {
  result <- sequentialtests(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.95,
    test2_spec = 0.98,
    strategy = "serial_positive",
    prevalence = 0.10
  )

  expect_s3_class(result, "sequentialtestsResults")

  summary_df <- result$summary_table$asDF
  expect_true(all(is.finite(summary_df$combined_sens)))
  expect_true(all(is.finite(summary_df$combined_spec)))
  expect_true(all(is.finite(summary_df$combined_ppv)))
  expect_true(all(is.finite(summary_df$combined_npv)))

  tests_df <- result$individual_tests_table$asDF
  numeric_cols_tests <- vapply(tests_df, is.numeric, logical(1))
  if (any(numeric_cols_tests)) {
    expect_false(any(is.nan(as.matrix(tests_df[ , numeric_cols_tests, drop = FALSE]))))
  }

  flow_df <- result$population_flow_table$asDF
  numeric_cols_flow <- vapply(flow_df, is.numeric, logical(1))
  if (any(numeric_cols_flow)) {
    expect_false(any(is.nan(as.matrix(flow_df[ , numeric_cols_flow, drop = FALSE]))))
  }
})

context("Sequential Testing Analysis - Strategy Comparisons")

test_that("Serial positive strategy maximizes specificity", {
  sens1 <- 0.90
  spec1 <- 0.80
  sens2 <- 0.85
  spec2 <- 0.95
  
  serial_pos_spec <- calculate_serial_positive_spec(spec1, spec2)
  serial_neg_spec <- calculate_serial_negative_spec(spec1, spec2)
  parallel_spec <- calculate_parallel_spec(spec1, spec2)
  
  expect_true(serial_pos_spec > serial_neg_spec)
  expect_true(serial_pos_spec > parallel_spec)
})

test_that("Serial negative strategy maximizes sensitivity", {
  sens1 <- 0.80
  spec1 <- 0.90
  sens2 <- 0.75
  spec2 <- 0.95
  
  serial_pos_sens <- calculate_serial_positive_sens(sens1, sens2)
  serial_neg_sens <- calculate_serial_negative_sens(sens1, sens2)
  parallel_sens <- calculate_parallel_sens(sens1, sens2)
  
  expect_true(serial_neg_sens > serial_pos_sens)
  expect_true(parallel_sens > serial_pos_sens)
  # Note: parallel and serial negative can be close, depends on specific values
})

test_that("Parallel testing provides balanced approach", {
  sens1 <- 0.80
  spec1 <- 0.90
  sens2 <- 0.85
  spec2 <- 0.88
  
  serial_pos_sens <- calculate_serial_positive_sens(sens1, sens2)
  serial_pos_spec <- calculate_serial_positive_spec(spec1, spec2)
  
  parallel_sens <- calculate_parallel_sens(sens1, sens2)
  parallel_spec <- calculate_parallel_spec(spec1, spec2)
  
  # Parallel testing should have higher sensitivity than serial positive
  expect_true(parallel_sens > serial_pos_sens)
  # But lower specificity than serial positive
  expect_true(parallel_spec < serial_pos_spec)
})

context("Sequential Testing Analysis - Input Validation")

# Input validation helper function
validate_probability <- function(value, name) {
  if (value < 0 || value > 1) {
    stop(paste(name, "must be between 0 and 1"))
  }
}

# Enhanced calculation functions with validation
calculate_ppv_validated <- function(sens, spec, prev) {
  validate_probability(sens, "Sensitivity")
  validate_probability(spec, "Specificity") 
  validate_probability(prev, "Prevalence")
  
  (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
}

calculate_npv_validated <- function(sens, spec, prev) {
  validate_probability(sens, "Sensitivity")
  validate_probability(spec, "Specificity")
  validate_probability(prev, "Prevalence")
  
  (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
}

test_that("Input parameter bounds are respected", {
  # Test that sensitivity and specificity are between 0 and 1
  expect_error(calculate_ppv_validated(-0.1, 0.9, 0.1))
  expect_error(calculate_ppv_validated(1.1, 0.9, 0.1))
  expect_error(calculate_ppv_validated(0.9, -0.1, 0.1))
  expect_error(calculate_ppv_validated(0.9, 1.1, 0.1))
  expect_error(calculate_ppv_validated(0.9, 0.9, -0.1))
  expect_error(calculate_ppv_validated(0.9, 0.9, 1.1))
})



test_that("Population flow calculations are consistent", {
  # Test with a standard scenario
  sens1 <- 0.90
  spec1 <- 0.85
  sens2 <- 0.80
  spec2 <- 0.95
  prev <- 0.10
  pop_size <- 1000
  
  # Calculate expected populations
  diseased <- pop_size * prev
  healthy <- pop_size - diseased
  
  # After first test
  test1_tp <- diseased * sens1
  test1_fp <- healthy * (1 - spec1)
  test1_pos <- test1_tp + test1_fp
  
  # For serial positive strategy
  combined_sens <- calculate_serial_positive_sens(sens1, sens2)
  combined_spec <- calculate_serial_positive_spec(spec1, spec2)
  
  final_tp <- diseased * combined_sens
  final_fp <- healthy * (1 - combined_spec)
  
  # Check consistency
  expect_true(final_tp <= test1_tp) # Can't have more TP than after first test
  expect_true(final_fp <= test1_fp) # Should have fewer FP due to confirmation
  expect_true(final_tp + final_fp <= test1_pos) # Total positives should decrease
})

test_that("Complex sequential scenarios maintain logical consistency", {
  # Test multiple scenarios for logical consistency
  test_scenarios <- data.frame(
    sens1 = c(0.95, 0.80, 0.70, 0.90),
    spec1 = c(0.70, 0.90, 0.95, 0.60),
    sens2 = c(0.80, 0.85, 0.90, 0.95),
    spec2 = c(0.95, 0.80, 0.85, 0.99),
    prev = c(0.01, 0.10, 0.30, 0.05)
  )
  
  for (i in 1:nrow(test_scenarios)) {
    sens1 <- test_scenarios$sens1[i]
    spec1 <- test_scenarios$spec1[i] 
    sens2 <- test_scenarios$sens2[i]
    spec2 <- test_scenarios$spec2[i]
    prev <- test_scenarios$prev[i]
    
    # Test all three strategies
    strategies <- list(
      serial_pos = list(
        sens = calculate_serial_positive_sens(sens1, sens2),
        spec = calculate_serial_positive_spec(spec1, spec2)
      ),
      serial_neg = list(
        sens = calculate_serial_negative_sens(sens1, sens2),
        spec = calculate_serial_negative_spec(spec1, spec2)
      ),
      parallel = list(
        sens = calculate_parallel_sens(sens1, sens2),
        spec = calculate_parallel_spec(spec1, spec2)
      )
    )
    
    # All combined metrics should be valid probabilities
    for (strategy in strategies) {
      expect_true(strategy$sens >= 0 && strategy$sens <= 1)
      expect_true(strategy$spec >= 0 && strategy$spec <= 1)
      
      ppv <- calculate_ppv(strategy$sens, strategy$spec, prev)
      npv <- calculate_npv(strategy$sens, strategy$spec, prev)
      
      expect_true(ppv >= 0 && ppv <= 1)
      expect_true(npv >= 0 && npv <= 1)
    }
  }
})

test_that("Likelihood ratio calculations are mathematically sound", {
  # Test LR calculations for combined strategies
  sens1 <- 0.90
  spec1 <- 0.80
  sens2 <- 0.85
  spec2 <- 0.95
  
  combined_sens <- calculate_serial_positive_sens(sens1, sens2)
  combined_spec <- calculate_serial_positive_spec(spec1, spec2)
  
  plr <- combined_sens / (1 - combined_spec)
  nlr <- (1 - combined_sens) / combined_spec
  
  expect_true(plr > 0)
  expect_true(nlr > 0)
  expect_true(plr > 1) # Good test should have PLR > 1
  expect_true(nlr < 1) # Good test should have NLR < 1
})

context("Sequential Testing Analysis - Clinical Decision Making")

test_that("Test strategies provide clinical decision guidance", {
  # Scenario: Need to minimize false positives (surgical decision)
  sens1 <- 0.85
  spec1 <- 0.80
  sens2 <- 0.80
  spec2 <- 0.98
  prev <- 0.15
  
  # Serial positive should be best for minimizing false positives
  serial_pos_spec <- calculate_serial_positive_spec(spec1, spec2)
  serial_neg_spec <- calculate_serial_negative_spec(spec1, spec2)
  
  expect_true(serial_pos_spec > spec1) # Should improve specificity
  expect_true(serial_pos_spec > spec2) # Should be better than either alone
  expect_true(serial_pos_spec > serial_neg_spec) # Should beat serial negative
})

test_that("Cost-effectiveness scenarios work correctly", {
  # High cost second test - serial strategies should be more efficient
  rapid_sens <- 0.90
  rapid_spec <- 0.85
  expensive_sens <- 0.95
  expensive_spec <- 0.98
  prev <- 0.05
  
  # Calculate how many need the expensive second test
  pop_size <- 1000
  diseased <- pop_size * prev
  healthy <- pop_size - diseased
  
  # Serial positive: only rapid test positives get expensive test
  rapid_pos <- (diseased * rapid_sens) + (healthy * (1 - rapid_spec))
  expensive_tests_needed <- rapid_pos
  
  # Parallel: everyone gets both tests
  total_tests_parallel <- pop_size * 2
  total_tests_serial <- pop_size + expensive_tests_needed
  
  expect_true(total_tests_serial < total_tests_parallel)
  expect_true(expensive_tests_needed < pop_size) # Should reduce testing burden
})
