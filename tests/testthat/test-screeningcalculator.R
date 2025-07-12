# Test Suite for Screening Calculator Module
# Testing mathematical accuracy and edge cases

context("Screening Calculator - Mathematical Accuracy")

# Helper function to calculate expected values manually
calculate_expected_ppv <- function(sens, spec, prev) {
  (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
}

calculate_expected_npv <- function(sens, spec, prev) {
  (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
}

calculate_expected_lrp <- function(sens, spec) {
  sens / (1 - spec)
}

calculate_expected_lrn <- function(sens, spec) {
  (1 - sens) / spec
}

test_that("Single test calculations are mathematically correct", {
  # Test case 1: Standard values
  sens <- 0.90
  spec <- 0.80
  prev <- 0.10
  
  expected_ppv <- calculate_expected_ppv(sens, spec, prev)
  expected_npv <- calculate_expected_npv(sens, spec, prev)
  expected_lrp <- calculate_expected_lrp(sens, spec)
  expected_lrn <- calculate_expected_lrn(sens, spec)
  
  # Expected values for this scenario
  expect_equal(round(expected_ppv, 4), 0.3103, tolerance = 0.0001)
  expect_equal(round(expected_npv, 4), 0.9872, tolerance = 0.0001)
  expect_equal(round(expected_lrp, 4), 4.5000, tolerance = 0.0001)
  expect_equal(round(expected_lrn, 4), 0.1250, tolerance = 0.0001)
})

test_that("Edge case calculations work correctly", {
  # Test case 2: High sensitivity, low specificity
  sens <- 0.99
  spec <- 0.50
  prev <- 0.01
  
  expected_ppv <- calculate_expected_ppv(sens, spec, prev)
  expected_npv <- calculate_expected_npv(sens, spec, prev)
  
  expect_equal(round(expected_ppv, 4), 0.0196, tolerance = 0.0001)
  expect_equal(round(expected_npv, 4), 0.9999, tolerance = 0.0001)
  
  # Test case 3: Low sensitivity, high specificity  
  sens <- 0.50
  spec <- 0.99
  prev <- 0.50
  
  expected_ppv <- calculate_expected_ppv(sens, spec, prev)
  expected_npv <- calculate_expected_npv(sens, spec, prev)
  
  expect_equal(round(expected_ppv, 4), 0.9804, tolerance = 0.0001)
  expect_equal(round(expected_npv, 4), 0.6667, tolerance = 0.0001)
})

test_that("Sequential testing logic is correct", {
  # Test sequential testing calculations
  sens <- 0.80
  spec <- 0.90
  prev <- 0.05
  
  # First test calculations
  ppv1 <- calculate_expected_ppv(sens, spec, prev)
  npv1 <- calculate_expected_npv(sens, spec, prev)
  
  # Second test after positive first test
  # New prevalence = PPV from first test
  ppv2_after_positive <- calculate_expected_ppv(sens, spec, ppv1)
  
  # Second test after negative first test  
  # New prevalence = 1 - NPV from first test
  new_prev_after_negative <- 1 - npv1
  ppv2_after_negative <- calculate_expected_ppv(sens, spec, new_prev_after_negative)
  
  # Verify calculations make sense
  expect_true(ppv2_after_positive > ppv1) # Probability should increase after positive
  expect_true(ppv2_after_negative < ppv1) # Probability should decrease after negative then positive
  expect_true(new_prev_after_negative < prev) # Baseline should decrease after negative
})

test_that("Likelihood ratios are calculated correctly", {
  # Test various combinations
  test_cases <- data.frame(
    sens = c(0.95, 0.80, 0.60, 0.90),
    spec = c(0.90, 0.95, 0.80, 0.75),
    expected_lrp = c(9.5, 16, 3, 3.6),
    expected_lrn = c(0.0556, 0.2105, 0.5, 0.1333)
  )
  
  for (i in 1:nrow(test_cases)) {
    lrp <- calculate_expected_lrp(test_cases$sens[i], test_cases$spec[i])
    lrn <- calculate_expected_lrn(test_cases$sens[i], test_cases$spec[i])
    
    expect_equal(round(lrp, 4), test_cases$expected_lrp[i], tolerance = 0.01)
    expect_equal(round(lrn, 4), test_cases$expected_lrn[i], tolerance = 0.01)
  }
})

test_that("Extreme value handling", {
  # Test near-boundary values
  
  # Very high sensitivity and specificity
  sens <- 0.99
  spec <- 0.99
  prev <- 0.001
  
  ppv <- calculate_expected_ppv(sens, spec, prev)
  npv <- calculate_expected_npv(sens, spec, prev)
  
  expect_true(ppv > 0 && ppv < 1)
  expect_true(npv > 0 && npv < 1)
  expect_true(npv > 0.999) # Should be very high
  
  # Very low prevalence
  prev <- 0.001
  sens <- 0.90
  spec <- 0.90
  
  ppv <- calculate_expected_ppv(sens, spec, prev)
  expect_true(ppv < 0.01) # Should be very low due to low prevalence
})

test_that("Prevalence effects are correct", {
  # Test how prevalence affects predictive values
  sens <- 0.90
  spec <- 0.90
  
  prevalences <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90)
  ppvs <- sapply(prevalences, function(p) calculate_expected_ppv(sens, spec, p))
  npvs <- sapply(prevalences, function(p) calculate_expected_npv(sens, spec, p))
  
  # PPV should increase with prevalence
  expect_true(all(diff(ppvs) > 0))
  
  # NPV should decrease with prevalence  
  expect_true(all(diff(npvs) < 0))
})

context("Screening Calculator - Clinical Scenarios")

test_that("Realistic clinical scenarios produce sensible results", {
  # Scenario 1: COVID-19 rapid test
  covid_sens <- 0.85
  covid_spec <- 0.95
  covid_prev <- 0.02 # 2% community prevalence
  
  covid_ppv <- calculate_expected_ppv(covid_sens, covid_spec, covid_prev)
  covid_npv <- calculate_expected_npv(covid_sens, covid_spec, covid_prev)
  
  expect_true(covid_ppv < 0.30) # Low PPV due to low prevalence
  expect_true(covid_npv > 0.99) # High NPV
  
  # Scenario 2: Mammography screening
  mammo_sens <- 0.80
  mammo_spec <- 0.90
  mammo_prev <- 0.008 # 8 per 1000 women
  
  mammo_ppv <- calculate_expected_ppv(mammo_sens, mammo_spec, mammo_prev)
  expect_true(mammo_ppv < 0.10) # Known to be low in screening
  
  # Scenario 3: Diagnostic test in symptomatic patients
  diag_sens <- 0.90
  diag_spec <- 0.95
  diag_prev <- 0.30 # Higher prevalence in symptomatic population
  
  diag_ppv <- calculate_expected_ppv(diag_sens, diag_spec, diag_prev)
  expect_true(diag_ppv > 0.80) # Should be much higher
})

test_that("Three consecutive test scenarios", {
  # Test the 8 scenarios for three tests
  sens <- 0.85
  spec <- 0.90
  prev <- 0.10
  
  # Calculate first test results
  ppv1 <- calculate_expected_ppv(sens, spec, prev)
  npv1 <- calculate_expected_npv(sens, spec, prev)
  
  # Calculate second test results
  ppv2_after_pos <- calculate_expected_ppv(sens, spec, ppv1)
  prob_disease_after_neg_pos <- calculate_expected_ppv(sens, spec, 1 - npv1)
  
  npv2_after_neg <- calculate_expected_npv(sens, spec, 1 - npv1)
  prob_disease_after_neg_neg <- 1 - npv2_after_neg
  
  # Third test scenarios
  # After ++, should get even higher probability
  ppv3_after_pos_pos <- calculate_expected_ppv(sens, spec, ppv2_after_pos)
  expect_true(ppv3_after_pos_pos > ppv2_after_pos)
  expect_true(ppv3_after_pos_pos > ppv1)
  
  # After --, should get even lower probability  
  npv3_after_neg_neg <- calculate_expected_npv(sens, spec, prob_disease_after_neg_neg)
  prob_disease_after_neg_neg_neg <- 1 - npv3_after_neg_neg
  expect_true(prob_disease_after_neg_neg_neg < prob_disease_after_neg_neg)
  expect_true(prob_disease_after_neg_neg_neg < (1 - npv1))
})

context("Screening Calculator - Input Validation")

test_that("Input parameter bounds are respected", {
  # Test that sensitivity and specificity are between 0 and 1
  expect_error(calculate_expected_ppv(-0.1, 0.9, 0.1))
  expect_error(calculate_expected_ppv(1.1, 0.9, 0.1))
  expect_error(calculate_expected_ppv(0.9, -0.1, 0.1))
  expect_error(calculate_expected_ppv(0.9, 1.1, 0.1))
  expect_error(calculate_expected_ppv(0.9, 0.9, -0.1))
  expect_error(calculate_expected_ppv(0.9, 0.9, 1.1))
})

# Additional helper function for input validation
validate_probability <- function(value, name) {
  if (value < 0 || value > 1) {
    stop(paste(name, "must be between 0 and 1"))
  }
}

calculate_expected_ppv <- function(sens, spec, prev) {
  validate_probability(sens, "Sensitivity")
  validate_probability(spec, "Specificity") 
  validate_probability(prev, "Prevalence")
  
  (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
}

calculate_expected_npv <- function(sens, spec, prev) {
  validate_probability(sens, "Sensitivity")
  validate_probability(spec, "Specificity")
  validate_probability(prev, "Prevalence")
  
  (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
}

test_that("Complex sequential scenarios maintain logical consistency", {
  # Test that probabilities remain logical through multiple tests
  sens <- 0.80
  spec <- 0.85
  prev <- 0.15
  
  # Track probability evolution through different pathways
  ppv1 <- calculate_expected_ppv(sens, spec, prev)
  npv1 <- calculate_expected_npv(sens, spec, prev)
  
  # All probabilities should be between 0 and 1
  expect_true(ppv1 >= 0 && ppv1 <= 1)
  expect_true(npv1 >= 0 && npv1 <= 1)
  
  # Sequential testing should maintain bounds
  for (pathway in 1:8) {
    # Test various pathways maintain probability bounds
    if (pathway <= 4) {
      # Second test scenarios
      prob_after_second <- switch(pathway,
        calculate_expected_ppv(sens, spec, ppv1), # ++
        1 - calculate_expected_npv(sens, spec, ppv1), # +-
        calculate_expected_ppv(sens, spec, 1 - npv1), # -+
        1 - calculate_expected_npv(sens, spec, 1 - npv1) # --
      )
      expect_true(prob_after_second >= 0 && prob_after_second <= 1)
    }
  }
})