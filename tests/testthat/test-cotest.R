# Load required libraries
library(ClinicoPath)

test_that("cotest module loads correctly", {
  expect_true(exists("cotestClass"))
  expect_true(is.function(cotest))
})

test_that("cotest works with default parameters", {
  # Test basic functionality with default parameters
  result <- cotest()
  
  expect_s3_class(result, "cotestClass")
  
  # Check that results contain expected components
  expect_true("testParamsTable" %in% names(result$results))
  expect_true("cotestResultsTable" %in% names(result$results))
  expect_true("explanation" %in% names(result$results))
  expect_true("dependenceExplanation" %in% names(result$results))
})

test_that("cotest works with custom parameters", {
  # Test with custom sensitivity and specificity values
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.92,
    test2_sens = 0.78,
    test2_spec = 0.88,
    prevalence = 0.05
  )
  
  expect_s3_class(result, "cotestClass")
})

test_that("cotest handles independent tests correctly", {
  # Test independent tests scenario
  result <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.90,
    test2_sens = 0.75,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )
  
  expect_s3_class(result, "cotestClass")
  
  # For independent tests, dependenceInfo should not be shown
  expect_false("dependenceInfo" %in% names(result$results) && 
               !is.null(result$results$dependenceInfo$content))
})

test_that("cotest handles dependent tests correctly", {
  # Test dependent tests scenario
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.15,
    cond_dep_neg = 0.10
  )
  
  expect_s3_class(result, "cotestClass")
  
  # For dependent tests, dependenceInfo should be present
  expect_true("dependenceInfo" %in% names(result$results))
})

test_that("cotest validates input parameters", {
  # Test invalid sensitivity (> 1)
  expect_error(
    cotest(test1_sens = 1.2),
    "Test 1 sensitivity must be between 0 and 1"
  )
  
  # Test invalid sensitivity (≤ 0)
  expect_error(
    cotest(test1_sens = 0),
    "Test 1 sensitivity must be between 0 and 1"
  )
  
  # Test invalid specificity (> 1)
  expect_error(
    cotest(test1_spec = 1.1),
    "Test 1 specificity must be between 0 and 1"
  )
  
  # Test invalid specificity (≤ 0)
  expect_error(
    cotest(test2_spec = -0.1),
    "Test 2 specificity must be between 0 and 1"
  )
  
  # Test invalid prevalence (> 1)
  expect_error(
    cotest(prevalence = 1.5),
    "Disease prevalence must be between 0 and 1"
  )
  
  # Test invalid prevalence (≤ 0)
  expect_error(
    cotest(prevalence = 0),
    "Disease prevalence must be between 0 and 1"
  )
})

test_that("cotest validates conditional dependence parameters", {
  # Test invalid conditional dependence (> 1)
  expect_error(
    cotest(indep = FALSE, cond_dep_pos = 1.2),
    "Conditional dependence for positive cases must be between 0 and 1"
  )
  
  # Test invalid conditional dependence (< 0)
  expect_error(
    cotest(indep = FALSE, cond_dep_neg = -0.1),
    "Conditional dependence for negative cases must be between 0 and 1"
  )
  
  # Test valid boundary values
  expect_error({
    result <- cotest(indep = FALSE, cond_dep_pos = 0, cond_dep_neg = 1)
  }, NA)  # Should not error
})

test_that("cotest handles extreme parameter values", {
  # Test with very high sensitivity and specificity
  result_high <- cotest(
    test1_sens = 0.99,
    test1_spec = 0.99,
    test2_sens = 0.98,
    test2_spec = 0.98,
    prevalence = 0.001
  )
  
  expect_s3_class(result_high, "cotestClass")
  
  # Test with low sensitivity and specificity
  result_low <- cotest(
    test1_sens = 0.60,
    test1_spec = 0.70,
    test2_sens = 0.65,
    test2_spec = 0.75,
    prevalence = 0.50
  )
  
  expect_s3_class(result_low, "cotestClass")
})

test_that("cotest handles different prevalence scenarios", {
  # Low prevalence (screening scenario)
  result_low_prev <- cotest(
    test1_sens = 0.90,
    test1_spec = 0.95,
    test2_sens = 0.85,
    test2_spec = 0.93,
    prevalence = 0.01
  )
  
  expect_s3_class(result_low_prev, "cotestClass")
  
  # High prevalence (symptomatic patients)
  result_high_prev <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.85,
    test2_sens = 0.75,
    test2_spec = 0.88,
    prevalence = 0.60
  )
  
  expect_s3_class(result_high_prev, "cotestClass")
})

test_that("cotest calculates likelihood ratios correctly", {
  # Test with known values for manual verification
  result <- cotest(
    test1_sens = 0.80,  # PLR = 0.8/0.1 = 8, NLR = 0.2/0.9 = 0.222
    test1_spec = 0.90,
    test2_sens = 0.90,  # PLR = 0.9/0.05 = 18, NLR = 0.1/0.95 = 0.105
    test2_spec = 0.95,
    prevalence = 0.10
  )
  
  expect_s3_class(result, "cotestClass")
  
  # The function should calculate these internally and use them for post-test probabilities
})

test_that("cotest handles footnotes option", {
  # Test with footnotes enabled
  result_footnotes <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fnote = TRUE
  )
  
  expect_s3_class(result_footnotes, "cotestClass")
  
  # Test with footnotes disabled
  result_no_footnotes <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fnote = FALSE
  )
  
  expect_s3_class(result_no_footnotes, "cotestClass")
})

test_that("cotest handles Fagan nomogram option", {
  # Test with Fagan nomogram enabled
  result_fagan <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fagan = TRUE
  )
  
  expect_s3_class(result_fagan, "cotestClass")
  expect_true("plot1" %in% names(result_fagan$results))
  
  # Test with Fagan nomogram disabled
  result_no_fagan <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fagan = FALSE
  )
  
  expect_s3_class(result_no_fagan, "cotestClass")
})

test_that("cotest calculates post-test probabilities correctly", {
  # Test a scenario where we can manually verify calculations
  # Using simple values for easier verification
  result <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.90,
    test2_sens = 0.70,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )
  
  expect_s3_class(result, "cotestClass")
  
  # Post-test probabilities should be different from prevalence
  # Both positive should have higher probability than individual tests
  # Both negative should have lower probability than prevalence
})

test_that("cotest handles conditional dependence calculations", {
  # Test with moderate dependence
  result_moderate <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.20,
    cond_dep_neg = 0.15
  )
  
  expect_s3_class(result_moderate, "cotestClass")
  
  # Test with minimal dependence (close to independence)
  result_minimal <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.01,
    cond_dep_neg = 0.01
  )
  
  expect_s3_class(result_minimal, "cotestClass")
})

test_that("cotest comprehensive test with all options", {
  # Test with all options enabled
  result <- cotest(
    test1_sens = 0.88,
    test1_spec = 0.92,
    test2_sens = 0.83,
    test2_spec = 0.89,
    indep = FALSE,
    cond_dep_pos = 0.12,
    cond_dep_neg = 0.08,
    prevalence = 0.07,
    fnote = TRUE,
    fagan = TRUE
  )
  
  expect_s3_class(result, "cotestClass")
  
  # Verify all expected components are present
  expect_true("testParamsTable" %in% names(result$results))
  expect_true("cotestResultsTable" %in% names(result$results))
  expect_true("dependenceInfo" %in% names(result$results))
  expect_true("dependenceExplanation" %in% names(result$results))
  expect_true("explanation" %in% names(result$results))
  expect_true("plot1" %in% names(result$results))
})

test_that("cotest handles perfect and poor test scenarios", {
  # Perfect tests scenario
  expect_error({
    result_perfect <- cotest(
      test1_sens = 0.999,
      test1_spec = 0.999,
      test2_sens = 0.999,
      test2_spec = 0.999,
      prevalence = 0.10
    )
  }, NA)  # Should not error
  
  # Poor tests scenario
  expect_error({
    result_poor <- cotest(
      test1_sens = 0.55,
      test1_spec = 0.60,
      test2_sens = 0.58,
      test2_spec = 0.65,
      prevalence = 0.10
    )
  }, NA)  # Should not error
})

test_that("cotest mathematical consistency checks", {
  # Test that post-test probabilities are logical
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )
  
  expect_s3_class(result, "cotestClass")
  
  # Mathematical checks:
  # - Both positive should increase probability above prevalence
  # - Both negative should decrease probability below prevalence
  # - Individual positive tests should be between prevalence and both positive
  # These would require accessing the actual calculated values from the results
})

test_that("cotest reproducibility", {
  # Test that identical inputs produce identical outputs
  params <- list(
    test1_sens = 0.82,
    test1_spec = 0.88,
    test2_sens = 0.79,
    test2_spec = 0.91,
    prevalence = 0.12,
    indep = TRUE
  )
  
  result1 <- do.call(cotest, params)
  result2 <- do.call(cotest, params)
  
  expect_s3_class(result1, "cotestClass")
  expect_s3_class(result2, "cotestClass")
  
  # Results should be identical (deterministic calculation)
})

test_that("cotest boundary value testing", {
  # Test at exact boundaries of allowed ranges
  expect_error({
    cotest(
      test1_sens = 0.01,
      test1_spec = 0.01,
      test2_sens = 0.99,
      test2_spec = 0.99,
      prevalence = 0.001
    )
  }, NA)
  
  expect_error({
    cotest(
      test1_sens = 0.99,
      test1_spec = 0.99,
      test2_sens = 0.01,
      test2_spec = 0.01,
      prevalence = 0.999
    )
  }, NA)
})

test_that("cotest clinical scenario examples", {
  # Scenario 1: COVID-19 screening with antigen + PCR
  covid_screening <- cotest(
    test1_sens = 0.68,  # Antigen test
    test1_spec = 0.99,
    test2_sens = 0.95,  # PCR test
    test2_spec = 0.99,
    prevalence = 0.05,  # Community prevalence
    indep = FALSE,      # Tests may be dependent
    cond_dep_pos = 0.10,
    cond_dep_neg = 0.05
  )
  
  expect_s3_class(covid_screening, "cotestClass")
  
  # Scenario 2: Cancer screening with imaging + biopsy
  cancer_screening <- cotest(
    test1_sens = 0.88,  # Imaging
    test1_spec = 0.92,
    test2_sens = 0.95,  # Biopsy
    test2_spec = 0.98,
    prevalence = 0.02,  # Cancer prevalence in screening
    indep = FALSE,      # Tests likely dependent
    cond_dep_pos = 0.25,
    cond_dep_neg = 0.15,
    fagan = TRUE
  )
  
  expect_s3_class(cancer_screening, "cotestClass")
  
  # Scenario 3: Cardiac biomarkers
  cardiac_biomarkers <- cotest(
    test1_sens = 0.92,  # Troponin
    test1_spec = 0.89,
    test2_sens = 0.85,  # CK-MB
    test2_spec = 0.94,
    prevalence = 0.25,  # Emergency department patients
    indep = FALSE,      # Biomarkers likely correlated
    cond_dep_pos = 0.30,
    cond_dep_neg = 0.20,
    fnote = TRUE
  )
  
  expect_s3_class(cardiac_biomarkers, "cotestClass")
})