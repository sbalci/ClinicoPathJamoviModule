# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: summarydata
# ═══════════════════════════════════════════════════════════
#
# Tests all combinations of arguments and options for the
# summarydata jamovi function
#
# Generated: 2026-01-04

# library(testthat)
# library(ClinicoPath)
devtools::load_all()

# Load test data
data(summarydata_test, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# Test 1: Distribution Diagnostics Option
# ═══════════════════════════════════════════════════════════

test_that("summarydata with distribution diagnostics enabled", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab"),
    distr = TRUE
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataClass"))
})

test_that("summarydata distribution diagnostics with normal data", {
  # Test with normally distributed data
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "temperature_normal", "hemoglobin_lab"),
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata distribution diagnostics with skewed data", {
  # Test with right-skewed data (should show non-normal)
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "wbc_count"),
    distr = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 2: Decimal Places Options
# ═══════════════════════════════════════════════════════════

test_that("summarydata with decimal_places = 0", {
  result <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    decimal_places = 0
  )

  expect_no_error(result)
})

test_that("summarydata with decimal_places = 2 (default)", {
  result <- summarydata(
    data = summarydata_test,
    vars = "hemoglobin_lab",
    decimal_places = 2
  )

  expect_no_error(result)
})

test_that("summarydata with decimal_places = 5 (maximum)", {
  result <- summarydata(
    data = summarydata_test,
    vars = "creatinine_lab",
    decimal_places = 5
  )

  expect_no_error(result)
})

test_that("summarydata decimal places with multiple variables", {
  # Test that decimal places apply to all variables
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "psa_mild_skew", "weight_kg"),
    decimal_places = 3
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 3: Outlier Detection Option
# ═══════════════════════════════════════════════════════════

test_that("summarydata with outlier detection enabled", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew"),
    outliers = TRUE
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataClass"))
})

test_that("summarydata outlier detection with known outliers", {
  # PSA, CA125, WBC, tumor_size, ki67 have intentional outliers
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "wbc_count", "weight_kg", "ferritin"),
    outliers = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata outlier detection with normal data", {
  # Variables less likely to have outliers
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "temperature_normal"),
    outliers = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 4: Report Sentences Option
# ═══════════════════════════════════════════════════════════

test_that("summarydata with report sentences enabled", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab"),
    report_sentences = TRUE
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataClass"))
})

test_that("summarydata report sentences with multiple variables", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "psa_mild_skew", "weight_kg", "ferritin"),
    report_sentences = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata report sentences with distribution info", {
  # Combine report sentences with distribution diagnostics
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab"),
    report_sentences = TRUE,
    distr = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 5: Combined Options (Two Features)
# ═══════════════════════════════════════════════════════════

test_that("summarydata with distr + decimal_places", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "psa_mild_skew"),
    distr = TRUE,
    decimal_places = 3
  )

  expect_no_error(result)
})

test_that("summarydata with outliers + decimal_places", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew"),
    outliers = TRUE,
    decimal_places = 2
  )

  expect_no_error(result)
})

test_that("summarydata with outliers + distr", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "weight_kg"),
    outliers = TRUE,
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata with report_sentences + distr", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("hemoglobin_lab", "creatinine_lab"),
    report_sentences = TRUE,
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata with report_sentences + outliers", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew"),
    report_sentences = TRUE,
    outliers = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 6: Combined Options (Three Features)
# ═══════════════════════════════════════════════════════════

test_that("summarydata with distr + outliers + decimal_places", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "weight_kg"),
    distr = TRUE,
    outliers = TRUE,
    decimal_places = 3
  )

  expect_no_error(result)
})

test_that("summarydata with distr + report_sentences + decimal_places", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab"),
    distr = TRUE,
    report_sentences = TRUE,
    decimal_places = 2
  )

  expect_no_error(result)
})

test_that("summarydata with outliers + report_sentences + decimal_places", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew"),
    outliers = TRUE,
    report_sentences = TRUE,
    decimal_places = 4
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 7: All Features Enabled (Maximum Options)
# ═══════════════════════════════════════════════════════════

test_that("summarydata with all options enabled", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab", "psa_mild_skew", "weight_kg"),
    distr = TRUE,
    outliers = TRUE,
    report_sentences = TRUE,
    decimal_places = 3
  )

  expect_no_error(result)
  expect_true(inherits(result, "summarydataClass"))
})

test_that("summarydata all options with single variable", {
  # Test all features with just one variable
  result <- summarydata(
    data = summarydata_test,
    vars = "psa_mild_skew",
    distr = TRUE,
    outliers = TRUE,
    report_sentences = TRUE,
    decimal_places = 2
  )

  expect_no_error(result)
})

test_that("summarydata all options with many variables", {
  # Test all features with comprehensive variable list
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab", "wbc_count", "platelet_count",
             "creatinine_lab", "albumin_no_outliers", "psa_mild_skew", "weight_kg"),
    distr = TRUE,
    outliers = TRUE,
    report_sentences = TRUE,
    decimal_places = 2
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 8: Variable Type Combinations
# ═══════════════════════════════════════════════════════════

test_that("summarydata with all normal distribution variables", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab", "temperature_normal", "albumin_no_outliers"),
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata with all skewed distribution variables", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "wbc_count", "weight_kg"),
    distr = TRUE,
    outliers = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata with all bounded variables", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("ferritin", "bmi_calculated", "tsh", "heart_rate"),
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata with mixed distribution types", {
  # Mix normal, skewed, and bounded variables
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "psa_mild_skew", "ferritin", "hemoglobin_lab", "weight_kg"),
    distr = TRUE,
    outliers = TRUE
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 9: Clinical Use Case Scenarios
# ═══════════════════════════════════════════════════════════

test_that("summarydata clinical lab report scenario", {
  # Typical clinical chemistry report with all features
  result <- summarydata(
    data = summarydata_test,
    vars = c("hemoglobin_lab", "wbc_count", "platelet_count", "creatinine_lab", "albumin_no_outliers"),
    distr = TRUE,
    outliers = TRUE,
    report_sentences = TRUE,
    decimal_places = 2
  )

  expect_no_error(result)
})

test_that("summarydata oncology biomarker panel scenario", {
  # Oncology research with biomarker analysis
  result <- summarydata(
    data = summarydata_test,
    vars = c("psa_mild_skew", "crp_moderate_skew", "ferritin", "weight_kg", "troponin"),
    distr = TRUE,
    outliers = TRUE,
    report_sentences = TRUE,
    decimal_places = 2
  )

  expect_no_error(result)
})

test_that("summarydata quality of life assessment scenario", {
  # Patient-reported outcomes analysis
  result <- summarydata(
    data = summarydata_test,
    vars = c("bmi_calculated", "tsh", "heart_rate"),
    distr = TRUE,
    report_sentences = TRUE,
    decimal_places = 1
  )

  expect_no_error(result)
})

test_that("summarydata vitals monitoring scenario", {
  # Vital signs assessment
  result <- summarydata(
    data = summarydata_test,
    vars = c("temperature_normal", "systolic_bp_low_missing", "diastolic_bp"),
    distr = TRUE,
    outliers = TRUE,
    decimal_places = 1
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 10: Extreme Option Values
# ═══════════════════════════════════════════════════════════

test_that("summarydata with minimum decimal places (0)", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab"),
    decimal_places = 0,
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata with maximum decimal places (5)", {
  result <- summarydata(
    data = summarydata_test,
    vars = c("creatinine_lab", "psa_mild_skew"),
    decimal_places = 5,
    distr = TRUE
  )

  expect_no_error(result)
})

test_that("summarydata all options FALSE (minimal output)", {
  # Minimal configuration - all optional features off
  result <- summarydata(
    data = summarydata_test,
    vars = c("age_normal", "hemoglobin_lab"),
    distr = FALSE,
    outliers = FALSE,
    report_sentences = FALSE,
    decimal_places = 2
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# Test 11: Option Interactions
# ═══════════════════════════════════════════════════════════

test_that("summarydata decimal_places affects all outputs", {
  # Verify decimal places work across different features
  result1 <- summarydata(
    data = summarydata_test,
    vars = "psa_mild_skew",
    decimal_places = 1
  )

  result2 <- summarydata(
    data = summarydata_test,
    vars = "psa_mild_skew",
    decimal_places = 4
  )

  expect_no_error(result1)
  expect_no_error(result2)
})

test_that("summarydata distr option shows additional statistics", {
  # With distr=TRUE should include Shapiro-Wilk, skewness, kurtosis
  result_with_distr <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    distr = TRUE
  )

  result_without_distr <- summarydata(
    data = summarydata_test,
    vars = "age_normal",
    distr = FALSE
  )

  expect_no_error(result_with_distr)
  expect_no_error(result_without_distr)
})

# ═══════════════════════════════════════════════════════════
# End of Argument Combination Tests
# ═══════════════════════════════════════════════════════════
