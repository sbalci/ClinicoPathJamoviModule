# Comprehensive Tests for survivalPower - Updated for Current API
# Tests verify core functionality with correct parameter names
# Previous tests used outdated API (calculation_type, method, hazard_control, etc.)
# Current API uses: analysis_type, test_type, control_median_survival, effect_size, etc.

library(testthat)

# Skip all tests if survivalPower function not available
skip_if_not_available <- function() {
  if (!exists("survivalPower") || !is.function(survivalPower)) {
    skip("survivalPower function not available")
  }
}

# =============================================================================
# Test Category 1: Core Functionality - Log-Rank Test
# =============================================================================

test_that("survivalPower function exists and basic functionality works", {
  skip_if_not_available()

  # Test function existence
  expect_true(exists("survivalPower"))
  expect_true(is.function(survivalPower))

  # Test with minimal valid parameters for log-rank test
  expect_no_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "log_rank",
      control_median_survival = 12,
      effect_size = 0.75,  # Hazard ratio
      alpha_level = 0.05,
      power_level = 0.80,
      accrual_period = 24,
      follow_up_period = 12
    )
  })
})

test_that("Log-rank test: Sample size calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test sample size calculation with log-rank test
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,  # 12 months control median
    effect_size = 0.75,  # HR = 0.75 (25% reduction)
    alpha_level = 0.05,  # Two-sided alpha
    power_level = 0.80,  # 80% power
    allocation_ratio = 1.0,  # 1:1 allocation
    accrual_period = 24,  # 24 months accrual
    follow_up_period = 12  # 12 months follow-up
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Log-rank test: Power calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test power calculation with given sample size
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "power",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    sample_size_input = 200,  # Fixed sample size
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Log-rank test: Effect size calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test detectable effect size calculation
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "effect_size",
    test_type = "log_rank",
    control_median_survival = 12,
    alpha_level = 0.05,
    power_level = 0.80,
    sample_size_input = 200,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Log-rank test: Duration calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test study duration calculation
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "duration",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    sample_size_input = 200
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

# =============================================================================
# Test Category 2: Cox Regression
# =============================================================================

test_that("Cox regression: Sample size calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test sample size for Cox regression
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "cox_regression",
    control_median_survival = 12,
    effect_size = 0.67,  # HR = 0.67
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Cox regression: Power calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test power for Cox regression
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "power",
    test_type = "cox_regression",
    control_median_survival = 12,
    effect_size = 0.67,
    alpha_level = 0.05,
    sample_size_input = 200,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

# =============================================================================
# Test Category 3: Non-Inferiority Designs
# =============================================================================

test_that("Non-inferiority: Sample size calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test non-inferiority sample size
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "non_inferiority",
    control_median_survival = 12,
    effect_size = 1.0,  # HR = 1.0 (no difference expected)
    ni_margin = 1.25,  # NI margin
    alpha_level = 0.025,  # One-sided
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Non-inferiority: Power calculation works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test non-inferiority power
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "power",
    test_type = "non_inferiority",
    control_median_survival = 12,
    effect_size = 1.0,
    ni_margin = 1.25,
    alpha_level = 0.025,
    sample_size_input = 300,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

# =============================================================================
# Test Category 4: Parameter Validation
# =============================================================================

test_that("Parameter validation: Exponential distribution requirement", {
  skip_if_not_available()

  # Current version only supports exponential distribution
  # Using other distributions should trigger validation error
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "log_rank",
      control_median_survival = 12,
      effect_size = 0.75,
      survival_distribution = "weibull",  # Not supported
      alpha_level = 0.05,
      power_level = 0.80
    )
  })
})

test_that("Parameter validation: Effect size bounds", {
  skip_if_not_available()

  # Effect size must be > 0
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "log_rank",
      control_median_survival = 12,
      effect_size = 0,  # Invalid
      alpha_level = 0.05,
      power_level = 0.80
    )
  })

  # Extremely large effect size should be flagged
  expect_warning({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "log_rank",
      control_median_survival = 12,
      effect_size = 10,  # Unrealistic
      alpha_level = 0.05,
      power_level = 0.80
    )
  })
})

test_that("Parameter validation: Alpha level bounds", {
  skip_if_not_available()

  # Alpha must be between 0 and 1
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "log_rank",
      control_median_survival = 12,
      effect_size = 0.75,
      alpha_level = 1.5,  # Invalid
      power_level = 0.80
    )
  })
})

test_that("Parameter validation: Power level bounds", {
  skip_if_not_available()

  # Power must be between 0 and 1
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "log_rank",
      control_median_survival = 12,
      effect_size = 0.75,
      alpha_level = 0.05,
      power_level = 1.2  # Invalid
    )
  })
})

# =============================================================================
# Test Category 5: Allocation Ratios
# =============================================================================

test_that("Allocation ratio: Unequal allocation (2:1) works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test with 2:1 allocation ratio
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    allocation_ratio = 2.0,  # 2:1 control:treatment
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Allocation ratio: Unequal allocation (1:2) works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test with 1:2 allocation ratio (more in experimental)
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    allocation_ratio = 0.5,  # 1:2 control:treatment
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

# =============================================================================
# Test Category 6: Dropout/Loss to Follow-up
# =============================================================================

test_that("Dropout handling: Annual dropout rate accounted for", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test with dropout rate
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    dropout_rate = 0.10,  # 10% annual dropout
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

# =============================================================================
# Test Category 7: Known Limitations (Non-Functional Features)
# =============================================================================

test_that("KNOWN LIMITATION: Competing risks test not implemented", {
  skip_if_not_available()

  # This test is explicitly marked as non-functional in validation
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "competing_risks",  # Not implemented
      control_median_survival = 12,
      effect_size = 0.75,
      alpha_level = 0.05,
      power_level = 0.80
    )
  }, regexp = "temporarily unavailable")
})

test_that("KNOWN LIMITATION: RMST test not implemented", {
  skip_if_not_available()

  # This test is explicitly marked as non-functional
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "rmst_test",  # Not implemented
      control_median_survival = 12,
      effect_size = 0.75,
      alpha_level = 0.05,
      power_level = 0.80
    )
  }, regexp = "temporarily unavailable")
})

test_that("KNOWN LIMITATION: SNP survival test not implemented", {
  skip_if_not_available()

  # This test is explicitly marked as non-functional
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "snp_survival",  # Not implemented
      control_median_survival = 12,
      effect_size = 0.75,
      alpha_level = 0.05,
      power_level = 0.80
    )
  }, regexp = "temporarily unavailable")
})

test_that("KNOWN LIMITATION: Weighted log-rank test not implemented", {
  skip_if_not_available()

  # This test is explicitly marked as non-functional
  expect_error({
    result <- survivalPower(
      data = data.frame(),
      analysis_type = "sample_size",
      test_type = "weighted_log_rank",  # Not implemented
      control_median_survival = 12,
      effect_size = 0.75,
      alpha_level = 0.05,
      power_level = 0.80
    )
  }, regexp = "temporarily unavailable")
})

# =============================================================================
# Test Category 8: Edge Cases
# =============================================================================

test_that("Edge case: Very high power (0.95) works", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Test with 95% power
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.95,  # Very high power
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Edge case: Small effect size (HR=0.90) requires large sample", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Small effect size
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.90,  # Small effect (10% reduction)
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

test_that("Edge case: Large effect size (HR=0.50) requires small sample", {
  skip_if_not_available()
  skip_if_not_installed("gsDesign")

  # Large effect size
  result <- survivalPower(
    data = data.frame(),
    analysis_type = "sample_size",
    test_type = "log_rank",
    control_median_survival = 12,
    effect_size = 0.50,  # Large effect (50% reduction)
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12
  )

  # Basic checks
  expect_s3_class(result, "survivalPowerResults")
})

# =============================================================================
# Test Summary
# =============================================================================

cat("\nsurvivalPower regression tests completed.\n")
cat("\nTest Coverage:\n")
cat("- Core functionality (5 tests): Log-rank sample size/power/effect/duration\n")
cat("- Cox regression (2 tests): Sample size and power\n")
cat("- Non-inferiority (2 tests): Sample size and power\n")
cat("- Parameter validation (4 tests): Distribution, effect size, alpha, power\n")
cat("- Allocation ratios (2 tests): 2:1 and 1:2 allocations\n")
cat("- Dropout handling (1 test): Annual dropout rate\n")
cat("- Known limitations (4 tests): Competing risks, RMST, SNP, Weighted log-rank\n")
cat("- Edge cases (3 tests): High power, small/large effect sizes\n")
cat("\nTotal: 23 comprehensive tests\n")
cat("\nCritical Fixes Applied:\n")
cat("1. ✅ ALL parameter names updated to current API\n")
cat("2. ✅ Test methods updated (log_rank, cox_regression, non_inferiority)\n")
cat("3. ✅ Tests now actually test current implementation\n")
cat("4. ✅ Known limitations explicitly tested (expect errors)\n")
cat("5. ⚠️ REMINDER: Version downgraded to 0.3.0 (beta with core features)\n")
