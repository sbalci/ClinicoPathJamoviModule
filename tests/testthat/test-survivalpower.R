test_that("survivalpower function exists and basic functionality works", {
  
  # Test function existence
  expect_true(exists("survivalpower"))
  expect_true(is.function(survivalpower))
  
  # Test with minimal valid parameters - should not error
  expect_no_error({
    result <- survivalpower(
      data = data.frame(),
      calculation_type = "sample_size",
      method = "lachin_foulkes",
      hazard_control = 0.083,
      hazard_treatment = 0.042
    )
  })
})

test_that("survivalpower Lachin-Foulkes sample size calculation works", {
  skip_if_not_installed("gsDesign")
  
  # Test sample size calculation with Lachin-Foulkes method
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "lachin_foulkes",
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    study_duration = 24,
    accrual_duration = 12,
    alpha = 0.025,
    beta = 0.1
  )
  
  # Basic checks
  expect_s3_class(result, "survivalpowerResults")
  expect_true("power_results" %in% names(result))
  expect_true("instructions" %in% names(result))
  
  # Check that power results are visible
  expect_true(result$power_results$visible)
})

test_that("survivalpower Lachin-Foulkes power calculation works", {
  skip_if_not_installed("gsDesign")
  
  # Test power calculation with given sample size
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "power",
    method = "lachin_foulkes",
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    study_duration = 24,
    accrual_duration = 12,
    sample_size_input = 200,
    alpha = 0.025
  )
  
  # Basic checks
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower Schoenfeld sample size calculation works", {
  skip_if_not_installed("gsDesign")
  
  # Test sample size calculation with Schoenfeld method
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 1
  )
  
  # Basic checks
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower Schoenfeld power calculation works", {
  skip_if_not_installed("gsDesign")
  
  # Test power calculation with Schoenfeld method
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "power",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    events_input = 100,
    alpha = 0.025,
    allocation_ratio = 1
  )
  
  # Basic checks
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower Schoenfeld events calculation works", {
  skip_if_not_installed("gsDesign")
  
  # Test events calculation
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 1
  )
  
  # Basic checks
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower Schoenfeld hazard ratio detection works", {
  skip_if_not_installed("gsDesign")
  
  # Test detectable hazard ratio calculation
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "hazard_ratio",
    method = "schoenfeld",
    events_input = 100,
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 1
  )
  
  # Basic checks
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower handles different allocation ratios", {
  skip_if_not_installed("gsDesign")
  
  # Test equal allocation (ratio = 1)
  result_equal <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 1
  )
  expect_s3_class(result_equal, "survivalpowerResults")
  
  # Test unequal allocation (ratio = 2)
  result_unequal <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 2
  )
  expect_s3_class(result_unequal, "survivalpowerResults")
})

test_that("survivalpower handles protective effects (HR < 1)", {
  skip_if_not_installed("gsDesign")
  
  # Test protective effect
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.5,  # Strong protective effect
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 1
  )
  
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower handles harmful effects (HR > 1)", {
  skip_if_not_installed("gsDesign")
  
  # Test harmful effect (need to adjust for one-sided test typically used in survival)
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 1.5,  # Harmful effect
    alpha = 0.025,
    beta = 0.1,
    allocation_ratio = 1,
    sided = "2"  # Use two-sided test for harmful effects
  )
  
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower handles different entry patterns", {
  skip_if_not_installed("gsDesign")
  
  # Test uniform entry
  result_unif <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "lachin_foulkes",
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    study_duration = 24,
    accrual_duration = 12,
    entry_type = "unif"
  )
  expect_s3_class(result_unif, "survivalpowerResults")
  
  # Test exponential entry
  result_expo <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "lachin_foulkes",
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    study_duration = 24,
    accrual_duration = 12,
    entry_type = "expo",
    gamma = 1
  )
  expect_s3_class(result_expo, "survivalpowerResults")
})

test_that("survivalpower handles different test types", {
  skip_if_not_installed("gsDesign")
  
  # Test one-sided test
  result_one <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    sided = "1"
  )
  expect_s3_class(result_one, "survivalpowerResults")
  
  # Test two-sided test
  result_two <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.6,
    sided = "2"
  )
  expect_s3_class(result_two, "survivalpowerResults")
})

test_that("survivalpower parameter validation works", {
  # Test with parameters at boundary values
  expect_no_error({
    survivalpower(
      data = data.frame(),
      calculation_type = "sample_size",
      method = "schoenfeld",
      hazard_ratio = 0.1,  # Very low but valid HR
      alpha = 0.001,       # Low but valid alpha
      beta = 0.01          # Low but valid beta
    )
  })
  
  expect_no_error({
    survivalpower(
      data = data.frame(),
      calculation_type = "sample_size",
      method = "schoenfeld",
      hazard_ratio = 10,    # High but valid HR
      alpha = 0.1,         # High but valid alpha
      beta = 0.5           # High but valid beta
    )
  })
})

test_that("survivalpower handles dropout rates", {
  skip_if_not_installed("gsDesign")
  
  # Test with dropout
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "lachin_foulkes",
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    study_duration = 24,
    accrual_duration = 12,
    dropout_rate = 0.05  # 5% dropout rate
  )
  
  expect_s3_class(result, "survivalpowerResults")
  expect_true(result$power_results$visible)
})

test_that("survivalpower show/hide options work", {
  skip_if_not_installed("gsDesign")
  
  # Test with all display options enabled
  result <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "lachin_foulkes",
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    show_summary = TRUE,
    show_formulas = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "survivalpowerResults")
  expect_true("formulas" %in% names(result))
  expect_true("interpretation" %in% names(result))
  expect_true(result$formulas$visible)
  expect_true(result$interpretation$visible)
})

test_that("survivalpower missing package handling", {
  # This test verifies error handling when gsDesign is not available
  # We can't easily test this without unloading the package,
  # but we ensure the error handling mechanism exists
  expect_true(exists("survivalpower"))
})

test_that("survivalpower edge cases", {
  skip_if_not_installed("gsDesign")
  
  # Test very small effect size
  expect_no_error({
    survivalpower(
      data = data.frame(),
      calculation_type = "sample_size",
      method = "schoenfeld",
      hazard_ratio = 0.99,  # Very small effect
      alpha = 0.025,
      beta = 0.1
    )
  })
  
  # Test very high power requirement
  expect_no_error({
    survivalpower(
      data = data.frame(),
      calculation_type = "sample_size",
      method = "schoenfeld",
      hazard_ratio = 0.6,
      alpha = 0.025,
      beta = 0.01  # 99% power
    )
  })
  
  # Test very short study duration
  expect_no_error({
    survivalpower(
      data = data.frame(),
      calculation_type = "sample_size",
      method = "lachin_foulkes",
      hazard_control = 0.5,    # High event rate
      hazard_treatment = 0.25, # for short studies
      study_duration = 6,      # Short duration
      accrual_duration = 3
    )
  })
})

test_that("survivalpower clinical scenarios", {
  skip_if_not_installed("gsDesign")
  
  # Oncology trial scenario - survival endpoint
  result_oncology <- survivalpower(
    data = data.frame(),
    calculation_type = "sample_size",
    method = "lachin_foulkes",
    hazard_control = 0.083,   # ~12 month median survival
    hazard_treatment = 0.042, # ~24 month median survival
    study_duration = 36,      # 3 year study
    accrual_duration = 24,    # 2 year accrual
    alpha = 0.025,            # One-sided 2.5%
    beta = 0.1                # 90% power
  )
  expect_s3_class(result_oncology, "survivalpowerResults")
  
  # Cardiology trial scenario - quick events-based calculation
  result_cardio <- survivalpower(
    data = data.frame(),
    calculation_type = "events",
    method = "schoenfeld",
    hazard_ratio = 0.75,      # 25% risk reduction
    alpha = 0.025,
    beta = 0.2,               # 80% power
    allocation_ratio = 1      # 1:1 randomization
  )
  expect_s3_class(result_cardio, "survivalpowerResults")
})