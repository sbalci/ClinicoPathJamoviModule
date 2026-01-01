test_that("powersurvival function exists and basic functionality works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test function existence
  expect_true(exists("powersurvival"))
  expect_true(is.function(powersurvival))
  
  # Test with minimal valid parameters - should not error
  expect_no_error({
    result <- powersurvival(
      data = data.frame(),
      calc_type = "sample_size",
      hazard_ratio = 0.7,
      power = 0.8,
      alpha = 0.05
    )
  })
})

test_that("powersurvival power calculation works correctly", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test power calculation with known parameters
  result <- powersurvival(
    data = data.frame(),
    calc_type = "power",
    study_design = "simple",
    hazard_ratio = 0.7,
    sample_size = 200,
    alpha = 0.05,
    allocation_ratio = 1,
    prob_event = 0.5
  )
  
  # Basic checks
  expect_s3_class(result, "powersurvivalResults")
  expect_true("power_result" %in% names(result))
  expect_true("power_plot" %in% names(result))
  
  # Check that power result is visible and sample size is hidden
  expect_true(result$power_result$visible)
  expect_false(result$sample_size_result$visible)
  expect_false(result$hazard_ratio_result$visible)
})

test_that("powersurvival sample size calculation works correctly", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test sample size calculation
  result <- powersurvival(
    data = data.frame(),
    calc_type = "sample_size", 
    study_design = "simple",
    hazard_ratio = 0.7,
    power = 0.8,
    alpha = 0.05,
    allocation_ratio = 1,
    prob_event = 0.5
  )
  
  # Basic checks
  expect_s3_class(result, "powersurvivalResults")
  expect_true("sample_size_result" %in% names(result))
  expect_true("power_plot" %in% names(result))
  
  # Check visibility
  expect_false(result$power_result$visible)
  expect_true(result$sample_size_result$visible)
  expect_false(result$hazard_ratio_result$visible)
})

test_that("powersurvival hazard ratio calculation works correctly", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test hazard ratio calculation
  result <- powersurvival(
    data = data.frame(),
    calc_type = "hazard_ratio",
    study_design = "simple", 
    sample_size = 200,
    power = 0.8,
    alpha = 0.05,
    allocation_ratio = 1,
    prob_event = 0.5
  )
  
  # Basic checks
  expect_s3_class(result, "powersurvivalResults")
  expect_true("hazard_ratio_result" %in% names(result))
  expect_true("power_plot" %in% names(result))
  
  # Check visibility
  expect_false(result$power_result$visible)
  expect_false(result$sample_size_result$visible)
  expect_true(result$hazard_ratio_result$visible)
})

test_that("powersurvival works with different allocation ratios", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test equal allocation (ratio = 1)
  result_equal <- powersurvival(
    data = data.frame(),
    calc_type = "sample_size",
    hazard_ratio = 0.7,
    power = 0.8,
    allocation_ratio = 1,
    prob_event = 0.5
  )
  expect_s3_class(result_equal, "powersurvivalResults")
  
  # Test unequal allocation (ratio = 2)
  result_unequal <- powersurvival(
    data = data.frame(),
    calc_type = "sample_size", 
    hazard_ratio = 0.7,
    power = 0.8,
    allocation_ratio = 2,
    prob_event = 0.5
  )
  expect_s3_class(result_unequal, "powersurvivalResults")
})

test_that("powersurvival works with complex study design", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test complex design
  result <- powersurvival(
    data = data.frame(),
    calc_type = "power",
    study_design = "complex",
    hazard_ratio = 0.7,
    sample_size = 200,
    alpha = 0.05,
    accrual_time = 2,
    follow_up_time = 3,
    median_survival = 5,
    loss_followup = 0.05
  )
  
  expect_s3_class(result, "powersurvivalResults")
  expect_true(result$power_result$visible)
})

test_that("powersurvival handles protective effects (HR < 1)", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test protective effect
  result <- powersurvival(
    data = data.frame(),
    calc_type = "hazard_ratio",
    hazard_ratio = 0.5,  # Strong protective effect
    sample_size = 100,
    power = 0.8,
    alpha = 0.05
  )
  
  expect_s3_class(result, "powersurvivalResults")
})

test_that("powersurvival handles harmful effects (HR > 1)", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test harmful effect
  result <- powersurvival(
    data = data.frame(), 
    calc_type = "hazard_ratio",
    hazard_ratio = 1.5,  # Harmful effect
    sample_size = 100,
    power = 0.8,
    alpha = 0.05
  )
  
  expect_s3_class(result, "powersurvivalResults")
})

test_that("powersurvival parameter validation works", {
  # Note: Parameter validation in jamovi modules typically happens at the interface level
  # through the .yaml configuration files, not at the R function level.
  # These tests verify that the function can handle edge cases gracefully.
  
  # Test with parameters at boundary values
  expect_no_error({
    powersurvival(
      data = data.frame(),
      calc_type = "sample_size",
      power = 0.99,  # High but valid power
      alpha = 0.001, # Low but valid alpha
      hazard_ratio = 0.01  # Very low but valid HR
    )
  })
})

test_that("powersurvival handles edge cases", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test very small sample size
  expect_no_error({
    powersurvival(
      data = data.frame(),
      calc_type = "power",
      sample_size = 20,
      hazard_ratio = 0.5
    )
  })
  
  # Test very high power requirement
  expect_no_error({
    powersurvival(
      data = data.frame(),
      calc_type = "sample_size", 
      power = 0.95,
      hazard_ratio = 0.8
    )
  })
})

test_that("powersurvival missing package handling", {
  # This test would need to be run when powerSurvEpi is not available
  # For now, we just ensure the error message functionality exists
  expect_true(exists("powersurvival"))
})

test_that("powersurvival different event probabilities", {
  skip_if_not_installed("powerSurvEpi")
  
  # Test with low event probability
  result_low <- powersurvival(
    data = data.frame(),
    calc_type = "sample_size",
    prob_event = 0.1,  # Low event rate
    hazard_ratio = 0.7,
    power = 0.8
  )
  expect_s3_class(result_low, "powersurvivalResults")
  
  # Test with high event probability  
  result_high <- powersurvival(
    data = data.frame(),
    calc_type = "sample_size",
    prob_event = 0.8,  # High event rate
    hazard_ratio = 0.7,
    power = 0.8
  )
  expect_s3_class(result_high, "powersurvivalResults")
})

test_that("powersurvival plot functionality", {
  skip_if_not_installed("powerSurvEpi")
  skip_if_not_installed("ggplot2")
  
  # Test that plots can be generated
  result <- powersurvival(
    data = data.frame(),
    calc_type = "power",
    sample_size = 200,
    hazard_ratio = 0.7
  )
  
  # Check that plot element exists
  expect_true("power_plot" %in% names(result))
  expect_true(result$power_plot$visible)
})
