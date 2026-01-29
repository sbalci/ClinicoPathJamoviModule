# Test simulation validation integration
library(testthat)

test_that("Simulation validation integrates correctly with sample size calculation", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("survival")
  
  result <- survivalPower(
    analysis_type = "sample_size",
    test_type = "log_rank",
    survival_distribution = "exponential",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12,
    run_simulation_validation = TRUE,
    simulation_runs = 1000  # Reduced for speed
  )
  
  # Check that simulation table exists and has data
  expect_true(result$simulation_validation_table$rowCount > 0)
  
  # Check that simulated power is reasonably close to analytical (80%)
  sim_row <- result$simulation_validation_table$asDF[1, ]
  expect_equal(sim_row$metric, "Statistical Power")
  expect_lt(abs(sim_row$analytical - sim_row$simulated), 0.10)  # Within 10%
  expect_true(!is.na(sim_row$ci_lower))
  expect_true(!is.na(sim_row$ci_upper))
  expect_true(nchar(sim_row$agreement) > 0)
})

test_that("Simulation validation works with Weibull distribution", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("survival")
  
  result <- survivalPower(
    analysis_type = "sample_size",
    test_type = "log_rank",
    survival_distribution = "weibull",
    weibull_shape = 1.5,
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12,
    run_simulation_validation = TRUE,
    simulation_runs = 1000
  )
  
  expect_true(result$simulation_validation_table$rowCount > 0)
  sim_row <- result$simulation_validation_table$asDF[1, ]
  expect_equal(sim_row$metric, "Statistical Power")
})

test_that("Simulation validation works with power calculation", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("survival")
  
  result <- survivalPower(
    analysis_type = "power",
    test_type = "log_rank",
    survival_distribution = "exponential",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    sample_size_input = 200,
    accrual_period = 24,
    follow_up_period = 12,
    run_simulation_validation = TRUE,
    simulation_runs = 1000
  )
  
  expect_true(result$simulation_validation_table$rowCount > 0)
})

test_that("Simulation validation is skipped when not requested", {
  result <- survivalPower(
    analysis_type = "sample_size",
    test_type = "log_rank",
    survival_distribution = "exponential",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12,
    run_simulation_validation = FALSE
  )
  
  # Table should exist but be empty
  expect_equal(result$simulation_validation_table$rowCount, 0)
})
