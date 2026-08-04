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

test_that("Non-exponential distributions are refused, not silently mis-computed", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("survival")

  # The event-probability and sample-size formulas assume a constant hazard, so
  # this release blocks every other distribution (.validate_inputs). A blocked
  # run must produce no number at all AND say why -- returning an exponential
  # answer under a Weibull label would be the dangerous failure mode.
  for (dist in c("weibull", "log_normal", "piecewise_exponential")) {
    result <- survivalPower(
      analysis_type = "sample_size",
      test_type = "log_rank",
      survival_distribution = dist,
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

    expect_true(is.na(result$power_summary$asDF$calculated_value[1]),
                info = paste(dist, "must not report a sample size"))
    expect_equal(result$simulation_validation_table$rowCount, 0)
    expect_match(as.character(result$notices$content), "Distribution Not Supported",
                 info = paste(dist, "must explain why it was blocked"))
  }
})

test_that("Exponential remains selectable and unaffected by the distribution gate", {
  skip_if_not_installed("gsDesign")

  result <- survivalPower(
    analysis_type = "sample_size",
    test_type = "log_rank",
    survival_distribution = "exponential",
    control_median_survival = 12,
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    accrual_period = 24,
    follow_up_period = 12
  )

  expect_match(result$power_summary$asDF$calculated_value[1], "Total Sample Size")
  expect_false(grepl("Distribution Not Supported",
                     as.character(result$notices$content)))
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
