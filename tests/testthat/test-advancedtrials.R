
test_that('advancedtrials analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    event_var = sample(c('A', 'B'), n, replace = TRUE),
    treatment_var = sample(c('A', 'B'), n, replace = TRUE),
    biomarker_var = sample(c('A', 'B'), n, replace = TRUE),
    interim_time_var = runif(n, 1, 100),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- advancedtrials(
      data = data,
    time_var = 'time_var',
    event_var = 'event_var',
    treatment_var = 'treatment_var',
    biomarker_var = 'biomarker_var',
    interim_time_var = 'interim_time_var',
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    design_type = 'group_sequential',
    primary_endpoint = 'overall_survival',
    statistical_test = 'log_rank',
    alpha_spending = 'obrien_fleming',
    beta_spending = 'obrien_fleming',
    number_of_looks = 3,
    adaptation_type = 'sample_size_reestimation',
    adaptation_timing = 50,
    conditional_power_threshold = 0.3,
    ni_margin = 1.25,
    ni_margin_type = 'hazard_ratio',
    overall_alpha = 0.025,
    overall_power = 0.8,
    expected_hr = 0.75,
    accrual_rate = 25,
    dropout_rate = 0.05,
    number_of_arms = 2,
    control_arm_sharing = TRUE,
    interim_arm_addition = FALSE,
    interim_arm_dropping = FALSE,
    biomarker_strategy = 'all_comers',
    biomarker_prevalence = 0.3,
    futility_analysis = TRUE,
    stochastic_curtailment = FALSE,
    predictive_power_threshold = 0.1,
    interim_monitoring_plan = TRUE,
    operating_characteristics = TRUE,
    boundary_plots = TRUE,
    conditional_power_analysis = FALSE,
    predictive_power_analysis = FALSE,
    bias_adjustment = FALSE,
    multiplicity_adjustment = FALSE,
    simulation_runs = 10000,
    seed_value = 12345
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'advancedtrials.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

