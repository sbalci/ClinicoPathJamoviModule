
test_that('adaptivetrialdesign analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    treatment = sample(c('A', 'B'), n, replace = TRUE),
    stratification_variables1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_variables2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_variables3 = sample(c('A', 'B'), n, replace = TRUE),
    time_variable = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- adaptivetrialdesign(
      data = data,
    outcome = 'outcome',
    treatment = 'treatment',
    stratification_variables = c('stratification_variables1', 'stratification_variables2', 'stratification_variables3'),
    time_variable = 'time_variable',
    adaptation_type = 'sample_size',
    design_framework = 'bayesian',
    interim_analysis = TRUE,
    interim_timing = 'information',
    max_interim_analyses = 3,
    efficacy_boundary = 0.95,
    futility_boundary = 0.1,
    boundary_type = 'obrien_fleming',
    planned_sample_size = 100,
    minimum_effect_size = 0.5,
    target_power = 0.8,
    type1_error_rate = 0.05,
    max_sample_size_inflation = 2,
    prior_type = 'weakly_informative',
    historical_data_weight = 0.1,
    decision_criterion = 'posterior_probability',
    bayes_factor_threshold = 3,
    predictive_power_threshold = 0.8,
    run_simulations = TRUE,
    n_simulations = 100,
    mcmc_samples = 5000,
    mcmc_burnin = 2000,
    mcmc_chains = 3,
    show_design_summary = TRUE,
    show_interim_results = TRUE,
    show_stopping_boundaries = TRUE,
    show_sample_size_evolution = TRUE,
    show_operating_characteristics = TRUE,
    show_posterior_evolution = TRUE,
    show_decision_analysis = TRUE,
    show_interpretation = TRUE,
    alpha_spending_function = 'obf_type',
    spending_parameter = 1,
    blinded_sample_size_reestimation = FALSE,
    conditional_power_threshold = 0.2,
    clinical_context = 'general',
    set_seed = TRUE,
    seed_value = 42,
    regulatory_framework = 'general',
    dmb_recommendations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'adaptivetrialdesign.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

