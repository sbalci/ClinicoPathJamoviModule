
test_that('illnessdeath analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_entry = runif(n, 1, 100),
    time_exit = runif(n, 1, 100),
    state_from = sample(c('A', 'B'), n, replace = TRUE),
    state_to = sample(c('A', 'B'), n, replace = TRUE),
    subject_id = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- illnessdeath(
      data = data,
    time_entry = 'time_entry',
    time_exit = 'time_exit',
    state_from = 'state_from',
    state_to = 'state_to',
    subject_id = 'subject_id',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    model_structure = 'standard',
    transition_specific = FALSE,
    baseline_hazard = 'nonparametric',
    common_baseline = FALSE,
    time_varying_covariates = c('time_varying_covariates1', 'time_varying_covariates2', 'time_varying_covariates3'),
    time_varying_effects = FALSE,
    estimation_method = 'partial',
    tie_method = 'breslow',
    prediction_horizon = 10,
    transition_probabilities = TRUE,
    sojourn_times = TRUE,
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    confidence_level = 0.95,
    goodness_of_fit = FALSE,
    residual_analysis = FALSE,
    cross_validation = FALSE,
    show_transition_summary = TRUE,
    show_hazard_ratios = TRUE,
    show_state_probabilities = TRUE,
    show_sojourn_analysis = TRUE,
    plot_transition_diagram = TRUE,
    plot_state_probabilities = TRUE,
    plot_transition_hazards = TRUE,
    plot_cumulative_incidence = FALSE,
    plot_sojourn_distributions = FALSE,
    plot_residuals = FALSE,
    left_truncation = FALSE,
    interval_censoring = FALSE,
    competing_mortality = FALSE,
    stratification_variable = 'stratification_variable',
    frailty_model = FALSE,
    penalization_parameter = 0,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'illnessdeath.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

