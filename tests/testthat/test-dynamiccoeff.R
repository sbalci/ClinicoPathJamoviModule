
test_that('dynamiccoeff analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    dynamic_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    dynamic_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    dynamic_covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- dynamiccoeff(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    dynamic_covariates = c('dynamic_covariates1', 'dynamic_covariates2', 'dynamic_covariates3'),
    updating_method = 'kalman',
    state_dimension = 2,
    process_variance = 0.1,
    observation_variance = 0.1,
    forgetting_factor = 0.99,
    burn_in_period = 10,
    confidence_level = 0.95,
    smoothing_parameter = 0.2,
    adaptation_rate = 0.1,
    show_model_summary = TRUE,
    show_coefficient_paths = TRUE,
    show_state_evolution = TRUE,
    show_adaptation_metrics = TRUE,
    show_dynamic_plots = TRUE,
    show_state_plots = TRUE,
    show_diagnostic_plots = TRUE,
    show_comparison_plots = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'dynamiccoeff.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

