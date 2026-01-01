
test_that('continuousmarkov analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    subject = sample(c('A', 'B'), n, replace = TRUE),
    time = runif(n, 1, 100),
    state = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    time_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    time_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    time_covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- continuousmarkov(
      data = data,
    subject = 'subject',
    time = 'time',
    state = 'state',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    time_covariates = c('time_covariates1', 'time_covariates2', 'time_covariates3'),
    model_structure = 'full',
    baseline_hazard = 'piecewise',
    n_intervals = 5,
    initial_values = FALSE,
    optimization_method = 'BFGS',
    calculate_sojourn = TRUE,
    transition_probabilities = TRUE,
    prevalence_estimates = TRUE,
    confidence_intervals = TRUE,
    bootstrap_se = FALSE,
    n_bootstrap = 100,
    plot_intensities = TRUE,
    plot_probabilities = TRUE,
    plot_prevalence = TRUE,
    model_selection = FALSE,
    goodness_of_fit = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'continuousmarkov.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

