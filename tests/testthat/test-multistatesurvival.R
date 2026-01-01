
test_that('multistatesurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    id = sample(c('A', 'B'), n, replace = TRUE),
    time_start = runif(n, 1, 100),
    time_stop = runif(n, 1, 100),
    state_from = sample(c('A', 'B'), n, replace = TRUE),
    state_to = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratified = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- multistatesurvival(
      data = data,
    id = 'id',
    time_start = 'time_start',
    time_stop = 'time_stop',
    state_from = 'state_from',
    state_to = 'state_to',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    model_type = 'markov',
    transition_probabilities = TRUE,
    state_probabilities = TRUE,
    sojourn_times = TRUE,
    hazard_ratios = TRUE,
    plot_transitions = TRUE,
    plot_probabilities = TRUE,
    plot_cumhazard = TRUE,
    plot_individual = FALSE,
    competing_risks = TRUE,
    time_varying = FALSE,
    stratified = 'stratified',
    confidence_level = 0.95,
    bootstrap_ci = FALSE,
    n_bootstrap = 1000
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'multistatesurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

