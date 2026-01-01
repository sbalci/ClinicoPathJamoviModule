
test_that('flexmultistate analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    start_state = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    shared_effects1 = sample(c('A', 'B'), n, replace = TRUE),
    shared_effects2 = sample(c('A', 'B'), n, replace = TRUE),
    shared_effects3 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_effects1 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_effects2 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_effects3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- flexmultistate(
      data = data,
    time = 'time',
    status = 'status',
    start_state = 'start_state',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    model_type = 'illness_death',
    hazard_distribution = 'royston_parmar',
    spline_knots = 3,
    time_scale = 'hazard',
    transition_specific = TRUE,
    shared_effects = c('shared_effects1', 'shared_effects2', 'shared_effects3'),
    time_varying_effects = c('time_varying_effects1', 'time_varying_effects2', 'time_varying_effects3'),
    transition_probabilities = TRUE,
    state_probabilities = TRUE,
    expected_sojourn = TRUE,
    plot_hazards = TRUE,
    plot_cumhaz = TRUE,
    plot_survival = TRUE,
    microsimulation = FALSE,
    n_simulation = 10000,
    confidence_intervals = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'flexmultistate.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

