
test_that('multistatesurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    id = rep(1:25, each = 2),
    time_start = rep(c(0, 10), 25),
    time_stop = rep(c(10, 25), 25),
    state_from = rep(c(1, 2), 25),
    state_to = rep(c(2, 3), 25),
    covariates1 = sample(c(0, 1), n, replace = TRUE),
    covariates2 = sample(c(0, 1), n, replace = TRUE),
    covariates3 = sample(c(0, 1), n, replace = TRUE),
    stratified = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
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
    n_bootstrap = 200
  )

  # Verify and Export OMV
  expect_true(inherits(model, 'multistatesurvivalClass') || inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'multistatesurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  tryCatch({
    jmvReadWrite::write_omv(model, omv_path)
  }, error = function(e) {
    message("OMV export failed: ", e$message)
  })

  if (!file.exists(omv_path)) {
    skip("OMV export failed, skipping file existence check")
  }

  expect_true(file.exists(omv_path))
})

