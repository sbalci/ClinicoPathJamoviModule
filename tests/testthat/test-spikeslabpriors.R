
test_that('spikeslabpriors analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    time_variable = runif(n, 1, 100),
    status_variable = sample(c('A', 'B'), n, replace = TRUE),
    group_structure = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- spikeslabpriors(
      data = data,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    time_variable = 'time_variable',
    status_variable = 'status_variable',
    model_type = 'regression',
    spike_slab_type = 'binary',
    prior_inclusion_prob = 0.5,
    expected_model_size = 5,
    spike_variance = 0.001,
    slab_variance = 1,
    hyperprior_type = 'empirical_bayes',
    hyperprior_alpha = 1,
    hyperprior_beta = 1,
    selection_criterion = 'median_probability',
    inclusion_threshold = 0.5,
    bayes_factor_threshold = 3,
    mcmc_samples = 10000,
    mcmc_burnin = 5000,
    mcmc_thin = 1,
    mcmc_chains = 3,
    model_averaging = TRUE,
    prediction_method = 'bma',
    cross_validation = FALSE,
    cv_folds = 10,
    standardize_predictors = TRUE,
    center_predictors = TRUE,
    max_model_size = 50,
    dimension_reduction = 'none',
    prescreening_threshold = 0.1,
    show_variable_selection = TRUE,
    show_inclusion_probabilities = TRUE,
    show_model_probabilities = TRUE,
    show_coefficient_estimates = TRUE,
    show_prediction_performance = TRUE,
    show_convergence_diagnostics = TRUE,
    show_variable_importance = TRUE,
    show_model_comparison = FALSE,
    show_interpretation = TRUE,
    interaction_terms = FALSE,
    max_interactions = 2,
    heredity_constraint = 'weak',
    group_variables = FALSE,
    group_structure = 'group_structure',
    clinical_context = 'biomarker_discovery',
    confidence_level = 0.95,
    set_seed = TRUE,
    seed_value = 42,
    parallel_processing = TRUE,
    n_cores = 1
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'spikeslabpriors.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

