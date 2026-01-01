
test_that('bayesiannetworkma analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    study_id = sample(c('A', 'B'), n, replace = TRUE),
    treatment = sample(c('A', 'B'), n, replace = TRUE),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    sample_size = runif(n, 1, 100),
    standard_error = runif(n, 1, 100),
    control_rate = runif(n, 1, 100),
    meta_regression_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    meta_regression_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    meta_regression_covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    treatment_classes = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- bayesiannetworkma(
      data = data,
    study_id = 'study_id',
    treatment = 'treatment',
    outcome = 'outcome',
    sample_size = 'sample_size',
    standard_error = 'standard_error',
    control_rate = 'control_rate',
    network_type = 'mixed_evidence',
    outcome_type = 'binary_or',
    baseline_model = 'exchangeable',
    random_effects_model = 'random_univariate',
    heterogeneity_model = 'common',
    correlation_structure = 'unstructured',
    treatment_effect_prior = 'vague_normal',
    heterogeneity_prior = 'half_normal',
    prior_mean_effect = 0,
    prior_sd_effect = 10,
    prior_heterogeneity_sd = 1,
    assess_coherence = TRUE,
    coherence_method = 'node_splitting',
    inconsistency_threshold = 0.05,
    mcmc_samples = 10000,
    mcmc_burnin = 5000,
    mcmc_thin = 2,
    mcmc_chains = 3,
    ranking_analysis = TRUE,
    ranking_measure = 'sucra',
    pairwise_probabilities = TRUE,
    superiority_threshold = 0,
    model_selection = TRUE,
    model_comparison_criteria = 'dic',
    network_plot = TRUE,
    network_layout = 'spring',
    edge_weights = 'n_studies',
    show_network_summary = TRUE,
    show_treatment_effects = TRUE,
    show_heterogeneity_analysis = TRUE,
    show_coherence_results = TRUE,
    show_ranking_results = TRUE,
    show_forest_plots = TRUE,
    show_league_table = TRUE,
    show_convergence_diagnostics = TRUE,
    show_interpretation = TRUE,
    meta_regression_covariates = c('meta_regression_covariates1', 'meta_regression_covariates2', 'meta_regression_covariates3'),
    treatment_classes = 'treatment_classes',
    multi_arm_correction = TRUE,
    zero_events_correction = 0.5,
    clinical_context = 'therapeutic',
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
  omv_path <- file.path('omv_output', 'bayesiannetworkma.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

