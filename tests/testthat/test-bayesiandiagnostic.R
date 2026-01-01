
test_that('bayesiandiagnostic analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    test_results = sample(c('A', 'B'), n, replace = TRUE),
    gold_standard = sample(c('A', 'B'), n, replace = TRUE),
    study_id = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    comparison_test = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- bayesiandiagnostic(
      data = data,
    test_results = 'test_results',
    gold_standard = 'gold_standard',
    study_id = 'study_id',
    patient_id = 'patient_id',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    analysis_type = 'single_test',
    comparison_test = 'comparison_test',
    prior_type = 'informative',
    prior_sensitivity_mean = 0.8,
    prior_sensitivity_precision = 10,
    prior_specificity_mean = 0.9,
    prior_specificity_precision = 10,
    correlation_prior = 0,
    bivariate_model = TRUE,
    hierarchical_model = FALSE,
    covariate_effects = FALSE,
    heterogeneity_model = 'random',
    mcmc_samples = 10000,
    mcmc_burnin = 5000,
    mcmc_thin = 1,
    mcmc_chains = 3,
    mcmc_adapt = TRUE,
    convergence_criterion = 1.1,
    compute_sensitivity = TRUE,
    compute_specificity = TRUE,
    compute_ppv = TRUE,
    compute_npv = TRUE,
    compute_likelihood_ratios = TRUE,
    compute_diagnostic_odds_ratio = TRUE,
    compute_accuracy = TRUE,
    compute_auc = FALSE,
    prevalence_prior = 0.5,
    decision_analysis = FALSE,
    cost_fn = 1,
    cost_fp = 1,
    utility_tp = 1,
    utility_tn = 1,
    threshold_analysis = FALSE,
    threshold_metric = 'youden',
    show_summary_statistics = TRUE,
    show_posterior_distributions = TRUE,
    show_credible_intervals = TRUE,
    show_convergence_diagnostics = TRUE,
    show_model_comparison = FALSE,
    show_predictive_checks = FALSE,
    show_clinical_interpretation = TRUE,
    credible_interval = 0.95,
    hpdi = TRUE,
    set_seed = TRUE,
    seed_value = 42,
    parallel_chains = TRUE,
    n_cores = 1,
    robust_estimation = FALSE,
    outlier_detection = FALSE,
    publication_bias = FALSE,
    sensitivity_analysis = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'bayesiandiagnostic.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

