
test_that('bayesianclinical analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome_var = sample(c('A', 'B'), n, replace = TRUE),
    treatment_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = runif(n, 1, 100),
    center_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- bayesianclinical(
      data = data,
    outcome_var = 'outcome_var',
    treatment_var = 'treatment_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    patient_id = 'patient_id',
    time_var = 'time_var',
    center_var = 'center_var',
    analysis_type = 'treatment_effect',
    outcome_type = 'continuous',
    prior_type = 'weakly_informative',
    prior_mean = 0,
    prior_sd = 1,
    historical_data_weight = 0.1,
    mcmc_chains = 4,
    mcmc_iterations = 2000,
    mcmc_warmup = 1000,
    mcmc_thinning = 1,
    credible_interval = 0.95,
    rope_lower = -0.1,
    rope_upper = 0.1,
    minimum_effect_size = 0.2,
    decision_analysis = FALSE,
    benefit_utility = 1,
    harm_utility = -0.5,
    cost_consideration = FALSE,
    model_diagnostics = TRUE,
    convergence_diagnostics = TRUE,
    posterior_predictive_checks = TRUE,
    leave_one_out_cv = FALSE,
    hierarchical_modeling = FALSE,
    mixture_modeling = FALSE,
    nonparametric_methods = FALSE,
    adaptive_design = FALSE,
    model_comparison = FALSE,
    bayes_factor_analysis = TRUE,
    evidence_thresholds = TRUE,
    probability_statements = TRUE,
    prediction_intervals = TRUE,
    sensitivity_to_priors = FALSE,
    posterior_plots = TRUE,
    trace_plots = TRUE,
    forest_plots = FALSE,
    decision_plots = FALSE,
    comprehensive_report = TRUE,
    clinical_interpretation = TRUE,
    regulatory_documentation = FALSE,
    layman_summary = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'bayesianclinical.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

