
test_that('treatmenteffects analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    treatment_var = sample(c('A', 'B'), n, replace = TRUE),
    outcome_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = runif(n, 1, 100),
    event_var = sample(c('A', 'B'), n, replace = TRUE),
    cluster_var = sample(c('A', 'B'), n, replace = TRUE),
    effect_modifiers1 = sample(c('A', 'B'), n, replace = TRUE),
    effect_modifiers2 = sample(c('A', 'B'), n, replace = TRUE),
    effect_modifiers3 = sample(c('A', 'B'), n, replace = TRUE),
    instrument_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- treatmenteffects(
      data = data,
    treatment_var = 'treatment_var',
    outcome_var = 'outcome_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    patient_id = 'patient_id',
    time_var = 'time_var',
    event_var = 'event_var',
    cluster_var = 'cluster_var',
    causal_method = 'propensity_score',
    outcome_type = 'continuous',
    estimand = 'ate',
    ps_method = 'logistic',
    ps_specification = 'main_effects',
    balance_threshold = 0.1,
    matching_method = 'nearest_neighbor',
    matching_ratio = '1to1',
    caliper_width = 0.2,
    replacement_matching = FALSE,
    weight_trimming = TRUE,
    trim_quantiles = 0.05,
    stabilized_weights = TRUE,
    weight_normalization = 'sum_to_n',
    outcome_model = 'linear',
    include_ps_in_outcome = TRUE,
    balance_assessment = TRUE,
    balance_methods = 'standardized_differences',
    overlap_assessment = TRUE,
    sensitivity_analysis = TRUE,
    sensitivity_method = 'rosenbaum_bounds',
    heterogeneous_effects = FALSE,
    effect_modifiers = c('effect_modifiers1', 'effect_modifiers2', 'effect_modifiers3'),
    causal_tree = FALSE,
    causal_forest = FALSE,
    instrument_var = 'instrument_var',
    iv_method = 'two_stage_least_squares',
    model_diagnostics = TRUE,
    bootstrap_inference = TRUE,
    n_bootstrap = 1000,
    cross_validation = FALSE,
    comprehensive_report = TRUE,
    individual_effects = FALSE,
    save_weights = FALSE,
    save_matched_data = FALSE,
    regulatory_documentation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'treatmenteffects.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

