
test_that('biomarkerdiscovery analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome_var = sample(c('A', 'B'), n, replace = TRUE),
    biomarker_vars1 = runif(n, 1, 100),
    biomarker_vars2 = runif(n, 1, 100),
    biomarker_vars3 = runif(n, 1, 100),
    clinical_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    batch_var = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = runif(n, 1, 100),
    event_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- biomarkerdiscovery(
      data = data,
    outcome_var = 'outcome_var',
    biomarker_vars = c('biomarker_vars1', 'biomarker_vars2', 'biomarker_vars3'),
    clinical_vars = c('clinical_vars1', 'clinical_vars2', 'clinical_vars3'),
    batch_var = 'batch_var',
    patient_id = 'patient_id',
    time_var = 'time_var',
    event_var = 'event_var',
    discovery_method = 'elastic_net',
    outcome_type = 'binary',
    data_type = 'genomics',
    data_preprocessing = TRUE,
    normalization_method = 'z_score',
    batch_correction = FALSE,
    batch_method = 'combat',
    filter_low_variance = TRUE,
    variance_threshold = 0.1,
    feature_selection_method = 'univariate_stats',
    n_features_select = 50,
    fdr_threshold = 0.05,
    correlation_threshold = 0.9,
    validation_method = 'cv_10fold',
    train_proportion = 0.7,
    hyperparameter_tuning = TRUE,
    n_bootstrap_samples = 1000,
    random_seed = 42,
    biomarker_ranking = TRUE,
    stability_analysis = TRUE,
    clinical_performance = TRUE,
    signature_development = TRUE,
    pathway_analysis = FALSE,
    interpretability = TRUE,
    shap_analysis = TRUE,
    lime_analysis = FALSE,
    feature_interaction = TRUE,
    partial_dependence = TRUE,
    biomarker_networks = FALSE,
    cutpoint_optimization = TRUE,
    risk_stratification = TRUE,
    nomogram_development = TRUE,
    decision_curve_analysis = TRUE,
    external_validation = TRUE,
    biomarker_generalizability = TRUE,
    robustness_testing = TRUE,
    quality_control = TRUE,
    outlier_detection = TRUE,
    missing_data_analysis = TRUE,
    detailed_results = TRUE,
    biomarker_report = TRUE,
    export_biomarkers = FALSE,
    save_signature = FALSE,
    regulatory_documentation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'biomarkerdiscovery.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

