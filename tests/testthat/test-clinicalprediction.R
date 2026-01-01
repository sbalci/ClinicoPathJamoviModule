
test_that('clinicalprediction analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome_var = sample(c('A', 'B'), n, replace = TRUE),
    predictor_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    predictor_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    predictor_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = runif(n, 1, 100),
    event_var = sample(c('A', 'B'), n, replace = TRUE),
    stratify_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalprediction(
      data = data,
    outcome_var = 'outcome_var',
    predictor_vars = c('predictor_vars1', 'predictor_vars2', 'predictor_vars3'),
    patient_id = 'patient_id',
    time_var = 'time_var',
    event_var = 'event_var',
    stratify_var = 'stratify_var',
    model_type = 'random_forest',
    problem_type = 'classification',
    outcome_type = 'binary',
    feature_selection = TRUE,
    selection_method = 'recursive_fe',
    feature_engineering = TRUE,
    handle_missing = 'mice_imputation',
    train_proportion = 0.7,
    validation_method = 'cv_10fold',
    hyperparameter_tuning = TRUE,
    tuning_method = 'random_search',
    random_seed = 42,
    interpretability = TRUE,
    shap_analysis = TRUE,
    lime_analysis = FALSE,
    permutation_importance = TRUE,
    partial_dependence = TRUE,
    individual_explanations = FALSE,
    n_explanations = 10,
    performance_metrics = TRUE,
    calibration_analysis = TRUE,
    clinical_metrics = TRUE,
    roc_analysis = TRUE,
    threshold_optimization = TRUE,
    compare_models = FALSE,
    baseline_models = TRUE,
    ensemble_models = FALSE,
    risk_stratification = TRUE,
    n_risk_groups = 3,
    nomogram = TRUE,
    decision_curve = TRUE,
    external_validation = TRUE,
    bootstrap_ci = 1000,
    stability_analysis = TRUE,
    bias_analysis = TRUE,
    detailed_output = TRUE,
    clinical_report = TRUE,
    save_model = FALSE,
    export_predictions = FALSE,
    regulatory_documentation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicalprediction.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

