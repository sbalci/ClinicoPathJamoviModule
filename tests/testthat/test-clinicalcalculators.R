
test_that('clinicalcalculators analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome_variable = sample(c('A', 'B'), n, replace = TRUE),
    predictor_variables1 = sample(c('A', 'B'), n, replace = TRUE),
    predictor_variables2 = sample(c('A', 'B'), n, replace = TRUE),
    predictor_variables3 = sample(c('A', 'B'), n, replace = TRUE),
    time_variable = runif(n, 1, 100),
    event_variable = sample(c('A', 'B'), n, replace = TRUE),
    stratification_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalcalculators(
      data = data,
    calculator_type = 'risk_score',
    outcome_variable = 'outcome_variable',
    predictor_variables = c('predictor_variables1', 'predictor_variables2', 'predictor_variables3'),
    time_variable = 'time_variable',
    event_variable = 'event_variable',
    stratification_variable = 'stratification_variable',
    model_type = 'logistic_regression',
    validation_method = 'bootstrap',
    risk_categories = 3,
    confidence_level = 0.95,
    bootstrap_samples = 100,
    cv_folds = 10,
    include_nomogram = TRUE,
    include_calibration = TRUE,
    include_discrimination = TRUE,
    include_decision_curve = TRUE,
    include_net_benefit = TRUE,
    clinical_threshold_low = 0.1,
    clinical_threshold_high = 0.3,
    feature_selection = FALSE,
    feature_selection_method = 'stepwise',
    regularization_alpha = 1,
    missing_data_method = 'complete_case',
    outlier_detection = FALSE,
    outlier_method = 'iqr',
    include_uncertainty = TRUE,
    interactive_calculator = TRUE,
    export_format = 'html',
    performance_metrics = 'comprehensive',
    include_interpretation = TRUE,
    risk_communication = 'multiple_formats',
    regulatory_compliance = 'none',
    bias_assessment = TRUE,
    subgroup_analysis = FALSE,
    sensitivity_analysis = TRUE,
    implementation_guide = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicalcalculators.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

