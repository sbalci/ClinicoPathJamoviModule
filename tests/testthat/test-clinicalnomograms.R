
test_that('clinicalnomograms analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    status_var = sample(c('A', 'B'), n, replace = TRUE),
    outcome_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalnomograms(
      data = data,
    time_var = 'time_var',
    status_var = 'status_var',
    outcome_var = 'outcome_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    nomogram_type = 'survival_nomogram',
    model_selection = 'all_variables',
    confidence_level = 0.95,
    validation_method = 'bootstrap',
    bootstrap_samples = 100,
    cv_folds = 10,
    points_scale = 100,
    calibration_assessment = TRUE,
    discrimination_assessment = TRUE,
    decision_curve_analysis = TRUE,
    risk_groups = TRUE,
    interactive_nomogram = TRUE,
    risk_calculator = TRUE,
    clinical_scenarios = TRUE,
    model_equation = TRUE,
    confidence_intervals = TRUE,
    performance_metrics = TRUE,
    variable_importance = TRUE,
    sensitivity_analysis = FALSE,
    missing_data_handling = 'complete_case',
    export_formats = TRUE,
    reporting_guidelines = 'tripod',
    clinical_implementation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicalnomograms.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

