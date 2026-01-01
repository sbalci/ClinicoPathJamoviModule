
test_that('survivalvalidation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    predicted_risk = runif(n, 1, 100),
    cause_specific = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- survivalvalidation(
      data = data,
    time = 'time',
    status = 'status',
    predicted_risk = 'predicted_risk',
    validation_method = 'cv',
    cv_folds = 10,
    bootstrap_samples = 100,
    concordance_index = TRUE,
    time_dependent_auc = TRUE,
    prediction_error = TRUE,
    integrated_brier = TRUE,
    calibration_plot = TRUE,
    decision_curve = TRUE,
    max_time = 0,
    plot_roc_curves = TRUE,
    plot_calibration = TRUE,
    plot_decision_curve = TRUE,
    plot_prediction_error = TRUE,
    confidence_level = 0.95,
    smoothing = TRUE,
    risk_groups = 4,
    competing_risks = FALSE,
    cause_specific = 'cause_specific',
    model_comparison = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'survivalvalidation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

