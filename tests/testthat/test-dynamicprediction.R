
test_that('dynamicprediction analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    baseline1 = sample(c('A', 'B'), n, replace = TRUE),
    baseline2 = sample(c('A', 'B'), n, replace = TRUE),
    baseline3 = sample(c('A', 'B'), n, replace = TRUE),
    longitudinal1 = runif(n, 1, 100),
    longitudinal2 = runif(n, 1, 100),
    longitudinal3 = runif(n, 1, 100),
    time_var = runif(n, 1, 100),
    subject_id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- dynamicprediction(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    baseline = c('baseline1', 'baseline2', 'baseline3'),
    longitudinal = c('longitudinal1', 'longitudinal2', 'longitudinal3'),
    time_var = 'time_var',
    subject_id = 'subject_id',
    prediction_horizon = 24,
    prediction_method = 'landmark',
    joint_model_type = 'linear_mixed',
    biomarker_model = 'linear',
    association_structure = 'current_value',
    confidence_level = 0.95,
    mc_samples = 500,
    window_width = 6,
    show_prediction_table = TRUE,
    show_accuracy_metrics = TRUE,
    show_biomarker_effects = TRUE,
    show_model_comparison = FALSE,
    prediction_curves = TRUE,
    biomarker_trajectory = TRUE,
    accuracy_plot = FALSE,
    risk_stratification = FALSE,
    showSummaries = TRUE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'dynamicprediction.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

