
test_that('mlpathology analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    actual_labels = sample(c('A', 'B'), n, replace = TRUE),
    predicted_labels = sample(c('A', 'B'), n, replace = TRUE),
    predicted_probabilities = runif(n, 1, 100),
    reference_segmentation = sample(c('A', 'B'), n, replace = TRUE),
    predicted_segmentation = sample(c('A', 'B'), n, replace = TRUE),
    model1_predictions = sample(c('A', 'B'), n, replace = TRUE),
    model2_predictions = sample(c('A', 'B'), n, replace = TRUE),
    model1_probabilities = runif(n, 1, 100),
    model2_probabilities = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- mlpathology(
      data = data,
    analysis_type = 'classification',
    actual_labels = 'actual_labels',
    predicted_labels = 'predicted_labels',
    predicted_probabilities = 'predicted_probabilities',
    reference_segmentation = 'reference_segmentation',
    predicted_segmentation = 'predicted_segmentation',
    model1_predictions = 'model1_predictions',
    model2_predictions = 'model2_predictions',
    model1_probabilities = 'model1_probabilities',
    model2_probabilities = 'model2_probabilities',
    roc_analysis = TRUE,
    roc_comparison = FALSE,
    confidence_level = 0.95,
    confusion_matrix_plot = TRUE,
    roc_plot = TRUE,
    bootstrap_validation = FALSE,
    bootstrap_runs = 1000
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mlpathology.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

