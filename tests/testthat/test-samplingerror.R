
test_that('samplingerror analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    sampleData = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- samplingerror(
      data = data,
    detectionSensitivity = 95,
    biologicalVarianceCV = 15,
    sampleSize = 10,
    eventFrequency = 5,
    referenceVolume = 100,
    sampleVolume = 10,
    calculationMode = 'theoretical',
    sampleData = 'sampleData',
    targetError = 10,
    showErrorComponents = TRUE,
    showOptimization = TRUE,
    showVisualization = TRUE,
    showMethodology = TRUE,
    showReferences = FALSE,
    confidenceLevel = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'samplingerror.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

