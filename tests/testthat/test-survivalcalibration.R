
test_that('survivalcalibration analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    predicted = runif(n, 1, 100),
    linearPredictor = runif(n, 1, 100),
    validationSet = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- survivalcalibration(
      data = data,
    time = 'time',
    event = 'event',
    predicted = 'predicted',
    linearPredictor = 'linearPredictor',
    validationSet = 'validationSet',
    calibrationTime = 60,
    nGroups = 10,
    validationMethod = 'bootstrap',
    nBootstrap = 100,
    nFolds = 10,
    showCalibrationPlot = TRUE,
    showCindexPlot = TRUE,
    showBrierPlot = TRUE,
    showGroupedCalibration = TRUE,
    smoothCalibration = TRUE,
    ciLevel = 0.95,
    tripodReport = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'survivalcalibration.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

