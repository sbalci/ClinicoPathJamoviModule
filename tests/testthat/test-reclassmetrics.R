
test_that('reclassmetrics analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    survivalTime = runif(n, 1, 100),
    oldModelProb = runif(n, 1, 100),
    newModelProb = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- reclassmetrics(
      data = data,
    outcome = 'outcome',
    outcomeType = 'binary',
    survivalTime = 'survivalTime',
    timePoint = 60,
    oldModelProb = 'oldModelProb',
    newModelProb = 'newModelProb',
    nriType = 'both',
    ciLevel = 0.95,
    bootstrapCI = TRUE,
    nBootstrap = 500,
    showReclassTable = TRUE,
    showEventNonEvent = TRUE,
    showIDIComponents = TRUE,
    showCalibrationComparison = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'reclassmetrics.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

