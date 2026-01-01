
test_that('flexcomprisk analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    covs1 = sample(c('A', 'B'), n, replace = TRUE),
    covs2 = sample(c('A', 'B'), n, replace = TRUE),
    covs3 = sample(c('A', 'B'), n, replace = TRUE),
    timeInteractions1 = sample(c('A', 'B'), n, replace = TRUE),
    timeInteractions2 = sample(c('A', 'B'), n, replace = TRUE),
    timeInteractions3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- flexcomprisk(
      data = data,
    time = 'time',
    event = 'event',
    covs = c('covs1', 'covs2', 'covs3'),
    eventOfInterest = 1,
    modelType = 'splines',
    splineType = 'rcs',
    splineDf = 4,
    timeInteractions = c('timeInteractions1', 'timeInteractions2', 'timeInteractions3'),
    validationType = 'cv',
    cvFolds = 5,
    bootstrapSamples = 500,
    conf = 0.95,
    showModelComparison = TRUE,
    showValidationResults = TRUE,
    showPredictions = TRUE,
    showEducational = TRUE,
    plotCumulativeIncidence = TRUE,
    plotModelComparison = FALSE,
    plotValidation = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'flexcomprisk.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

