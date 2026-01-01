
test_that('markovmultistate analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    subject = sample(c('A', 'B'), n, replace = TRUE),
    covs1 = sample(c('A', 'B'), n, replace = TRUE),
    covs2 = sample(c('A', 'B'), n, replace = TRUE),
    covs3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- markovmultistate(
      data = data,
    time = 'time',
    event = 'event',
    subject = 'subject',
    covs = c('covs1', 'covs2', 'covs3'),
    modelType = 'homogeneous',
    transitionMatrix = 'progressive',
    baselineHazard = 'exponential',
    estimationMethod = 'ml',
    timeUnits = 'auto',
    conf = 0.95,
    maxIterations = 1000,
    tolerance = 1e-8,
    showTransitionIntensities = TRUE,
    showTransitionProbs = TRUE,
    showStateProbabilities = TRUE,
    showMeanSojournTimes = TRUE,
    showExpectedRewards = FALSE,
    showModelFit = TRUE,
    showPredictions = TRUE,
    showEducational = TRUE,
    plotStateProbs = TRUE,
    plotTransitionProbs = FALSE,
    plotHazardRatios = FALSE,
    plotModelDiagnostics = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'markovmultistate.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

