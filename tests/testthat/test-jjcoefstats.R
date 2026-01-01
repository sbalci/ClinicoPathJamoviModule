
test_that('jjcoefstats analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    term = sample(c('A', 'B'), n, replace = TRUE),
    estimate = runif(n, 1, 100),
    stdError = runif(n, 1, 100),
    confLow = runif(n, 1, 100),
    confHigh = runif(n, 1, 100),
    pValue = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = runif(n, 1, 100),
    predictors2 = runif(n, 1, 100),
    predictors3 = runif(n, 1, 100),
    survivalTime = runif(n, 1, 100),
    eventStatus = sample(c('A', 'B'), n, replace = TRUE),
    randomEffects = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- jjcoefstats(
      data = data,
    inputMode = 'precomputed',
    term = 'term',
    estimate = 'estimate',
    stdError = 'stdError',
    confLow = 'confLow',
    confHigh = 'confHigh',
    pValue = 'pValue',
    degreesOfFreedom = 1,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    modelType = 'lm',
    survivalTime = 'survivalTime',
    eventStatus = 'eventStatus',
    randomEffects = 'randomEffects',
    sortCoefs = FALSE,
    excludeIntercept = FALSE,
    showPValues = FALSE,
    pValueDisplay = 'numeric',
    ciLevel = 0.95,
    exponentiate = FALSE,
    expScaleLabel = 'exp',
    referenceValue = 0,
    colorScheme = 'default',
    plotTheme = 'default',
    plotWidth = 700,
    plotHeight = 500,
    showexplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'jjcoefstats.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

