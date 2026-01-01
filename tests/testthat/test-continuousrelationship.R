
test_that('continuousrelationship analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = runif(n, 1, 100),
    predictor = runif(n, 1, 100),
    covariates1 = runif(n, 1, 100),
    covariates2 = runif(n, 1, 100),
    covariates3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- continuousrelationship(
      data = data,
    outcome = 'outcome',
    predictor = 'predictor',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    modelType = 'spline',
    nKnots = '4',
    knotPositions = 'quantile',
    plotType = 'doseresponse',
    referenceValue = 1,
    showCI = TRUE,
    showRug = TRUE,
    showGuidance = TRUE,
    compareWithLinear = TRUE,
    showCategorizedPitfall = FALSE,
    testLinearity = TRUE,
    showModelFit = TRUE,
    showModel = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'continuousrelationship.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

