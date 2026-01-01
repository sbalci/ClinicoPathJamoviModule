
test_that('partialcorrelation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100),
    controls1 = runif(n, 1, 100),
    controls2 = runif(n, 1, 100),
    controls3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- partialcorrelation(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    controls = c('controls1', 'controls2', 'controls3'),
    method = 'pearson',
    ci = TRUE,
    ciWidth = 95,
    sig = TRUE,
    sigLevel = 0.05,
    matrixPlot = FALSE,
    showZeroOrder = TRUE,
    correlationType = 'partial',
    multipleComparison = 'none',
    robustErrorHandling = TRUE,
    showDiagnostics = FALSE,
    detailedOutput = FALSE,
    bootstrapCI = FALSE,
    bootstrapSamples = 1000,
    showEffectSizes = TRUE,
    assumptionChecks = FALSE,
    clinicalInterpretation = TRUE,
    showRecommendations = FALSE,
    bluesky_integration = TRUE,
    comprehensive_output = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'partialcorrelation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

