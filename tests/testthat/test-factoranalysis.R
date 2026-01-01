
test_that('factoranalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- factoranalysis(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    method = 'ml',
    nFactorsMethod = 'kaiser',
    nFactors = 1,
    rotation = 'varimax',
    scores = 'none',
    saveScores = FALSE,
    showUnrotated = FALSE,
    showRotated = TRUE,
    hideSmallLoadings = FALSE,
    loadingsCutoff = 0.3,
    screePlot = TRUE,
    loadingsPlot = FALSE,
    biplot = FALSE,
    priorCommunalities = 'smc',
    maxIterations = 25,
    convergence = 1e-6,
    adequacyTests = TRUE,
    bartlettTest = TRUE,
    kmoTest = TRUE,
    factorCorrelations = TRUE,
    communalities = TRUE,
    eigenvalues = TRUE,
    bluesky_integration = TRUE,
    comprehensive_output = FALSE,
    clinical_interpretation = TRUE,
    parallel_iterations = 100,
    parallel_percentile = 95,
    plotWidth = 600,
    plotHeight = 400
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'factoranalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

