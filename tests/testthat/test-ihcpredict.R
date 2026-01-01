
test_that('ihcpredict analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    catVars1 = sample(c('A', 'B'), n, replace = TRUE),
    catVars2 = sample(c('A', 'B'), n, replace = TRUE),
    catVars3 = sample(c('A', 'B'), n, replace = TRUE),
    contVars1 = runif(n, 1, 100),
    contVars2 = runif(n, 1, 100),
    contVars3 = runif(n, 1, 100),
    caseId = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- ihcpredict(
      data = data,
    useStoredModel = 'stored',
    catVars = c('catVars1', 'catVars2', 'catVars3'),
    contVars = c('contVars1', 'contVars2', 'contVars3'),
    caseId = 'caseId',
    predictionMethod = 'hybrid',
    confidenceThreshold = 0.5,
    highConfidenceThreshold = 0.75,
    lowConfidenceThreshold = 0.25,
    panelWeight = 0.5,
    distanceWeight = 0.3,
    silhouetteWeight = 0.2,
    showAlternatives = TRUE,
    nAlternatives = 3,
    showEvidence = TRUE,
    flagLowConfidence = TRUE,
    showMarkerComparison = TRUE,
    validateMarkers = TRUE,
    requireAllMarkers = TRUE,
    scaleContVars = TRUE,
    handleMissing = 'pairwise',
    exportPredictions = FALSE,
    showPredictionPlot = TRUE,
    showConfidencePlot = TRUE,
    colorPalette = 'colorblind',
    fontSize = 'medium',
    plotContrast = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcpredict.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

