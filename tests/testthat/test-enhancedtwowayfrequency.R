
test_that('enhancedtwowayfrequency analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    rowVar = sample(c('A', 'B'), n, replace = TRUE),
    colVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- enhancedtwowayfrequency(
      data = data,
    rowVar = 'rowVar',
    colVar = 'colVar',
    cellPercent = TRUE,
    rowPercent = FALSE,
    colPercent = FALSE,
    showCounts = TRUE,
    showTotals = TRUE,
    chiSquareTest = TRUE,
    fisherTest = FALSE,
    continuityCorrection = TRUE,
    associationMeasures = TRUE,
    residualAnalysis = FALSE,
    expectedFrequencies = FALSE,
    minimumExpected = 5,
    robustErrorHandling = TRUE,
    showDiagnostics = FALSE,
    detailedOutput = FALSE,
    clinicalInterpretation = TRUE,
    showRecommendations = FALSE,
    bluesky_integration = TRUE,
    comprehensive_output = FALSE,
    heatmapPlot = FALSE,
    mosaicPlot = FALSE,
    percentageDisplay = 'percentage'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'enhancedtwowayfrequency.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

