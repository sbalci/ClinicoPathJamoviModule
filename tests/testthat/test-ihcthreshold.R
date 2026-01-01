
test_that('ihcthreshold analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    sampleArea = sample(c('A', 'B'), n, replace = TRUE),
    threshold = sample(c('A', 'B'), n, replace = TRUE),
    positiveCells = runif(n, 1, 100),
    negativeCells = runif(n, 1, 100),
    totalCells = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- ihcthreshold(
      data = data,
    sampleArea = 'sampleArea',
    threshold = 'threshold',
    positiveCells = 'positiveCells',
    negativeCells = 'negativeCells',
    totalCells = 'totalCells',
    stainType = 'ki67',
    tissueType = 'other',
    optimizationMethod = 'cv',
    showSummaryTable = TRUE,
    showThresholdComparison = TRUE,
    showOptimalThreshold = TRUE,
    showPlot = TRUE,
    plotType = 'line',
    showMethodology = TRUE,
    showReferences = FALSE,
    confidenceLevel = 0.95,
    minSampleAreas = 5
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcthreshold.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

