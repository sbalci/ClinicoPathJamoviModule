
test_that('enhancedfrequency analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = sample(c('A', 'B'), n, replace = TRUE),
    vars2 = sample(c('A', 'B'), n, replace = TRUE),
    vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- enhancedfrequency(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    orderBy = 'freq_desc',
    showDatasetOverview = TRUE,
    showVariableSummary = TRUE,
    showFrequencyTables = TRUE,
    showCombinedSummary = FALSE,
    showPercentages = TRUE,
    showValidPercentages = TRUE,
    showCumulative = TRUE,
    showValidCumulative = TRUE,
    includeMissing = TRUE,
    percentageDecimals = 1,
    frequencyDecimals = 0,
    minFrequency = 0,
    maxCategories = 50,
    combineRareCategories = FALSE,
    bluesky_integration = TRUE,
    comprehensive_output = FALSE,
    clinical_interpretation = TRUE,
    dataQualityAssessment = TRUE,
    categoricalDiagnostics = FALSE,
    exportFormat = 'standard',
    includeMethodology = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'enhancedfrequency.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

