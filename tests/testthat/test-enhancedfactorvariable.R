
test_that('enhancedfactorvariable analysis works', {
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
    model <- enhancedfactorvariable(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    showOnlyTopFactors = TRUE,
    maxTopFactors = 30,
    sortingMethod = 'freq_desc',
    showDatasetOverview = TRUE,
    showNominalSummary = TRUE,
    showDetailedLevels = TRUE,
    showCombinedAnalysis = FALSE,
    includePercentages = TRUE,
    includeValidPercentages = TRUE,
    includeCumulativeStats = FALSE,
    includeMissing = TRUE,
    minimumCount = 1,
    minimumPercentage = 0,
    excludeRareFactors = FALSE,
    rareFactorThreshold = 1,
    factorComplexityAnalysis = FALSE,
    levelBalanceAnalysis = FALSE,
    bluesky_integration = TRUE,
    comprehensive_output = FALSE,
    clinical_interpretation = TRUE,
    createVisualizations = TRUE,
    plotTopFactorsOnly = TRUE,
    plotOrientation = 'horizontal',
    plotTheme = 'clinical',
    outputFormat = 'standard',
    includeMethodology = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'enhancedfactorvariable.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

