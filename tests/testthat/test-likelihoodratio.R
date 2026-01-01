
test_that('likelihoodratio analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    testVariable = sample(c('A', 'B'), n, replace = TRUE),
    referenceStandard = sample(c('A', 'B'), n, replace = TRUE),
    groupVariable = sample(c('A', 'B'), n, replace = TRUE),
    compareVariable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- likelihoodratio(
      data = data,
    testVariable = 'testVariable',
    referenceStandard = 'referenceStandard',
    groupVariable = 'groupVariable',
    analysisType = 'binary',
    testDirection = 'higher',
    cutpointMethod = 'youden',
    manualCutpoint = 1,
    costRatio = 1,
    confidenceLevel = 0.95,
    ciMethod = 'log',
    bootstrapSamples = 2000,
    calculatePostTest = TRUE,
    prevalenceRange = 'clinical',
    customPrevalenceMin = 5,
    customPrevalenceMax = 50,
    calculateDOR = TRUE,
    calculateNRI = FALSE,
    compareVariable = 'compareVariable',
    predictiveValueCurves = TRUE,
    performStratified = FALSE,
    testHomogeneity = FALSE,
    showSummaryTable = TRUE,
    showCrosstabulation = TRUE,
    showLikelihoodRatios = TRUE,
    showPostTestProbs = TRUE,
    showOptimalCutpoint = TRUE,
    showDiagnosticOdds = TRUE,
    showClinicalInterpretation = TRUE,
    plotROC = TRUE,
    plotPredictiveValues = TRUE,
    plotLikelihoodRatios = TRUE,
    plotPostTestProbabilities = TRUE,
    plotDistributions = FALSE,
    correctContinuity = TRUE,
    exactTests = FALSE,
    smoothROC = FALSE,
    randomSeed = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'likelihoodratio.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

