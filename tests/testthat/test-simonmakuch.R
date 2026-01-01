
test_that('simonmakuch analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    survivalTime = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    timeDepVariable = sample(c('A', 'B'), n, replace = TRUE),
    timeDepTime = runif(n, 1, 100),
    timeDepStatus = sample(c('A', 'B'), n, replace = TRUE),
    additionalTimeDepVars1 = sample(c('A', 'B'), n, replace = TRUE),
    additionalTimeDepVars2 = sample(c('A', 'B'), n, replace = TRUE),
    additionalTimeDepVars3 = sample(c('A', 'B'), n, replace = TRUE),
    timeDependentCovariates1 = sample(c('A', 'B'), n, replace = TRUE),
    timeDependentCovariates2 = sample(c('A', 'B'), n, replace = TRUE),
    timeDependentCovariates3 = sample(c('A', 'B'), n, replace = TRUE),
    clusterVariable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- simonmakuch(
      data = data,
    survivalTime = 'survivalTime',
    event = 'event',
    timeDepVariable = 'timeDepVariable',
    timeDepTime = 'timeDepTime',
    timeDepStatus = 'timeDepStatus',
    enableMultipleTimeDep = FALSE,
    additionalTimeDepVars = c('additionalTimeDepVars1', 'additionalTimeDepVars2', 'additionalTimeDepVars3'),
    analysisType = 'comprehensive',
    confidenceLevel = 0.95,
    performLandmarkAnalysis = FALSE,
    landmarkWindow = 1,
    performTimeDependentCox = FALSE,
    timeDependentCovariates = c('timeDependentCovariates1', 'timeDependentCovariates2', 'timeDependentCovariates3'),
    testTimeVaryingEffect = FALSE,
    assessImmortalTimeBias = FALSE,
    naiveComparison = FALSE,
    showSimonMakuchPlot = TRUE,
    showLandmarkPlots = FALSE,
    showCumulativeIncidencePlot = FALSE,
    showConfidenceIntervals = TRUE,
    showRiskTables = TRUE,
    plotExposureStatus = FALSE,
    performLogRankTest = TRUE,
    performMantelByarTest = FALSE,
    performTimeDepLRTest = FALSE,
    handleTieBreaking = 'efron',
    robustVariance = FALSE,
    clusterVariable = 'clusterVariable',
    showSurvivalEstimates = TRUE,
    showHazardRatios = TRUE,
    showLandmarkResults = FALSE,
    showExposurePatterns = FALSE,
    showModelDiagnostics = FALSE,
    performBootstrapValidation = FALSE,
    bootstrapSamples = 500,
    performSensitivityAnalysis = FALSE,
    showExplanations = TRUE,
    showMethodologyNotes = FALSE,
    includeClinicalGuidance = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'simonmakuch.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

