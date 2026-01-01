
test_that('pathsampling analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    totalSamples = runif(n, 1, 100),
    firstDetection = runif(n, 1, 100),
    positiveCount = runif(n, 1, 100),
    positiveSamplesList = sample(c('A', 'B'), n, replace = TRUE),
    sampleType = sample(c('A', 'B'), n, replace = TRUE),
    positiveCassettes = runif(n, 1, 100),
    maxPositiveSingle = runif(n, 1, 100),
    totalPopulation = runif(n, 1, 100),
    successStates = runif(n, 1, 100),
    totalLymphNodes = runif(n, 1, 100),
    positiveLymphNodes = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- pathsampling(
      data = data,
    analysisContext = 'general',
    totalSamples = 'totalSamples',
    firstDetection = 'firstDetection',
    positiveCount = 'positiveCount',
    positiveSamplesList = 'positiveSamplesList',
    sampleType = 'sampleType',
    targetConfidence = 0.95,
    maxSamples = 10,
    bootstrapIterations = 10000,
    showBinomialModel = FALSE,
    showBootstrap = FALSE,
    showDetectionCurve = FALSE,
    showSensitivityCI = FALSE,
    setSeed = FALSE,
    seedValue = 42,
    positiveCassettes = 'positiveCassettes',
    maxPositiveSingle = 'maxPositiveSingle',
    showTumorBurden = FALSE,
    showStageMigration = FALSE,
    showCorrelation = FALSE,
    showDistributionPattern = FALSE,
    distributionThreshold = 5,
    modelType = 'binomial',
    totalPopulation = 'totalPopulation',
    successStates = 'successStates',
    targetDetections = 1,
    showHypergeometric = FALSE,
    showBetaBinomial = FALSE,
    totalLymphNodes = 'totalLymphNodes',
    positiveLymphNodes = 'positiveLymphNodes',
    showLNAnalysis = FALSE,
    lnrThreshold1 = 0.1,
    lnrThreshold2 = 0.3,
    showEffectSizes = FALSE,
    showOmentumAnalysis = FALSE,
    showClinicalSummary = FALSE,
    showGuidedInstructions = FALSE,
    showConciseInstructions = FALSE,
    showEmpiricalCumulative = FALSE,
    showSpatialClustering = FALSE,
    showStratifiedAnalysis = FALSE,
    showPopulationDetection = FALSE,
    showIncrementalYield = FALSE,
    showMultifocalAnalysis = FALSE,
    showProbabilityExplanation = FALSE,
    showKeyResults = FALSE,
    showRecommendText = FALSE,
    showInterpretText = FALSE,
    showReferencesText = FALSE,
    estimationMethod = 'auto',
    showHeterogeneityTest = FALSE,
    useGeometricCI = TRUE,
    ciMethod = 'auto',
    showModelFit = FALSE,
    showObsPred = FALSE,
    showMarginalInterpretation = TRUE,
    showPowerAnalysis = FALSE,
    targetPower = 0.8,
    targetDetectionProb = 0.95,
    autoSelectModel = FALSE,
    appendVariables = FALSE,
    autoDetectHeterogeneity = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pathsampling.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

