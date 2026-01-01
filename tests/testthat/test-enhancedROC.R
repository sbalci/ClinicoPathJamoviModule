
test_that('enhancedROC analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = runif(n, 1, 100),
    predictors2 = runif(n, 1, 100),
    predictors3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- enhancedROC(
      data = data,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    analysisType = 'single',
    direction = 'auto',
    youdenOptimization = FALSE,
    sensitivityThreshold = 0.8,
    specificityThreshold = 0.8,
    confidenceLevel = 95,
    bootstrapSamples = 1000,
    useBootstrap = TRUE,
    bootstrapMethod = 'bca',
    bootstrapCutoffCI = FALSE,
    bootstrapPartialAUC = FALSE,
    stratifiedBootstrap = TRUE,
    pairwiseComparisons = FALSE,
    comparisonMethod = 'delong',
    rocCurve = TRUE,
    aucTable = TRUE,
    cutoffTable = FALSE,
    optimalCutoffs = FALSE,
    diagnosticMetrics = FALSE,
    clinicalMetrics = FALSE,
    smoothMethod = 'none',
    partialAuc = FALSE,
    partialAucType = 'specificity',
    crocAnalysis = FALSE,
    crocAlpha = 7,
    convexHull = FALSE,
    tiedScoreHandling = 'average',
    detectImbalance = TRUE,
    imbalanceThreshold = 3,
    showImbalanceWarning = TRUE,
    recommendPRC = TRUE,
    prevalence = 0.1,
    useObservedPrevalence = TRUE,
    clinicalContext = 'general',
    clinicalPresets = 'custom',
    comprehensive_output = FALSE,
    clinical_interpretation = TRUE,
    plotTheme = 'clinical',
    plotWidth = 600,
    plotHeight = 600,
    showCutoffPoints = TRUE,
    showConfidenceBands = FALSE,
    showMetricsDiff = TRUE,
    statisticalComparison = TRUE,
    calibrationAnalysis = FALSE,
    calibrationPlot = TRUE,
    hosmerLemeshow = TRUE,
    hlGroups = 10,
    brierScore = TRUE,
    calibrationMetrics = TRUE,
    splineCalibration = FALSE,
    splineKnots = 4,
    eoRatio = FALSE,
    namDagostino = FALSE,
    greenwoodNam = FALSE,
    calibrationBelt = FALSE,
    calibrationDensity = FALSE,
    multiClassROC = FALSE,
    multiClassStrategy = 'ovr',
    multiClassAveraging = 'macro',
    timeDependentROC = FALSE,
    tdRocMethod = 'km',
    clinicalImpact = FALSE,
    nntCalculation = TRUE,
    clinicalUtilityCurve = TRUE,
    decisionImpactTable = TRUE,
    survivalROC = FALSE,
    harrellCIndex = FALSE,
    unoCStatistic = FALSE,
    incidentDynamic = FALSE,
    cumulativeDynamic = FALSE,
    competingRisksConcordance = FALSE,
    internalValidation = FALSE,
    validationMethod = 'bootstrap',
    optimismCorrection = TRUE,
    externalValidation = FALSE,
    decisionImpactCurves = FALSE,
    netBenefitRegression = FALSE,
    modelUpdating = FALSE,
    transportability = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'enhancedROC.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

