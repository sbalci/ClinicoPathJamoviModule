enhancedROC <- function(...) {
  args <- list(...)
  f_args <- formals(ClinicoPath::enhancedROC)
  for(arg in names(f_args)) {
    if(arg %in% c('...', 'data')) next
    if(!(arg %in% names(args))) {
      if(is.name(f_args[[arg]]) && as.character(f_args[[arg]]) == '') {
        args[[arg]] <- ""
      }
    }
  }
  do.call(ClinicoPath::enhancedROC, args)
}

# ═══════════════════════════════════════════════════════════
# Advanced Feature Tests: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# Tests for advanced features not covered in basic/arguments/
# edge-cases/integration tests:
# - CROC analysis
# - Convex hull
# - Tied score handling
# - Kernel smoothing
# - Extended calibration (spline, E/O ratio, Nam-D'Agostino, etc.)
# - Bootstrap extensions (cutoff CI, partial AUC CI)
# - Multi-class OVO strategy & weighted averaging
# - Clinical impact (NNT, clinical utility curve, decision impact)
# - Validation extensions (optimism correction, net benefit, etc.)
# - Clinical presets (all variants)
# - Comprehensive output mode

library(testthat)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")
data(enhancedroc_comparative, package = "ClinicoPath")
data(enhancedroc_calibration, package = "ClinicoPath")
data(enhancedroc_multiclass, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# CROC ANALYSIS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC performs CROC analysis with default alpha", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    crocAnalysis = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs CROC analysis with custom alpha", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    crocAnalysis = TRUE,
    crocAlpha = 14.0
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC CROC with multiple predictors", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    crocAnalysis = TRUE,
    crocAlpha = 7.0
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# CONVEX HULL
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC computes ROC convex hull", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    convexHull = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC convex hull with multiple predictors", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    convexHull = TRUE,
    rocCurve = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# TIED SCORE HANDLING
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC handles tied scores with average method", {
  # Create data with many ties
  test_data_ties <- enhancedroc_biomarker
  test_data_ties$tied_marker <- round(test_data_ties$biomarker1, -1)

  result <- enhancedROC(
    data = test_data_ties,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "tied_marker",
    tiedScoreHandling = "average"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles tied scores with upper bound", {
  test_data_ties <- enhancedroc_biomarker
  test_data_ties$tied_marker <- round(test_data_ties$biomarker1, -1)

  result <- enhancedROC(
    data = test_data_ties,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "tied_marker",
    tiedScoreHandling = "upper"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles tied scores with lower bound", {
  test_data_ties <- enhancedroc_biomarker
  test_data_ties$tied_marker <- round(test_data_ties$biomarker1, -1)

  result <- enhancedROC(
    data = test_data_ties,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "tied_marker",
    tiedScoreHandling = "lower"
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# KERNEL SMOOTHING
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC applies kernel smoothing to ROC curve", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    smoothMethod = "kernel",
    rocCurve = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# EXTENDED CALIBRATION METHODS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC performs spline calibration", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    splineCalibration = TRUE,
    splineKnots = 4
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs spline calibration with 3 knots", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    splineCalibration = TRUE,
    splineKnots = 3
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC calculates E/O ratio", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    eoRatio = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs Nam-D'Agostino test", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    namDagostino = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs Greenwood-Nam-D'Agostino test", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    greenwoodNam = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC displays calibration belt", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    calibrationBelt = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC displays calibration density", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = "predictor1",
    calibrationAnalysis = TRUE,
    calibrationDensity = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs comprehensive calibration analysis", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = c("predictor1", "predictor2"),
    calibrationAnalysis = TRUE,
    calibrationPlot = TRUE,
    hosmerLemeshow = TRUE,
    hlGroups = 10,
    brierScore = TRUE,
    calibrationMetrics = TRUE,
    splineCalibration = TRUE,
    splineKnots = 4,
    eoRatio = TRUE,
    namDagostino = TRUE,
    calibrationBelt = TRUE,
    calibrationDensity = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# BOOTSTRAP EXTENSIONS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC computes bootstrap CI for optimal cutoff", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    bootstrapCutoffCI = TRUE,
    youdenOptimization = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC computes bootstrap CI for partial AUC", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    partialAuc = TRUE,
    partialAucType = "specificity",
    partialRange = "0.8,1.0",
    bootstrapPartialAUC = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC uses basic bootstrap method", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    bootstrapMethod = "basic"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC uses stratified bootstrap", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    stratifiedBootstrap = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# MULTI-CLASS VARIANTS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC performs multi-class ROC with OVO strategy", {
  result <- enhancedROC(
    data = enhancedroc_multiclass,
    outcome = "disease_severity",
    predictors = c("biomarker_A", "biomarker_B"),
    multiClassROC = TRUE,
    multiClassStrategy = "ovo"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs multi-class ROC with weighted averaging", {
  result <- enhancedROC(
    data = enhancedroc_multiclass,
    outcome = "disease_severity",
    predictors = c("biomarker_A", "biomarker_B"),
    multiClassROC = TRUE,
    multiClassStrategy = "ovr",
    multiClassAveraging = "weighted"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs multi-class ROC with OVO + weighted", {
  result <- enhancedROC(
    data = enhancedroc_multiclass,
    outcome = "disease_severity",
    predictors = c("biomarker_A", "biomarker_B", "imaging_severity_score"),
    multiClassROC = TRUE,
    multiClassStrategy = "ovo",
    multiClassAveraging = "weighted",
    rocCurve = TRUE,
    aucTable = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# CLINICAL IMPACT
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC calculates NNT/NND", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalImpact = TRUE,
    nntCalculation = TRUE,
    youdenOptimization = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC displays clinical utility curve", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalImpact = TRUE,
    clinicalUtilityCurve = TRUE,
    clinicalMetrics = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC generates decision impact table", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalImpact = TRUE,
    decisionImpactTable = TRUE,
    useObservedPrevalence = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC clinical impact with multiple predictors", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    clinicalImpact = TRUE,
    nntCalculation = TRUE,
    clinicalUtilityCurve = TRUE,
    decisionImpactTable = TRUE,
    clinicalMetrics = TRUE,
    prevalence = 0.25
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# VALIDATION EXTENSIONS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC performs internal validation with bootstrap", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    validationMethod = "bootstrap"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs internal validation with cross-validation", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    validationMethod = "cv"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs internal validation with both methods", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    validationMethod = "both"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC applies optimism correction", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    validationMethod = "bootstrap",
    optimismCorrection = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC enables external validation framework", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    externalValidation = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC plots decision impact curves", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    internalValidation = TRUE,
    decisionImpactCurves = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs net benefit regression", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    internalValidation = TRUE,
    netBenefitRegression = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC performs model updating analysis", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    modelUpdating = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC assesses transportability", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    internalValidation = TRUE,
    transportability = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# SURVIVAL CONCORDANCE OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC calculates Harrell's C-Index", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    harrellCIndex = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC calculates Uno's C-statistic", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    unoCStatistic = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC calculates incident/dynamic AUC", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    incidentDynamic = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC calculates cumulative/dynamic AUC", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    cumulativeDynamic = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC calculates competing risks concordance", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    competingRisksConcordance = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# CLINICAL PRESETS (ALL VARIANTS)
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC applies diagnostic_validation preset", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalPresets = "diagnostic_validation"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC applies research_comprehensive preset", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalPresets = "research_comprehensive"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC applies prognosis clinical context", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalContext = "prognosis"
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC applies monitoring clinical context", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalContext = "monitoring"
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# COMPREHENSIVE OUTPUT
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC produces comprehensive output", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    comprehensive_output = TRUE,
    clinical_interpretation = TRUE,
    rocCurve = TRUE,
    aucTable = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC comprehensive analysis type produces full output", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1", "novel_marker2"),
    analysisType = "comprehensive",
    comprehensive_output = TRUE,
    clinical_interpretation = TRUE,
    youdenOptimization = TRUE,
    rocCurve = TRUE,
    aucTable = TRUE,
    cutoffTable = TRUE,
    optimalCutoffs = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# PLOT SETTINGS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC applies custom plot dimensions", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    plotWidth = 800,
    plotHeight = 800
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC applies classic plot theme", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    plotTheme = "classic"
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# VENKATRAMAN COMPARISON METHOD
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC uses Venkatraman comparison method", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    pairwiseComparisons = TRUE,
    comparisonMethod = "venkatraman"
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# COMBINED ADVANCED FEATURES
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC CROC + convex hull + comprehensive output", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    crocAnalysis = TRUE,
    crocAlpha = 7.0,
    convexHull = TRUE,
    comprehensive_output = TRUE,
    clinical_interpretation = TRUE,
    rocCurve = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC validation + calibration combined", {
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = c("predictor1", "predictor2"),
    calibrationAnalysis = TRUE,
    calibrationPlot = TRUE,
    hosmerLemeshow = TRUE,
    brierScore = TRUE,
    calibrationMetrics = TRUE,
    internalValidation = TRUE,
    validationMethod = "bootstrap",
    optimismCorrection = TRUE,
    rocCurve = TRUE,
    aucTable = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC clinical impact + validation + bootstrap combined", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    clinicalImpact = TRUE,
    nntCalculation = TRUE,
    decisionImpactTable = TRUE,
    internalValidation = TRUE,
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    rocCurve = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC kitchen sink: all non-conflicting advanced features", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1", "novel_marker2"),
    analysisType = "comprehensive",
    # Cutoff optimization
    youdenOptimization = TRUE,
    customCutoffs = "50, 100, 150",
    sensitivityThreshold = 0.85,
    specificityThreshold = 0.80,
    # Bootstrap
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    bootstrapMethod = "percentile",
    bootstrapCutoffCI = TRUE,
    stratifiedBootstrap = TRUE,
    # Comparison
    pairwiseComparisons = TRUE,
    comparisonMethod = "delong",
    showMetricsDiff = TRUE,
    statisticalComparison = TRUE,
    # Advanced ROC
    partialAuc = TRUE,
    partialAucType = "specificity",
    partialRange = "0.8,1.0",
    crocAnalysis = TRUE,
    convexHull = TRUE,
    # Output
    rocCurve = TRUE,
    aucTable = TRUE,
    cutoffTable = TRUE,
    optimalCutoffs = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE,
    # Clinical impact
    clinicalImpact = TRUE,
    nntCalculation = TRUE,
    decisionImpactTable = TRUE,
    # Comprehensive
    comprehensive_output = TRUE,
    clinical_interpretation = TRUE,
    clinicalContext = "diagnosis",
    plotTheme = "clinical"
  )

  expect_s3_class(result, "enhancedROCResults")
})
