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
# - Tied scores (automatic tie note with the AUC bounds)
# - Kernel smoothing
# - Extended calibration (spline, E/O ratio, calibration density)
# - Bootstrap extensions (partial AUC CI, stratified, method)
# - Multi-class OVO strategy & weighted averaging
# - Clinical impact (NNT, clinical utility curve, decision impact)
# - Validation extensions (bootstrap, cross-validation, decision impact curves)
# - Clinical presets (all variants)
# - Comprehensive output mode

library(testthat)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")
data(enhancedroc_comparative, package = "ClinicoPath")
data(enhancedroc_calibration, package = "ClinicoPath")
data(enhancedroc_multiclass, package = "ClinicoPath")

# ---------------------------------------------------------------------------
# Sixteen blocks that used to live in this file passed arguments that no longer
# exist. The first 14 options below were never implemented; tiedScoreHandling was
# retired on 2026-10-09 because its Upper / Lower choices only raised a notice and
# left the AUC unchanged (an automatic tie note with the AUC bounds replaced it).
# All are commented out in BOTH jamovi/enhancedROC.a.yaml and
# jamovi/enhancedROC.u.yaml, so jamovi ships no control that produces no output
# and the R wrapper does not accept them either. Calling one is an "unused
# argument" error, not a feature under test. The full contract (a.yaml, u.yaml,
# backend) is asserted in test-enhancedROC-release-review.R; this is the guard for
# THIS file's call sites.
test_that("no test in this file sets an option that was never implemented or was retired", {
  removed <- c(
    "harrellCIndex", "unoCStatistic", "namDagostino", "greenwoodNam",
    "calibrationBelt", "optimismCorrection", "externalValidation", "transportability",
    "bootstrapCutoffCI", "modelUpdating", "netBenefitRegression", "incidentDynamic",
    "cumulativeDynamic", "competingRisksConcordance", "tiedScoreHandling")
  expect_equal(intersect(removed, names(formals(ClinicoPath::enhancedROC))), character(0))
})

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
# TIED SCORES
# ═══════════════════════════════════════════════════════════
# Ties count one half in the AUC. The retired tiedScoreHandling option offered
# Upper / Lower but never changed the AUC; the AUC table now carries a 'ties' note
# with the share of tied case-control pairs and the AUC if every tie favoured the
# cases (AUC + share/2) or the non-cases (AUC - share/2).

# Oracle: Mann-Whitney AUC with half credit for ties, read in the declared direction.
tie_oracle <- function(d, marker, higher = TRUE) {
  cs <- d[[marker]][d$disease_status == "Disease"]
  ct <- d[[marker]][d$disease_status != "Disease"]
  eq <- mean(outer(cs, ct, "=="))
  auc <- mean(outer(cs, ct, if (higher) ">" else "<")) + eq / 2
  list(auc = auc, share = eq, hi = sprintf("%.3f", auc + eq / 2), lo = sprintf("%.3f", auc - eq / 2))
}
tied_biomarker <- function() {
  d <- enhancedroc_biomarker
  d$tied_marker <- round(d$biomarker1, -1)
  d
}

test_that("tied scores get the automatic tie note with the AUC bounds", {
  d <- tied_biomarker()
  o <- tie_oracle(d, "tied_marker")
  expect_gt(o$share, 0.1)   # the fixture really is heavily tied

  result <- enhancedROC(
    data = d,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "tied_marker"
  )

  aucs <- result$results$aucSummary
  expect_equal(aucs$asDF$auc[1], o$auc, tolerance = 1e-12)   # ties still count one half
  note <- aucs$notes[["ties"]]$note
  expect_match(note, sprintf("tied_marker: %s%% of case-control pairs are tied", base::format(signif(100 * o$share, 2))), fixed = TRUE)
  expect_match(note, paste0("If every tie favoured the cases the AUC would be ", o$hi), fixed = TRUE)
  expect_match(note, paste0("if every tie favoured the non-cases ", o$lo), fixed = TRUE)
  expect_match(note, "not a confidence interval", fixed = TRUE)
})

test_that("the tie bounds follow the declared direction", {
  d <- tied_biomarker()
  o <- tie_oracle(d, "tied_marker", higher = FALSE)

  result <- enhancedROC(
    data = d,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "tied_marker",
    direction = "lower"
  )

  aucs <- result$results$aucSummary
  expect_equal(aucs$asDF$auc[1], o$auc, tolerance = 1e-12)
  expect_match(aucs$notes[["ties"]]$note,
               sprintf("the AUC would be %s, if every tie favoured the non-cases %s", o$hi, o$lo), fixed = TRUE)
})

test_that("a marker with no tied case-control pair gets no tie note", {
  expect_equal(tie_oracle(enhancedroc_biomarker, "biomarker1")$share, 0)

  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_null(result$results$aucSummary$notes[["ties"]])
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
    calibrationDensity = TRUE
  )

  expect_s3_class(result, "enhancedROCResults")
})

# ═══════════════════════════════════════════════════════════
# BOOTSTRAP EXTENSIONS
# ═══════════════════════════════════════════════════════════

test_that("enhancedROC bootstraps the AUC while Youden picks the cutoff", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
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

# positiveClass is omitted in these calls (the local wrapper sends ""). With more than
# two levels and Multi-class ROC ticked that is multi-class-only mode: the multi-class
# tables are computed, every binary table stays empty, and an INFO notice says that
# choosing a positive class adds the binary tables. No level is picked from the order.
expect_multiclass_only <- function(result) {
  expect_gt(result$results$multiClassAUC$rowCount, 0)
  expect_gt(result$results$multiClassAverage$rowCount, 0)
  expect_equal(result$results$aucSummary$rowCount, 0)
  expect_match(paste(result$results$notices$content, collapse = " "),
               "Binary ROC tables need a positive class", fixed = TRUE)
  expect_true(all(result$results$outcomeCoding$asDF$role == "Class in the multi-class analysis"))
}

test_that("enhancedROC performs multi-class ROC with OVO strategy", {
  result <- enhancedROC(
    data = enhancedroc_multiclass,
    outcome = "disease_severity",
    predictors = c("biomarker_A", "biomarker_B"),
    multiClassROC = TRUE,
    multiClassStrategy = "ovo"
  )

  expect_s3_class(result, "enhancedROCResults")
  expect_multiclass_only(result)
  # 4 classes give 6 pairs per predictor
  expect_equal(result$results$multiClassAUC$rowCount, 2 * choose(4, 2))
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
  expect_multiclass_only(result)
  expect_true(all(is.finite(result$results$multiClassAverage$asDF$weighted_auc)))
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
  expect_multiclass_only(result)

  # Hand-Till OVO averages over class PAIRS, so there is no prevalence-weighted
  # version of it. Ticking "weighted" must say so rather than quietly report the
  # unweighted value; rgba(202, 138, 4, 0.12) is .renderNotices()'s WARNING panel.
  notices <- paste(result$results$notices$content, collapse = " ")
  expect_match(notices, "Weighted Averaging Is Not Available with the One-vs-One Strategy",
               fixed = TRUE)
  expect_match(notices, "rgba(202, 138, 4, 0.12)", fixed = TRUE)
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
