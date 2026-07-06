# Clinical ROC Analysis

Clinical ROC analysis toolkit for comprehensive diagnostic performance
evaluation. Includes ROC curve analysis, Youden Index optimization,
sensitivity/specificity analysis, optimal cutoff determination, and
comparative ROC analysis. Essential for biomarker validation, diagnostic
test evaluation, and clinical decision support in medical research.

## Usage

``` r
enhancedROC(
  data,
  outcome,
  positiveClass,
  predictors,
  analysisType = "single",
  direction = "auto",
  youdenOptimization = TRUE,
  customCutoffs = "",
  sensitivityThreshold = 0.8,
  specificityThreshold = 0.8,
  confidenceLevel = 95,
  bootstrapSamples = 1000,
  useBootstrap = FALSE,
  bootstrapMethod = "bca",
  bootstrapCutoffCI = FALSE,
  bootstrapPartialAUC = FALSE,
  stratifiedBootstrap = FALSE,
  pairwiseComparisons = FALSE,
  comparisonMethod = "delong",
  rocCurve = TRUE,
  aucTable = TRUE,
  cutoffTable = FALSE,
  optimalCutoffs = TRUE,
  diagnosticMetrics = TRUE,
  clinicalMetrics = FALSE,
  smoothMethod = "none",
  partialAuc = FALSE,
  partialAucType = "specificity",
  partialRange = "0.8,1.0",
  crocAnalysis = FALSE,
  crocAlpha = 7,
  convexHull = FALSE,
  tiedScoreHandling = "average",
  detectImbalance = FALSE,
  imbalanceThreshold = 3,
  showImbalanceWarning = FALSE,
  recommendPRC = FALSE,
  prevalence = 0.1,
  useObservedPrevalence = FALSE,
  clinicalContext = "general",
  clinicalPresets = "custom",
  comprehensive_output = FALSE,
  clinical_interpretation = FALSE,
  plotTheme = "clinical",
  plotWidth = 600,
  plotHeight = 600,
  showCutoffPoints = FALSE,
  showConfidenceBands = FALSE,
  showMetricsDiff = FALSE,
  statisticalComparison = FALSE,
  calibrationAnalysis = FALSE,
  calibrationPlot = FALSE,
  hosmerLemeshow = FALSE,
  hlGroups = 10,
  brierScore = FALSE,
  calibrationMetrics = FALSE,
  splineCalibration = FALSE,
  splineKnots = 4,
  eoRatio = FALSE,
  namDagostino = FALSE,
  greenwoodNam = FALSE,
  calibrationBelt = FALSE,
  calibrationDensity = FALSE,
  multiClassROC = FALSE,
  multiClassStrategy = "ovr",
  multiClassAveraging = "macro",
  clinicalImpact = FALSE,
  nntCalculation = FALSE,
  clinicalUtilityCurve = FALSE,
  decisionImpactTable = FALSE,
  harrellCIndex = FALSE,
  unoCStatistic = FALSE,
  incidentDynamic = FALSE,
  cumulativeDynamic = FALSE,
  competingRisksConcordance = FALSE,
  internalValidation = FALSE,
  validationMethod = "bootstrap",
  optimismCorrection = FALSE,
  externalValidation = FALSE,
  decisionImpactCurves = FALSE,
  netBenefitRegression = FALSE,
  modelUpdating = FALSE,
  transportability = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome variable (disease status)

- positiveClass:

  Select which level represents the positive class (disease/condition
  present)

- predictors:

  Numeric predictor variables for ROC analysis

- analysisType:

  Type of ROC analysis to perform

- direction:

  Direction of the predictor-outcome relationship

- youdenOptimization:

  Find optimal cutoff using Youden Index (Sensitivity + Specificity - 1)

- customCutoffs:

  Comma-separated list of custom cutoffs to evaluate (e.g., 0.1, 0.5,
  0.9)

- sensitivityThreshold:

  Minimum required sensitivity for screening applications

- specificityThreshold:

  Minimum required specificity for confirmatory testing

- confidenceLevel:

  Confidence level for AUC confidence intervals

- bootstrapSamples:

  Number of bootstrap samples for confidence intervals

- useBootstrap:

  Use bootstrap methods for confidence intervals

- bootstrapMethod:

  Method for calculating bootstrap confidence intervals. BCa provides
  better coverage but requires more computation.

- bootstrapCutoffCI:

  Calculate bootstrap confidence intervals for sensitivity and
  specificity at optimal cutoff

- bootstrapPartialAUC:

  Calculate bootstrap confidence intervals for partial AUC estimates

- stratifiedBootstrap:

  Maintain outcome class proportions in bootstrap samples (recommended
  for imbalanced data)

- pairwiseComparisons:

  Perform pairwise comparisons between ROC curves

- comparisonMethod:

  Method for comparing ROC curves

- rocCurve:

  Display ROC curve plot

- aucTable:

  Display AUC summary table

- cutoffTable:

  Display detailed cutoff analysis

- optimalCutoffs:

  Display optimal cutoff summary

- diagnosticMetrics:

  Display comprehensive diagnostic metrics

- clinicalMetrics:

  Display clinical application metrics (PPV, NPV, LR+, LR-)

- smoothMethod:

  Method for smoothing ROC curves

- partialAuc:

  Calculate partial AUC for specific sensitivity/specificity ranges

- partialAucType:

  Whether to calculate pAUC over a specificity or sensitivity range

- partialRange:

  Range for partial AUC (min,max) - e.g., 0.8,1.0 for high specificity
  or sensitivity

- crocAnalysis:

  Calculate Concentrated ROC curves for early retrieval analysis

- crocAlpha:

  Concentration parameter for CROC exponential magnifier function

- convexHull:

  Calculate ROC convex hull (optimal achievable performance)

- tiedScoreHandling:

  Method for handling tied predictor scores in ROC calculation

- detectImbalance:

  Automatically detect class imbalance and recommend PRC when
  appropriate

- imbalanceThreshold:

  Ratio threshold for imbalance detection (e.g., 3.0 means 3:1 or 1:3
  ratio)

- showImbalanceWarning:

  Display warning message when class imbalance is detected

- recommendPRC:

  Recommend using Precision-Recall curves when imbalance is detected

- prevalence:

  Disease prevalence for calculating predictive values

- useObservedPrevalence:

  Use the prevalence observed in the data for PPV/NPV and clinical
  impact calculations (recommended)

- clinicalContext:

  Clinical application context for interpretation

- clinicalPresets:

  Pre-configured settings for common clinical scenarios

- comprehensive_output:

  Include comprehensive statistical details

- clinical_interpretation:

  Provide clinical context for ROC analysis results

- plotTheme:

  Visual theme for ROC plots

- plotWidth:

  Width of ROC plots

- plotHeight:

  Height of ROC plots

- showCutoffPoints:

  Highlight optimal cutoff points on ROC curve

- showConfidenceBands:

  Display confidence bands around ROC curve

- showMetricsDiff:

  Display detailed differences between model metrics

- statisticalComparison:

  Perform comprehensive statistical comparison between models

- calibrationAnalysis:

  Assess calibration (agreement between observed and predicted
  probabilities)

- calibrationPlot:

  Display calibration plot with loess smoothing

- hosmerLemeshow:

  Perform Hosmer-Lemeshow goodness-of-fit test

- hlGroups:

  Number of groups for Hosmer-Lemeshow test

- brierScore:

  Calculate Brier score and scaled Brier score

- calibrationMetrics:

  Calculate calibration slope, intercept, and calibration-in-the-large

- splineCalibration:

  Use restricted cubic splines for flexible calibration curves

- splineKnots:

  Number of knots for restricted cubic splines (3-7 recommended)

- eoRatio:

  Calculate Expected/Observed ratio for overall calibration assessment

- namDagostino:

  Perform Nam-D'Agostino calibration test (more powerful than H-L)

- greenwoodNam:

  Greenwood-Nam-D'Agostino test for survival model calibration

- calibrationBelt:

  Display calibration belt showing uncertainty around calibration curve

- calibrationDensity:

  Show distribution of predicted probabilities as density overlay

- multiClassROC:

  Enable multi-class ROC analysis for outcomes with \>2 levels

- multiClassStrategy:

  Strategy for multi-class ROC analysis

- multiClassAveraging:

  Method for averaging AUC across classes

- clinicalImpact:

  Calculate clinical impact metrics (NNT, NND, clinical utility)

- nntCalculation:

  Calculate number needed to test and number needed to diagnose

- clinicalUtilityCurve:

  Display clinical utility curve showing test consequences

- decisionImpactTable:

  Show decision impact at various thresholds

- harrellCIndex:

  Calculate Harrell's concordance index for time-to-event outcomes

- unoCStatistic:

  Calculate Uno's C-statistic (more robust to censoring)

- incidentDynamic:

  Calculate incident/dynamic AUC (sensitivity for events at specific
  time)

- cumulativeDynamic:

  Calculate cumulative/dynamic AUC (sensitivity for events by specific
  time)

- competingRisksConcordance:

  Calculate cause-specific concordance for competing risks

- internalValidation:

  Perform internal validation using cross-validation or bootstrap

- validationMethod:

  Method for internal validation

- optimismCorrection:

  Apply optimism correction to performance metrics

- externalValidation:

  Enable external validation reporting framework

- decisionImpactCurves:

  Plot decision impact curves showing clinical consequences

- netBenefitRegression:

  Model net benefit as function of threshold probabilities

- modelUpdating:

  Analyze need for model recalibration or updating

- transportability:

  Assess model transportability across populations

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$results$notices` |  |  |  |  | a html |
| `results$results$instructions` |  |  |  |  | a html |
| `results$results$imbalanceMetrics` |  |  |  |  | a table |
| `results$results$precisionRecallTable` |  |  |  |  | Metrics for imbalanced data analysis |
| `results$results$analysisSummary` |  |  |  |  | Plain language summary of key findings |
| `results$results$clinicalReport` |  |  |  |  | Copy-ready clinical report sentences for publications and reports |
| `results$results$aucSummary` |  |  |  |  | AUC values with confidence intervals for each predictor |
| `results$results$rocComparisons` |  |  |  |  | Pairwise comparisons between ROC curves |
| `results$results$detailedComparison` |  |  |  |  | Comprehensive comparison of diagnostic metrics between models |
| `results$results$statisticalSummary` |  |  |  |  | Summary of statistical tests for model comparison |
| `results$results$optimalCutoffSummary` |  |  |  |  | Youden Index optimization results for each predictor |
| `results$results$cutoffAnalysis` |  |  |  |  | Comprehensive analysis across multiple cutoff values |
| `results$results$diagnosticPerformance` |  |  |  |  | Comprehensive diagnostic performance measures at optimal cutoff |
| `results$results$clinicalApplicationMetrics` |  |  |  |  | Clinical metrics including predictive values and likelihood ratios |
| `results$results$partialAucAnalysis` |  |  |  |  | Partial AUC analysis for specific sensitivity/specificity ranges |
| `results$results$crocAnalysisTable` |  |  |  |  | Concentrated ROC analysis with early retrieval focus |
| `results$results$convexHullTable` |  |  |  |  | ROC convex hull showing optimal achievable performance |
| `results$results$comprehensiveAnalysisSummary` |  |  |  |  | Enhanced statistical summary for comprehensive output |
| `results$results$clinicalInterpretationGuide` |  |  |  |  | a html |
| `results$results$methodsExplanation` |  |  |  |  | a html |
| `results$results$rocCurvePlot` |  |  |  |  | ROC curves with optimal cutoff points |
| `results$results$prcPlot` |  |  |  |  | Precision-Recall curve for imbalanced data |
| `results$results$comparativeROCPlot` |  |  |  |  | Multiple ROC curves for comparison |
| `results$results$cutoffAnalysisPlot` |  |  |  |  | Sensitivity and specificity across cutoff values |
| `results$results$youdenIndexPlot` |  |  |  |  | Youden Index values across cutoff range |
| `results$results$clinicalDecisionPlot` |  |  |  |  | Clinical decision curves and threshold analysis |
| `results$results$crocCurvePlot` |  |  |  |  | Concentrated ROC curve with exponential magnifier transformation |
| `results$results$convexHullPlot` |  |  |  |  | ROC curve with convex hull overlay |
| `results$results$calibrationSummary` |  |  |  |  | Overall calibration metrics including Brier score |
| `results$results$hosmerLemeshowTable` |  |  |  |  | Hosmer-Lemeshow test for calibration |
| `results$results$calibrationPlotImage` |  |  |  |  | Calibration plot showing observed vs predicted probabilities |
| `results$results$multiClassAUC` |  |  |  |  | AUC values for each class in multi-class analysis |
| `results$results$multiClassAverage` |  |  |  |  | Averaged AUC across all classes |
| `results$results$multiClassROCPlot` |  |  |  |  | ROC curves for all classes |
| `results$results$clinicalImpactTable` |  |  |  |  | Number needed to test and clinical utility metrics |
| `results$results$decisionImpactSummary` |  |  |  |  | Clinical consequences at various decision thresholds |
| `results$results$clinicalUtilityPlot` |  |  |  |  | Clinical utility showing test consequences across thresholds |
