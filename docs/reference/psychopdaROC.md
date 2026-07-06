# Advanced ROC Analysis

Receiver Operating Characteristic (ROC) curve analysis with optimal
cutpoint determination.

## Usage

``` r
psychopdaROC(
  manualRun = FALSE,
  run = FALSE,
  clinicalMode = "basic",
  data,
  dependentVars,
  classVar,
  positiveClass,
  subGroup = NULL,
  clinicalPreset = "none",
  method = "maximize_metric",
  metric = "youden",
  direction = ">=",
  specifyCutScore = "",
  tol_metric = 0.05,
  break_ties = "mean",
  allObserved = FALSE,
  boot_runs = 0,
  seed = 123,
  usePriorPrev = FALSE,
  priorPrev = 0.5,
  costratioFP = 1,
  sensSpecTable = FALSE,
  showThresholdTable = FALSE,
  maxThresholds = 20,
  delongTest = FALSE,
  plotROC = TRUE,
  combinePlots = TRUE,
  cleanPlot = FALSE,
  showOptimalPoint = TRUE,
  displaySE = FALSE,
  smoothing = FALSE,
  showConfidenceBands = FALSE,
  legendPosition = "right",
  directLabel = FALSE,
  interactiveROC = FALSE,
  showCriterionPlot = FALSE,
  showPrevalencePlot = FALSE,
  showDotPlot = FALSE,
  precisionRecallCurve = FALSE,
  partialAUC = FALSE,
  partialAUCfrom = 0.8,
  partialAUCto = 1,
  rocSmoothingMethod = "none",
  bootstrapCI = FALSE,
  bootstrapReps = 2000,
  quantileCIs = FALSE,
  quantiles = "0.1,0.25,0.5,0.75,0.9",
  compareClassifiers = FALSE,
  calculateIDI = FALSE,
  calculateNRI = FALSE,
  refVar,
  nriThresholds = "",
  idiNriBootRuns = 1000,
  effectSizeAnalysis = FALSE,
  powerAnalysis = FALSE,
  powerAnalysisType = "post_hoc",
  expectedAUCDifference = 0.1,
  targetPower = 0.8,
  significanceLevel = 0.05,
  correlationROCs = 0.5,
  bayesianAnalysis = FALSE,
  priorAUC = 0.7,
  priorPrecision = 10,
  clinicalUtilityAnalysis = FALSE,
  treatmentThreshold = "0.05,0.5,0.05",
  harmBenefitRatio = 0.25,
  interventionCost = FALSE,
  fixedSensSpecAnalysis = FALSE,
  fixedAnalysisType = "sensitivity",
  fixedSensitivityValue = 0.9,
  fixedSpecificityValue = 0.9,
  showFixedROC = TRUE,
  fixedInterpolation = "linear",
  showFixedExplanation = TRUE,
  metaAnalysis = FALSE,
  metaAnalysisMethod = "both",
  heterogeneityTest = TRUE,
  forestPlot = FALSE,
  overrideMetaAnalysisWarning = FALSE
)
```

## Arguments

- manualRun:

  When TRUE, results are only computed after clicking the Run button.
  Useful for skipping intermediate recomputes while adjusting options on
  slow bootstrap or cutpoint analyses.

- run:

  .

- clinicalMode:

  Select the complexity level of analysis: Basic - Essential ROC metrics
  for clinical decision making Advanced - Additional statistical
  comparisons and metrics Comprehensive - Full research-grade analysis
  with all options

- data:

  The data as a data frame.

- dependentVars:

  Test variable(s) to be evaluated for classification performance.
  Multiple variables can be selected for comparison.

- classVar:

  Binary classification variable representing the true class (gold
  standard). Must have exactly two levels.

- positiveClass:

  Specifies which level of the class variable should be treated as the
  positive class.

- subGroup:

  Optional grouping variable for stratified analysis. ROC curves will be
  calculated separately for each group.

- clinicalPreset:

  Choose a preset configuration optimized for specific clinical
  scenarios: Screening - High sensitivity to avoid missing cases
  Confirmation - High specificity to avoid false positives Balanced -
  Equal weight to sensitivity and specificity Research - Comprehensive
  analysis for publication

- method:

  Method for determining the optimal cutpoint. Different methods
  optimize different aspects of classifier performance.

- metric:

  Metric to optimize when determining the cutpoint. Only applies to
  maximize/minimize methods.

- direction:

  Direction of classification relative to the cutpoint. Use '\>=' when
  higher test values indicate the positive class.

- specifyCutScore:

  Specific cutpoint value to use when method is set to 'Manual
  cutpoint'.

- tol_metric:

  Tolerance for the metric value when multiple cutpoints yield similar
  performance. Cutpoints within this tolerance are considered
  equivalent.

- break_ties:

  Method for handling ties when multiple cutpoints achieve the same
  metric value.

- allObserved:

  Display performance metrics for all observed test values as potential
  cutpoints, not just the optimal cutpoint.

- boot_runs:

  Number of bootstrap iterations for methods using bootstrapping. Set to
  0 to disable bootstrapping.

- seed:

  Random seed for reproducibility of bootstrap and permutation tests.

- usePriorPrev:

  Use a specified prior prevalence instead of the sample prevalence for
  calculating predictive values.

- priorPrev:

  Population prevalence to use for predictive value calculations. Only
  used when 'Use Prior Prevalence' is checked.

- costratioFP:

  Relative cost of false positives compared to false negatives. Values
  \> 1 penalize false positives more heavily.

- sensSpecTable:

  Display detailed confusion matrices at optimal cutpoints.

- showThresholdTable:

  Display detailed table with performance metrics at multiple
  thresholds.

- maxThresholds:

  Maximum number of threshold values to show in the threshold table.

- delongTest:

  Test whether the diagnostic performance differs significantly between
  multiple tests. Uses DeLong's method to compare Area Under the Curve
  (AUC) values. Requires at least two test variables.

- plotROC:

  Display ROC curves for visual assessment of classifier performance.

- combinePlots:

  When multiple test variables are selected, combine all ROC curves in a
  single plot.

- cleanPlot:

  Create clean ROC curves without annotations, suitable for
  publications.

- showOptimalPoint:

  Display the optimal cutpoint on the ROC curve.

- displaySE:

  Display standard error bands on ROC curves (when LOESS smoothing is
  applied).

- smoothing:

  Apply LOESS smoothing to ROC curves for visualization.

- showConfidenceBands:

  Display confidence bands around the ROC curve.

- legendPosition:

  Position of the legend in plots with multiple ROC curves.

- directLabel:

  Label curves directly on the plot instead of using a legend.

- interactiveROC:

  Create an interactive HTML ROC plot (requires plotROC package).

- showCriterionPlot:

  Plot showing how sensitivity and specificity change across different
  thresholds.

- showPrevalencePlot:

  Plot showing how PPV and NPV change with disease prevalence.

- showDotPlot:

  Dot plot showing the distribution of test values by class.

- precisionRecallCurve:

  Display precision-recall curves alongside ROC curves.

- partialAUC:

  Calculate AUC for a specific region of the ROC curve.

- partialAUCfrom:

  Lower bound of specificity range for partial AUC calculation.

- partialAUCto:

  Upper bound of specificity range for partial AUC calculation.

- rocSmoothingMethod:

  Method for smoothing the ROC curve (requires pROC package).

- bootstrapCI:

  Calculate bootstrap confidence intervals for AUC and optimal
  cutpoints.

- bootstrapReps:

  Number of bootstrap replications for confidence interval calculation.

- quantileCIs:

  Display confidence intervals at specific quantiles of the test
  variable.

- quantiles:

  Comma-separated list of quantiles (0-1) at which to display confidence
  intervals.

- compareClassifiers:

  Perform comprehensive comparison of classifier performance metrics.

- calculateIDI:

  Calculate how much better one test is at discriminating between
  diseased and healthy patients. IDI (Integrated Discrimination
  Improvement) measures the average improvement in predicted
  probabilities.

- calculateNRI:

  Calculate how many patients are correctly reclassified when using a
  new test. NRI (Net Reclassification Index) measures the net
  improvement in patient classification.

- refVar:

  Reference test variable for IDI and NRI calculations. Other variables
  will be compared against this reference.

- nriThresholds:

  Comma-separated probability thresholds (0-1) defining risk categories
  for NRI. Leave empty for continuous NRI.

- idiNriBootRuns:

  Number of bootstrap iterations for IDI and NRI confidence intervals.

- effectSizeAnalysis:

  Calculate effect sizes for ROC curve differences using Cohen's
  conventions and standardized mean differences between AUC values.

- powerAnalysis:

  Perform statistical power analysis for ROC curve comparisons including
  sample size estimation and power calculations for detecting AUC
  differences.

- powerAnalysisType:

  Type of power analysis to perform.

- expectedAUCDifference:

  Expected difference in AUC values for power calculations and sample
  size estimation.

- targetPower:

  Target statistical power for sample size calculations (typically 0.8
  or 0.9).

- significanceLevel:

  Type I error rate for power calculations (typically 0.05).

- correlationROCs:

  Expected correlation between paired ROC curves for power calculations.
  Use 0.5 for moderate correlation, 0.0 for independent samples.

- bayesianAnalysis:

  Perform bootstrap-based ROC analysis with optional prior weighting to
  estimate uncertainty in AUC. Uses bootstrap resampling to create an
  empirical distribution. NOTE: This is NOT full Bayesian MCMC
  inference; it uses bootstrap simulation with prior parameters as
  weights. Interpret "credible intervals" as bootstrap percentile
  confidence intervals.

- priorAUC:

  Prior belief about AUC value for Bayesian analysis (center of prior
  distribution).

- priorPrecision:

  Precision of prior belief (higher values = more confident prior).

- clinicalUtilityAnalysis:

  Perform clinical utility analysis including net benefit curves,
  decision curve analysis, and clinical impact assessment.

- treatmentThreshold:

  Treatment threshold range for decision curve analysis (min,max,step).
  Example: "0.05,0.5,0.05" creates thresholds from 5 percent to 50
  percent in 5 percent steps.

- harmBenefitRatio:

  Ratio of harm from unnecessary treatment to benefit from necessary
  treatment. Lower values favor more aggressive treatment policies.

- interventionCost:

  Include cost-effectiveness considerations in clinical utility
  analysis.

- fixedSensSpecAnalysis:

  Determine cutoffs based on fixed sensitivity or specificity values and
  display corresponding performance metrics.

- fixedAnalysisType:

  Choose whether to fix sensitivity or specificity value for cutoff
  determination.

- fixedSensitivityValue:

  Target sensitivity value (0-1) for determining the corresponding
  cutoff and specificity.

- fixedSpecificityValue:

  Target specificity value (0-1) for determining the corresponding
  cutoff and sensitivity.

- showFixedROC:

  Display separate ROC curve highlighting the fixed
  sensitivity/specificity point.

- fixedInterpolation:

  Method for interpolating between observed points to achieve target
  sensitivity/specificity.

- showFixedExplanation:

  Display explanatory guide for fixed sensitivity/specificity analysis
  including clinical interpretation, interpolation methods, and usage
  recommendations.

- metaAnalysis:

  Perform meta-analysis of AUC values across multiple test variables.
  Requires at least 3 test variables to enable pooled effect estimation.

- metaAnalysisMethod:

  Statistical method for combining AUC estimates across
  studies/variables.

- heterogeneityTest:

  Perform Cochran's Q test and calculate I² statistic to assess
  heterogeneity between AUC estimates.

- forestPlot:

  Create forest plot visualization of individual and pooled AUC
  estimates with confidence intervals.

- overrideMetaAnalysisWarning:

  ADVANCED OPTION: Bypass the independence assumption check for
  meta-analysis. WARNING - Only use if you fully understand the
  statistical implications. Meta-analysis on non-independent data
  produces invalid results and should NOT be used for formal inference.
  Use DeLong's test instead for within-study comparisons of multiple
  markers.

## Value

A results object containing:

|                                       |     |     |     |     |                    |
|---------------------------------------|-----|-----|-----|-----|--------------------|
| `results$instructions`                |     |     |     |     | a html             |
| `results$procedureNotes`              |     |     |     |     | a html             |
| `results$runSummary`                  |     |     |     |     | a html             |
| `results$simpleResultsTable`          |     |     |     |     | a table            |
| `results$clinicalInterpretationTable` |     |     |     |     | a table            |
| `results$resultsTable`                |     |     |     |     | an array of tables |
| `results$sensSpecTable`               |     |     |     |     | an array of htmls  |
| `results$thresholdTable`              |     |     |     |     | a table            |
| `results$fixedSensSpecTable`          |     |     |     |     | a table            |
| `results$fixedSensSpecExplanation`    |     |     |     |     | a html             |
| `results$aucSummaryTable`             |     |     |     |     | a table            |
| `results$delongComparisonTable`       |     |     |     |     | a table            |
| `results$delongTest`                  |     |     |     |     | a preformatted     |
| `results$plotROC`                     |     |     |     |     | an array of images |
| `results$interactivePlot`             |     |     |     |     | an image           |
| `results$fixedSensSpecROC`            |     |     |     |     | an array of images |
| `results$criterionPlot`               |     |     |     |     | an array of images |
| `results$prevalencePlot`              |     |     |     |     | an array of images |
| `results$dotPlot`                     |     |     |     |     | an array of images |
| `results$dotPlotMessage`              |     |     |     |     | a html             |
| `results$precisionRecallPlot`         |     |     |     |     | an array of images |
| `results$idiTable`                    |     |     |     |     | a table            |
| `results$nriTable`                    |     |     |     |     | a table            |
| `results$effectSizeTable`             |     |     |     |     | a table            |
| `results$powerAnalysisTable`          |     |     |     |     | a table            |
| `results$bayesianROCTable`            |     |     |     |     | a table            |
| `results$clinicalUtilityTable`        |     |     |     |     | a table            |
| `results$metaAnalysisWarning`         |     |     |     |     | a html             |
| `results$metaAnalysisTable`           |     |     |     |     | a table            |
| `results$decisionCurveTable`          |     |     |     |     | a table            |
| `results$partialAUCTable`             |     |     |     |     | a table            |
| `results$bootstrapCITable`            |     |     |     |     | a table            |
| `results$rocComparisonTable`          |     |     |     |     | a table            |
| `results$effectSizePlot`              |     |     |     |     | an array of images |
| `results$powerCurvePlot`              |     |     |     |     | an array of images |
| `results$bayesianTracePlot`           |     |     |     |     | an array of images |
| `results$decisionCurvePlot`           |     |     |     |     | an array of images |
| `results$metaAnalysisForestPlot`      |     |     |     |     | an array of images |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$simpleResultsTable$asDF`

`as.data.frame(results$simpleResultsTable)`
