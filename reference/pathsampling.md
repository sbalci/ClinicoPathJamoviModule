# Pathology Sampling Adequacy Analysis

Pathology Sampling Adequacy Analysis

## Usage

``` r
pathsampling(
  data,
  analysisContext = "general",
  totalSamples = NULL,
  firstDetection = NULL,
  positiveCount = NULL,
  positiveSamplesList = NULL,
  sampleType = NULL,
  targetConfidence = 0.95,
  maxSamples = 10,
  bootstrapIterations = 10000,
  showBinomialModel = FALSE,
  showBootstrap = FALSE,
  showDetectionCurve = FALSE,
  showSensitivityCI = FALSE,
  setSeed = FALSE,
  seedValue = 42,
  positiveCassettes = NULL,
  maxPositiveSingle = NULL,
  showTumorBurden = FALSE,
  showStageMigration = FALSE,
  showCorrelation = FALSE,
  showDistributionPattern = FALSE,
  distributionThreshold = 5,
  modelType = "binomial",
  totalPopulation = NULL,
  successStates = NULL,
  targetDetections = 1,
  showHypergeometric = FALSE,
  showBetaBinomial = FALSE,
  totalLymphNodes = NULL,
  positiveLymphNodes = NULL,
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
  estimationMethod = "auto",
  showHeterogeneityTest = FALSE,
  useGeometricCI = TRUE,
  ciMethod = "auto",
  showModelFit = FALSE,
  showObsPred = FALSE,
  showMarginalInterpretation = TRUE,
  showPowerAnalysis = FALSE,
  targetPower = 0.8,
  targetDetectionProb = 0.95,
  autoSelectModel = FALSE,
  appendVariables = FALSE,
  appendPrefix = "ps_",
  autoDetectHeterogeneity = TRUE
)
```

## Arguments

- data:

  .

- analysisContext:

  Select the type of pathology sampling analysis: - Lymph Node: Adequacy
  of lymph node dissection - Omentum: Omentum sampling in
  ovarian/gastric cancer - Tumor: Tumor block sampling for VI, EMVI,
  PNI, or budding detection - Margin: Margin sampling adequacy -
  General: Custom sampling adequacy analysis

- totalSamples:

  Total number of samples examined per case. Examples: total tumor
  blocks examined, total lymph nodes dissected, total omentum sections.

- firstDetection:

  Sample number where outcome was first detected (e.g., which block
  first showed VI). Leave as NA for negative cases. For positive cases,
  enter the sequential sample number (1, 2, 3, etc.).

- positiveCount:

  .

- positiveSamplesList:

  .

- sampleType:

  .

- targetConfidence:

  .

- maxSamples:

  .

- bootstrapIterations:

  .

- showBinomialModel:

  .

- showBootstrap:

  .

- showDetectionCurve:

  .

- showSensitivityCI:

  .

- setSeed:

  .

- seedValue:

  .

- positiveCassettes:

  .

- maxPositiveSingle:

  .

- showTumorBurden:

  .

- showStageMigration:

  .

- showCorrelation:

  .

- showDistributionPattern:

  .

- distributionThreshold:

  .

- modelType:

  .

- totalPopulation:

  .

- successStates:

  .

- targetDetections:

  .

- showHypergeometric:

  .

- showBetaBinomial:

  .

- totalLymphNodes:

  .

- positiveLymphNodes:

  .

- showLNAnalysis:

  .

- lnrThreshold1:

  .

- lnrThreshold2:

  .

- showEffectSizes:

  .

- showOmentumAnalysis:

  .

- showClinicalSummary:

  .

- showGuidedInstructions:

  Display detailed step-by-step checklist with examples and common use
  cases. Best for first-time users and training.

- showConciseInstructions:

  Display brief analysis overview with required variables and methods.
  Best for experienced users.

- showEmpiricalCumulative:

  .

- showSpatialClustering:

  .

- showStratifiedAnalysis:

  .

- showPopulationDetection:

  .

- showIncrementalYield:

  .

- showMultifocalAnalysis:

  .

- showProbabilityExplanation:

  Show detailed explanation distinguishing conditional vs
  population-level detection probabilities.

- showKeyResults:

  Display a concise summary of key findings at the top of results.

- showRecommendText:

  Display context-specific clinical recommendations based on analysis
  results.

- showInterpretText:

  Show statistical interpretation and guidance for result
  interpretation.

- showReferencesText:

  Display statistical methods documentation and literature references.

- estimationMethod:

  .

- showHeterogeneityTest:

  Test if detection probability varies significantly across sample
  types. Only available when Sample Type variable is specified. Uses
  likelihood ratio test.

- useGeometricCI:

  Use theoretical geometric model confidence intervals when bootstrap
  shows ceiling effect (100 percent bounds). Provides more realistic
  uncertainty estimates.

- ciMethod:

  Method for calculating confidence intervals. Auto selects geometric
  when bootstrap shows ceiling effect, otherwise uses bootstrap.

- showModelFit:

  Perform goodness-of-fit test comparing observed vs predicted detection
  distribution. Validates geometric/binomial model assumptions.

- showObsPred:

  Compare observed detection rates with model predictions at each sample
  number. Visual model validation tool.

- showMarginalInterpretation:

  Add cost-benefit interpretation to marginal gain analysis.
  Auto-enabled when Show Binomial Model is checked.

- showPowerAnalysis:

  .

- targetPower:

  .

- targetDetectionProb:

  .

- autoSelectModel:

  .

- appendVariables:

  Add new columns to dataset with calculated detection probabilities and
  classifications. Variables include cumulative probabilities,
  recommended sample counts, and detection categories.

- appendPrefix:

  Prefix for appended variable names (e.g., 'ps\_' creates
  'ps_cumulative_prob_5'). Only used when Append Variables is enabled.

- autoDetectHeterogeneity:

  Automatically analyze and report if sample types have significantly
  different detection probabilities. Requires Sample Type variable.
  Warns if CV \> 30 percent.

## Value

A results object containing:

|                                       |     |     |     |     |                |
|---------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                     |     |     |     |     | a preformatted |
| `results$welcome`                     |     |     |     |     | a html         |
| `results$guidedInstructions`          |     |     |     |     | a html         |
| `results$conciseInstructions`         |     |     |     |     | a html         |
| `results$dataInfo`                    |     |     |     |     | a table        |
| `results$probabilityExplanation`      |     |     |     |     | a html         |
| `results$keyResults`                  |     |     |     |     | a html         |
| `results$clinicalSummary`             |     |     |     |     | a html         |
| `results$binomialText`                |     |     |     |     | a html         |
| `results$binomialTable`               |     |     |     |     | a table        |
| `results$recommendTable`              |     |     |     |     | a table        |
| `results$bootstrapText`               |     |     |     |     | a html         |
| `results$bootstrapTable`              |     |     |     |     | a table        |
| `results$detectionCurve`              |     |     |     |     | an image       |
| `results$sensitivityPlot`             |     |     |     |     | an image       |
| `results$heterogeneityText`           |     |     |     |     | a html         |
| `results$heterogeneityTest`           |     |     |     |     | a table        |
| `results$modelFitText`                |     |     |     |     | a html         |
| `results$modelFitTable`               |     |     |     |     | a table        |
| `results$obsPredText`                 |     |     |     |     | a html         |
| `results$obsPredTable`                |     |     |     |     | a table        |
| `results$empiricalCumulativeText`     |     |     |     |     | a html         |
| `results$empiricalCumulativeTable`    |     |     |     |     | a table        |
| `results$empiricalCumulativePlot`     |     |     |     |     | an image       |
| `results$incrementalYieldText`        |     |     |     |     | a html         |
| `results$incrementalYieldTable`       |     |     |     |     | a table        |
| `results$populationDetectionText`     |     |     |     |     | a html         |
| `results$populationDetectionTable`    |     |     |     |     | a table        |
| `results$spatialClusteringText`       |     |     |     |     | a html         |
| `results$clusteringTable`             |     |     |     |     | a table        |
| `results$multifocalText`              |     |     |     |     | a html         |
| `results$multifocalTable`             |     |     |     |     | a table        |
| `results$stratifiedText`              |     |     |     |     | a html         |
| `results$prevalenceTable`             |     |     |     |     | a table        |
| `results$stratifiedDetectionTable`    |     |     |     |     | a table        |
| `results$tumorBurdenText`             |     |     |     |     | a html         |
| `results$tumorBurdenInfo`             |     |     |     |     | a table        |
| `results$cassetteDistribution`        |     |     |     |     | a table        |
| `results$stageMigrationText`          |     |     |     |     | a html         |
| `results$stageMigrationTable`         |     |     |     |     | a table        |
| `results$correlationText`             |     |     |     |     | a html         |
| `results$correlationStats`            |     |     |     |     | a table        |
| `results$correlationPlot`             |     |     |     |     | an image       |
| `results$distributionPatternText`     |     |     |     |     | a html         |
| `results$distributionPatternTable`    |     |     |     |     | a table        |
| `results$distributionComparisonTable` |     |     |     |     | a table        |
| `results$hypergeometricText`          |     |     |     |     | a html         |
| `results$hypergeometricTable`         |     |     |     |     | a table        |
| `results$hyperRecommendTable`         |     |     |     |     | a table        |
| `results$betaBinomialText`            |     |     |     |     | a html         |
| `results$betaBinomialTable`           |     |     |     |     | a table        |
| `results$betaBinomialRecommendTable`  |     |     |     |     | a table        |
| `results$powerAnalysisText`           |     |     |     |     | a html         |
| `results$powerTable`                  |     |     |     |     | a table        |
| `results$multifocalAnalysisText`      |     |     |     |     | a html         |
| `results$multifocalProbTable`         |     |     |     |     | a table        |
| `results$modelSelectionText`          |     |     |     |     | a html         |
| `results$modelComparisonTable`        |     |     |     |     | a table        |
| `results$lnAnalysisText`              |     |     |     |     | a html         |
| `results$lnrClassification`           |     |     |     |     | a table        |
| `results$ajccNStage`                  |     |     |     |     | a table        |
| `results$adequacyByELN`               |     |     |     |     | a table        |
| `results$effectSizesText`             |     |     |     |     | a html         |
| `results$effectSizesTable`            |     |     |     |     | a table        |
| `results$omentumText`                 |     |     |     |     | a html         |
| `results$recommendText`               |     |     |     |     | a html         |
| `results$interpretText`               |     |     |     |     | a html         |
| `results$referencesText`              |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
