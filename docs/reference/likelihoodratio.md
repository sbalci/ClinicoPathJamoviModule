# Likelihood Ratio Analysis

Comprehensive likelihood ratio analysis for diagnostic test evaluation.

## Usage

``` r
likelihoodratio(
  data,
  testVariable,
  referenceStandard,
  groupVariable,
  analysisType = "binary",
  testDirection = "higher",
  cutpointMethod = "youden",
  manualCutpoint,
  costRatio = 1,
  confidenceLevel = 0.95,
  ciMethod = "log",
  bootstrapSamples = 2000,
  calculatePostTest = TRUE,
  prevalenceRange = "clinical",
  customPrevalenceMin = 5,
  customPrevalenceMax = 50,
  specificPrevalences = "10, 25, 50",
  calculateDOR = TRUE,
  calculateNRI = FALSE,
  compareVariable,
  predictiveValueCurves = TRUE,
  clinicalThresholds = "0.15, 0.85",
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
```

## Arguments

- data:

  The data as a data frame containing test results and reference
  standard.

- testVariable:

  Diagnostic test results (continuous or categorical)

- referenceStandard:

  True disease status (binary: positive/negative)

- groupVariable:

  Variable for stratified analysis (e.g., subgroups, centers)

- analysisType:

  Type of diagnostic test data

- testDirection:

  Direction of test values relative to disease presence

- cutpointMethod:

  Method for determining optimal cutpoint

- manualCutpoint:

  User-specified cutpoint value

- costRatio:

  Relative cost of false positive vs false negative

- confidenceLevel:

  Confidence level for likelihood ratio confidence intervals

- ciMethod:

  Method for calculating confidence intervals

- bootstrapSamples:

  Number of bootstrap samples for CI calculation

- calculatePostTest:

  Calculate post-test probabilities for different prevalences

- prevalenceRange:

  Range of disease prevalence for post-test probability analysis

- customPrevalenceMin:

  Minimum prevalence for custom range

- customPrevalenceMax:

  Maximum prevalence for custom range

- specificPrevalences:

  Specific prevalence values for detailed analysis

- calculateDOR:

  Calculate diagnostic odds ratio and confidence interval

- calculateNRI:

  Calculate NRI for comparing diagnostic tests

- compareVariable:

  Second test for comparison (NRI calculation)

- predictiveValueCurves:

  Generate PPV and NPV curves across prevalence range

- clinicalThresholds:

  Clinical probability thresholds for decision making (comma-separated)

- performStratified:

  Analyze likelihood ratios within subgroups

- testHomogeneity:

  Test for homogeneity of likelihood ratios across strata

- showSummaryTable:

  Show summary of diagnostic performance measures

- showCrosstabulation:

  Show detailed crosstabulation with cell counts

- showLikelihoodRatios:

  Show positive and negative likelihood ratios with CI

- showPostTestProbs:

  Show post-test probabilities at different prevalences

- showOptimalCutpoint:

  Show optimal cutpoint determination results

- showDiagnosticOdds:

  Show diagnostic odds ratio with confidence interval

- showClinicalInterpretation:

  Show clinical interpretation and usage guidelines

- plotROC:

  Generate ROC curve with optimal cutpoint marked

- plotPredictiveValues:

  Plot PPV and NPV across prevalence range

- plotLikelihoodRatios:

  Generate likelihood ratio nomogram for clinical use

- plotPostTestProbabilities:

  Visualize post-test probabilities vs prevalence

- plotDistributions:

  Show distributions of test results by disease status

- correctContinuity:

  Apply continuity correction for zero cells in 2×2 table

- exactTests:

  Use exact tests instead of asymptotic approximations

- smoothROC:

  Apply smoothing to ROC curve estimation

- randomSeed:

  Random seed for reproducible bootstrap results

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$summaryTable`           |     |     |     |     | a table  |
| `results$crosstabulation`        |     |     |     |     | a table  |
| `results$likelihoodRatios`       |     |     |     |     | a table  |
| `results$optimalCutpoint`        |     |     |     |     | a table  |
| `results$postTestProbabilities`  |     |     |     |     | a table  |
| `results$diagnosticOddsRatio`    |     |     |     |     | a table  |
| `results$stratifiedAnalysis`     |     |     |     |     | a table  |
| `results$homogeneityTest`        |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$rocCurve`               |     |     |     |     | an image |
| `results$predictiveValueCurves`  |     |     |     |     | an image |
| `results$likelihoodNomogram`     |     |     |     |     | an image |
| `results$postTestPlot`           |     |     |     |     | an image |
| `results$distributionPlot`       |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`

## Details

**Key Features:**

- Positive and negative likelihood ratios with confidence intervals

- Diagnostic odds ratios and clinical utility measures

- Youden Index for optimal cutpoint determination

- Post-test probability calculations across prevalence ranges

- Predictive value curves and clinical interpretation guides

**Applications:**

- Laboratory test validation and comparison

- Biomarker evaluation and cutpoint optimization

- Diagnostic accuracy assessment in pathology

- Clinical decision support tool evaluation

- Method comparison and diagnostic performance studies
