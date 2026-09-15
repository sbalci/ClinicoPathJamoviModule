# AI Model Validation

Simplified AI model validation tool for comparing diagnostic
performance. Calculates AUC, sensitivity, and specificity for predictor
variables and performs statistical comparison using DeLong test.

## Usage

``` r
aivalidation(
  data,
  predictorVars,
  outcomeVar = NULL,
  positiveLevel,
  compareModels = FALSE,
  youdensJ = FALSE,
  matthewsCC = FALSE,
  bootstrapCI = FALSE,
  nBootstrap = 1000,
  rocPlot = FALSE,
  crossValidation = "none",
  stratified = TRUE,
  randomSeed = 42,
  showExplanations = FALSE,
  showSummaries = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- predictorVars:

  a vector of strings naming the predictor variables (AI scores, human
  scores, biomarkers, etc.) from `data`. Limited to first 5 for pairwise
  comparisons.

- outcomeVar:

  a string naming the binary outcome variable (gold standard) from
  `data`

- positiveLevel:

  the level of the outcome variable which represents the positive case

- compareModels:

  perform statistical comparison between models using DeLong test for
  AUC comparison

- youdensJ:

  calculate and display Youden's J statistic (Sensitivity +
  Specificity - 1)

- matthewsCC:

  calculate and display Matthews Correlation Coefficient (MCC)

- bootstrapCI:

  use bootstrap resampling for confidence intervals (more robust for
  small samples)

- nBootstrap:

  number of bootstrap iterations (higher values are more accurate but
  slower)

- rocPlot:

  generate ROC curves for all predictor variables

- crossValidation:

  cross-validation method for model validation (simplified to avoid
  resource limits)

- stratified:

  maintain outcome variable proportions across folds

- randomSeed:

  random seed for reproducible cross-validation results

- showExplanations:

  show detailed methodology explanations

- showSummaries:

  show interpretation summaries of results

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$performanceTable` |  |  |  |  | Performance metrics for each predictor variable |
| `results$comparisonTable` |  |  |  |  | Statistical comparison between predictor models using DeLong test |
| `results$cvPerformanceTable` |  |  |  |  | Cross-validated performance metrics for each predictor |
| `results$rocPlot` |  |  |  |  | ROC curves for all predictor models |
| `results$methodologyExplanation` |  |  |  |  | a html |
| `results$resultsInterpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performanceTable$asDF`

`as.data.frame(results$performanceTable)`
