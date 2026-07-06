# Multi-class Diagnostic Performance Evaluation

Comprehensive evaluation of multi-class classification performance
including per-class and overall metrics, ROC curves, confusion matrices,
and model comparison capabilities.

## Usage

``` r
multiclassdiagnostics(
  data,
  predicted,
  actual,
  positiveClass,
  confidenceLevel = 0.95,
  showROC = TRUE,
  showConfusion = TRUE,
  showPerClass = TRUE,
  showOverall = TRUE,
  compareModels = FALSE,
  predicted2,
  modelNames = "Model 1,Model 2",
  deLongTest = TRUE,
  mcnemarTest = TRUE,
  plotTheme = "default",
  saveResults = FALSE
)
```

## Arguments

- data:

  .

- predicted:

  Variable containing predicted class labels

- actual:

  Variable containing true class labels

- positiveClass:

  For binary classification, specify which class is considered positive

- confidenceLevel:

  Confidence level for intervals

- showROC:

  Display ROC curves for each class (one-vs-rest)

- showConfusion:

  Display confusion matrix

- showPerClass:

  Show sensitivity, specificity, PPV, NPV for each class

- showOverall:

  Show overall accuracy, kappa, and weighted metrics

- compareModels:

  Enable model comparison

- predicted2:

  Predicted classes for second model (for comparison)

- modelNames:

  Names for the models being compared (comma-separated)

- deLongTest:

  Perform DeLong test for ROC curve comparison (binary classification
  only)

- mcnemarTest:

  Perform McNemar test for paired model comparison

- plotTheme:

  Theme for plots

- saveResults:

  Save detailed results to file

## Value

A results object containing:

|                               |     |     |     |     |                |
|-------------------------------|-----|-----|-----|-----|----------------|
| `results$confusionMatrix`     |     |     |     |     | a table        |
| `results$perClassMetrics`     |     |     |     |     | a table        |
| `results$overallMetrics`      |     |     |     |     | a table        |
| `results$modelComparison`     |     |     |     |     | a table        |
| `results$deLongResults`       |     |     |     |     | a table        |
| `results$mcnemarResults`      |     |     |     |     | a table        |
| `results$rocPlot`             |     |     |     |     | an image       |
| `results$confusionPlot`       |     |     |     |     | an image       |
| `results$metricsPlot`         |     |     |     |     | an image       |
| `results$modelComparisonPlot` |     |     |     |     | an image       |
| `results$text`                |     |     |     |     | a preformatted |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$confusionMatrix$asDF`

`as.data.frame(results$confusionMatrix)`
