# Multi-Model Performance Comparison

Compare performance of multiple statistical models side-by-side.
Supports Cox proportional hazards, logistic regression, and linear
regression models. Provides unified comparison tables with AIC, BIC, R²,
C-index, and other metrics. Inspired by Orange Data Mining's Test &
Score widget, adapted for clinical research with comprehensive model
diagnostics.

## Usage

``` r
modelperformance(
  data,
  modelType = "cox",
  outcome = NULL,
  outcomeLevel,
  timeVar = NULL,
  model1vars = NULL,
  model1name = "Model 1",
  model2vars = NULL,
  model2name = "Model 2",
  model3vars = NULL,
  model3name = "Model 3",
  model4vars = NULL,
  model4name = "Model 4",
  model5vars = NULL,
  model5name = "Model 5",
  showAIC = TRUE,
  showRSquared = TRUE,
  showCIndex = TRUE,
  showLogLik = FALSE,
  showMCC = TRUE,
  crossValidation = FALSE,
  cvFolds = 5,
  showForestPlot = TRUE,
  showROC = FALSE,
  showCalibration = FALSE,
  autoRecommend = TRUE,
  recommendBy = "aic"
)
```

## Arguments

- data:

  .

- modelType:

  .

- outcome:

  .

- outcomeLevel:

  .

- timeVar:

  .

- model1vars:

  .

- model1name:

  .

- model2vars:

  .

- model2name:

  .

- model3vars:

  .

- model3name:

  .

- model4vars:

  .

- model4name:

  .

- model5vars:

  .

- model5name:

  .

- showAIC:

  .

- showRSquared:

  .

- showCIndex:

  .

- showLogLik:

  .

- showMCC:

  Matthews Correlation Coefficient (MCC) - a balanced metric for binary
  classification, especially useful for imbalanced datasets. Ranges from
  -1 (total disagreement) to +1 (perfect prediction).

- crossValidation:

  .

- cvFolds:

  .

- showForestPlot:

  .

- showROC:

  .

- showCalibration:

  .

- autoRecommend:

  .

- recommendBy:

  .

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$instructions`    |     |     |     |     | a html   |
| `results$comparisonTable` |     |     |     |     | a table  |
| `results$cvTable`         |     |     |     |     | a table  |
| `results$forestPlot`      |     |     |     |     | an image |
| `results$rocPlot`         |     |     |     |     | an image |
| `results$calibrationPlot` |     |     |     |     | an image |
| `results$recommendation`  |     |     |     |     | a html   |
| `results$modelDetails`    |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$comparisonTable$asDF`

`as.data.frame(results$comparisonTable)`
