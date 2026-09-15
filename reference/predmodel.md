# Clinical Prediction Model

Build and validate clinical prediction models with integrated
calibration and discrimination metrics. Supports logistic regression
with stepwise/LASSO selection, ROC analysis, calibration plots, and
bootstrap validation.

## Usage

``` r
predmodel(
  data,
  outcome,
  predictors,
  modelSelection = "none",
  validationMethod = "bootstrap",
  nBootstrap = 200,
  nFolds = 10,
  showCalibration = TRUE,
  showDiscrimination = TRUE,
  showRiskGroups = TRUE,
  riskCutoffs = "0.33, 0.67",
  ciLevel = 0.95
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome variable (disease/event vs. no disease/no event).

- predictors:

  Continuous or categorical predictor variables for the model.

- modelSelection:

  Method for variable selection.

- validationMethod:

  Internal validation method to assess model performance.

- nBootstrap:

  Number of bootstrap samples for optimism correction.

- nFolds:

  Number of folds for cross-validation.

- showCalibration:

  Display calibration plot and Hosmer-Lemeshow test.

- showDiscrimination:

  Display ROC curve, AUC, and Brier score.

- showRiskGroups:

  Stratify patients into risk groups (low/medium/high).

- riskCutoffs:

  Comma-separated risk probability cutoffs (e.g., "0.25, 0.75").

- ciLevel:

  Confidence level for intervals (0.80-0.99).

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`          |     |     |     |     | a html   |
| `results$coefficients`          |     |     |     |     | a table  |
| `results$discriminationMetrics` |     |     |     |     | a table  |
| `results$calibrationMetrics`    |     |     |     |     | a table  |
| `results$riskStratification`    |     |     |     |     | a table  |
| `results$validationMetrics`     |     |     |     |     | a table  |
| `results$rocPlot`               |     |     |     |     | an image |
| `results$calibrationPlot`       |     |     |     |     | an image |
| `results$interpretation`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficients$asDF`

`as.data.frame(results$coefficients)`
