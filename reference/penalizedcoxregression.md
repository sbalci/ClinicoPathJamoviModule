# Penalized Cox Regression

Performs penalized Cox proportional hazards regression for
high-dimensional survival data using L1 (LASSO), L2 (Ridge), and Elastic
Net regularization. This method is particularly useful when the number
of covariates approaches or exceeds the number of observations, or when
variable selection is needed.

## Usage

``` r
penalizedcoxregression(
  data,
  time,
  status,
  predictors,
  penaltyType = "lasso",
  alphaValue = 0.5,
  crossValidation = TRUE,
  cvFolds = 10,
  lambdaSelection = "lambda.1se",
  customLambda = 0.01,
  standardize = TRUE,
  plotCoefficientPath = TRUE,
  plotCrossValidation = TRUE,
  variableSelection = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time:

  the time variable for survival analysis

- status:

  the status variable (0 = censored, 1 = event)

- predictors:

  predictor variables for the penalized Cox model

- penaltyType:

  the type of penalty to apply in regularization

- alphaValue:

  mixing parameter for elastic net (0 = ridge, 1 = lasso)

- crossValidation:

  whether to use cross-validation for penalty parameter selection

- cvFolds:

  number of folds for cross-validation

- lambdaSelection:

  criterion for selecting the regularization parameter

- customLambda:

  custom lambda value when using custom selection

- standardize:

  whether to standardize predictor variables

- plotCoefficientPath:

  whether to plot coefficient paths across lambda values

- plotCrossValidation:

  whether to plot cross-validation error curves

- variableSelection:

  whether to output selected variables and their importance

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$modelSummary`           |     |     |     |     | a table  |
| `results$selectedVariables`      |     |     |     |     | a table  |
| `results$crossValidationResults` |     |     |     |     | a table  |
| `results$penaltyPath`            |     |     |     |     | a table  |
| `results$coefficientPath`        |     |     |     |     | an image |
| `results$crossValidationPlot`    |     |     |     |     | an image |
| `results$analysisReport`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`

## Details

Key features:

- LASSO (L1) regularization for automatic variable selection

- Ridge (L2) regularization for correlated predictors

- Elastic Net combining L1 and L2 penalties

- Cross-validation for optimal penalty parameter selection

- Coefficient path visualization across penalty values

- Variable importance ranking and selection

- High-dimensional survival analysis capabilities

## Examples

``` r
# Penalized Cox regression with LASSO
penalizedcoxregression(
    data = data,
    time = "time",
    status = "status",
    predictors = c("age", "gene1", "gene2", "treatment"),
    penaltyType = "lasso",
    crossValidation = TRUE
)
#> Error: Argument 'data' must be a data frame
```
