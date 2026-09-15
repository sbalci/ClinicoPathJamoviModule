# Threshold Regression Models

Performs threshold regression analysis for survival data with
change-points. This method is designed to identify and model thresholds
or change-points in survival processes where the hazard function changes
at unknown time points.

## Usage

``` r
thresholdregression(
  data,
  time,
  status,
  covariates,
  thresholdType = "single",
  estimationMethod = "mle",
  maxThresholds = 3,
  confidenceLevel = 0.95,
  bootstrapSamples = 500,
  performBootstrap = FALSE,
  selectionCriterion = "bic",
  plotHazard = TRUE,
  plotResiduals = FALSE,
  diagnosticTests = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time:

  the time variable for survival analysis

- status:

  the status variable (0 = censored, 1 = event)

- covariates:

  covariates to include in the threshold regression model

- thresholdType:

  the type of threshold model to fit

- estimationMethod:

  the estimation method for threshold parameters

- maxThresholds:

  maximum number of thresholds to consider for multiple threshold models

- confidenceLevel:

  the confidence level for threshold estimates

- bootstrapSamples:

  number of bootstrap samples for confidence intervals

- performBootstrap:

  whether to perform bootstrap analysis for confidence intervals

- selectionCriterion:

  criterion for selecting the optimal number of thresholds

- plotHazard:

  whether to plot the estimated hazard function with thresholds

- plotResiduals:

  whether to plot model residuals

- diagnosticTests:

  whether to perform diagnostic tests for threshold significance

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$thresholdEstimates` |     |     |     |     | a table  |
| `results$modelCoefficients`  |     |     |     |     | a table  |
| `results$modelSelection`     |     |     |     |     | a table  |
| `results$diagnosticTests`    |     |     |     |     | a table  |
| `results$bootstrapResults`   |     |     |     |     | a table  |
| `results$hazardPlot`         |     |     |     |     | an image |
| `results$residualPlot`       |     |     |     |     | an image |
| `results$modelSummary`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$thresholdEstimates$asDF`

`as.data.frame(results$thresholdEstimates)`

## Details

Key features:

- Multiple change-point detection in survival data

- Threshold regression with parametric and semi-parametric approaches

- Covariate effects on threshold parameters

- Model selection for optimal number of thresholds

- Bootstrap confidence intervals for threshold estimates

- Graphical visualization of hazard changes

## Examples

``` r
# Threshold regression with single change-point
thresholdregression(
    data = data,
    time = "time",
    status = "status",
    covariates = c("age", "treatment"),
    thresholdType = "single",
    estimationMethod = "mle"
)
#> Error in thresholdregression(data = data, time = "time", status = "status",     covariates = c("age", "treatment"), thresholdType = "single",     estimationMethod = "mle"): object 'thresholdregressionClass' not found
```
