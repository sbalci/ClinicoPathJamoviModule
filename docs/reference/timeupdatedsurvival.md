# Time-Updated Survival Estimates

Performs time-updated survival estimates using dynamic regression models
for survival data. This analysis allows for time-varying effects of
covariates, dynamic hazard functions, and temporal changes in survival
relationships. Multiple modeling approaches are supported including
Aalen additive models, Cox-Aalen hybrid models, and dynamic regression
with time-updated coefficients.

## Usage

``` r
timeupdatedsurvival(
  data,
  timeVar,
  statusVar,
  covariates,
  timeVaryingCovs,
  stratificationVar,
  modelType = "dynreg",
  timePoints = "6,12,24,36,60",
  bandwidthMethod = "auto",
  manualBandwidth = 1,
  confidenceLevel = 0.95,
  plotType = "coefficients",
  showTable = TRUE,
  showPlots = TRUE,
  showGoFTests = TRUE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- timeVar:

  Survival time variable (numeric)

- statusVar:

  Event indicator (0=censored, 1=event)

- covariates:

  Predictive variables for time-updated regression analysis

- timeVaryingCovs:

  Variables with time-varying effects

- stratificationVar:

  Variable for stratified analysis

- modelType:

  Type of time-updated survival model

- timePoints:

  Comma-separated time points for survival estimates (e.g., 6,12,24,60)

- bandwidthMethod:

  Method for bandwidth selection in smoothing

- manualBandwidth:

  Bandwidth value for manual specification

- confidenceLevel:

  Confidence level for intervals

- plotType:

  Type of plots to display

- showTable:

  Display time-updated parameter estimates table

- showPlots:

  Display time-updated survival plots

- showGoFTests:

  Display model goodness-of-fit assessments

- showExplanations:

  Display interpretation and methodology explanations

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$modelSummary`       |     |     |     |     | a table  |
| `results$timeVaryingEffects` |     |     |     |     | a table  |
| `results$survivalEstimates`  |     |     |     |     | a table  |
| `results$goodnessOfFit`      |     |     |     |     | a table  |
| `results$timeVaryingPlots`   |     |     |     |     | an image |
| `results$survivalPlots`      |     |     |     |     | an image |
| `results$methodExplanation`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
