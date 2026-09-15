# Interval-Censored Cure Models

Performs cure model analysis for interval-censored survival data using
the ICGOR (Interval-Censored Generalized Odds Rate) methodology. This
approach is specifically designed for situations where the exact event
time is not known, but falls within an interval, and a fraction of the
population may be cured (i.e., will never experience the event).

## Usage

``` r
intervalcensorcure(
  data,
  leftTime,
  rightTime,
  covariates,
  survivalDistribution = "weibull",
  cureModel = "mixture",
  includeUncured = TRUE,
  confidenceLevel = 0.95,
  maxIterations = 1000,
  tolerance = 1e-06,
  bootstrapSamples = 500,
  performBootstrap = FALSE,
  modelComparison = TRUE,
  plotSurvival = TRUE,
  plotCure = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- leftTime:

  the left endpoint of the interval for interval-censored observations

- rightTime:

  the right endpoint of the interval for interval-censored observations

- covariates:

  covariates to include in the cure model

- survivalDistribution:

  the parametric survival distribution for the uncured fraction

- cureModel:

  the type of cure model to fit

- includeUncured:

  whether to include detailed analysis of the uncured fraction

- confidenceLevel:

  the confidence level for parameter estimates

- maxIterations:

  maximum number of iterations for model convergence

- tolerance:

  convergence tolerance for parameter estimation

- bootstrapSamples:

  number of bootstrap samples for confidence intervals

- performBootstrap:

  whether to perform bootstrap analysis for confidence intervals

- modelComparison:

  whether to perform model comparison across different distributions

- plotSurvival:

  whether to plot estimated survival functions

- plotCure:

  whether to plot cure fraction estimates

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$instructions`     |     |     |     |     | a html   |
| `results$modelSummary`     |     |     |     |     | a table  |
| `results$cureResults`      |     |     |     |     | a table  |
| `results$survivalResults`  |     |     |     |     | a table  |
| `results$modelFit`         |     |     |     |     | a table  |
| `results$modelComparison`  |     |     |     |     | a table  |
| `results$bootstrapResults` |     |     |     |     | a table  |
| `results$survivalPlot`     |     |     |     |     | an image |
| `results$curePlot`         |     |     |     |     | an image |
| `results$modelSummaryText` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`

## Details

Key features:

- Mixture and non-mixture cure models

- Various survival distributions (Weibull, exponential, log-normal,
  log-logistic)

- Covariate effects on both cure fraction and survival parameters

- Model comparison and goodness-of-fit assessment

- Bootstrap confidence intervals

- Parametric and non-parametric approaches

## Examples

``` r
# Interval-censored cure model with Weibull distribution
intervalcensorcure(
    data = data,
    leftTime = "left_time",
    rightTime = "right_time",
    covariates = c("age", "treatment"),
    survivalDistribution = "weibull",
    cureModel = "mixture",
    includeUncured = TRUE
)
#> Error in intervalcensorcure(data = data, leftTime = "left_time", rightTime = "right_time",     covariates = c("age", "treatment"), survivalDistribution = "weibull",     cureModel = "mixture", includeUncured = TRUE): object 'intervalcensorcureClass' not found
```
