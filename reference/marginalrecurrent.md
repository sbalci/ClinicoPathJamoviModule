# Marginal Models for Recurrent Events

Marginal models for analyzing recurrent event data using rate-based
approaches. This analysis provides population-average estimates of
covariate effects on recurrent event rates, with options for marginal
rate models, accelerated rate models, and gamma frailty models to handle
within-subject correlation.

## Usage

``` r
marginalrecurrent(
  data,
  subjectID,
  time,
  event,
  terminal_time,
  terminal_event,
  covariates,
  model_type = "marginal",
  baseline_type = "nonparametric",
  confidence_level = 0.95,
  bootstrap = TRUE,
  bootstrap_samples = 1000,
  robust_se = TRUE,
  include_cumulative = TRUE,
  include_survival = TRUE,
  time_points = "",
  plotRecurrentEvents = TRUE,
  plotCumulative = TRUE,
  plotSurvival = TRUE,
  plotResiduals = FALSE,
  showEducation = TRUE,
  showInterpretation = TRUE,
  exportResults = FALSE
)
```

## Arguments

- data:

  .

- subjectID:

  .

- time:

  .

- event:

  .

- terminal_time:

  .

- terminal_event:

  .

- covariates:

  .

- model_type:

  .

- baseline_type:

  .

- confidence_level:

  .

- bootstrap:

  .

- bootstrap_samples:

  .

- robust_se:

  .

- include_cumulative:

  .

- include_survival:

  .

- time_points:

  .

- plotRecurrentEvents:

  .

- plotCumulative:

  .

- plotSurvival:

  .

- plotResiduals:

  .

- showEducation:

  .

- showInterpretation:

  .

- exportResults:

  .

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                  |     |     |     |     | a html   |
| `results$modelfit`              |     |     |     |     | a table  |
| `results$coefficients`          |     |     |     |     | a table  |
| `results$cumulativeRate`        |     |     |     |     | a table  |
| `results$survivalFunction`      |     |     |     |     | a table  |
| `results$goodnessOfFit`         |     |     |     |     | a table  |
| `results$educationalContent`    |     |     |     |     | a html   |
| `results$interpretationContent` |     |     |     |     | a html   |
| `results$recurrentEventsPlot`   |     |     |     |     | an image |
| `results$cumulativePlot`        |     |     |     |     | an image |
| `results$survivalPlot`          |     |     |     |     | an image |
| `results$residualsPlot`         |     |     |     |     | an image |
| `results$exportTable`           |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelfit$asDF`

`as.data.frame(results$modelfit)`
