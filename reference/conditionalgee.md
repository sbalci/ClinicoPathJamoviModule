# Conditional GEE for Recurrent Events Gap Times

Conditional Generalized Estimating Equations for analyzing gap times
between recurrent events. This analysis models the time between
consecutive event occurrences conditional on previous event history,
accounting for within-subject correlation and providing robust
population-average estimates of covariate effects on inter-event times.

## Usage

``` r
conditionalgee(
  data,
  subjectID,
  gap_time,
  event,
  event_number,
  covariates,
  time_varying_covariates,
  baseline_covariates,
  distribution_family = "weibull",
  correlation_structure = "exchangeable",
  link_function = "log",
  conditioning_set = "previous_gap",
  confidence_level = 0.95,
  max_iterations = 200,
  tolerance = 1e-06,
  robust_se = TRUE,
  include_diagnostics = TRUE,
  include_predictions = TRUE,
  plotGapTimes = TRUE,
  plotCorrelation = TRUE,
  plotResiduals = FALSE,
  plotPredictions = FALSE,
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

- gap_time:

  .

- event:

  .

- event_number:

  .

- covariates:

  .

- time_varying_covariates:

  .

- baseline_covariates:

  .

- distribution_family:

  .

- correlation_structure:

  .

- link_function:

  .

- conditioning_set:

  .

- confidence_level:

  .

- max_iterations:

  .

- tolerance:

  .

- robust_se:

  .

- include_diagnostics:

  .

- include_predictions:

  .

- plotGapTimes:

  .

- plotCorrelation:

  .

- plotResiduals:

  .

- plotPredictions:

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
| `results$correlationStructure`  |     |     |     |     | a table  |
| `results$modelDiagnostics`      |     |     |     |     | a table  |
| `results$gapTimePredictions`    |     |     |     |     | a table  |
| `results$educationalContent`    |     |     |     |     | a html   |
| `results$interpretationContent` |     |     |     |     | a html   |
| `results$gapTimesPlot`          |     |     |     |     | an image |
| `results$correlationPlot`       |     |     |     |     | an image |
| `results$residualsPlot`         |     |     |     |     | an image |
| `results$predictionsPlot`       |     |     |     |     | an image |
| `results$exportTable`           |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelfit$asDF`

`as.data.frame(results$modelfit)`
