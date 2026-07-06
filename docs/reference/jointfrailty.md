# Joint Frailty Models for Recurrent Events

Joint frailty models for simultaneous analysis of recurrent and terminal
events. This analysis models both the recurrent event process and
terminal event process jointly, accounting for their association through
shared frailty terms and providing comprehensive risk assessment for
subjects experiencing multiple types of events.

## Usage

``` r
jointfrailty(
  data,
  subjectID,
  time,
  event,
  terminal_time,
  terminal_event,
  covariates,
  recurrent_covariates,
  terminal_covariates,
  frailty_distribution = "gamma",
  recurrent_baseline = "weibull",
  terminal_baseline = "weibull",
  association_type = "shared_frailty",
  confidence_level = 0.95,
  max_iterations = 1000,
  convergence_tolerance = 1e-06,
  include_predictions = TRUE,
  include_residuals = TRUE,
  plotRecurrentHazard = TRUE,
  plotTerminalHazard = TRUE,
  plotFrailtyDistribution = TRUE,
  plotSurvivalPredictions = FALSE,
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

- recurrent_covariates:

  .

- terminal_covariates:

  .

- frailty_distribution:

  .

- recurrent_baseline:

  .

- terminal_baseline:

  .

- association_type:

  .

- confidence_level:

  .

- max_iterations:

  .

- convergence_tolerance:

  .

- include_predictions:

  .

- include_residuals:

  .

- plotRecurrentHazard:

  .

- plotTerminalHazard:

  .

- plotFrailtyDistribution:

  .

- plotSurvivalPredictions:

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
| `results$recurrentCoefficients` |     |     |     |     | a table  |
| `results$terminalCoefficients`  |     |     |     |     | a table  |
| `results$frailtyParameters`     |     |     |     |     | a table  |
| `results$associationMeasures`   |     |     |     |     | a table  |
| `results$individualPredictions` |     |     |     |     | a table  |
| `results$residualAnalysis`      |     |     |     |     | a table  |
| `results$educationalContent`    |     |     |     |     | a html   |
| `results$interpretationContent` |     |     |     |     | a html   |
| `results$recurrentHazardPlot`   |     |     |     |     | an image |
| `results$terminalHazardPlot`    |     |     |     |     | an image |
| `results$frailtyPlot`           |     |     |     |     | an image |
| `results$predictionsPlot`       |     |     |     |     | an image |
| `results$residualsPlot`         |     |     |     |     | an image |
| `results$exportTable`           |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelfit$asDF`

`as.data.frame(results$modelfit)`
