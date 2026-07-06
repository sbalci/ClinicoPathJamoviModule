# Spline-based Hazard Functions

Spline-based Hazard Functions

## Usage

``` r
splinehazard(
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  knots_method = "automatic",
  num_knots = 3,
  spline_degree = "3",
  hazard_scale = "log",
  confidence_level = 0.95,
  show_hazard_plot = TRUE,
  show_survival_plot = TRUE,
  show_cumulative_plot = FALSE,
  show_model_comparison = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- elapsedtime:

  Time to event or censoring

- outcome:

  Event indicator (1 = event, 0 = censored)

- explanatory:

  Explanatory variables for modeling

- outcomeLevel:

  Level indicating event occurrence

- knots_method:

  Method for selecting spline knots

- num_knots:

  Number of internal knots for spline

- spline_degree:

  Polynomial degree for spline basis

- hazard_scale:

  Scale for modeling hazard function

- confidence_level:

  Confidence level for intervals

- show_hazard_plot:

  Display estimated hazard function over time

- show_survival_plot:

  Display survival curves

- show_cumulative_plot:

  Display cumulative hazard function

- show_model_comparison:

  Compare different knot configurations

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show methodology explanations

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$modelSummary`      |     |     |     |     | a table  |
| `results$parametersTable`   |     |     |     |     | a table  |
| `results$knotsTable`        |     |     |     |     | a table  |
| `results$modelComparison`   |     |     |     |     | a table  |
| `results$hazardPlot`        |     |     |     |     | an image |
| `results$survivalPlot`      |     |     |     |     | an image |
| `results$cumulativePlot`    |     |     |     |     | an image |
| `results$splineBasisPlot`   |     |     |     |     | an image |
| `results$analysisSummary`   |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
