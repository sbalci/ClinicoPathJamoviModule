# Direct Regression on Survival Function

Direct regression on the survival function using pseudo-observations.
This method allows for direct modeling of survival probabilities at
specific time points using standard regression techniques, providing an
alternative to hazard-based models. Particularly useful when interest
lies in survival probabilities rather than hazard ratios.

## Usage

``` r
directregression(
  data,
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  time_points = "12,24,60",
  regression_type = "linear",
  link_function = "identity",
  confidence_level = 0.95,
  bootstrap_se = FALSE,
  bootstrap_reps = 500,
  pseudo_method = "jackknife",
  show_pseudo_values = FALSE,
  show_residuals = FALSE,
  model_comparison = TRUE,
  survival_plot = TRUE,
  residual_plot = FALSE,
  prediction_plot = TRUE,
  showSummaries = TRUE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Time to event or censoring

- outcome:

  Event indicator (1 = event, 0 = censored)

- explanatory:

  Explanatory variables for direct regression modeling

- outcomeLevel:

  Level indicating event occurrence

- time_points:

  Comma-separated list of time points at which to perform direct
  regression

- regression_type:

  Type of regression model for pseudo-observations

- link_function:

  Link function for the regression model

- confidence_level:

  Confidence level for confidence intervals

- bootstrap_se:

  Use bootstrap method for standard error estimation

- bootstrap_reps:

  Number of bootstrap replications for standard errors

- pseudo_method:

  Method for calculating pseudo-observations

- show_pseudo_values:

  Display pseudo-observation values in results

- show_residuals:

  Display residual analysis

- model_comparison:

  Compare models across different time points

- survival_plot:

  Display survival function with regression predictions

- residual_plot:

  Display residual diagnostic plots

- prediction_plot:

  Display model predictions at specified time points

- showSummaries:

  Show comprehensive analysis summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                   |     |     |     |     | a html   |
| `results$modelSummary`           |     |     |     |     | a table  |
| `results$pseudoValues`           |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$residualAnalysis`       |     |     |     |     | a table  |
| `results$methodologyExplanation` |     |     |     |     | a html   |
| `results$analysisSummary`        |     |     |     |     | a html   |
| `results$survivalPlot`           |     |     |     |     | an image |
| `results$residualPlot`           |     |     |     |     | an image |
| `results$predictionPlot`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
