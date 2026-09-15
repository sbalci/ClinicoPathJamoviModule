# Restricted Mean Survival Time Regression

Restricted Mean Survival Time (RMST) regression analysis provides an
alternative to hazard-based models by directly modeling the mean
survival time up to a specific time point (tau). This approach offers
clinically interpretable results and does not require proportional
hazards assumptions. RMST represents the area under the survival curve
up to time tau and can be compared between groups or modeled with
covariates using regression techniques.

## Usage

``` r
rmstregression(
  data,
  elapsedtime,
  outcome,
  explanatory,
  group_var,
  outcomeLevel = "1",
  tau = 60,
  tau_method = "fixed",
  tau_percentile = 0.8,
  analysis_type = "regression",
  regression_method = "pseudo_observation",
  confidence_level = 0.95,
  bootstrap_se = FALSE,
  bootstrap_reps = 1000,
  adjustment_method = "none",
  show_rmst_table = TRUE,
  show_difference_table = TRUE,
  show_regression_table = TRUE,
  show_model_diagnostics = FALSE,
  rmst_plot = TRUE,
  difference_plot = FALSE,
  residual_plot = FALSE,
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

  Explanatory variables for RMST regression modeling

- group_var:

  Primary grouping variable for RMST comparison (optional)

- outcomeLevel:

  Level indicating event occurrence

- tau:

  Time point for restriction in RMST calculation

- tau_method:

  Method for selecting restriction time tau

- tau_percentile:

  Percentile of follow-up time for tau selection (when using percentile
  method)

- analysis_type:

  Type of RMST analysis to perform

- regression_method:

  Method for RMST regression analysis

- confidence_level:

  Confidence level for confidence intervals

- bootstrap_se:

  Use bootstrap method for standard error estimation

- bootstrap_reps:

  Number of bootstrap replications for standard errors

- adjustment_method:

  Method for multiple comparison adjustment

- show_rmst_table:

  Display RMST summary statistics by group

- show_difference_table:

  Display pairwise RMST differences between groups

- show_regression_table:

  Display regression coefficient estimates

- show_model_diagnostics:

  Display model diagnostic information

- rmst_plot:

  Display RMST curves with confidence intervals

- difference_plot:

  Display plot of RMST differences over time

- residual_plot:

  Display residual diagnostic plots for regression models

- showSummaries:

  Show comprehensive analysis summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                   |     |     |     |     | a html   |
| `results$rmstSummary`            |     |     |     |     | a table  |
| `results$rmstDifferences`        |     |     |     |     | a table  |
| `results$regressionResults`      |     |     |     |     | a table  |
| `results$modelDiagnostics`       |     |     |     |     | a table  |
| `results$tauSelection`           |     |     |     |     | a table  |
| `results$bootstrapResults`       |     |     |     |     | a table  |
| `results$methodologyExplanation` |     |     |     |     | a html   |
| `results$analysisSummary`        |     |     |     |     | a html   |
| `results$rmstPlot`               |     |     |     |     | an image |
| `results$differencePlot`         |     |     |     |     | an image |
| `results$residualPlot`           |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$rmstSummary$asDF`

`as.data.frame(results$rmstSummary)`
