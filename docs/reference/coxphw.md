# Weighted Cox Regression

Implements weighted Cox proportional hazards regression models
specifically designed for rare events and imbalanced survival data. Uses
average hazard weights to improve estimation accuracy when events are
rare or covariates are imbalanced, providing more reliable hazard ratios
and confidence intervals in challenging clinical research scenarios.

## Usage

``` r
coxphw(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  weight_method = "average",
  alpha = 0.5,
  offset_variable,
  stratify_variable,
  cluster_variable,
  confidence_level = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_weights = TRUE,
  show_diagnostics = TRUE,
  show_comparison = TRUE,
  show_weight_plots = TRUE,
  show_residual_plots = TRUE,
  show_survival_plots = TRUE,
  show_forest_plot = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Survival time or follow-up duration variable

- outcome:

  Event indicator variable (0/1, FALSE/TRUE, or factor)

- covariates:

  Predictor variables for the Cox model

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- weight_method:

  Weighting method for the partial likelihood

- alpha:

  Weight parameter (0=log-rank, 0.5=average, 1=Breslow)

- offset_variable:

  Optional offset variable for the model

- stratify_variable:

  Variable for stratified analysis

- cluster_variable:

  Variable defining clusters for robust variance

- confidence_level:

  Confidence level for intervals

- max_iterations:

  Maximum number of iterations

- convergence_tolerance:

  Convergence tolerance for estimation

- bootstrap_ci:

  Compute bootstrap confidence intervals

- bootstrap_samples:

  Number of bootstrap samples

- show_model_summary:

  Display comprehensive model summary

- show_coefficients:

  Display coefficient estimates table

- show_weights:

  Display weight distribution analysis

- show_diagnostics:

  Display model diagnostics

- show_comparison:

  Compare weighted vs standard Cox models

- show_weight_plots:

  Display weight distribution plots

- show_residual_plots:

  Display residual diagnostic plots

- show_survival_plots:

  Display survival curves

- show_forest_plot:

  Display forest plot of hazard ratios

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`      |     |     |     |     | a table  |
| `results$coefficients`      |     |     |     |     | a table  |
| `results$weightAnalysis`    |     |     |     |     | a table  |
| `results$diagnostics`       |     |     |     |     | a table  |
| `results$modelComparison`   |     |     |     |     | a table  |
| `results$convergenceInfo`   |     |     |     |     | a table  |
| `results$rareEventAnalysis` |     |     |     |     | a table  |
| `results$weightPlots`       |     |     |     |     | an image |
| `results$residualPlots`     |     |     |     |     | an image |
| `results$survivalPlots`     |     |     |     |     | an image |
| `results$forestPlot`        |     |     |     |     | an image |
| `results$comparisonPlots`   |     |     |     |     | an image |
| `results$summaryTable`      |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
