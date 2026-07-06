# Rank-based AFT Estimation

Implements rank-based accelerated failure time (AFT) models using
generalized estimating equations (GEE) for survival data analysis. This
approach provides robust estimation when proportional hazards
assumptions are violated, offering distribution-free methods for
modeling survival times with emphasis on median survival relationships
and time acceleration effects in clinical research applications.

## Usage

``` r
raftgee(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  rank_method = "logrank",
  correlation_structure = "independence",
  cluster_variable,
  confidence_level = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  weights_variable,
  robust_variance = TRUE,
  bootstrap_samples = 0,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_diagnostics = TRUE,
  show_residual_plots = TRUE,
  show_qq_plots = TRUE,
  show_survival_plots = TRUE,
  show_acceleration_plots = TRUE,
  show_comparison_plots = TRUE,
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

  Covariate variables for AFT modeling

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- rank_method:

  Rank-based test method for AFT estimation

- correlation_structure:

  Working correlation structure for GEE

- cluster_variable:

  Variable defining clusters for correlated observations

- confidence_level:

  Confidence level for parameter estimates

- max_iterations:

  Maximum number of iterations for GEE algorithm

- convergence_tolerance:

  Convergence tolerance for parameter estimation

- weights_variable:

  Optional weights variable for observations

- robust_variance:

  Use robust sandwich variance estimator

- bootstrap_samples:

  Number of bootstrap samples for variance estimation (0 = no bootstrap)

- show_model_summary:

  Display comprehensive AFT model summary

- show_coefficients:

  Display AFT model coefficients table

- show_diagnostics:

  Display model diagnostic statistics

- show_residual_plots:

  Display residual diagnostic plots

- show_qq_plots:

  Display quantile-quantile plots

- show_survival_plots:

  Display AFT-based survival curves

- show_acceleration_plots:

  Display time acceleration factor plots

- show_comparison_plots:

  Display comparison with Cox models

- showSummaries:

  Generate natural language summaries of the analysis results

- showExplanations:

  Show detailed explanations of the methodology and interpretation

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`      |     |     |     |     | a table  |
| `results$coefficients`      |     |     |     |     | a table  |
| `results$diagnostics`       |     |     |     |     | a table  |
| `results$geeDiagnostics`    |     |     |     |     | a table  |
| `results$convergenceInfo`   |     |     |     |     | a table  |
| `results$modelComparison`   |     |     |     |     | a table  |
| `results$residualPlots`     |     |     |     |     | an image |
| `results$qqPlots`           |     |     |     |     | an image |
| `results$survivalPlots`     |     |     |     |     | an image |
| `results$accelerationPlots` |     |     |     |     | an image |
| `results$comparisonPlots`   |     |     |     |     | an image |
| `results$correlationPlots`  |     |     |     |     | an image |
| `results$summaryTable`      |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
