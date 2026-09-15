# Robust Cox Regression

Implements robust Cox proportional hazards regression models that are
resistant to outliers and model misspecification. Provides multiple
robust estimation methods including Huber M-estimation, bounded
influence functions, and weighted partial likelihood approaches for
reliable survival analysis in the presence of data anomalies.

## Usage

``` r
coxrobust(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  robust_method = "huber",
  tuning_constant = 1.345,
  efficiency_target = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  outlier_detection = TRUE,
  outlier_threshold = 3,
  stratify_variable,
  weights_variable,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  confidence_level = 0.95,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_outliers = TRUE,
  show_influence = TRUE,
  show_comparison = TRUE,
  show_residual_plots = TRUE,
  show_influence_plots = TRUE,
  show_weight_plots = TRUE,
  show_survival_plots = TRUE,
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

- robust_method:

  Robust estimation method to use

- tuning_constant:

  Tuning constant for robust estimation (method-specific)

- efficiency_target:

  Target efficiency relative to standard Cox model

- max_iterations:

  Maximum number of iterations for robust estimation

- convergence_tolerance:

  Convergence tolerance for parameter estimation

- outlier_detection:

  Perform automatic outlier detection and flagging

- outlier_threshold:

  Threshold for outlier detection (in standard deviations)

- stratify_variable:

  Variable for stratified analysis

- weights_variable:

  Optional observation weights

- bootstrap_ci:

  Compute bootstrap confidence intervals

- bootstrap_samples:

  Number of bootstrap samples for CI estimation

- confidence_level:

  Confidence level for intervals

- show_model_summary:

  Display comprehensive model summary

- show_coefficients:

  Display coefficient estimates table

- show_outliers:

  Display outlier detection results

- show_influence:

  Display influence diagnostics

- show_comparison:

  Compare robust vs standard Cox models

- show_residual_plots:

  Display residual diagnostic plots

- show_influence_plots:

  Display influence diagnostic plots

- show_weight_plots:

  Display robust weight distributions

- show_survival_plots:

  Display survival curves

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
| `results$outlierAnalysis`   |     |     |     |     | a table  |
| `results$influenceMeasures` |     |     |     |     | a table  |
| `results$modelComparison`   |     |     |     |     | a table  |
| `results$convergenceInfo`   |     |     |     |     | a table  |
| `results$residualPlots`     |     |     |     |     | an image |
| `results$influencePlots`    |     |     |     |     | an image |
| `results$weightPlots`       |     |     |     |     | an image |
| `results$survivalPlots`     |     |     |     |     | an image |
| `results$outlierPlots`      |     |     |     |     | an image |
| `results$comparisonPlots`   |     |     |     |     | an image |
| `results$summaryTable`      |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
