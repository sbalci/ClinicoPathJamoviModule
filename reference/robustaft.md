# Robust AFT Models

Implements robust accelerated failure time (AFT) models with
M-estimation and resistant estimators. Provides outlier-resistant
estimation for parametric survival models when standard AFT models are
sensitive to extreme observations or model misspecification, ensuring
reliable parameter estimates in challenging clinical datasets.

## Usage

``` r
robustaft(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  distribution = "weibull",
  robust_method = "huber",
  tuning_constant = 1.345,
  efficiency_target = 0.95,
  scale_estimation = "mad",
  stratify_variable,
  weights_variable,
  confidence_level = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  outlier_detection = TRUE,
  outlier_threshold = 2.5,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_outliers = TRUE,
  show_diagnostics = TRUE,
  show_comparison = TRUE,
  show_residual_plots = TRUE,
  show_outlier_plots = TRUE,
  show_survival_plots = TRUE,
  show_qq_plots = TRUE,
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

  Predictor variables for the AFT model

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- distribution:

  Parametric distribution for AFT model

- robust_method:

  Robust estimation method for outlier resistance

- tuning_constant:

  Tuning constant controlling robustness vs efficiency

- efficiency_target:

  Target efficiency relative to non-robust estimator

- scale_estimation:

  Method for robust scale estimation

- stratify_variable:

  Variable for stratified analysis

- weights_variable:

  Observation weights variable

- confidence_level:

  Confidence level for intervals

- max_iterations:

  Maximum number of iterations

- convergence_tolerance:

  Convergence tolerance for estimation

- outlier_detection:

  Automatically detect and flag outliers

- outlier_threshold:

  Threshold for outlier detection

- bootstrap_ci:

  Compute bootstrap confidence intervals

- bootstrap_samples:

  Number of bootstrap samples

- show_model_summary:

  Display comprehensive model summary

- show_coefficients:

  Display coefficient estimates table

- show_outliers:

  Display outlier detection results

- show_diagnostics:

  Display model diagnostics

- show_comparison:

  Compare robust vs standard AFT models

- show_residual_plots:

  Display residual diagnostic plots

- show_outlier_plots:

  Display outlier identification plots

- show_survival_plots:

  Display survival curves

- show_qq_plots:

  Display quantile-quantile plots

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
| `results$diagnostics`       |     |     |     |     | a table  |
| `results$modelComparison`   |     |     |     |     | a table  |
| `results$robustnessInfo`    |     |     |     |     | a table  |
| `results$scaleEstimates`    |     |     |     |     | a table  |
| `results$outlierTable`      |     |     |     |     | a table  |
| `results$residualPlots`     |     |     |     |     | an image |
| `results$outlierPlots`      |     |     |     |     | an image |
| `results$survivalPlots`     |     |     |     |     | an image |
| `results$qqPlots`           |     |     |     |     | an image |
| `results$summaryTable`      |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
