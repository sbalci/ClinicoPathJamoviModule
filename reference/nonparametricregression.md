# Non-parametric Regression Methods

Comprehensive non-parametric regression methods including kernel
regression, local polynomial smoothing (LOESS), spline regression, and
robust regression techniques designed for clinical research where
parametric assumptions are violated.

## Usage

``` r
nonparametricregression(
  data,
  outcome,
  predictors,
  grouping_variable = NULL,
  regression_type = "kernel",
  kernel_type = "gaussian",
  bandwidth_method = "cross_validation",
  manual_bandwidth = 0.1,
  loess_span = 0.75,
  loess_degree = "linear",
  loess_iterations = 3,
  spline_type = "smooth",
  spline_df = 4,
  spline_lambda = 0.1,
  quantile_tau = 0.5,
  quantile_method = "interior_point",
  gam_smoother = "cubic_spline",
  gam_basis_dim = 10,
  cv_folds = 10,
  cv_repeats = 1,
  model_selection_criterion = "cv_error",
  robust_regression = FALSE,
  robust_method = "huber",
  outlier_threshold = 3,
  confidence_level = 0.95,
  bootstrap_ci = TRUE,
  bootstrap_samples = 1000,
  prediction_intervals = TRUE,
  residual_diagnostics = TRUE,
  influence_diagnostics = FALSE,
  goodness_of_fit = TRUE,
  show_fitted_curves = TRUE,
  show_confidence_bands = TRUE,
  show_prediction_bands = FALSE,
  show_residual_plots = TRUE,
  show_qq_plots = TRUE,
  show_partial_plots = FALSE,
  show_model_summary = TRUE,
  show_parameter_estimates = TRUE,
  show_model_comparison = FALSE,
  show_interpretation = TRUE,
  multivariate_method = "additive",
  missing_data_handling = "complete_cases",
  clinical_context = "general",
  set_seed = TRUE,
  seed_value = 42,
  parallel_processing = FALSE,
  n_cores = 2
)
```

## Arguments

- data:

  The data as a data frame for non-parametric regression analysis.

- outcome:

  .

- predictors:

  .

- grouping_variable:

  .

- regression_type:

  .

- kernel_type:

  .

- bandwidth_method:

  .

- manual_bandwidth:

  .

- loess_span:

  .

- loess_degree:

  .

- loess_iterations:

  .

- spline_type:

  .

- spline_df:

  .

- spline_lambda:

  .

- quantile_tau:

  .

- quantile_method:

  .

- gam_smoother:

  .

- gam_basis_dim:

  .

- cv_folds:

  .

- cv_repeats:

  .

- model_selection_criterion:

  .

- robust_regression:

  .

- robust_method:

  .

- outlier_threshold:

  .

- confidence_level:

  .

- bootstrap_ci:

  .

- bootstrap_samples:

  .

- prediction_intervals:

  .

- residual_diagnostics:

  .

- influence_diagnostics:

  .

- goodness_of_fit:

  .

- show_fitted_curves:

  .

- show_confidence_bands:

  .

- show_prediction_bands:

  .

- show_residual_plots:

  .

- show_qq_plots:

  .

- show_partial_plots:

  .

- show_model_summary:

  .

- show_parameter_estimates:

  .

- show_model_comparison:

  .

- show_interpretation:

  .

- multivariate_method:

  .

- missing_data_handling:

  .

- clinical_context:

  .

- set_seed:

  .

- seed_value:

  .

- parallel_processing:

  .

- n_cores:

  .

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`           |     |     |     |     | a table  |
| `results$parameterEstimates`     |     |     |     |     | a table  |
| `results$bandwidthSelection`     |     |     |     |     | a table  |
| `results$modelFit`               |     |     |     |     | a table  |
| `results$residualDiagnostics`    |     |     |     |     | a table  |
| `results$influenceDiagnostics`   |     |     |     |     | a table  |
| `results$predictionTable`        |     |     |     |     | a table  |
| `results$goodnessOfFit`          |     |     |     |     | a table  |
| `results$crossValidation`        |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$methodsExplanation`     |     |     |     |     | a html   |
| `results$fittedCurvePlot`        |     |     |     |     | an image |
| `results$residualPlots`          |     |     |     |     | an image |
| `results$qqPlots`                |     |     |     |     | an image |
| `results$partialEffectPlots`     |     |     |     |     | an image |
| `results$influencePlots`         |     |     |     |     | an image |
| `results$bandwidthPlots`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
