# Time-Dependent Covariates & ROC Analysis

Analysis of time-dependent covariates and time-varying ROC curves for
survival data. Handles covariates that change over time, landmark
analysis, and dynamic prediction accuracy assessment through
time-dependent AUC and optimal cutpoint selection.

## Usage

``` r
timedependent(
  data,
  id,
  start_time,
  stop_time,
  event,
  time_dependent_vars,
  baseline_vars,
  perform_landmark = TRUE,
  landmark_times = "6,12,24",
  prediction_window = 12,
  time_dependent_roc = TRUE,
  roc_times = "1,2,3,5",
  roc_method = "incident_dynamic",
  optimal_cutpoint = TRUE,
  cutpoint_method = "youden",
  model_type = "extended_cox",
  time_transform = "none",
  test_proportional_hazards = TRUE,
  schoenfeld_transform = "km",
  internal_validation = TRUE,
  cv_folds = 5,
  bootstrap_validation = FALSE,
  n_bootstrap = 100,
  compare_models = FALSE,
  comparison_metric = "iauc",
  plot_time_varying_effects = TRUE,
  plot_roc_curves = TRUE,
  plot_auc_trajectory = TRUE,
  plot_cutpoint_stability = TRUE,
  plot_landmark_predictions = TRUE,
  plot_schoenfeld_residuals = TRUE,
  confidence_level = 0.95,
  decimals = 3,
  export_predictions = FALSE,
  export_roc_data = FALSE
)
```

## Arguments

- data:

  The data as a data frame (can be in counting process format).

- id:

  Patient ID variable

- start_time:

  Beginning of time interval (tstart)

- stop_time:

  End of time interval (tstop)

- event:

  Event status variable

- time_dependent_vars:

  Time-varying predictors

- baseline_vars:

  Time-fixed predictors

- perform_landmark:

  Enable landmark analysis

- landmark_times:

  Landmark time points

- prediction_window:

  Prediction horizon from landmark

- time_dependent_roc:

  Enable TD-ROC analysis

- roc_times:

  ROC assessment times

- roc_method:

  TD-ROC approach

- optimal_cutpoint:

  Optimal threshold selection

- cutpoint_method:

  Cutpoint optimization approach

- model_type:

  Model specification

- time_transform:

  Time function transformation

- test_proportional_hazards:

  PH assumption testing

- schoenfeld_transform:

  Residual test transformation

- internal_validation:

  Cross-validation assessment

- cv_folds:

  CV fold specification

- bootstrap_validation:

  Bootstrap assessment

- n_bootstrap:

  Bootstrap iterations

- compare_models:

  Model comparison

- comparison_metric:

  Comparison measure

- plot_time_varying_effects:

  Time-varying coefficient plots

- plot_roc_curves:

  TD-ROC visualization

- plot_auc_trajectory:

  AUC evolution plot

- plot_cutpoint_stability:

  Cutpoint trajectory plot

- plot_landmark_predictions:

  Landmark prediction plots

- plot_schoenfeld_residuals:

  Residual diagnostic plots

- confidence_level:

  CI level

- decimals:

  Output precision

- export_predictions:

  Save predictions

- export_roc_data:

  Save ROC results

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                      |     |     |     |     | a html   |
| `results$model_summary`             |     |     |     |     | a html   |
| `results$cox_results`               |     |     |     |     | a table  |
| `results$time_varying_effects`      |     |     |     |     | a table  |
| `results$landmark_results`          |     |     |     |     | a table  |
| `results$roc_results`               |     |     |     |     | a table  |
| `results$model_comparison`          |     |     |     |     | a table  |
| `results$validation_results`        |     |     |     |     | a table  |
| `results$time_varying_plot`         |     |     |     |     | an image |
| `results$roc_curves_plot`           |     |     |     |     | an image |
| `results$auc_trajectory_plot`       |     |     |     |     | an image |
| `results$cutpoint_stability_plot`   |     |     |     |     | an image |
| `results$landmark_predictions_plot` |     |     |     |     | an image |
| `results$schoenfeld_plot`           |     |     |     |     | an image |
| `results$interpretation`            |     |     |     |     | a html   |
| `results$recommendations`           |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$cox_results$asDF`

`as.data.frame(results$cox_results)`
