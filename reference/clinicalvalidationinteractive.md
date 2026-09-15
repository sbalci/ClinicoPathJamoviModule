# Interactive Clinical Model Validation

Interactive clinical prediction model validation with real-time
parameter validation, intelligent defaults, and guided workflows.
Features automatic parameter correction, clinical scenario presets, and
dynamic threshold optimization for medical research and diagnostic
applications.

## Usage

``` r
clinicalvalidationinteractive(
  data,
  outcome,
  outcomeLevel,
  predictors,
  time_variable = NULL,
  clinical_preset = "custom",
  model_type = "logistic",
  validation_method = "bootstrap",
  bootstrap_samples = 1000,
  cv_folds = 10,
  cv_repeats = 3,
  holdout_proportion = 0.25,
  stratified_sampling = TRUE,
  clinical_context = "diagnosis",
  prevalence_adjustment = FALSE,
  population_prevalence = 10,
  cost_matrix = "equal",
  fn_fp_cost_ratio = 2,
  min_sensitivity = 0.8,
  min_specificity = 0.8,
  min_ppv = 0.7,
  min_npv = 0.9,
  auto_optimize_threshold = FALSE,
  optimization_metric = "youden",
  performance_metrics = "all",
  confidence_level = 0.95,
  show_realtime_metrics = TRUE,
  show_parameter_warnings = TRUE,
  show_clinical_guidance = TRUE,
  show_model_summary = TRUE,
  show_performance_table = TRUE,
  show_calibration_plot = TRUE,
  show_roc_curve = TRUE,
  show_threshold_optimization = FALSE,
  show_clinical_interpretation = TRUE,
  missing_data_handling = "complete_cases",
  set_seed = TRUE,
  seed_value = 42
)
```

## Arguments

- data:

  The data as a data frame for clinical model validation.

- outcome:

  .

- outcomeLevel:

  .

- predictors:

  .

- time_variable:

  .

- clinical_preset:

  .

- model_type:

  .

- validation_method:

  .

- bootstrap_samples:

  .

- cv_folds:

  .

- cv_repeats:

  .

- holdout_proportion:

  .

- stratified_sampling:

  .

- clinical_context:

  .

- prevalence_adjustment:

  .

- population_prevalence:

  .

- cost_matrix:

  .

- fn_fp_cost_ratio:

  .

- min_sensitivity:

  .

- min_specificity:

  .

- min_ppv:

  .

- min_npv:

  .

- auto_optimize_threshold:

  .

- optimization_metric:

  .

- performance_metrics:

  .

- confidence_level:

  .

- show_realtime_metrics:

  .

- show_parameter_warnings:

  .

- show_clinical_guidance:

  .

- show_model_summary:

  .

- show_performance_table:

  .

- show_calibration_plot:

  .

- show_roc_curve:

  .

- show_threshold_optimization:

  .

- show_clinical_interpretation:

  .

- missing_data_handling:

  .

- set_seed:

  .

- seed_value:

  .

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$modelSummary` |  |  |  |  | Summary of the fitted model |
| `results$validationResults` |  |  |  |  | Comprehensive validation performance metrics with confidence intervals |
| `results$performanceWarnings` |  |  |  |  | Real-time warnings and clinical recommendations |
| `results$prevalenceAnalysis` |  |  |  |  | Impact of disease prevalence on predictive values |
| `results$thresholdOptimization` |  |  |  |  | Optimal decision thresholds for different clinical scenarios |
| `results$calibrationAssessment` |  |  |  |  | Calibration performance metrics |
| `results$clinicalGuidelines` |  |  |  |  | Evidence-based clinical interpretation and recommendations |
| `results$rocCurve` |  |  |  |  | ROC curve showing optimal decision thresholds |
| `results$calibrationPlot` |  |  |  |  | Model calibration assessment plot |
| `results$thresholdPlot` |  |  |  |  | Performance metrics across decision thresholds |
| `results$validationCurves` |  |  |  |  | Learning curves showing model performance vs sample size |
| `results$realtimeMetrics` |  |  |  |  | Dynamic performance metric updates |
| `results$interactiveGuidance` |  |  |  |  | Dynamic clinical decision guidance |
| `results$parameterValidation` |  |  |  |  | Real-time parameter validation and warnings |
| `results$exportSummary` |  |  |  |  | Comprehensive validation summary for clinical reporting |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
