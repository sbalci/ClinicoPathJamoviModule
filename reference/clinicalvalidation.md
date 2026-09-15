# Clinical Model Validation Suite

Comprehensive validation suite for clinical prediction models including
bootstrap validation, cross-validation, model calibration assessment,
and clinical performance evaluation designed for medical research and
diagnostic test evaluation.

## Usage

``` r
clinicalvalidation(
  data,
  outcome,
  outcomeLevel,
  predictors,
  time_variable = NULL,
  model_type = "logistic",
  model_formula = "",
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
  performance_metrics = "all",
  confidence_level = 0.95,
  calibration_method = "all",
  calibration_bins = 10,
  compare_models = FALSE,
  comparison_models = "logistic_rf",
  show_model_summary = FALSE,
  show_performance_table = FALSE,
  show_calibration_plot = FALSE,
  show_roc_curve = FALSE,
  show_prc_curve = FALSE,
  show_validation_curves = FALSE,
  show_residual_plots = FALSE,
  show_clinical_interpretation = FALSE,
  export_results = FALSE,
  detailed_bootstrap = FALSE,
  missing_data_handling = "complete_cases",
  imputation_methods = 5,
  parallel_processing = FALSE,
  n_cores = 2,
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

- model_type:

  .

- model_formula:

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

- performance_metrics:

  .

- confidence_level:

  .

- calibration_method:

  .

- calibration_bins:

  .

- compare_models:

  .

- comparison_models:

  .

- show_model_summary:

  .

- show_performance_table:

  .

- show_calibration_plot:

  .

- show_roc_curve:

  .

- show_prc_curve:

  Display Precision-Recall curve analysis. Recommended for imbalanced
  datasets where ROC curves may be misleading (Saito & Rehmsmeier,
  2015).

- show_validation_curves:

  .

- show_residual_plots:

  .

- show_clinical_interpretation:

  .

- export_results:

  .

- detailed_bootstrap:

  .

- missing_data_handling:

  .

- imputation_methods:

  .

- parallel_processing:

  .

- n_cores:

  .

- set_seed:

  .

- seed_value:

  .

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$modelsummary`           |     |     |     |     | a html   |
| `results$performancetable`       |     |     |     |     | a table  |
| `results$bootstrapresults`       |     |     |     |     | a table  |
| `results$calibrationtable`       |     |     |     |     | a table  |
| `results$brierscore`             |     |     |     |     | a table  |
| `results$modelcomparison`        |     |     |     |     | a table  |
| `results$costreduction`          |     |     |     |     | a table  |
| `results$validationcurves`       |     |     |     |     | an image |
| `results$calibrationplot`        |     |     |     |     | an image |
| `results$roccurve`               |     |     |     |     | an image |
| `results$prccurve`               |     |     |     |     | an image |
| `results$residualplots`          |     |     |     |     | an image |
| `results$clinicalinterpretation` |     |     |     |     | a html   |
| `results$validationreport`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performancetable$asDF`

`as.data.frame(results$performancetable)`
