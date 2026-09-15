# Advanced Clinical Decision Trees

Advanced decision tree analysis with enhanced CART algorithm,
comprehensive validation methods, hyperparameter tuning, and detailed
clinical performance assessment for medical research and clinical
decision support.

## Usage

``` r
treeadvanced(
  data,
  vars = NULL,
  facs = NULL,
  target,
  targetLevel,
  train = NULL,
  trainLevel,
  validation = "repeated_cv",
  cv_folds = 5,
  cv_repeats = 3,
  bootstrap_samples = 200,
  stratified_sampling = TRUE,
  test_split = 0.25,
  hyperparameter_tuning = FALSE,
  tuning_method = "grid",
  tuning_metric = "bacc",
  max_depth_range = "3:8",
  cp_range = "0.001:0.1",
  min_samples_range = "10:50",
  pruning_method = "one_se",
  custom_cp = 0.01,
  auto_prune = TRUE,
  show_cp_analysis = FALSE,
  splitting_criterion = "gini",
  surrogate_splits = TRUE,
  max_surrogate = 5,
  competing_splits = 4,
  cost_sensitive = FALSE,
  clinical_loss_preset = "equal",
  fn_fp_cost_ratio = 2,
  prevalence_adjustment = FALSE,
  population_prevalence = 10,
  feature_selection = FALSE,
  feature_selection_method = "rfe",
  max_features = 10,
  show_tree_plot = TRUE,
  plot_palette = "auto",
  plot_parttree = FALSE,
  show_performance_metrics = TRUE,
  show_confusion_matrix = TRUE,
  show_importance_plot = TRUE,
  show_validation_curves = FALSE,
  show_calibration_plot = FALSE,
  show_roc_curve = TRUE,
  bootstrap_confidence = FALSE,
  n_bootstrap = 500,
  clinical_context = "diagnosis",
  show_clinical_interpretation = TRUE,
  clinical_importance_interpretation = TRUE,
  mdg_threshold = 1,
  show_feature_contributions = FALSE,
  survival_integration = FALSE,
  set_seed = TRUE,
  seed_value = 42
)
```

## Arguments

- data:

  The data as a data frame for advanced tree analysis.

- vars:

  .

- facs:

  .

- target:

  .

- targetLevel:

  .

- train:

  .

- trainLevel:

  .

- validation:

  .

- cv_folds:

  .

- cv_repeats:

  .

- bootstrap_samples:

  .

- stratified_sampling:

  .

- test_split:

  .

- hyperparameter_tuning:

  .

- tuning_method:

  .

- tuning_metric:

  .

- max_depth_range:

  .

- cp_range:

  .

- min_samples_range:

  .

- pruning_method:

  .

- custom_cp:

  .

- auto_prune:

  .

- show_cp_analysis:

  .

- splitting_criterion:

  .

- surrogate_splits:

  .

- max_surrogate:

  .

- competing_splits:

  .

- cost_sensitive:

  .

- clinical_loss_preset:

  .

- fn_fp_cost_ratio:

  .

- prevalence_adjustment:

  .

- population_prevalence:

  .

- feature_selection:

  .

- feature_selection_method:

  .

- max_features:

  .

- show_tree_plot:

  Display visual representation of the decision tree.

- plot_palette:

  Color palette for the decision tree plot.

- plot_parttree:

  .

- show_performance_metrics:

  .

- show_confusion_matrix:

  .

- show_importance_plot:

  .

- show_validation_curves:

  .

- show_calibration_plot:

  .

- show_roc_curve:

  .

- bootstrap_confidence:

  .

- n_bootstrap:

  .

- clinical_context:

  .

- show_clinical_interpretation:

  .

- clinical_importance_interpretation:

  .

- mdg_threshold:

  .

- show_feature_contributions:

  .

- survival_integration:

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
| `results$tuningresults`          |     |     |     |     | a html   |
| `results$performancetable`       |     |     |     |     | a table  |
| `results$confusionmatrix`        |     |     |     |     | a table  |
| `results$variableimportance`     |     |     |     |     | a table  |
| `results$treeplot`               |     |     |     |     | an image |
| `results$parttreeplot`           |     |     |     |     | an image |
| `results$importanceplot`         |     |     |     |     | an image |
| `results$validationcurves`       |     |     |     |     | an image |
| `results$calibrationplot`        |     |     |     |     | an image |
| `results$roccurve`               |     |     |     |     | an image |
| `results$clinicalinterpretation` |     |     |     |     | a html   |
| `results$clinicalimportance`     |     |     |     |     | a table  |
| `results$featurecontributions`   |     |     |     |     | a table  |
| `results$survivalintegration`    |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performancetable$asDF`

`as.data.frame(results$performancetable)`
