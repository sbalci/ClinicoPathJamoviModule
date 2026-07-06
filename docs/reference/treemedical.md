# Medical Decision Trees

Simple decision tree analysis for medical research and clinical decision
making. Uses enhanced CART algorithm optimized for healthcare
applications with clinical validation, performance metrics, and medical
interpretation guidelines.

## Usage

``` r
treemedical(
  data,
  vars = NULL,
  facs = NULL,
  target,
  targetLevel,
  validation = "cv",
  cv_folds = 5,
  bootstrap_samples = 100,
  stratified_sampling = TRUE,
  holdout_split = 0.75,
  handle_missing = "remove",
  max_depth = 5,
  min_samples_split = 20,
  cost_complexity = 0.01,
  use_1se_rule = TRUE,
  clinical_context = "diagnosis",
  cost_sensitive = FALSE,
  fn_fp_ratio = 2,
  show_tree_plot = TRUE,
  plot_palette = "auto",
  plot_parttree = FALSE,
  show_performance_metrics = TRUE,
  show_confusion_matrix = TRUE,
  show_importance_plot = TRUE,
  show_roc_plot = TRUE,
  show_calibration_plot = TRUE,
  show_clinical_interpretation = TRUE,
  set_seed = TRUE,
  seed_value = 42
)
```

## Arguments

- data:

  The data as a data frame containing clinical variables and outcomes.

- vars:

  Continuous variables such as biomarker levels, age, laboratory values,
  or quantitative measurements.

- facs:

  Categorical variables such as tumor grade, stage, histological type,
  or patient demographics.

- target:

  Primary outcome variable: disease status, treatment response, or
  diagnostic category.

- targetLevel:

  Level representing disease presence or positive outcome.

- validation:

  Validation approach for assessing model performance.

- cv_folds:

  Number of folds for cross-validation.

- bootstrap_samples:

  Number of bootstrap samples for bootstrap validation.

- stratified_sampling:

  Maintain class proportions in train/test splits.

- holdout_split:

  Proportion of data for training in holdout validation (rest for
  testing).

- handle_missing:

  How to handle missing values in predictor variables.

- max_depth:

  Maximum depth of decision tree. Clinical trees typically 3-6 levels.

- min_samples_split:

  Minimum number of samples required to split a node.

- cost_complexity:

  Controls tree pruning - lower values create more complex trees.

- use_1se_rule:

  Select simplest tree within 1 SE of optimal performance.

- clinical_context:

  Clinical application context affects interpretation guidelines.

- cost_sensitive:

  Consider different costs of false positives vs false negatives.

- fn_fp_ratio:

  Relative cost of missing positive case vs false alarm.

- show_tree_plot:

  Display visual representation of the decision tree.

- plot_palette:

  Color palette for the decision tree plot.

- plot_parttree:

  Display 2D partition plot of the decision tree.

- show_performance_metrics:

  Display accuracy, sensitivity, specificity, AUC, and clinical metrics.

- show_confusion_matrix:

  Display confusion matrix with clinical interpretations.

- show_importance_plot:

  Display ranking of most important clinical variables.

- show_roc_plot:

  Display Receiver Operating Characteristic curve.

- show_calibration_plot:

  Display calibration plot (observed vs predicted probability).

- show_clinical_interpretation:

  Display clinical interpretation and usage guidelines.

- set_seed:

  Set random seed for reproducible results.

- seed_value:

  Seed value for random number generation.

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$model_summary`           |     |     |     |     | a html   |
| `results$performance_table`       |     |     |     |     | a table  |
| `results$confusion_matrix`        |     |     |     |     | a table  |
| `results$variable_importance`     |     |     |     |     | a table  |
| `results$tree_plot`               |     |     |     |     | an image |
| `results$parttree_plot`           |     |     |     |     | an image |
| `results$importance_plot`         |     |     |     |     | an image |
| `results$clinical_interpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performance_table$asDF`

`as.data.frame(results$performance_table)`
