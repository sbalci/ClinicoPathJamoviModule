# Clinical Random Forest Analysis

Random Forest and ensemble methods for clinical research and biomarker
discovery. Provides high-accuracy predictions with robust feature
importance analysis, out-of-bag error estimation, and clinical
interpretation guidelines.

## Usage

``` r
treeensemble(
  data,
  vars = NULL,
  facs = NULL,
  target,
  targetLevel,
  n_trees = 500,
  mtry_method = "auto",
  mtry_custom = 3,
  min_node_size = 5,
  validation = "oob",
  cv_folds = 5,
  test_split = 0.25,
  importance_type = "permutation",
  feature_selection = FALSE,
  max_features = 10,
  clinical_context = "biomarker",
  class_weights = FALSE,
  show_performance_metrics = TRUE,
  show_importance_plot = TRUE,
  show_oob_error = TRUE,
  show_confusion_matrix = TRUE,
  show_clinical_interpretation = TRUE,
  set_seed = TRUE,
  seed_value = 42
)
```

## Arguments

- data:

  .

- vars:

  .

- facs:

  .

- target:

  .

- targetLevel:

  .

- n_trees:

  .

- mtry_method:

  .

- mtry_custom:

  .

- min_node_size:

  .

- validation:

  .

- cv_folds:

  .

- test_split:

  .

- importance_type:

  .

- feature_selection:

  .

- max_features:

  .

- clinical_context:

  .

- class_weights:

  .

- show_performance_metrics:

  .

- show_importance_plot:

  .

- show_oob_error:

  .

- show_confusion_matrix:

  .

- show_clinical_interpretation:

  .

- set_seed:

  .

- seed_value:

  .

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$model_summary`           |     |     |     |     | a html   |
| `results$performance_table`       |     |     |     |     | a table  |
| `results$confusion_matrix`        |     |     |     |     | a table  |
| `results$importance_table`        |     |     |     |     | a table  |
| `results$importance_plot`         |     |     |     |     | an image |
| `results$oob_error_plot`          |     |     |     |     | an image |
| `results$clinical_interpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performance_table$asDF`

`as.data.frame(results$performance_table)`
