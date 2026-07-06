# Clinical Tree Algorithm Comparison

Comprehensive comparison of decision tree algorithms for clinical
research. Compares CART, Random Forest, and Gradient Boosting with
cross-validation, statistical testing, and clinical performance
assessment.

## Usage

``` r
treecompare(
  data,
  vars = NULL,
  facs = NULL,
  target,
  targetLevel,
  include_cart = TRUE,
  include_rf = TRUE,
  include_gbm = FALSE,
  include_xgboost = FALSE,
  include_ctree = FALSE,
  validation = "repeated_cv",
  cv_folds = 5,
  cv_repeats = 5,
  bootstrap_samples = 200,
  test_split = 0.25,
  stratified_sampling = TRUE,
  primary_metric = "bacc",
  statistical_testing = TRUE,
  correction_method = "holm",
  tune_parameters = TRUE,
  tuning_method = "grid",
  cart_max_depth = 5,
  cart_min_split = 20,
  rf_ntrees = 500,
  rf_mtry_method = "auto",
  clinical_context = "diagnosis",
  interpretability_weight = 0.3,
  show_comparison_table = TRUE,
  show_performance_plot = TRUE,
  plot_parttree = FALSE,
  show_roc_comparison = TRUE,
  show_statistical_tests = TRUE,
  show_ranking_table = TRUE,
  show_computational_time = TRUE,
  show_clinical_recommendations = TRUE,
  show_detailed_metrics = FALSE,
  ensemble_best_models = FALSE,
  save_best_models = FALSE,
  set_seed = TRUE,
  seed_value = 42,
  parallel_processing = TRUE,
  verbose_output = FALSE
)
```

## Arguments

- data:

  The data as a data frame for algorithm comparison.

- vars:

  .

- facs:

  .

- target:

  .

- targetLevel:

  .

- include_cart:

  Include Classification and Regression Trees (CART) algorithm.

- include_rf:

  Include Random Forest ensemble method.

- include_gbm:

  Include Gradient Boosting Machine (requires gbm package).

- include_xgboost:

  Include XGBoost algorithm (requires xgboost package).

- include_ctree:

  Include conditional inference trees (requires party package).

- validation:

  Validation method for fair algorithm comparison.

- cv_folds:

  .

- cv_repeats:

  .

- bootstrap_samples:

  .

- test_split:

  .

- stratified_sampling:

  .

- primary_metric:

  Primary metric for ranking algorithms.

- statistical_testing:

  Perform statistical tests to compare algorithm performance.

- correction_method:

  Correction method for multiple pairwise comparisons.

- tune_parameters:

  Automatically tune key parameters for each algorithm.

- tuning_method:

  .

- cart_max_depth:

  .

- cart_min_split:

  .

- rf_ntrees:

  .

- rf_mtry_method:

  .

- clinical_context:

  .

- interpretability_weight:

  Weight given to interpretability in final recommendations
  (0=performance only, 1=interpretability only).

- show_comparison_table:

  Display comprehensive comparison table with all metrics.

- show_performance_plot:

  Display box plots comparing algorithm performance.

- plot_parttree:

  Display 2D partition plot for CART or Conditional Trees (if enabled).

- show_roc_comparison:

  Display overlaid ROC curves for all algorithms.

- show_statistical_tests:

  Display pairwise statistical test results.

- show_ranking_table:

  Display final algorithm ranking with recommendations.

- show_computational_time:

  Include computational time in comparison.

- show_clinical_recommendations:

  Provide clinical recommendations based on comparison results.

- show_detailed_metrics:

  Show detailed metrics for each algorithm (sensitivity, specificity,
  etc.).

- ensemble_best_models:

  Create ensemble combining top-performing algorithms.

- save_best_models:

  Save the best-performing models for future use.

- set_seed:

  .

- seed_value:

  .

- parallel_processing:

  Use multiple cores for faster comparison (if available).

- verbose_output:

  Show detailed progress during model comparison.

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`             |     |     |     |     | a html   |
| `results$algorithm_summary`        |     |     |     |     | a html   |
| `results$comparison_table`         |     |     |     |     | a table  |
| `results$performance_plot`         |     |     |     |     | an image |
| `results$roc_comparison`           |     |     |     |     | an image |
| `results$statistical_tests`        |     |     |     |     | a table  |
| `results$ranking_table`            |     |     |     |     | a table  |
| `results$clinical_recommendations` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$comparison_table$asDF`

`as.data.frame(results$comparison_table)`
