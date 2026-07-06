# Multi-class ROC Analysis

Multi-class ROC Analysis for evaluating diagnostic performance with 3 or
more diagnostic classes. This analysis extends traditional binary ROC to
handle multi-category outcomes using one-vs-rest, one-vs-one, and global
approaches.

## Usage

``` r
multiclassroc(
  data,
  outcome,
  predictors,
  method = "ovr",
  calculate_macro_auc = TRUE,
  calculate_micro_auc = TRUE,
  calculate_weighted_auc = TRUE,
  confidence_intervals = TRUE,
  ci_method = "bootstrap",
  bootstrap_samples = 1000,
  confidence_level = 0.95,
  pairwise_comparisons = FALSE,
  confusion_matrix = TRUE,
  class_metrics = TRUE,
  plot_roc_curves = TRUE,
  plot_method = "overlay",
  plot_diagonal = TRUE,
  random_seed = 42
)
```

## Arguments

- data:

  the data as a data frame

- outcome:

  a string naming the outcome variable (must have 3+ levels)

- predictors:

  a vector of strings naming predictor variables (continuous scores)

- method:

  method for multi-class ROC: 'ovr' (one class vs all others), 'ovo'
  (all pairwise comparisons), or 'multinomial' (global probability
  model)

- calculate_macro_auc:

  calculate macro-average AUC across all classes (unweighted mean)

- calculate_micro_auc:

  calculate micro-average AUC (aggregate all predictions)

- calculate_weighted_auc:

  calculate weighted-average AUC (weighted by class prevalence)

- confidence_intervals:

  calculate confidence intervals for AUC estimates

- ci_method:

  method for confidence interval calculation

- bootstrap_samples:

  number of bootstrap samples for CI calculation

- confidence_level:

  confidence level for intervals (default: 0.95 for 95 percent CI)

- pairwise_comparisons:

  show detailed results for all pairwise class comparisons (OvO method)

- confusion_matrix:

  show confusion matrix at optimal global threshold

- class_metrics:

  calculate sensitivity, specificity, PPV, NPV for each class

- plot_roc_curves:

  plot ROC curves for each class

- plot_method:

  display method for ROC curves

- plot_diagonal:

  show diagonal reference line (random classifier)

- random_seed:

  random seed for reproducible bootstrap sampling

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructionsText`   |     |     |     |     | a html   |
| `results$summaryTable`       |     |     |     |     | a table  |
| `results$classAucTable`      |     |     |     |     | a table  |
| `results$pairwiseTable`      |     |     |     |     | a table  |
| `results$classMetricsTable`  |     |     |     |     | a table  |
| `results$confusionMatrix`    |     |     |     |     | a table  |
| `results$rocPlot`            |     |     |     |     | an image |
| `results$interpretationText` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`

## Details

Common applications include tumor subtype classification, disease
staging with multiple levels, and AI model validation for multi-class
predictions.
