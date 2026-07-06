# Conditional Inference Trees for Survival

Conditional Inference Trees for survival analysis using unbiased
recursive partitioning. This method addresses the variable selection
bias inherent in traditional CART by employing conditional inference
procedures. The algorithm separates variable selection from splitting
point selection, providing unbiased tree structures particularly
valuable for survival data with mixed-type predictors and complex
censoring patterns. Features include statistical significance-based
stopping criteria, handling of missing values without surrogate splits,
and robust performance with correlated predictors. The method is
particularly suitable for exploratory survival analysis and biomarker
discovery in clinical research.

## Usage

``` r
conditionalinference(
  data,
  time,
  event,
  predictors,
  strata,
  teststat = "quad",
  testtype = "Bonferroni",
  mincriterion = 0.95,
  minsplit = 20,
  minbucket = 7,
  maxdepth = 5,
  nresample = 9999,
  logrank_scores = TRUE,
  show_splits = TRUE,
  show_nodes = TRUE,
  show_importance = TRUE,
  plot_tree = TRUE,
  plot_survival = TRUE,
  plot_importance = FALSE,
  mtry,
  replace = FALSE,
  subset
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time to event variable (numeric). For right-censored data, this is the
  time from study entry to event or censoring.

- event:

  Event indicator variable. For survival analysis: 0 = censored, 1 =
  event. For competing risks: 0 = censored, 1+ = different event types.

- predictors:

  Variables to use for tree construction. Can include numeric, ordinal,
  and nominal variables. The algorithm handles mixed-type predictors
  optimally.

- strata:

  Optional stratification variable for stratified survival analysis.
  Creates separate baseline hazards for each stratum.

- teststat:

  Test statistic to use. 'quad' uses quadratic form for multivariate
  tests, 'max' uses maximum-type statistic. Quadratic form is more
  powerful for small effects, maximum-type for large effects.

- testtype:

  Multiple testing correction method. Bonferroni provides conservative
  but reliable correction. Monte Carlo uses permutation-based p-values.

- mincriterion:

  Minimum criterion (1 - p-value) for variable selection. Higher values
  create more conservative trees with fewer splits. Default 0.95
  corresponds to significance level of 0.05.

- minsplit:

  Minimum number of observations required in a node before splitting.
  Higher values create simpler trees and reduce overfitting.

- minbucket:

  Minimum number of observations in terminal nodes. Should be smaller
  than minsplit. Higher values increase tree stability.

- maxdepth:

  Maximum depth of the tree. Controls tree complexity and prevents
  overfitting. Deeper trees may overfit, shallower trees may underfit.

- nresample:

  Number of Monte Carlo resamples for p-value computation when using
  Monte Carlo test type. More resamples provide more accurate p-values
  but increase computation time.

- logrank_scores:

  Use log-rank scores for survival splitting. When TRUE, uses log-rank
  statistics optimized for survival data. When FALSE, uses standard
  conditional inference.

- show_splits:

  Display detailed split statistics including test statistics, p-values,
  and variable importance measures.

- show_nodes:

  Display detailed information for each terminal node including
  Kaplan-Meier estimates, risk tables, and survival summaries.

- show_importance:

  Calculate and display variable importance measures based on the
  improvement in prediction accuracy.

- plot_tree:

  Generate tree structure plot with nodes, splits, and survival curves.

- plot_survival:

  Plot Kaplan-Meier survival curves for each terminal node with
  confidence intervals and risk tables.

- plot_importance:

  Generate variable importance plot showing relative importance of
  predictors in tree construction.

- mtry:

  Number of variables randomly selected at each split. Leave empty to
  use all variables. Setting this creates a random forest-like approach.

- replace:

  Use bootstrap sampling with replacement for tree construction. Creates
  more robust trees but may reduce interpretability.

- subset:

  Fraction of observations to use for tree construction. Leave empty to
  use all observations. Useful for large datasets.

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$todo`               |     |     |     |     | a html   |
| `results$modelSummary`       |     |     |     |     | a table  |
| `results$splitStatistics`    |     |     |     |     | a table  |
| `results$nodeDetails`        |     |     |     |     | a table  |
| `results$variableImportance` |     |     |     |     | a table  |
| `results$treePlot`           |     |     |     |     | an image |
| `results$survivalPlot`       |     |     |     |     | an image |
| `results$importancePlot`     |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
