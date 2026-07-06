# Extremely Randomized Trees for Survival

Extremely Randomized Trees (Extra Trees) for survival analysis using the
ranger package implementation. This method extends random forests by
introducing additional randomization in both variable selection and
split point selection, leading to increased bias but reduced variance
and computational efficiency. The algorithm is particularly effective
for high-dimensional survival data, large datasets, and scenarios
requiring fast training and prediction. Features include built-in
variable importance measures, out-of-bag error estimation, support for
competing risks, and excellent scalability. Ideal for exploratory
analysis, baseline modeling, and ensemble methods in survival prediction
tasks.

## Usage

``` r
extratrees(
  data,
  time,
  event,
  predictors,
  strata,
  num_trees = 500,
  mtry = "sqrt",
  mtry_custom,
  min_node_size = 10,
  max_depth = 0,
  splitrule = "extratrees",
  num_random_splits = 1,
  sample_fraction = 1,
  replace = TRUE,
  case_weights,
  importance = "permutation",
  scale_permutation = TRUE,
  keep_inbag = FALSE,
  oob_error = TRUE,
  probability = FALSE,
  show_forest_summary = TRUE,
  show_importance = TRUE,
  show_oob_predictions = FALSE,
  plot_importance = TRUE,
  plot_oob_error = TRUE,
  plot_survival = FALSE,
  plot_partial = FALSE,
  regularization = 1,
  alpha = 0.05,
  num_threads = 0,
  random_seed = 123
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
  efficiently.

- strata:

  Optional stratification variable for stratified survival analysis.
  Creates separate baseline hazards for each stratum.

- num_trees:

  Number of trees in the forest. More trees generally improve stability
  and performance but increase computation time. 500-2000 is typical.

- mtry:

  Number of variables randomly selected at each split. Square root of
  total variables is default. Smaller values increase randomization.

- mtry_custom:

  Custom number of variables per split when mtry is set to 'custom'.
  Should be between 1 and total number of predictors.

- min_node_size:

  Minimum number of observations in terminal nodes. Larger values create
  simpler trees and reduce overfitting, smaller values allow more
  complex patterns.

- max_depth:

  Maximum depth of trees. 0 means no limit. Deep trees may overfit,
  while shallow trees may underfit. Usually left unlimited for Extra
  Trees.

- splitrule:

  Splitting criterion. 'extratrees' uses extreme randomization,
  'logrank' uses log-rank statistic, 'C' optimizes concordance index,
  'maxstat' uses maximally selected rank statistics.

- num_random_splits:

  Number of random splits to try per variable for Extra Trees rule.
  Higher values reduce randomization but may improve performance.

- sample_fraction:

  Fraction of observations sampled for each tree. Values \< 1.0 create
  subsampled forests which can improve generalization and reduce
  computation.

- replace:

  Use bootstrap sampling (with replacement) for each tree. When FALSE,
  uses subsampling without replacement.

- case_weights:

  Optional variable containing case weights for observations. Higher
  weights give observations more influence in tree construction.

- importance:

  Method for calculating variable importance. Permutation importance
  measures prediction accuracy decrease when variables are permuted.
  Impurity importance measures decrease in node impurity.

- scale_permutation:

  Scale permutation importance by standard error. Provides more stable
  importance measures especially for correlated predictors.

- keep_inbag:

  Store which observations are in-bag for each tree. Required for some
  post-processing analyses but increases memory usage.

- oob_error:

  Calculate out-of-bag prediction error. Provides unbiased estimate of
  model performance without need for separate validation set.

- probability:

  Estimate survival probabilities instead of hazard ratios. Useful for
  risk prediction and probability-based decisions.

- show_forest_summary:

  Display summary statistics for the Extra Trees forest including OOB
  error, variable importance rankings, and model parameters.

- show_importance:

  Display detailed variable importance measures with rankings and
  statistical significance tests.

- show_oob_predictions:

  Display out-of-bag predictions for model evaluation and risk
  stratification analysis.

- plot_importance:

  Create variable importance plot showing relative importance of
  predictors with confidence intervals.

- plot_oob_error:

  Plot out-of-bag error as function of number of trees to assess
  convergence and optimal forest size.

- plot_survival:

  Create Kaplan-Meier survival curves for risk groups defined by Extra
  Trees predictions with statistical comparisons.

- plot_partial:

  Generate partial dependence plots for top variables showing marginal
  effects on survival predictions.

- regularization:

  Regularization parameter for Extra Trees. Values \> 1 increase
  randomization, values \< 1 reduce it. 1.0 is standard Extra Trees.

- alpha:

  Significance level for variable importance testing and confidence
  intervals in plots.

- num_threads:

  Number of threads for parallel computation. 0 uses all available
  cores. Parallel computation significantly speeds up training.

- random_seed:

  Random seed for reproducible results. Change to get different random
  forests and bootstrap samples.

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$todo`               |     |     |     |     | a html   |
| `results$forestSummary`      |     |     |     |     | a table  |
| `results$variableImportance` |     |     |     |     | a table  |
| `results$oobPredictions`     |     |     |     |     | a table  |
| `results$importancePlot`     |     |     |     |     | an image |
| `results$oobErrorPlot`       |     |     |     |     | an image |
| `results$survivalPlot`       |     |     |     |     | an image |
| `results$partialPlots`       |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$forestSummary$asDF`

`as.data.frame(results$forestSummary)`
