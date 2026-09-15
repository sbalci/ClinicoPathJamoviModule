# Gradient Boosting for Survival Analysis

Gradient Boosting for survival analysis using ensemble methods that
combine multiple weak learners (typically shallow trees) to create
strong predictive models. This implementation supports both traditional
gradient boosting (gbm) and modern component-wise boosting (mboost)
approaches. The method is particularly effective for high-dimensional
survival data and complex non-linear relationships between predictors
and survival outcomes. Features include automatic variable selection,
handling of mixed-type predictors, built-in cross-validation for optimal
stopping, and robust performance with noisy data. Especially suitable
for biomarker discovery, prognostic modeling, and complex survival
prediction tasks.

## Usage

``` r
gradientboosting(
  data,
  time,
  event,
  predictors,
  strata,
  algorithm = "mboost",
  n_trees = 100,
  learning_rate = 0.1,
  max_depth = 3,
  min_node_size = 10,
  subsample = 1,
  cv_folds = 5,
  early_stopping = TRUE,
  patience = 10,
  reg_alpha = 0,
  reg_lambda = 1,
  variable_selection = TRUE,
  importance_threshold = 0.01,
  show_convergence = TRUE,
  show_importance = TRUE,
  show_predictions = FALSE,
  plot_convergence = TRUE,
  plot_importance = TRUE,
  plot_partial = FALSE,
  plot_survival = FALSE,
  interaction_depth = 1,
  bag_fraction = 0.5,
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

  Variables to use for boosting. Can include numeric, ordinal, and
  nominal variables. The algorithm automatically handles mixed-type
  predictors and performs variable selection.

- strata:

  Optional stratification variable for stratified survival analysis.
  Creates separate baseline hazards for each stratum.

- algorithm:

  Boosting algorithm to use. mboost provides component-wise boosting
  with statistical framework, gbm offers traditional gradient boosting,
  xgboost provides extreme gradient boosting with advanced
  regularization.

- n_trees:

  Number of boosting iterations (trees). More trees can improve
  performance but may lead to overfitting. Use cross-validation to
  determine optimal value.

- learning_rate:

  Learning rate (shrinkage parameter). Lower values require more trees
  but often provide better generalization. Typical values: 0.01-0.3.

- max_depth:

  Maximum depth of individual trees. Shallow trees (1-6) are typically
  sufficient for boosting. Deeper trees may capture interactions but
  increase overfitting risk.

- min_node_size:

  Minimum number of observations in terminal nodes. Higher values create
  simpler trees and reduce overfitting.

- subsample:

  Fraction of observations used for each tree. Values \< 1.0 introduce
  stochasticity and can improve generalization (stochastic gradient
  boosting).

- cv_folds:

  Number of folds for cross-validation to determine optimal number of
  trees and prevent overfitting. Set to 0 to disable cross-validation.

- early_stopping:

  Use early stopping based on cross-validation to prevent overfitting.
  Stops training when validation error stops improving.

- patience:

  Number of iterations without improvement before early stopping. Higher
  values allow more exploration but may lead to overfitting.

- reg_alpha:

  L1 (Lasso) regularization parameter for XGBoost. Higher values
  increase sparsity by driving coefficients to zero.

- reg_lambda:

  L2 (Ridge) regularization parameter for XGBoost. Higher values reduce
  overfitting by penalizing large coefficients.

- variable_selection:

  Perform automatic variable selection during boosting. Variables with
  low importance are excluded from final model.

- importance_threshold:

  Minimum relative importance for variable inclusion in final model.
  Variables below this threshold are excluded.

- show_convergence:

  Display convergence diagnostics including training and validation
  error curves, optimal stopping point, and convergence statistics.

- show_importance:

  Calculate and display variable importance measures based on the
  frequency and improvement of splits.

- show_predictions:

  Generate survival predictions and risk scores for the training data.
  Useful for model evaluation and risk stratification.

- plot_convergence:

  Plot training and validation error curves showing convergence behavior
  and optimal stopping point.

- plot_importance:

  Generate variable importance plot showing relative importance of
  predictors in the boosted model.

- plot_partial:

  Create partial dependence plots for top variables showing marginal
  effect on survival hazard.

- plot_survival:

  Plot Kaplan-Meier curves for risk groups defined by boosted model
  predictions with statistical comparisons.

- interaction_depth:

  Maximum order of variable interactions to consider. Higher values
  capture complex interactions but increase computational complexity.

- bag_fraction:

  Fraction of variables randomly selected for each tree (for gbm).
  Introduces randomness and can improve generalization.

- random_seed:

  Random seed for reproducible results. Change to get different random
  splits and variable selections.

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$todo`               |     |     |     |     | a html   |
| `results$modelSummary`       |     |     |     |     | a table  |
| `results$convergenceStats`   |     |     |     |     | a table  |
| `results$variableImportance` |     |     |     |     | a table  |
| `results$predictions`        |     |     |     |     | a table  |
| `results$convergencePlot`    |     |     |     |     | an image |
| `results$importancePlot`     |     |     |     |     | an image |
| `results$partialPlots`       |     |     |     |     | an image |
| `results$survivalPlot`       |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
