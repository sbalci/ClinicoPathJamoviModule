# Leave-One-Center-Out Cross-Validation

Performs leave-one-center-out cross-validation for multi-institutional
prediction models. Trains on all-but-one center and evaluates on the
held-out center, repeating for each center. Provides internal-external
validation recommended by TRIPOD and Debray et al. (2015).

## Usage

``` r
leaveonecenterout(
  data,
  outcome,
  outcomeLevel,
  predictors,
  centerVariable,
  elapsedtime,
  modelType = "logistic",
  useLasso = FALSE,
  lambdaMethod = "lambda.1se",
  random_seed = 42,
  pooledPerformance = TRUE,
  forestPlot = TRUE,
  calibrationCheck = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Outcome variable to predict. Binary for logistic, continuous for
  linear.

- outcomeLevel:

  Level of outcome considered as the event (for binary outcomes).

- predictors:

  Variables to include in the prediction model.

- centerVariable:

  Grouping variable identifying center, institution, or study site. Each
  unique level is used as a held-out test set in turn. Minimum 3 centers
  required.

- elapsedtime:

  Survival time variable. Only required when modelType is cox.

- modelType:

  Type of prediction model to fit in each fold.

- useLasso:

  Apply LASSO (L1) penalty for automatic variable selection within each
  CV fold. Recommended when number of predictors is large.

- lambdaMethod:

  Lambda selection method for LASSO (inner CV within each training
  fold).

- random_seed:

  Random seed for reproducibility.

- pooledPerformance:

  Show pooled (average) performance across all held-out centers.

- forestPlot:

  Show forest plot of AUC/C-index per center with pooled estimate.

- calibrationCheck:

  Assess calibration (Brier score) per center and pooled.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$notices`           |     |     |     |     | a html   |
| `results$designSummary`     |     |     |     |     | a table  |
| `results$perCenterResults`  |     |     |     |     | a table  |
| `results$pooledPerformance` |     |     |     |     | a table  |
| `results$forestplot`        |     |     |     |     | an image |
| `results$interpretation`    |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$designSummary$asDF`

`as.data.frame(results$designSummary)`
