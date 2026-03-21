# Testing Checklist: leaveonecenterout

## Test Datasets

| Dataset | File | N | Centers | Variables | Use For |
|---|---|---|---|---|---|
| loocv_multicenter | `data/loocv_multicenter.rda` | 200 | 5 | 12 | Standard testing (all model types) |
| loocv_small | `data/loocv_small.rda` | 45 | 3 | 9 | Edge cases, minimum centers |

### loocv_multicenter Variables

| Variable | Type | Role |
|---|---|---|
| `institution` | factor (5 levels) | centerVariable |
| `treatment_response` | factor (2 levels) | outcome (logistic) |
| `tumor_shrinkage` | numeric | outcome (linear) |
| `os_time` | numeric | elapsedtime (Cox) |
| `os_status` | factor (2 levels) | outcome (Cox) |
| `age` | numeric | predictor |
| `ki67_score` | numeric (some NA) | predictor |
| `tumor_size_mm` | numeric (some NA) | predictor |
| `ctdna_level` | numeric (some NA) | predictor |
| `grade` | factor (G1/G2/G3) | predictor |
| `gender` | factor (Male/Female) | predictor |
| `stage` | factor (I/II/III) | predictor |

## Test Scenarios

### 1. Model Type Coverage

| # | Scenario | modelType | Dataset | Outcome | Predictors | Expected |
|---|---|---|---|---|---|---|
| 1.1 | Logistic default | logistic | multicenter | treatment_response | age, ki67_score, grade | 5-row perCenterResults, AUC values |
| 1.2 | Cox with time | cox | multicenter | os_status (Dead) | age, ki67_score, grade | 5 rows, C-index values |
| 1.3 | Linear continuous | linear | multicenter | tumor_shrinkage | age, ki67_score | 5 rows, R-squared values |
| 1.4 | Logistic small | logistic | small | diagnosis (Positive) | age, marker1 | 3 rows |
| 1.5 | Cox small | cox | small | surv_event (Event) | age, marker1 | 3 rows |
| 1.6 | Linear small | linear | small | continuous_outcome | age, marker1 | 3 rows |

### 2. LASSO Regularization

| # | Scenario | modelType | useLasso | lambdaMethod | Expected |
|---|---|---|---|---|---|
| 2.1 | LASSO logistic 1SE | logistic | true | lambda.1se | Results with LASSO variable selection |
| 2.2 | LASSO logistic min | logistic | true | lambda.min | Different lambda, potentially different AUCs |
| 2.3 | LASSO Cox | cox | true | lambda.1se | C-index from penalized Cox |
| 2.4 | LASSO + linear | linear | true | -- | Warning notice: LASSO not available for linear |

### 3. Option Toggles

| # | Scenario | Option | Value | Expected |
|---|---|---|---|---|
| 3.1 | Pooled on | pooledPerformance | true | Pooled table visible with rows |
| 3.2 | Pooled off | pooledPerformance | false | Pooled table empty/hidden |
| 3.3 | Forest on | forestPlot | true | Forest plot rendered |
| 3.4 | Forest off | forestPlot | false | Forest plot hidden |
| 3.5 | Calibration on | calibrationCheck | true | Brier column visible |
| 3.6 | Calibration off | calibrationCheck | false | Brier column hidden |

### 4. Reproducibility

| # | Scenario | Expected |
|---|---|---|
| 4.1 | Same seed, same data | Identical AUC values across runs |
| 4.2 | Different seed | Potentially different LASSO results |

### 5. Predictor Types

| # | Scenario | Predictors | Expected |
|---|---|---|---|
| 5.1 | Single predictor | ki67_score | Runs successfully |
| 5.2 | Factor-only | grade, stage, gender | Runs with dummy encoding |
| 5.3 | Continuous-only | age, ki67_score, tumor_size_mm | Standard numeric model |
| 5.4 | Mixed | age, grade, stage | Correct handling of both types |
| 5.5 | Many predictors | All 7 predictors | EPV warning likely |

### 6. Edge Cases and Errors

| # | Scenario | Data Condition | Expected |
|---|---|---|---|
| 6.1 | < 3 centers | 2-center subset | Error in todo HTML |
| 6.2 | < 20 observations | 10-row subset | Error in todo HTML |
| 6.3 | Missing data | ki67 has NAs | Complete-case analysis, reduced N |
| 6.4 | Small centers | Center with < 5 cases | Warning notice |
| 6.5 | No events in test center | Degenerate fold | "Skipped" assessment |
| 6.6 | All events in test center | Degenerate fold | "Skipped" assessment |
| 6.7 | Cox without time var | elapsedtime=NULL | Error in todo HTML |

### 7. Notice Triggers

| # | Notice | Trigger Condition | Type |
|---|---|---|---|
| 7.1 | Small centers | Any center n < 5 | warning |
| 7.2 | Low EPV | events/predictors < 10 | strong_warning |
| 7.3 | Event level info | logistic/cox model | info |
| 7.4 | LASSO+linear | useLasso + linear | warning |
| 7.5 | Poor discrimination | Pooled AUC < 0.70 | strong_warning |
| 7.6 | Negative R-squared | Pooled R-squared < 0 | strong_warning |
| 7.7 | High heterogeneity | SD > 0.10 | warning |
| 7.8 | Completion | Always | info |

## Coverage Matrix

| Option | Tested In Scenarios | Coverage |
|---|---|---|
| `outcome` | 1.1-1.6, 5.1-5.5 | Full |
| `outcomeLevel` | 1.1, 1.2, 1.4, 1.5 | Full |
| `predictors` | 1.1-1.6, 5.1-5.5 | Full |
| `centerVariable` | 1.1-1.6 | Full |
| `elapsedtime` | 1.2, 1.5, 6.7 | Full |
| `modelType` | 1.1-1.6 | Full (3 types) |
| `useLasso` | 2.1-2.4 | Full |
| `lambdaMethod` | 2.1, 2.2 | Full (both) |
| `random_seed` | 4.1, 4.2 | Full |
| `pooledPerformance` | 3.1, 3.2 | Full |
| `forestPlot` | 3.3, 3.4 | Full |
| `calibrationCheck` | 3.5, 3.6 | Full |

## Automated Test File

All scenarios 1.x, 2.1-2.3, 3.1-3.2, 4.1, 5.1, 5.2, and 6.1-6.2 are covered in:
`tests/testthat/test-leaveonecenterout.R` (21 tests, all passing)
