# Time-Dependent Survival Calibration

Evaluate survival model performance with time-dependent calibration
metrics. Assess calibration, discrimination, and predictive accuracy at
specific time points using bootstrap validation and cross-validation.
TRIPOD-compliant reporting.

## Usage

``` r
survivalcalibration(
  data,
  time,
  event,
  predicted,
  linearPredictor,
  validationSet,
  calibrationTime = 60,
  calibrationTimes = "12,36,60",
  nGroups = 10,
  validationMethod = "bootstrap",
  nBootstrap = 100,
  nFolds = 10,
  showCalibrationPlot = TRUE,
  showCindexPlot = TRUE,
  showBrierPlot = TRUE,
  showGroupedCalibration = TRUE,
  smoothCalibration = TRUE,
  ciLevel = 0.95,
  tripodReport = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time to event or censoring (in months).

- event:

  Event indicator (0 = censored, 1 = event).

- predicted:

  Predicted survival probability at calibration time (0-1 scale).

- linearPredictor:

  Linear predictor from Cox model (alternative to predicted
  probability).

- validationSet:

  Grouping variable for training/validation/external sets.

- calibrationTime:

  Time point (in months) for calibration assessment (e.g., 60 = 5
  years).

- calibrationTimes:

  Comma-separated time points (in months) for calibration curves.

- nGroups:

  Number of groups (deciles) for grouped calibration curves.

- validationMethod:

  Internal validation method to assess model performance.

- nBootstrap:

  Number of bootstrap samples for optimism correction.

- nFolds:

  Number of folds for cross-validation.

- showCalibrationPlot:

  Display calibration plot (observed vs predicted survival).

- showCindexPlot:

  Display C-index over time with confidence intervals.

- showBrierPlot:

  Display Brier score over time (prediction error).

- showGroupedCalibration:

  Display calibration curves stratified by risk groups.

- smoothCalibration:

  Use loess smoothing for calibration curves.

- ciLevel:

  Confidence level for intervals (0.80-0.99).

- tripodReport:

  Generate TRIPOD-compliant validation report.

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$performanceMetrics`     |     |     |     |     | a table  |
| `results$calibrationMetrics`     |     |     |     |     | a table  |
| `results$discriminationMetrics`  |     |     |     |     | a table  |
| `results$validationSummary`      |     |     |     |     | a html   |
| `results$calibrationPlot`        |     |     |     |     | an image |
| `results$groupedCalibrationPlot` |     |     |     |     | an image |
| `results$cindexPlot`             |     |     |     |     | an image |
| `results$brierPlot`              |     |     |     |     | an image |
| `results$tripodReport`           |     |     |     |     | a html   |
| `results$clinicalInterpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performanceMetrics$asDF`

`as.data.frame(results$performanceMetrics)`
