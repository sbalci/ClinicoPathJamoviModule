# Model Reclassification Metrics (NRI & IDI)

Calculate reclassification metrics to compare predictive performance
between two models. Provides Net Reclassification Improvement (NRI),
Integrated Discrimination Improvement (IDI), and related metrics for
both binary and survival outcomes. Essential for demonstrating
incremental value of new biomarkers or predictors.

## Usage

``` r
reclassmetrics(
  data,
  outcome,
  outcomeType = "binary",
  survivalTime,
  timePoint = 60,
  oldModelProb,
  newModelProb,
  riskCategories = "0.0,0.1,0.2,1.0",
  nriType = "both",
  ciLevel = 0.95,
  bootstrapCI = TRUE,
  nBootstrap = 500,
  showReclassTable = TRUE,
  showEventNonEvent = TRUE,
  showIDIComponents = TRUE,
  showCalibrationComparison = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome (for binary analysis) or event indicator (for
  survival).

- outcomeType:

  Type of outcome variable.

- survivalTime:

  Time to event or censoring (in months) for survival analysis.

- timePoint:

  Time point (in months) at which to evaluate survival predictions.

- oldModelProb:

  Predicted probabilities or survival probabilities from the baseline
  model.

- newModelProb:

  Predicted probabilities or survival probabilities from the enhanced
  model.

- riskCategories:

  Comma-separated risk thresholds for categorical NRI (e.g.,
  "0.0,0.1,0.2,1.0" for low/medium/high risk groups).

- nriType:

  Type of NRI calculation.

- ciLevel:

  Confidence level for intervals (0.80-0.99).

- bootstrapCI:

  Use bootstrap resampling for confidence intervals (recommended).

- nBootstrap:

  Number of bootstrap samples for confidence intervals.

- showReclassTable:

  Display cross-tabulation of risk category changes.

- showEventNonEvent:

  Display NRI components separately for events and non-events.

- showIDIComponents:

  Display integrated sensitivity and 1-specificity improvements.

- showCalibrationComparison:

  Compare calibration between old and new models.

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`          |     |     |     |     | a html   |
| `results$nriResults`            |     |     |     |     | a table  |
| `results$idiResults`            |     |     |     |     | a table  |
| `results$reclassTable`          |     |     |     |     | a table  |
| `results$eventNonEventNRI`      |     |     |     |     | a table  |
| `results$idiComponents`         |     |     |     |     | a table  |
| `results$calibrationComparison` |     |     |     |     | a table  |
| `results$reclassPlot`           |     |     |     |     | an image |
| `results$improvementPlot`       |     |     |     |     | an image |
| `results$summary`               |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$nriResults$asDF`

`as.data.frame(results$nriResults)`
