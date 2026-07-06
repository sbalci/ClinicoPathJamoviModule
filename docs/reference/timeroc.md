# Enhanced ROC Analysis

Enhanced ROC Analysis

## Usage

``` r
timeroc(
  data,
  elapsedtime = NULL,
  outcome = NULL,
  outcomeLevel,
  marker = NULL,
  timepoints = "12, 36, 60",
  method = "marginal",
  bootstrapCI = FALSE,
  nboot = 100,
  plotROC = TRUE,
  plotAUC = TRUE,
  timetypeoutput = "months",
  showOptimalCutoff = TRUE,
  showMarkerStats = TRUE,
  compareBaseline = FALSE,
  smoothAUC = FALSE,
  analysisType = "timedep",
  compareROCs = FALSE,
  markers = list(),
  rocComparison = "delong",
  youdenIndex = TRUE
)
```

## Arguments

- data:

  .

- elapsedtime:

  .

- outcome:

  .

- outcomeLevel:

  .

- marker:

  .

- timepoints:

  .

- method:

  Method for Inverse Probability of Censoring Weights. Marginal uses
  Kaplan-Meier and assumes independent censoring. Cox and Aalen adjust
  for covariates affecting censoring.

- bootstrapCI:

  Compute asymptotic confidence intervals for AUC using influence
  function-based variance estimation.

- nboot:

  Deprecated. Kept for backward compatibility with saved analyses. The
  CI computation uses analytic influence functions, not bootstrap.

- plotROC:

  .

- plotAUC:

  .

- timetypeoutput:

  Time units for display in plots and results.

- showOptimalCutoff:

  Calculate and display optimal cutoff points that maximize Youden
  index.

- showMarkerStats:

  Display descriptive statistics for the marker variable.

- compareBaseline:

  Compare marker performance to a baseline model (AUC = 0.5).

- smoothAUC:

  Apply smoothing to the AUC over time plot for better visualization.

- analysisType:

  Choose between time-dependent ROC analysis or general binary
  classification ROC.

- compareROCs:

  Compare multiple ROC curves using statistical tests (DeLong test for
  binary ROC).

- markers:

  Additional marker variables to compare against the primary marker.

- rocComparison:

  Statistical method for comparing ROC curves (binary analysis only).

- youdenIndex:

  Calculate Youden index (sensitivity + specificity - 1) for optimal
  threshold selection.

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$notices`                |     |     |     |     | a html   |
| `results$text`                   |     |     |     |     | a html   |
| `results$aucTable`               |     |     |     |     | a table  |
| `results$rocPlot`                |     |     |     |     | an image |
| `results$aucPlot`                |     |     |     |     | an image |
| `results$markerStats`            |     |     |     |     | a table  |
| `results$cutoffTable`            |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a html   |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$binaryROCTable`         |     |     |     |     | a table  |
| `results$rocComparisonTable`     |     |     |     |     | a table  |
| `results$binaryROCPlot`          |     |     |     |     | an image |
| `results$diagnosticPerformance`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$aucTable$asDF`

`as.data.frame(results$aucTable)`
