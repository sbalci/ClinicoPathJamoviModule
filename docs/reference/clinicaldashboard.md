# Interactive Clinical Dashboard

Interactive Clinical Dashboard

## Usage

``` r
clinicaldashboard(
  data,
  patientId,
  timeVar,
  outcomeVars,
  groupVar,
  dashboardType = "population",
  timeWindow = "last_90_days",
  showTrends = TRUE,
  showAlerts = TRUE,
  alertThresholds = "",
  showSummaryStats = TRUE,
  showDistributions = TRUE,
  realTimeUpdate = FALSE,
  exportDashboard = FALSE
)
```

## Arguments

- data:

  .

- patientId:

  .

- timeVar:

  .

- outcomeVars:

  .

- groupVar:

  .

- dashboardType:

  .

- timeWindow:

  .

- showTrends:

  .

- showAlerts:

  .

- alertThresholds:

  .

- showSummaryStats:

  .

- showDistributions:

  .

- realTimeUpdate:

  .

- exportDashboard:

  .

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$instructions`     |     |     |     |     | a html   |
| `results$summaryMetrics`   |     |     |     |     | a table  |
| `results$clinicalAlerts`   |     |     |     |     | a table  |
| `results$outcomeStats`     |     |     |     |     | a table  |
| `results$trendPlot`        |     |     |     |     | an image |
| `results$distributionPlot` |     |     |     |     | an image |
| `results$dashboardSummary` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryMetrics$asDF`

`as.data.frame(results$summaryMetrics)`
