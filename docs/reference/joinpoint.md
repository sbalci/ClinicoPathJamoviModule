# Joinpoint Trend Analysis

Fits a joinpoint (segmented log-linear) regression to a series of rates
measured over time - the standard analysis for cancer-registry incidence
and mortality trends. The number of joinpoints (change points in trend)
is selected data-adaptively, and each segment is summarized by its
Annual Percent Change (APC). The overall Average Annual Percent Change
(AAPC) summarizes the whole period. Use it to detect when a rate began
rising or falling and to quantify the rate of change in each period.

## Usage

``` r
joinpoint(
  data,
  time,
  rate,
  maxJoinpoints = 3,
  conf_level = 0.95,
  showSegments = TRUE,
  showAAPC = TRUE,
  showPlot = TRUE,
  logScale = FALSE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per time point).

- time:

  Time variable (e.g. calendar year).

- rate:

  The rate or count measured at each time point. Modelled on the log
  scale, so all values must be positive.

- maxJoinpoints:

  The maximum number of joinpoints (change points) to consider. The best
  number up to this maximum is selected using the Bayesian Information
  Criterion.

- conf_level:

  Confidence level for APC / AAPC intervals.

- showSegments:

  Report the Annual Percent Change for each fitted segment.

- showAAPC:

  Report the Average Annual Percent Change over the whole period.

- showPlot:

  Display the observed rates with the fitted joinpoint trend.

- logScale:

  Plot the y-axis on a logarithmic scale (log-linear segments become
  straight).

- showSummary:

  Display a plain-language summary of the trend.

- showExplanation:

  Display an explanation of joinpoint regression.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$joinpointTable` |     |     |     |     | a table  |
| `results$segmentTable`   |     |     |     |     | a table  |
| `results$aapcTable`      |     |     |     |     | a table  |
| `results$plot`           |     |     |     |     | an image |
| `results$summary`        |     |     |     |     | a html   |
| `results$explanation`    |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$joinpointTable$asDF`

`as.data.frame(results$joinpointTable)`

## Examples

``` r
# \donttest{
joinpoint(
    data = mydata,
    time = "year",
    rate = "incidence_rate",
    maxJoinpoints = 3)
#> Error: object 'mydata' not found
# }
```
