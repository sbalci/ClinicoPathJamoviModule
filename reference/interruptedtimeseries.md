# Interrupted Time Series Analysis

Evaluates the effect of an intervention on an outcome measured
repeatedly over time using segmented regression of an interrupted time
series. The model estimates the pre-intervention level and trend, the
immediate change in level at the intervention point, and the change in
trend (slope) afterwards. Standard errors can be adjusted for
autocorrelation using Newey-West (HAC) estimators, and a Durbin-Watson
test reports residual autocorrelation. This is the standard
quasi-experimental design for evaluating quality-improvement, policy, or
laboratory-process interventions where randomization is not possible.

## Usage

``` r
interruptedtimeseries(
  data,
  time,
  outcome,
  interventionTime = 0,
  hac = TRUE,
  lag = 0,
  counterfactual = TRUE,
  predictAt = 0,
  showDiagnostics = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per time point).

- time:

  Sequential time index (e.g. month or week number), evenly spaced.

- outcome:

  The continuous outcome measured at each time point.

- interventionTime:

  The value of the time variable at which the intervention began. The
  first post-intervention observation is the first time point at or
  after this value.

- hac:

  Adjust standard errors for autocorrelation and heteroscedasticity
  using the Newey-West estimator. Recommended for time series data.

- lag:

  Maximum lag for the Newey-West estimator. Set to 0 to choose the lag
  automatically from the series length.

- counterfactual:

  Overlay the projected counterfactual (the pre-intervention trend
  extrapolated forward as if no intervention had occurred) on the plot.

- predictAt:

  A post-intervention time point at which to report the absolute and
  relative effect of the intervention (observed model prediction minus
  counterfactual). Set to 0 to skip.

- showDiagnostics:

  Report the Durbin-Watson test for residual autocorrelation.

- showPlot:

  Display the observed series with fitted segments and intervention
  marker.

- showSummary:

  Display a plain-language summary of the intervention effect.

- showExplanation:

  Display an explanation of the segmented regression methodology.

## Value

A results object containing:

|                       |     |     |     |     |          |
|-----------------------|-----|-----|-----|-----|----------|
| `results$todo`        |     |     |     |     | a html   |
| `results$coefTable`   |     |     |     |     | a table  |
| `results$effectTable` |     |     |     |     | a table  |
| `results$diagnostics` |     |     |     |     | a table  |
| `results$plot`        |     |     |     |     | an image |
| `results$summary`     |     |     |     |     | a html   |
| `results$explanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefTable$asDF`

`as.data.frame(results$coefTable)`

## Examples

``` r
# \donttest{
interruptedtimeseries(
    data = mydata,
    time = "month",
    outcome = "turnaround_time",
    interventionTime = 25,
    hac = TRUE)
#> Error: object 'mydata' not found
# }
```
