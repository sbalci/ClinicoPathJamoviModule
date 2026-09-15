# Confidence Intervals for Mean Values

Confidence Intervals for Mean Values

## Usage

``` r
ciSingle(
  data,
  deps,
  splitBy,
  ciWidth = 95,
  method = "t",
  showPlot = FALSE,
  bootstrapSamples = 1000,
  showDiagnostics = FALSE
)
```

## Arguments

- data:

  .

- deps:

  .

- splitBy:

  .

- ciWidth:

  Confidence level for interval estimation.

- method:

  Method for calculating confidence intervals.

- showPlot:

  Display confidence interval visualization.

- bootstrapSamples:

  Number of bootstrap samples (when using bootstrap method).

- showDiagnostics:

  Display normality tests and other diagnostic information.

## Value

A results object containing:

|                       |     |     |     |     |                |
|-----------------------|-----|-----|-----|-----|----------------|
| `results$conflevel`   |     |     |     |     | a preformatted |
| `results$citable`     |     |     |     |     | a table        |
| `results$diagnostics` |     |     |     |     | a table        |
| `results$plot`        |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$citable$asDF`

`as.data.frame(results$citable)`
