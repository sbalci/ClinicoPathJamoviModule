# Categorize Continuous Variables

Categorize Continuous Variables

## Usage

``` r
categorize(
  data,
  var,
  method = "quantile",
  nbins = 4,
  breaks = "",
  sdmult = 1,
  labels = "auto",
  customlabels = "",
  newvarname = "",
  includelowest = TRUE,
  rightclosed = TRUE,
  ordered = TRUE,
  excl = TRUE,
  showcode = TRUE,
  showplot = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- var:

  A numeric variable to be categorized into bins.

- method:

  Method for creating bins. Options are 'equal' for equal-width
  intervals, 'quantile' for quantile-based bins, 'manual' for custom
  break points, 'meansd' for mean +/- standard deviation bins, 'median'
  for median split, or 'jenks' for natural breaks clustering.

- nbins:

  Number of categories to create (2-20). Used for equal, quantile, and
  jenks methods.

- breaks:

  Comma-separated break points for manual binning (e.g., "0, 25, 50, 75,
  100"). Values should be in ascending order.

- sdmult:

  Multiplier for standard deviation when using meansd method (default =
  1).

- labels:

  How to label the resulting categories.

- customlabels:

  Comma-separated custom labels (e.g., "Low, Medium, High"). Must match
  the number of categories.

- newvarname:

  Name for the new categorized variable. Leave empty for automatic
  naming.

- includelowest:

  If TRUE, the first interval will be left-closed (include.lowest in
  cut).

- rightclosed:

  If TRUE, intervals are right-closed (left-open). If FALSE, left-closed
  (right-open).

- ordered:

  If TRUE, creates an ordered factor to preserve category ordering.

- excl:

  Exclude missing values from the analysis.

- showcode:

  Display R code to reproduce the categorization.

- showplot:

  Display a histogram/density plot showing the original distribution and
  cut points.

## Value

A results object containing:

|                            |     |     |     |     |           |
|----------------------------|-----|-----|-----|-----|-----------|
| `results$todo`             |     |     |     |     | a html    |
| `results$summaryText`      |     |     |     |     | a html    |
| `results$freqTable`        |     |     |     |     | a table   |
| `results$breakpointsTable` |     |     |     |     | a table   |
| `results$plot`             |     |     |     |     | an image  |
| `results$rcode`            |     |     |     |     | a html    |
| `results$addtodata`        |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$freqTable$asDF`

`as.data.frame(results$freqTable)`
