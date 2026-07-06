# Line Chart

Creates line charts for time series analysis and trend visualization,
with support for multiple groups, confidence intervals, and statistical
overlays.

## Usage

``` r
linechart(
  data,
  xvar,
  yvar,
  groupby = NULL,
  confidence = FALSE,
  trendline = FALSE,
  points = TRUE,
  smooth = FALSE,
  refline = 0,
  reflineLabel = "Reference",
  colorPalette = "default",
  theme = "default",
  xlabel = "",
  ylabel = "",
  title = "",
  width = 800,
  height = 600
)
```

## Arguments

- data:

  The data as a data frame.

- xvar:

  The variable for the x-axis. Typically represents time, sequence, or
  ordered categories.

- yvar:

  The variable for the y-axis. Must be numeric (continuous variable).

- groupby:

  Optional grouping variable to create multiple lines (e.g., treatment
  groups, patient categories).

- confidence:

  Whether to display confidence intervals around the line(s).

- trendline:

  Whether to add a trend line (linear regression) to the plot.

- points:

  Whether to show individual data points on the line(s).

- smooth:

  Whether to apply smoothing (loess) to the line(s).

- refline:

  Optional reference line value (e.g., normal range, threshold). Set to
  0 for no reference line.

- reflineLabel:

  Label for the reference line.

- colorPalette:

  Color palette for multiple groups.

- theme:

  Overall theme/appearance of the plot.

- xlabel:

  Custom label for the x-axis.

- ylabel:

  Custom label for the y-axis.

- title:

  Custom title for the plot.

- width:

  Width of the plot in pixels.

- height:

  Height of the plot in pixels.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$naturalSummary` |     |     |     |     | a html   |
| `results$summary`        |     |     |     |     | a table  |
| `results$correlation`    |     |     |     |     | a table  |
| `results$assumptions`    |     |     |     |     | a html   |
| `results$plot`           |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
