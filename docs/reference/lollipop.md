# Lollipop Chart

Creates lollipop charts for categorical data visualization following R
Graph Gallery best practices, with emphasis on clinical applications
like patient timelines, treatment outcomes, and biomarker comparisons.
Uses geom_segment() and geom_point() for optimal visual presentation.

## Usage

``` r
lollipop(
  data,
  dep,
  group,
  useHighlight = FALSE,
  highlight,
  aggregation = "none",
  sortBy = "original",
  orientation = "vertical",
  showValues = FALSE,
  showMean = FALSE,
  colorScheme = "default",
  theme = "default",
  pointSize = 3,
  lineWidth = 1,
  lineType = "solid",
  baseline = 0,
  conditionalColor = FALSE,
  colorThreshold = 0,
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

- dep:

  The numeric variable for the values (lollipop heights/lengths).

- group:

  The categorical variable for grouping (lollipop categories).

- useHighlight:

  Enable or disable highlighting of specific levels in the plot.

- highlight:

  Specific level to highlight in the plot with different color/style.
  Only used when useHighlight is TRUE.

- aggregation:

  How to aggregate multiple values per group. Use 'Mean' or 'Median' for
  typical clinical measurements, 'Sum' for counts. 'No Aggregation' will
  over-plot if multiple rows per group exist.

- sortBy:

  How to sort the lollipops in the chart.

- orientation:

  Chart orientation (vertical or horizontal lollipops).

- showValues:

  Whether to display value labels on the lollipops.

- showMean:

  Whether to display a reference line at the mean value.

- colorScheme:

  Color scheme for the lollipops.

- theme:

  Overall theme/appearance of the plot.

- pointSize:

  Size of the lollipop points.

- lineWidth:

  Width of the lollipop stems.

- lineType:

  Type of line for lollipop stems.

- baseline:

  Starting point for lollipop stems (default is 0).

- conditionalColor:

  Enable coloring based on value thresholds.

- colorThreshold:

  Threshold value for conditional coloring (values above/below get
  different colors).

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

|                   |     |     |     |     |                |
|-------------------|-----|-----|-----|-----|----------------|
| `results$notices` |     |     |     |     | a preformatted |
| `results$todo`    |     |     |     |     | a html         |
| `results$summary` |     |     |     |     | a table        |
| `results$plot`    |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
