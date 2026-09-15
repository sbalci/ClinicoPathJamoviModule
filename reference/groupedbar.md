# Grouped Bar Chart Comparison

Creates grouped bar charts to compare measurements, responses, or
outcomes across different groups. Suitable for comparing lab values
across disease groups, treatment responses across hospitals, survey
responses across demographics, or any metric that varies by categorical
groupings.

## Usage

``` r
groupedbar(
  data,
  items,
  groups,
  values = NULL,
  facetby = NULL,
  statistic = "mean",
  plottype = "grouped",
  sortorder = "highest_first",
  grouporder = "data",
  customgrouporder = "",
  showvalues = TRUE,
  valueposition = "outside",
  showerrorbars = FALSE,
  errortype = "se",
  showconnectors = FALSE,
  colorscheme = "default",
  customcolors = "",
  referenceline,
  referencelinelabel = "",
  referenceband = "",
  title = "",
  subtitle = "",
  xlabel = "",
  ylabel = "",
  showlegend = TRUE,
  legendtitle = "",
  legendposition = "right",
  showstatistics = FALSE,
  testtype = "auto",
  posthoc = FALSE,
  padjust = "none",
  decimals = 1,
  highlightmax = FALSE,
  highlightmin = FALSE,
  highlightgroups = "",
  gridlines = TRUE,
  aspectratio = "auto",
  width = 10,
  height = 8,
  showTable = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- items:

  The measurements, variables, or items to compare across groups. Can be
  categorical (for frequencies) or continuous (for summary statistics).

- groups:

  Variable containing group categories (e.g., disease groups, hospitals,
  age groups, treatment arms, regions, etc.).

- values:

  Numeric variable containing measurement values. If NULL, frequencies
  or summary statistics will be calculated from the data.

- facetby:

  Optional variable to create separate panels (e.g., by gender, time
  point, location).

- statistic:

  Which summary statistic to display for continuous variables.

- plottype:

  Choose the bar chart layout style.

- sortorder:

  How to sort the items in the chart.

- grouporder:

  How to order the groups in the legend and bars.

- customgrouporder:

  Comma-separated list of group names in desired order (e.g.,
  "Control,Mild,Moderate,Severe").

- showvalues:

  Display numeric values on or next to bars.

- valueposition:

  Position of value labels relative to bars.

- showerrorbars:

  Display error bars for continuous variables.

- errortype:

  Type of error bars to display.

- showconnectors:

  Draw lines connecting values across groups to show patterns.

- colorscheme:

  Color palette for groups.

- customcolors:

  Comma-separated hex colors for each group (e.g.,
  "#1f77b4,#ff7f0e,#2ca02c").

- referenceline:

  Add a horizontal reference line at this value (e.g., normal range
  limit, cutoff).

- referencelinelabel:

  Label for the reference line.

- referenceband:

  Add a reference band using format "min,max" (e.g., "3.5,5.5" for
  normal range).

- title:

  Main title for the chart.

- subtitle:

  Subtitle or description for the chart.

- xlabel:

  Label for the x-axis.

- ylabel:

  Label for the y-axis.

- showlegend:

  Display legend for groups.

- legendtitle:

  Title for the legend.

- legendposition:

  Position of the legend.

- showstatistics:

  Perform and display statistical tests comparing groups.

- testtype:

  Statistical test for comparing groups.

- posthoc:

  Perform pairwise post-hoc comparisons when significant.

- padjust:

  Method for adjusting p-values in multiple comparisons.

- decimals:

  Number of decimal places for displayed values.

- highlightmax:

  Emphasize the highest value for each item.

- highlightmin:

  Emphasize the lowest value for each item.

- highlightgroups:

  Comma-separated list of group names to highlight.

- gridlines:

  Display background grid lines.

- aspectratio:

  Aspect ratio of the plot.

- width:

  Width of the plot in inches.

- height:

  Height of the plot in inches.

- showTable:

  Display a summary table below the plot.

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$instructions` |     |     |     |     | a html   |
| `results$plot`         |     |     |     |     | an image |
| `results$plotnotes`    |     |     |     |     | a html   |
| `results$descriptives` |     |     |     |     | a table  |
| `results$statistics`   |     |     |     |     | a table  |
| `results$posthoctests` |     |     |     |     | a table  |
| `results$plotcode`     |     |     |     |     | a html   |
| `results$dataexport`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptives$asDF`

`as.data.frame(results$descriptives)`
