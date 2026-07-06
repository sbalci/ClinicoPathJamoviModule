# Segmented Total Bar Charts

Create segmented total bar charts (100 percent stacked bars) that show
proportional breakdowns within categories. Perfect for displaying
composition data where each bar represents 100 percent and segments show
relative proportions.

## Usage

``` r
jjsegmentedtotalbar(
  data,
  analysis_preset = "custom",
  x_var,
  y_var,
  fill_var,
  facet_var = NULL,
  show_plot = FALSE,
  plot_type = "stacked",
  chart_style = "clinical",
  color_palette = "clinical",
  show_percentages = FALSE,
  percentage_format = "integer",
  show_counts = FALSE,
  label_threshold = 5,
  orientation = "vertical",
  sort_categories = "none",
  plot_title = "",
  x_title = "",
  y_title = "Percentage",
  legend_title = "",
  legend_position = "right",
  bar_width = 0.8,
  plot_width = 10,
  plot_height = 6,
  add_outline = FALSE,
  outline_color = "white",
  export_ready = FALSE,
  flerlage_show_labels = FALSE,
  flerlage_label_size = 4,
  flerlage_label_color = "black",
  flerlage_alpha = 0.3,
  flerlage_box_color = "lightgrey",
  show_statistical_tests = FALSE,
  confidence_level = 0.95,
  exclude_missing = TRUE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- analysis_preset:

  Predefined clinical analysis configurations.

- x_var:

  Categorical variable for x-axis categories.

- y_var:

  Numeric variable for segment values.

- fill_var:

  Categorical variable for bar segment colors and composition.

- facet_var:

  Optional variable for creating multiple panels.

- show_plot:

  Show segmented total bar chart.

- plot_type:

  Type of visualization - traditional 100 percent stacked bars or
  Flerlage-style segmented total bars.

- chart_style:

  Overall visual style for the chart.

- color_palette:

  Color palette for segment fills.

- show_percentages:

  Whether to display percentage labels on segments.

- percentage_format:

  Format for percentage display.

- show_counts:

  Whether to display raw count values on segments.

- label_threshold:

  Minimum percentage for showing labels (0-50 percent).

- orientation:

  Orientation of the bars.

- sort_categories:

  How to sort the categories.

- plot_title:

  Main title for the plot.

- x_title:

  Title for x-axis.

- y_title:

  Title for y-axis.

- legend_title:

  Title for the legend.

- legend_position:

  Position of the legend.

- bar_width:

  Width of the bars (0.1 to 1.0).

- plot_width:

  Width of the plot in inches.

- plot_height:

  Height of the plot in inches.

- add_outline:

  Whether to add white outlines around segments.

- outline_color:

  Color for segment outlines.

- export_ready:

  Whether to optimize plot for high-quality export.

- flerlage_show_labels:

  Show value labels on Flerlage-style plots.

- flerlage_label_size:

  Size of value labels in Flerlage plots.

- flerlage_label_color:

  Color of value labels in Flerlage plots.

- flerlage_alpha:

  Transparency of background boxes (0=transparent, 1=opaque).

- flerlage_box_color:

  Color of background boxes in Flerlage plots.

- show_statistical_tests:

  Whether to perform chi-square tests for proportion differences.

- confidence_level:

  Confidence level for statistical tests (0.80-0.99).

- exclude_missing:

  Whether to exclude rows with missing values in key variables.

- showExplanations:

  Show explanations of the statistical results

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$instructions`      |     |     |     |     | a html   |
| `results$plot`              |     |     |     |     | an image |
| `results$summary`           |     |     |     |     | a table  |
| `results$composition_table` |     |     |     |     | a table  |
| `results$detailed_stats`    |     |     |     |     | a table  |
| `results$interpretation`    |     |     |     |     | a html   |
| `results$clinical_summary`  |     |     |     |     | a html   |
| `results$statistical_tests` |     |     |     |     | a table  |
| `results$preset_guidance`   |     |     |     |     | a html   |
| `results$presetInfo`        |     |     |     |     | a html   |
| `results$warnings`          |     |     |     |     | a html   |
| `results$explanations`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
