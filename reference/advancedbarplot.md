# Advanced Bar Charts

Advanced bar chart visualization module implementing 5 different
approaches for creating professional bar charts. Choose from ggplot2
basics, polished presentations, statistical annotations, interactive
plots, and publication-ready designs. Each approach optimized for
different use cases in clinical research.

## Usage

``` r
advancedbarplot(
  data,
  x_var,
  y_var,
  fill_var = NULL,
  facet_var = NULL,
  chart_approach = "polished",
  bar_position = "dodge",
  stat_type = "mean",
  error_bars = "se",
  color_palette = "clinical",
  show_values = TRUE,
  value_format = "auto",
  add_statistics = FALSE,
  stat_method = "anova",
  orientation = "vertical",
  plot_title = "",
  x_title = "",
  y_title = "",
  legend_position = "right",
  theme_style = "clean",
  bar_width = 0.8,
  plot_width = 10,
  plot_height = 6,
  sort_bars = "none",
  add_trend_line = FALSE,
  highlight_bars = "",
  transparency = 0.9,
  diverging_center = 0,
  pattern_type = "none",
  radial_start_angle = 0,
  label_inside = FALSE,
  subtitle_text = "",
  source_text = "",
  export_options = TRUE,
  show_comparison = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- x_var:

  Categorical variable for x-axis categories.

- y_var:

  Numeric variable for bar heights.

- fill_var:

  Optional variable for bar fill colors (grouped/stacked bars).

- facet_var:

  Optional variable for creating multiple panels.

- chart_approach:

  Choose the bar chart approach and styling.

- bar_position:

  Position adjustment for grouped bars.

- stat_type:

  Type of statistical summary for y-axis values.

- error_bars:

  Type of error bars to display.

- color_palette:

  Color palette for bar fills including GraphPad Prism palettes.

- show_values:

  Whether to display values on top of bars.

- value_format:

  Format for displayed values.

- add_statistics:

  Whether to perform and display statistical tests.

- stat_method:

  Type of statistical test to perform.

- orientation:

  Orientation of the bars.

- plot_title:

  Main title for the plot.

- x_title:

  Title for x-axis.

- y_title:

  Title for y-axis.

- legend_position:

  Position of the legend.

- theme_style:

  Overall theme style for the plot including GraphPad Prism themes.

- bar_width:

  Width of the bars (0.1 to 1.0).

- plot_width:

  Width of the plot in inches.

- plot_height:

  Height of the plot in inches.

- sort_bars:

  How to sort the bars.

- add_trend_line:

  Whether to add a trend line for numeric x-axis.

- highlight_bars:

  Comma-separated list of categories to highlight.

- transparency:

  Transparency level for bars (alpha value).

- diverging_center:

  Center value for diverging bar plots.

- pattern_type:

  Pattern type for textured bar plots.

- radial_start_angle:

  Starting angle for circular bar plots.

- label_inside:

  Whether to place value labels inside bars.

- subtitle_text:

  Subtitle text for the plot.

- source_text:

  Source text for data attribution.

- export_options:

  Whether to optimize plot for high-quality export.

- show_comparison:

  Whether to show comparison grid of all 5 visualization approaches.

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`         |     |     |     |     | a html   |
| `results$approach_description` |     |     |     |     | a html   |
| `results$main_plot`            |     |     |     |     | an image |
| `results$statistical_results`  |     |     |     |     | a html   |
| `results$summary_stats`        |     |     |     |     | a html   |
| `results$interactive_plot`     |     |     |     |     | a html   |
| `results$comparison_grid`      |     |     |     |     | an image |
| `results$code_example`         |     |     |     |     | a html   |
| `results$interpretation_guide` |     |     |     |     | a html   |
