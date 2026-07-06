# Base Graphics Visualization

Base R graphics visualization module providing fast, blazing fast, and
extremely customizable data visualization solutions using pure base R
graphics.

## Usage

``` r
basegraphics(
  data,
  plot_type = "scatter",
  x_var = NULL,
  y_var = NULL,
  group_var = NULL,
  main_title = "",
  x_label = "",
  y_label = "",
  point_type = "16",
  point_size = 1,
  color_scheme = "default",
  add_grid = TRUE,
  add_legend = TRUE,
  bins = 15,
  show_statistics = FALSE,
  correlation_method = "pearson",
  show_summary = FALSE,
  custom_limits = FALSE,
  x_min = "",
  x_max = "",
  y_min = "",
  y_max = "",
  line_type = "1",
  line_width = 1,
  bar_width = 1,
  grid_style = "default",
  show_regression_line = FALSE,
  regression_line_color = "red",
  show_confidence_interval = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- plot_type:

  Type of base R plot to generate.

- x_var:

  Variable for x-axis.

- y_var:

  Variable for y-axis (continuous plots).

- group_var:

  Optional grouping variable for stratified plots.

- main_title:

  Main title for the plot.

- x_label:

  Label for x-axis.

- y_label:

  Label for y-axis.

- point_type:

  Point type for scatter plots (pch parameter).

- point_size:

  Size of points in plots (cex parameter).

- color_scheme:

  Color scheme for grouped plots.

- add_grid:

  Whether to add grid lines to the plot.

- add_legend:

  Whether to add legend for grouped plots.

- bins:

  Number of bins for histogram plots.

- show_statistics:

  Whether to display basic statistics on the plot.

- correlation_method:

  Method for calculating correlation coefficient.

- show_summary:

  Display interpretation in non-technical language.

- custom_limits:

  Whether to use custom axis limits.

- x_min:

  Minimum value for x-axis (when custom limits enabled).

- x_max:

  Maximum value for x-axis (when custom limits enabled).

- y_min:

  Minimum value for y-axis (when custom limits enabled).

- y_max:

  Maximum value for y-axis (when custom limits enabled).

- line_type:

  Line type for line plots (lty parameter).

- line_width:

  Width of lines in line plots (lwd parameter).

- bar_width:

  Width of bars in bar plots.

- grid_style:

  Style of grid lines.

- show_regression_line:

  Whether to add a regression line to scatter plots.

- regression_line_color:

  Color of the regression line.

- show_confidence_interval:

  Whether to display confidence interval for regression line.

## Value

A results object containing:

|                            |     |     |     |     |                |
|----------------------------|-----|-----|-----|-----|----------------|
| `results$notices`          |     |     |     |     | a preformatted |
| `results$instructions`     |     |     |     |     | a html         |
| `results$plot_description` |     |     |     |     | a html         |
| `results$glossary`         |     |     |     |     | a html         |
| `results$summary`          |     |     |     |     | a html         |
| `results$base_plot`        |     |     |     |     | an image       |
