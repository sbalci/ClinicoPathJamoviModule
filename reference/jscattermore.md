# High-Performance Scatter Plots

High-performance scatter plots using the scattermore package for
plotting millions of points efficiently with advanced customization
options.

## Usage

``` r
jscattermore(
  data,
  x_var,
  y_var,
  color_var = NULL,
  size_var = NULL,
  plot_type = "ggplot2",
  point_size = 0.5,
  alpha = 0.8,
  pixels = 512,
  pointsize = 0,
  interpolate = TRUE,
  color_palette = "viridis",
  show_smooth = FALSE,
  smooth_method = "loess",
  show_density = FALSE,
  facet_var = NULL,
  log_transform_x = FALSE,
  log_transform_y = FALSE,
  x_label = "",
  y_label = "",
  plot_title = "",
  show_correlation = TRUE,
  show_performance = FALSE,
  theme_style = "default"
)
```

## Arguments

- data:

  The data as a data frame.

- x_var:

  Variable for the X-axis.

- y_var:

  Variable for the Y-axis.

- color_var:

  Variable for color mapping.

- size_var:

  Variable for size mapping.

- plot_type:

  Type of scatter plot to generate.

- point_size:

  Size of the points in the scatter plot.

- alpha:

  Transparency level of points (0 = fully transparent, 1 = opaque).

- pixels:

  Resolution of the raster graphics in pixels.

- pointsize:

  Point size for raster graphics (0 = automatic).

- interpolate:

  Whether to interpolate the raster graphics.

- color_palette:

  Color palette for continuous color mapping.

- show_smooth:

  Whether to add a smooth regression line.

- smooth_method:

  Method for smooth line fitting.

- show_density:

  Whether to overlay density contours.

- facet_var:

  Variable for creating multiple panels.

- log_transform_x:

  Whether to apply log transformation to X variable.

- log_transform_y:

  Whether to apply log transformation to Y variable.

- x_label:

  Custom label for X-axis.

- y_label:

  Custom label for Y-axis.

- plot_title:

  Title for the plot.

- show_correlation:

  Whether to display correlation coefficient.

- show_performance:

  Whether to display rendering performance information.

- theme_style:

  Visual theme for the plot.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$plot` |  |  |  |  | Fast scatter plot using scattermore package |
| `results$summary` |  |  |  |  | Summary statistics and performance information |
| `results$performance` |  |  |  |  | Detailed performance metrics and comparisons |
