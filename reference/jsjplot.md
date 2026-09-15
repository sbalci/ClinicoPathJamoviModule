# Social Science Statistical Visualization

Social science statistical visualization using the sjPlot package for
regression tables, model plots, and interaction visualizations.

## Usage

``` r
jsjplot(
  data,
  analysis_type = "coefficient_plot",
  dependent_var = NULL,
  independent_vars = NULL,
  grouping_var = NULL,
  interaction_vars = NULL,
  model_type = "lm",
  family = "gaussian",
  plot_type = "est",
  confidence_level = 0.95,
  standardized = FALSE,
  show_values = TRUE,
  show_p_values = TRUE,
  sort_estimates = FALSE,
  remove_intercept = TRUE,
  grid_breaks = 0.5,
  dot_size = 3,
  line_size = 0.8,
  colors = "default",
  theme_style = "sjplot",
  title = "",
  axis_labels = "",
  transform_axis = "none",
  show_data = FALSE,
  show_statistics = TRUE,
  show_summary = TRUE,
  html_output = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- analysis_type:

  Type of statistical visualization to generate.

- dependent_var:

  Dependent/outcome variable for analysis.

- independent_vars:

  Independent/predictor variables for analysis.

- grouping_var:

  Variable for grouping or stratification.

- interaction_vars:

  Variables for interaction analysis.

- model_type:

  Type of statistical model to fit.

- family:

  Error distribution family for GLM.

- plot_type:

  Type of model plot to generate.

- confidence_level:

  Confidence level for intervals.

- standardized:

  Whether to show standardized coefficients.

- show_values:

  Whether to display coefficient values on plot.

- show_p_values:

  Whether to display p-values.

- sort_estimates:

  Whether to sort coefficients by effect size.

- remove_intercept:

  Whether to exclude intercept from plots.

- grid_breaks:

  Spacing for grid lines in plots.

- dot_size:

  Size of points in coefficient plots.

- line_size:

  Thickness of confidence interval lines.

- colors:

  Color scheme for plots.

- theme_style:

  Visual theme for plots.

- title:

  Custom title for the plot.

- axis_labels:

  Custom labels for axes (comma-separated).

- transform_axis:

  Transformation to apply to axis scale.

- show_data:

  Whether to overlay raw data points.

- show_statistics:

  Whether to display model fit statistics.

- show_summary:

  Whether to display detailed model summary.

- html_output:

  Whether to generate HTML formatted output.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$plot` |  |  |  |  | Statistical plots using sjPlot package |
| `results$model_table` |  |  |  |  | Regression table or statistical summary table |
| `results$statistics` |  |  |  |  | Model fit statistics and diagnostics |
| `results$summary` |  |  |  |  | Comprehensive analysis summary and interpretation |
