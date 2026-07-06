# Hull Plot

Creates Hull plots to visualize clusters and groups in scatter plots
using ggforce. Hull plots draw polygonal boundaries around data points
grouped by categorical variables, making it easy to identify customer
segments, group membership, and data clusters. Based on the
geom_mark_hull() function from ggforce package.

## Usage

``` r
hullplot(
  data,
  x_var = NULL,
  y_var = NULL,
  group_var = NULL,
  color_var = NULL,
  size_var = NULL,
  hull_concavity = 2,
  hull_alpha = 0.3,
  show_labels = TRUE,
  point_size = 2,
  point_alpha = 0.7,
  color_palette = "default",
  plot_theme = "minimal",
  plot_title = "Hull Plot - Group Visualization",
  x_label = "",
  y_label = "",
  hull_expand = 0.05,
  show_statistics = FALSE,
  outlier_detection = FALSE,
  confidence_ellipses = FALSE,
  show_summary = FALSE,
  show_assumptions = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- x_var:

  Continuous variable for the X-axis of the scatter plot.

- y_var:

  Continuous variable for the Y-axis of the scatter plot.

- group_var:

  Categorical variable that defines the groups for hull boundaries. Each
  group will have its own hull polygon.

- color_var:

  Optional variable for coloring points. If not specified, uses the
  grouping variable.

- size_var:

  Optional continuous variable for sizing points based on values.

- hull_concavity:

  Controls the concavity of hull polygons. Lower values create more
  concave hulls, higher values create more convex hulls. Range: 0-2,
  default: 2.

- hull_alpha:

  Transparency level for hull polygons. 0 = completely transparent, 1 =
  opaque.

- show_labels:

  If TRUE, displays group labels inside hull regions.

- point_size:

  Size of scatter plot points.

- point_alpha:

  Transparency level for scatter plot points.

- color_palette:

  Color palette for hulls and points.

- plot_theme:

  Overall visual theme for the plot.

- plot_title:

  Custom title for the hull plot.

- x_label:

  Custom label for X-axis. If empty, uses variable name.

- y_label:

  Custom label for Y-axis. If empty, uses variable name.

- hull_expand:

  Amount to expand hull boundaries beyond data points. Higher values
  create larger hulls.

- show_statistics:

  If TRUE, displays summary statistics for each group in the output.

- outlier_detection:

  If TRUE, identifies and highlights potential outliers within groups.

- confidence_ellipses:

  If TRUE, adds confidence ellipses in addition to hull polygons.

- show_summary:

  If TRUE, displays a plain-language summary of the results with
  copy-ready text.

- show_assumptions:

  If TRUE, displays data requirements, assumptions, and usage
  guidelines.

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$plot`           |     |     |     |     | an image |
| `results$statistics`     |     |     |     |     | a html   |
| `results$outliers`       |     |     |     |     | a html   |
| `results$interpretation` |     |     |     |     | a html   |
| `results$summary`        |     |     |     |     | a html   |
| `results$assumptions`    |     |     |     |     | a html   |
