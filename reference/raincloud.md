# Raincloud Plot

Creates Raincloud plots to visualize data distributions using ggdist
package. Raincloud plots combine three visualization techniques:
half-violin plots showing distribution density, box plots showing
summary statistics, and dot plots showing individual data points. This
provides a comprehensive view of data distribution that reveals patterns
traditional box plots might miss, including multimodality and
distribution shape. Based on the ggdist R-Bloggers tutorial.

## Usage

``` r
raincloud(
  data,
  dep_var,
  group_var,
  facet_var = NULL,
  color_var = NULL,
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = TRUE,
  dots_side = "left",
  violin_width = 0.7,
  box_width = 0.2,
  dots_size = 1.2,
  alpha_violin = 0.7,
  alpha_dots = 0.8,
  orientation = "horizontal",
  color_palette = "clinical",
  plot_theme = "clinical",
  plot_title = "Raincloud Plot - Distribution Visualization",
  x_label = "",
  y_label = "",
  show_statistics = FALSE,
  show_outliers = FALSE,
  outlier_method = "iqr",
  normality_test = FALSE,
  comparison_test = FALSE,
  comparison_method = "auto",
  adjust_method = "none",
  effect_size = FALSE,
  log_transform = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- dep_var:

  Continuous variable whose distribution will be visualized in the
  raincloud plot.

- group_var:

  Categorical variable for grouping. Each group will have its own
  raincloud visualization.

- facet_var:

  Optional variable for creating separate panels. Creates multiple
  raincloud plots in a grid layout.

- color_var:

  Optional variable for coloring different elements. If not specified,
  uses grouping variable.

- show_violin:

  If TRUE, displays half-violin plot showing probability density
  distribution.

- show_boxplot:

  If TRUE, displays box plot with median, quartiles, and outliers.

- show_dots:

  If TRUE, displays individual data points as dots.

- dots_side:

  Position of data point dots relative to the violin plot.

- violin_width:

  Width scaling factor for the violin plot component.

- box_width:

  Width of the box plot component.

- dots_size:

  Size of individual data point dots.

- alpha_violin:

  Transparency level for violin plot (0 = transparent, 1 = opaque).

- alpha_dots:

  Transparency level for data point dots.

- orientation:

  Orientation of the plot. Horizontal creates the classic "raincloud"
  appearance.

- color_palette:

  Color palette for different groups including GraphPad Prism palettes.

- plot_theme:

  Overall visual theme for the plot.

- plot_title:

  Custom title for the raincloud plot.

- x_label:

  Custom label for X-axis. If empty, uses variable name.

- y_label:

  Custom label for Y-axis. If empty, uses variable name.

- show_statistics:

  If TRUE, displays summary statistics table for each group.

- show_outliers:

  If TRUE, identifies and highlights outliers in the visualization.

- outlier_method:

  Method for detecting outliers when highlight outliers is enabled.

- normality_test:

  If TRUE, performs normality tests (Shapiro-Wilk) for each group.

- comparison_test:

  If TRUE, performs statistical tests to compare groups.

- comparison_method:

  Statistical test method for comparing groups.

- adjust_method:

  Adjustment method applied to the reported p-value (useful for multiple
  comparisons).

- effect_size:

  If TRUE and two groups are present, reports Cohen's d effect size.

- log_transform:

  Apply log10 transformation to the Y-axis (requires all positive
  values).

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$todo`           |     |     |     |     | a html   |
| `results$plot`           |     |     |     |     | an image |
| `results$statistics`     |     |     |     |     | a html   |
| `results$outliers`       |     |     |     |     | a html   |
| `results$normality`      |     |     |     |     | a html   |
| `results$comparison`     |     |     |     |     | a html   |
| `results$interpretation` |     |     |     |     | a html   |
