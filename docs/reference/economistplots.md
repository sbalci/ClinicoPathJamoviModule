# Economist-Style Distribution Plots

Creates elegant Economist-style distribution plots using ggeconodist
package for professional publication-quality visualizations. Provides
diminutive distribution charts that effectively communicate statistical
distribution characteristics with The Economist's distinctive visual
style. Perfect for comparing distributions across categories, showing
price variations, or visualizing any statistical distribution data in
clinical research.

## Usage

``` r
economistplots(
  data,
  y_var,
  x_var,
  facet_var,
  color_var,
  plot_orientation = "vertical",
  economist_theme = TRUE,
  show_legend = TRUE,
  percentile_colors = FALSE,
  tenth_color = "#c7254e",
  ninetieth_color = "#18bc9c",
  median_color = "#2c3e50",
  distribution_fill = "#95a5a6",
  alpha_level = 0.7,
  bandwidth_adjust = 1,
  show_points = FALSE,
  point_jitter = 0.1,
  add_statistics = TRUE,
  stat_method = "anova",
  effect_size = TRUE,
  plot_title = "",
  x_title = "",
  y_title = "",
  caption_text = "",
  plot_width = 10,
  plot_height = 6,
  left_align_title = TRUE,
  custom_breaks = "",
  show_quartiles = TRUE,
  distribution_style = "classic",
  comparison_annotations = FALSE,
  outlier_treatment = "all",
  summary_statistics = TRUE,
  export_economist_code = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- y_var:

  Continuous variable whose distribution will be visualized.

- x_var:

  Categorical variable for grouping distributions.

- facet_var:

  Optional variable for creating multiple panels.

- color_var:

  Optional variable for coloring distribution elements.

- plot_orientation:

  Orientation of the distribution plots.

- economist_theme:

  Whether to apply the full Economist visual theme.

- show_legend:

  Whether to display the Economist-style legend.

- percentile_colors:

  Whether to use custom colors for different percentiles.

- tenth_color:

  Color for the 10th percentile in the distribution.

- ninetieth_color:

  Color for the 90th percentile in the distribution.

- median_color:

  Color for the median line in the distribution.

- distribution_fill:

  Fill color for the main distribution area.

- alpha_level:

  Transparency level for distribution elements.

- bandwidth_adjust:

  Adjustment factor for distribution bandwidth smoothing.

- show_points:

  Whether to overlay individual data points.

- point_jitter:

  Amount of horizontal jittering for overlaid points.

- add_statistics:

  Whether to include statistical summaries and tests.

- stat_method:

  Statistical test to perform for group comparisons.

- effect_size:

  Whether to calculate and display effect sizes.

- plot_title:

  Custom title for the plot.

- x_title:

  Custom label for x-axis.

- y_title:

  Custom label for y-axis.

- caption_text:

  Custom caption text for the plot.

- plot_width:

  Width of the plot in inches.

- plot_height:

  Height of the plot in inches.

- left_align_title:

  Whether to left-align title and caption elements.

- custom_breaks:

  Comma-separated values for custom y-axis breaks.

- show_quartiles:

  Whether to highlight quartile boundaries.

- distribution_style:

  Overall visual style for the distribution plots.

- comparison_annotations:

  Whether to add pairwise comparison annotations.

- outlier_treatment:

  How to handle outliers in the visualization.

- summary_statistics:

  Whether to display comprehensive summary statistics.

- export_economist_code:

  Whether to generate reproducible R code using ggeconodist.

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`             |     |     |     |     | a html   |
| `results$main_plot`                |     |     |     |     | an image |
| `results$statistical_results`      |     |     |     |     | a html   |
| `results$summary_statistics`       |     |     |     |     | a html   |
| `results$economist_legend_info`    |     |     |     |     | a html   |
| `results$comparison_results`       |     |     |     |     | a html   |
| `results$distribution_diagnostics` |     |     |     |     | an image |
| `results$r_code_output`            |     |     |     |     | a html   |
| `results$interpretation_guide`     |     |     |     |     | a html   |
