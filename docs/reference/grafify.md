# Scientific Visualization with Grafify

Scientific data visualization using the grafify R package for easy and
accessible plotting. Provides quick exploration graphs with few lines of
code, color-blind friendly palettes, and integrated statistical
analysis. Perfect for clinical research, experimental data, and
scientific publications. Features scatter plots with error bars,
distribution plots, before-after comparisons, and factorial designs with
professional styling.

## Usage

``` r
grafify(
  data,
  vars,
  groups = NULL,
  blocks = NULL,
  facet_var = NULL,
  plot_type = "scatterbar",
  x_var,
  y_var,
  error_type = "sd",
  summary_function = "mean",
  color_palette = "default",
  reverse_palette = FALSE,
  use_grafify_theme = TRUE,
  jitter_width = 0.3,
  transparency = 0.7,
  point_size = 3,
  line_size = 1,
  log_transform = "none",
  add_statistics = FALSE,
  stat_method = "anova_1way",
  posthoc_comparisons = FALSE,
  comparison_method = "pairwise",
  befafter_shape_var = NULL,
  befafter_id_var = NULL,
  show_individual_points = TRUE,
  show_summary_stats = TRUE,
  show_model_diagnostics = FALSE,
  export_data = FALSE,
  plot_width = 8,
  plot_height = 6,
  title_text = "",
  x_label = "",
  y_label = "",
  legend_position = "right",
  experimental_design = "crd",
  random_effects = NULL,
  alpha_level = 0.05
)
```

## Arguments

- data:

  The data as a data frame containing scientific or clinical variables
  for professional visualization with grafify.

- vars:

  Continuous variables for plotting. Can be used as X, Y variables or
  multiple variables for distribution analysis.

- groups:

  Categorical variable for grouping data in plots. Creates different
  colors, shapes, or panels for each group.

- blocks:

  Variable for blocking or pairing observations. Used in randomized
  block designs or before-after comparisons.

- facet_var:

  Variable for creating multiple panels (facets). Useful for complex
  experimental designs with multiple factors.

- plot_type:

  Type of plot to create. Each type is optimized for different data
  structures and research questions.

- x_var:

  Variable for X-axis. Can be continuous or categorical depending on the
  plot type selected.

- y_var:

  Variable for Y-axis. Should be continuous for most plot types.

- error_type:

  Type of error bars to display. SD shows data spread, SEM shows
  precision of mean, CI95 shows statistical confidence.

- summary_function:

  Function used to summarize data for central tendency. Geometric mean
  is useful for log-scaled or ratio data.

- color_palette:

  Color palette for the plot. All palettes are color-blind friendly and
  designed for scientific publications.

- reverse_palette:

  Reverse the order of colors in the selected palette. Useful for
  adjusting color assignments to groups.

- use_grafify_theme:

  Apply the grafify theme for classic-style scientific plots. Provides
  clean, publication-ready styling.

- jitter_width:

  Amount of horizontal jittering for data points. Helps avoid
  overlapping points in categorical plots.

- transparency:

  Transparency level for data points (alpha value). Lower values make
  points more transparent.

- point_size:

  Size of data points in the plot.

- line_size:

  Thickness of lines (error bars, connecting lines).

- log_transform:

  Apply logarithmic transformation to axes. Useful for data spanning
  multiple orders of magnitude.

- add_statistics:

  Perform and display statistical analysis on the plot. Integrates with
  grafify's built-in statistical functions.

- stat_method:

  Statistical method to apply. Results will be displayed as text
  annotations or in separate output sections.

- posthoc_comparisons:

  Perform post-hoc pairwise comparisons after ANOVA. Uses emmeans for
  estimated marginal means and contrasts.

- comparison_method:

  Method for post-hoc comparisons. Pairwise compares all groups, vs
  reference compares to control group, trends test for patterns.

- befafter_shape_var:

  Variable defining before/after or timepoint for paired data. Used in
  before-after plot types.

- befafter_id_var:

  Variable identifying individual subjects or experimental units. Used
  to connect paired observations in before-after plots.

- show_individual_points:

  Display individual data points on the plot. Recommended for
  transparency and data exploration.

- show_summary_stats:

  Display summary statistics table with means, SD, SEM, and sample
  sizes.

- show_model_diagnostics:

  Display model diagnostic plots (Q-Q plots, residuals) when statistical
  analysis is performed.

- export_data:

  Export the processed data used for plotting. Includes summary
  statistics and model results.

- plot_width:

  Width of the plot in inches for optimal display and export.

- plot_height:

  Height of the plot in inches for optimal display and export.

- title_text:

  Custom title for the plot. Leave empty for automatic title based on
  variables and analysis type.

- x_label:

  Custom label for X-axis. Leave empty to use variable name.

- y_label:

  Custom label for Y-axis. Leave empty to use variable name.

- legend_position:

  Position of the plot legend.

- experimental_design:

  Type of experimental design for appropriate statistical analysis.
  Affects model specification and interpretation.

- random_effects:

  Variables to include as random effects in mixed models. Typically
  subjects, blocks, or experimental units.

- alpha_level:

  Significance level for statistical tests and confidence intervals.

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                 |     |     |     |     | a html   |
| `results$main_plot`            |     |     |     |     | an image |
| `results$summary_stats`        |     |     |     |     | a html   |
| `results$statistical_analysis` |     |     |     |     | a html   |
| `results$posthoc_results`      |     |     |     |     | a html   |
| `results$diagnostic_plots`     |     |     |     |     | an image |
| `results$qqplot`               |     |     |     |     | an image |
| `results$palette_preview`      |     |     |     |     | an image |
| `results$plot_interpretation`  |     |     |     |     | a html   |
| `results$export_info`          |     |     |     |     | a html   |
