# GraphPad Prism Style Plots

Creates publication-ready plots with GraphPad Prism styling using
ggprism package. This module provides Prism-style themes, color
palettes, and statistical annotations to create professional scientific
visualizations. Perfect for clinical research publications,
biostatistics, and academic presentations. Supports various plot types
including violin plots, box plots, scatter plots, and dose-response
curves with seamless ggplot2 integration and Prism-style aesthetics.

## Usage

``` r
ggprism(
  data,
  x_var,
  y_var,
  group_var = NULL,
  facet_var = NULL,
  plot_type = "violin",
  prism_theme = "default",
  prism_palette = "floral",
  show_points = TRUE,
  point_size = 1.5,
  point_alpha = 0.6,
  show_statistics = TRUE,
  stats_method = "auto",
  pvalue_format = "exact",
  plot_title = "GraphPad Prism Style Plot",
  x_label = "",
  y_label = "",
  legend_position = "right",
  base_size = 12,
  show_summary = TRUE,
  error_bars = "se",
  prism_guides = "standard",
  annotation_ticks = FALSE,
  preview_mode = FALSE,
  prism_shape_palette = "default",
  jitter_width = 0.2,
  violin_width = 1,
  publication_ready = FALSE,
  export_dpi = 300,
  custom_comparisons = ""
)
```

## Arguments

- data:

  The data as a data frame.

- x_var:

  Variable for the X-axis. Can be continuous or categorical.

- y_var:

  Continuous variable for the Y-axis.

- group_var:

  Optional categorical variable for grouping and statistical
  comparisons.

- facet_var:

  Optional variable for creating separate panels in a grid layout.

- plot_type:

  Type of plot to create with Prism styling.

- prism_theme:

  GraphPad Prism theme variant to apply.

- prism_palette:

  Prism-style color palette for different groups. Choose from 20+
  authentic GraphPad Prism palettes.

- show_points:

  If TRUE, overlays individual data points on the plot.

- point_size:

  Size of individual data points.

- point_alpha:

  Transparency level for data points (0 = transparent, 1 = opaque).

- show_statistics:

  If TRUE, performs statistical tests and adds p-value annotations.

- stats_method:

  Statistical test method for group comparisons.

- pvalue_format:

  Format for displaying p-values in statistical annotations.

- plot_title:

  Custom title for the plot.

- x_label:

  Custom label for X-axis. If empty, uses variable name.

- y_label:

  Custom label for Y-axis. If empty, uses variable name.

- legend_position:

  Position of the legend in the plot.

- base_size:

  Base font size for all text elements in the plot.

- show_summary:

  If TRUE, displays summary statistics table.

- error_bars:

  Type of error bars to display (for applicable plot types).

- prism_guides:

  Axis guide style following GraphPad Prism conventions.

- annotation_ticks:

  Add tick marks as annotations in Prism style.

- preview_mode:

  Enable palette preview mode to see all colors in the selected palette.

- prism_shape_palette:

  Shape palette for point plots following Prism conventions.

- jitter_width:

  Width of horizontal jittering for overlaid points.

- violin_width:

  Scale factor for violin plot widths.

- publication_ready:

  Optimize plot for publication with enhanced styling and formatting.

- export_dpi:

  DPI for high-resolution export (publication standard: 300).

- custom_comparisons:

  Custom pairwise comparisons (e.g., "Group1-Group2,Group1-Group3").

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`         |     |     |     |     | a html   |
| `results$clinical_summary`     |     |     |     |     | a html   |
| `results$assumptions_check`    |     |     |     |     | a html   |
| `results$interpretation_guide` |     |     |     |     | a html   |
| `results$report_sentences`     |     |     |     |     | a html   |
| `results$statistical_glossary` |     |     |     |     | a html   |
| `results$main_plot`            |     |     |     |     | an image |
| `results$palette_preview`      |     |     |     |     | an image |
| `results$publication_plot`     |     |     |     |     | an image |
| `results$summary_statistics`   |     |     |     |     | a html   |
| `results$statistical_tests`    |     |     |     |     | a html   |
| `results$prism_guide`          |     |     |     |     | a html   |
| `results$palette_information`  |     |     |     |     | a html   |
| `results$export_code`          |     |     |     |     | a html   |
| `results$accessibility_notes`  |     |     |     |     | a html   |
| `results$clinical_presets`     |     |     |     |     | a html   |
