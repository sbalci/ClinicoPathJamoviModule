# BBC-Style Data Visualization

Creates professional BBC News-style data visualizations using the bbplot
package design principles.

## Usage

``` r
bbcplots(
  data,
  y_var,
  x_var,
  group_var,
  facet_var,
  chart_type = "column",
  bbc_colors = "bbc_blue",
  custom_colors = "#1380A1, #FAAB18, #007f7f, #333333",
  title_text = "",
  subtitle_text = "",
  source_text = "Source: Data analysis",
  x_axis_title = "",
  y_axis_title = "",
  show_values = FALSE,
  value_position = "above",
  horizontal_gridlines = TRUE,
  vertical_gridlines = FALSE,
  legend_position = "top",
  chart_width = 640,
  chart_height = 450,
  left_align_title = TRUE,
  font_family = "Helvetica",
  add_branding = FALSE,
  export_finalized = FALSE,
  statistical_annotations = FALSE,
  stat_method = "auto",
  confidence_level = 0.95,
  show_sample_sizes = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- y_var:

  The dependent variable for the main axis.

- x_var:

  The independent variable for categories or groups.

- group_var:

  Optional variable for creating grouped or series comparisons.

- facet_var:

  Optional variable for creating faceted subplots.

- chart_type:

  The type of chart to create using BBC styling.

- bbc_colors:

  BBC-approved color scheme for data visualization.

- custom_colors:

  Custom color palette as comma-separated hex codes.

- title_text:

  Main title for the chart in BBC style (28pt Helvetica bold).

- subtitle_text:

  Subtitle for additional context (22pt Helvetica).

- source_text:

  Data source attribution for the chart footer.

- x_axis_title:

  Custom title for the X-axis (leave blank for variable name).

- y_axis_title:

  Custom title for the Y-axis (leave blank for variable name).

- show_values:

  Whether to display data values on the chart.

- value_position:

  Position of data value labels on chart elements.

- horizontal_gridlines:

  Whether to show horizontal gridlines (BBC standard).

- vertical_gridlines:

  Whether to show vertical gridlines (not BBC standard).

- legend_position:

  Position of the chart legend following BBC guidelines.

- chart_width:

  Width of the exported chart in pixels (BBC standard: 640).

- chart_height:

  Height of the exported chart in pixels (BBC standard: 450).

- left_align_title:

  Whether to left-align the title (BBC standard).

- font_family:

  Font family for text elements (Helvetica is BBC standard).

- add_branding:

  Whether to add BBC-style branding elements to the chart.

- export_finalized:

  Whether to export a finalized version with BBC formatting.

- statistical_annotations:

  Whether to include statistical test results on the chart.

- stat_method:

  Statistical test to perform and display on the chart.

- confidence_level:

  Confidence level for statistical intervals and tests.

- show_sample_sizes:

  Whether to display sample sizes for each group.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`        |     |     |     |     | a html   |
| `results$main_plot`           |     |     |     |     | an image |
| `results$finalized_plot`      |     |     |     |     | an image |
| `results$statistical_results` |     |     |     |     | a html   |
| `results$chart_summary`       |     |     |     |     | a html   |
| `results$color_guide`         |     |     |     |     | a html   |
| `results$design_notes`        |     |     |     |     | a html   |
| `results$export_code`         |     |     |     |     | a html   |
| `results$accessibility_info`  |     |     |     |     | a html   |

## Details

This module implements the BBC Visual and Data Journalism team's design
standards for creating publication-ready graphics with clean typography,
consistent color schemes, and professional branding elements. Perfect
for creating news-quality visualizations for reports, presentations, and
publications.

Features BBC's signature minimalist aesthetic with Helvetica typography,
strategic use of gridlines, and standardized chart dimensions optimized
for digital publication.
