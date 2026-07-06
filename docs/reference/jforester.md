# Forest Plot Visualization

Create publication-ready forest plots for meta-analyses, subgroup
analyses, and clinical trial results. Display point estimates with
confidence intervals in a professional format suitable for academic
publications.

## Usage

``` r
jforester(
  data,
  study_labels,
  estimates,
  ci_lower,
  ci_upper,
  sample_sizes = NULL,
  events = NULL,
  effect_type = "or",
  confidence_level = "95",
  reference_line = 1,
  log_scale = TRUE,
  plot_title = "",
  x_axis_label = "",
  show_summary = FALSE,
  summary_estimate = 1,
  summary_ci_lower = 0.8,
  summary_ci_upper = 1.2,
  point_size_range = "medium",
  color_scheme = "default",
  custom_point_color = "#2166AC",
  custom_ci_color = "#4D4D4D",
  font_family = "Arial",
  plot_width = 10,
  plot_height = 8,
  dpi = 600,
  show_table = TRUE,
  table_position = "left",
  include_weights = FALSE,
  show_heterogeneity = FALSE,
  arrow_labels = FALSE,
  left_arrow_label = "Favors Control",
  right_arrow_label = "Favors Treatment",
  stripe_rows = TRUE,
  export_format = "png"
)
```

## Arguments

- data:

  Dataset for forest plot analysis

- study_labels:

  Variable containing study or subgroup names

- estimates:

  Point estimates (odds ratios, risk ratios, mean differences, etc.)

- ci_lower:

  Lower bounds of confidence intervals

- ci_upper:

  Upper bounds of confidence intervals

- sample_sizes:

  Sample sizes for each study/group (optional)

- events:

  Number of events or cases (optional)

- effect_type:

  Type of effect measure being displayed

- confidence_level:

  Confidence level for intervals

- reference_line:

  Position of vertical reference line

- log_scale:

  Display forest plot on logarithmic scale

- plot_title:

  Custom title for forest plot

- x_axis_label:

  Custom x-axis label

- show_summary:

  Display overall summary effect at bottom

- summary_estimate:

  Overall summary effect estimate

- summary_ci_lower:

  Lower bound of summary confidence interval

- summary_ci_upper:

  Upper bound of summary confidence interval

- point_size_range:

  Range of point sizes based on sample size

- color_scheme:

  Color scheme for points and confidence intervals

- custom_point_color:

  Custom color for points (hex code)

- custom_ci_color:

  Custom color for confidence intervals (hex code)

- font_family:

  Font family for plot text

- plot_width:

  Width of exported plot

- plot_height:

  Height of exported plot

- dpi:

  Resolution for exported plot

- show_table:

  Display accompanying data table

- table_position:

  Position of data table relative to plot

- include_weights:

  Display study weights in table (requires sample sizes)

- show_heterogeneity:

  Display I² and Q-test statistics

- arrow_labels:

  Add directional arrows with labels

- left_arrow_label:

  Label for left arrow

- right_arrow_label:

  Label for right arrow

- stripe_rows:

  Add alternating row colors for readability

- export_format:

  Format for plot export

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$forest_plot`        |     |     |     |     | an image |
| `results$data_table`         |     |     |     |     | a table  |
| `results$summary_statistics` |     |     |     |     | a table  |
| `results$interpretation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$data_table$asDF`

`as.data.frame(results$data_table)`
