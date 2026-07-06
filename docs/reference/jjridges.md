# Ridge Plot

'Create advanced ridgeline plots. Visualize distributions across groups
with multiple style options, annotations, statistical overlays, and
publication-ready formatting. Supports both basic and complex ridge
plots with inside plots, double ridgelines, and color gradients.'

## Usage

``` r
jjridges(
  data,
  x_var,
  y_var,
  fill_var = NULL,
  facet_var = NULL,
  plot_type = "density_ridges",
  scale = 1,
  bandwidth = "nrd0",
  bandwidth_value = 1,
  binwidth = 1,
  add_boxplot = FALSE,
  add_points = FALSE,
  point_alpha = 0.3,
  add_quantiles = FALSE,
  quantiles = "0.25, 0.5, 0.75",
  add_mean = FALSE,
  add_median = FALSE,
  show_stats = FALSE,
  test_type = "parametric",
  p_adjust_method = "none",
  effsize_type = "d",
  alpha = 0.8,
  color_palette = "clinical_colorblind",
  custom_colors = "#3498db,#e74c3c,#2ecc71,#f39c12",
  gradient_low = "#0000FF",
  gradient_high = "#FF0000",
  fill_ridges = TRUE,
  reverse_order = FALSE,
  show_fill_legend = TRUE,
  show_facet_legend = TRUE,
  theme_style = "theme_ridges",
  grid_lines = FALSE,
  expand_panels = TRUE,
  legend_position = "none",
  plot_title = "",
  plot_subtitle = "",
  plot_caption = "",
  x_label = "",
  y_label = "",
  add_sample_size = FALSE,
  add_density_values = FALSE,
  custom_annotations = "",
  width = 800,
  height = 600,
  dpi = 300,
  clinicalPreset = "custom",
  showAboutPanel = FALSE,
  showAssumptions = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- x_var:

  The continuous variable to display as distributions (e.g., biomarker
  values, age, tumor size). Each group will show the distribution
  pattern of this variable.

- y_var:

  The grouping variable for comparison (e.g., disease stage, treatment
  group, pathology grade). Each group creates a separate ridge for
  visual comparison.

- fill_var:

  Optional variable for color/fill mapping within ridges. Creates
  color-coded segments within each ridge.

- facet_var:

  Optional variable for creating separate panels. Useful for comparing
  ridge plots across another dimension.

- plot_type:

  Type of ridge plot. 'density_ridges' for smooth curves,
  'density_ridges_gradient' adds color gradients, 'histogram_ridges' for
  discrete bins, 'violin_ridges' for violin-style plots.

- scale:

  Controls ridge height and overlap. Values \> 1 create overlapping
  ridges, \< 1 creates more separation between ridges.

- bandwidth:

  Bandwidth selection for density estimation. Controls smoothness of
  curves.

- bandwidth_value:

  Custom bandwidth value when bandwidth method is 'custom'.

- binwidth:

  Width of bins for histogram-style ridges.

- add_boxplot:

  Add boxplot elements inside each ridge for enhanced visualization of
  quartiles and outliers.

- add_points:

  Show individual data points below ridges as jittered points.

- point_alpha:

  Transparency of data points when shown.

- add_quantiles:

  Add vertical lines at specified quantiles within each ridge.

- quantiles:

  Comma-separated quantile values to display as vertical lines.

- add_mean:

  Add vertical line at the mean of each distribution.

- add_median:

  Add vertical line at the median of each distribution.

- show_stats:

  Display statistical annotations including p-values and effect sizes.
  WARNING: Assumes independent observations. Not valid for repeated
  measures data.

- test_type:

  Type of statistical test for group comparisons when show_stats is
  TRUE.

- p_adjust_method:

  Method for adjusting p-values in multiple comparisons.

- effsize_type:

  Type of effect size to calculate and display. For skewed data (like
  lymph node counts), consider using nonparametric effect sizes: Cliff's
  Delta shows probability that one group has higher values,
  Hodges-Lehmann shift shows typical difference in units.

- alpha:

  Transparency level for ridge fills.

- color_palette:

  Color palette for ridges. 'Clinical' is optimized for accessibility
  and medical publications. Viridis family are also colorblind-friendly.

- custom_colors:

  Comma-separated list of custom colors when palette is 'custom'.

- gradient_low:

  Low value color for gradient plots.

- gradient_high:

  High value color for gradient plots.

- fill_ridges:

  Whether to fill ridges with color or show only outlines.

- reverse_order:

  Reverse the vertical order of groups.

- show_fill_legend:

  Show legend for fill variable when a fill variable is specified.

- show_facet_legend:

  Show facet labels when faceting is used.

- theme_style:

  Overall plot theme style.

- grid_lines:

  Whether to show grid lines in the plot.

- expand_panels:

  Remove space around plot area for cleaner look.

- legend_position:

  Position of color legend.

- plot_title:

  Main plot title.

- plot_subtitle:

  Plot subtitle for additional context.

- plot_caption:

  Caption for data source or notes.

- x_label:

  Custom label for X axis.

- y_label:

  Custom label for Y axis.

- add_sample_size:

  Display sample size (n) for each group.

- add_density_values:

  Display density values at peaks.

- custom_annotations:

  Custom text annotations in format 'x,y,text;x2,y2,text2'.

- width:

  Width of the plot in pixels.

- height:

  Height of the plot in pixels.

- dpi:

  Resolution for plot export.

- clinicalPreset:

  Clinical analysis preset for ridge plots. Biomarker Distribution:
  nonparametric tests with Cliff's delta. Treatment Response: violin
  plots with Bonferroni correction. Age by Stage: parametric tests with
  Cohen's d. Tumor Size: Hodges-Lehmann shift. Lab Values: robust tests
  with Hedges' g. Survival Time: median with quartiles.

- showAboutPanel:

  Display comprehensive information about ridge plots, interpretation
  guidance, and clinical examples.

- showAssumptions:

  Display statistical assumptions, caveats, and methodological notes for
  the selected test type.

## Value

A results object containing:

|                            |     |     |     |     |                |
|----------------------------|-----|-----|-----|-----|----------------|
| `results$notices`          |     |     |     |     | a preformatted |
| `results$instructions`     |     |     |     |     | a html         |
| `results$clinicalSummary`  |     |     |     |     | a html         |
| `results$reportSummary`    |     |     |     |     | a html         |
| `results$aboutPanel`       |     |     |     |     | a html         |
| `results$assumptionsPanel` |     |     |     |     | a html         |
| `results$plot`             |     |     |     |     | an image       |
| `results$statistics`       |     |     |     |     | a table        |
| `results$tests`            |     |     |     |     | a table        |
| `results$interpretation`   |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$statistics$asDF`

`as.data.frame(results$statistics)`
