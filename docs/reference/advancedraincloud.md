# Advanced Raincloud Plot

Creates advanced raincloud plots with longitudinal connections using
ggrain package. This module complements the existing Raincloud Plot
module by providing advanced features including longitudinal data
connections, Likert scale support, and flexible raincloud positioning.
Perfect for repeated measures data, survey analysis, and complex
distribution visualization in clinical research. Uses the ggrain package
for enhanced customization and connectivity features.

## Usage

``` r
advancedraincloud(
  data,
  y_var,
  x_var,
  fill_var = NULL,
  id_var = NULL,
  cov_var = NULL,
  rain_side = "l",
  likert_mode = FALSE,
  show_longitudinal = FALSE,
  point_size = 1.5,
  point_alpha = 0.7,
  violin_alpha = 0.7,
  boxplot_width = 0.1,
  jitter_seed = 42,
  color_palette = "clinical",
  plot_title = "Advanced Raincloud Plot",
  x_label = "",
  y_label = "",
  show_statistics = TRUE,
  show_comparisons = FALSE,
  show_interpretation = TRUE,
  clinical_cutoff = 0,
  reference_range_min = 0,
  reference_range_max = 0,
  show_mcid = FALSE,
  mcid_value = 0,
  show_effect_size = FALSE,
  effect_size_type = "cohens_d",
  show_change_scores = FALSE,
  baseline_group = "",
  responder_threshold = 20,
  show_sample_size = TRUE,
  show_missing_info = FALSE,
  trial_arms = "",
  time_labels = "",
  population_type = "itt",
  log_transform = FALSE,
  outlier_method = "none",
  show_cv_bands = FALSE,
  cv_band_1 = 15,
  cv_band_2 = 20,
  p_value_position = "above",
  journal_style = "default",
  generate_report = FALSE,
  include_methods = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- y_var:

  Continuous variable for distribution visualization on Y-axis.

- x_var:

  Grouping variable for X-axis categories.

- fill_var:

  Optional variable for color filling different groups.

- id_var:

  Optional ID variable for connecting longitudinal observations across
  groups.

- cov_var:

  Optional variable for remapping point colors based on covariate
  values.

- rain_side:

  Position of the raincloud relative to the data points. 'Left' (l):
  Density on left, points on right. 'Right' (r): Density on right,
  points on left. 'Flanking' (f): Density on both sides. 'Flanking 1x1'
  (f1x1): Paired flanking for 1x1 comparisons. 'Flanking 2x2' (f2x2):
  Grouped flanking for 2x2 comparisons.

- likert_mode:

  If TRUE, adds Y-axis jittering for Likert scale or ordinal data.

- show_longitudinal:

  If TRUE, connects repeated observations using the ID variable.

- point_size:

  Size of individual data points.

- point_alpha:

  Transparency level for data points.

- violin_alpha:

  Transparency level for violin plots.

- boxplot_width:

  Width of the boxplot component.

- jitter_seed:

  Random seed for consistent point jittering across plot updates.

- color_palette:

  Color palette for different groups.

- plot_title:

  Title for the raincloud plot.

- x_label:

  Custom label for X-axis. If empty, uses variable name.

- y_label:

  Custom label for Y-axis. If empty, uses variable name.

- show_statistics:

  If TRUE, displays summary statistics table.

- show_comparisons:

  If TRUE, performs statistical tests between groups.

- show_interpretation:

  If TRUE, displays interpretation and feature guide.

- clinical_cutoff:

  Add horizontal line for clinical threshold or decision boundary.

- reference_range_min:

  Lower bound of normal reference range (shaded area).

- reference_range_max:

  Upper bound of normal reference range (shaded area).

- show_mcid:

  Display Minimal Clinically Important Difference band.

- mcid_value:

  Size of the Minimal Clinically Important Difference.

- show_effect_size:

  Show effect size calculations between groups.

- effect_size_type:

  Method for calculating effect size.

- show_change_scores:

  Display change from baseline analysis for longitudinal data.

- baseline_group:

  Name or level of the baseline/reference group.

- responder_threshold:

  Percentage change threshold for responder classification.

- show_sample_size:

  Show sample size (N) for each group on the plot.

- show_missing_info:

  Display information about missing data and exclusions.

- trial_arms:

  Custom labels for treatment arms (comma-separated).

- time_labels:

  Custom labels for time points (comma-separated).

- population_type:

  Type of analysis population for clinical trials.

- log_transform:

  Apply log transformation to Y-axis values.

- outlier_method:

  Method for handling outliers in the data.

- show_cv_bands:

  Display coefficient of variation bands for assay variability.

- cv_band_1:

  First CV band percentage (typically 15 percent for analytical
  variability).

- cv_band_2:

  Second CV band percentage (typically 20 percent for biological
  variability).

- p_value_position:

  Where to display p-values from group comparisons.

- journal_style:

  Apply journal-specific formatting guidelines.

- generate_report:

  Generate comprehensive clinical analysis report.

- include_methods:

  Include methods section text for publications.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$todo`            |     |     |     |     | a html   |
| `results$plot`            |     |     |     |     | an image |
| `results$statistics`      |     |     |     |     | a html   |
| `results$comparisons`     |     |     |     |     | a html   |
| `results$interpretation`  |     |     |     |     | a html   |
| `results$effect_sizes`    |     |     |     |     | a html   |
| `results$change_analysis` |     |     |     |     | a html   |
| `results$clinical_report` |     |     |     |     | a html   |
| `results$methods_text`    |     |     |     |     | a html   |
