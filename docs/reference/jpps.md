# Predictive Power Score Analysis

Predictive Power Score (PPS) analysis provides an asymmetric,
data-type-agnostic score that can detect linear or non-linear
relationships between variables. PPS ranges from 0 (no predictive power)
to 1 (perfect predictive power).

## Usage

``` r
jpps(
  data,
  analysis_type = "predictors",
  target_var = NULL,
  predictor_var = NULL,
  predictor_vars = NULL,
  matrix_vars = NULL,
  algorithm = "auto",
  cv_folds = 4,
  sample_size = 5000,
  show_heatmap = TRUE,
  show_barplot = TRUE,
  show_correlation_comparison = FALSE,
  correlation_method = "pearson",
  min_pps_threshold = 0,
  sort_results = "pps_desc",
  color_scheme = "viridis",
  custom_color_low = "#FFFFFF",
  custom_color_high = "#FF0000",
  show_values_on_plot = TRUE,
  plot_title = "",
  show_summary = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  Dataset for PPS analysis

- analysis_type:

  Type of PPS analysis to perform

- target_var:

  Target variable to predict (for single/predictors analysis)

- predictor_var:

  Single predictor variable (for single analysis)

- predictor_vars:

  Multiple predictor variables (for predictors analysis)

- matrix_vars:

  Variables to include in full PPS matrix

- algorithm:

  Machine learning algorithm for PPS calculation

- cv_folds:

  Number of cross-validation folds

- sample_size:

  Maximum sample size for analysis (0 = no limit)

- show_heatmap:

  Display PPS results as heatmap

- show_barplot:

  Display PPS scores as barplot

- show_correlation_comparison:

  Compare PPS with Pearson correlation

- correlation_method:

  Correlation method for comparison

- min_pps_threshold:

  Minimum PPS score to display in results

- sort_results:

  How to sort the results

- color_scheme:

  Color scheme for visualizations

- custom_color_low:

  Color for low PPS values (hex code)

- custom_color_high:

  Color for high PPS values (hex code)

- show_values_on_plot:

  Display PPS values on heatmap/barplot

- plot_title:

  Custom title for plots (auto-generated if empty)

- show_summary:

  Display summary of PPS analysis

- show_interpretation:

  Provide interpretation guidance for PPS results

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$pps_scores`             |     |     |     |     | a table  |
| `results$correlation_comparison` |     |     |     |     | a table  |
| `results$pps_heatmap`            |     |     |     |     | an image |
| `results$pps_barplot`            |     |     |     |     | an image |
| `results$comparison_plot`        |     |     |     |     | an image |
| `results$summary_stats`          |     |     |     |     | a table  |
| `results$interpretation`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$pps_scores$asDF`

`as.data.frame(results$pps_scores)`
