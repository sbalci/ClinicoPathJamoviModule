# Enhanced Statistical Visualization

Enhanced Statistical Visualization

## Usage

``` r
jggstats(
  data,
  analysis_type = "ggcoef_model",
  dependent_var = NULL,
  independent_vars = NULL,
  grouping_var = NULL,
  weight_var = NULL,
  model_formula = "",
  model_type = "lm",
  family = "gaussian",
  confidence_level = 0.95,
  likert_levels = 5,
  show_intercept = FALSE,
  sort_coefficients = TRUE,
  standardized = FALSE,
  show_statistics = TRUE,
  show_significance = TRUE,
  color_palette = "default",
  theme_style = "default",
  plot_title = "",
  plot_subtitle = "",
  x_label = "",
  y_label = "",
  facet_var = NULL,
  facet_type = "wrap",
  show_model_summary = TRUE,
  show_interpretation = TRUE,
  output_format = "plot_only"
)
```

## Arguments

- data:

  .

- analysis_type:

  .

- dependent_var:

  Main outcome variable for analysis

- independent_vars:

  Predictor variables for model

- grouping_var:

  Variable for grouping/stratification

- weight_var:

  Variable containing survey weights

- model_formula:

  Custom model formula (optional)

- model_type:

  .

- family:

  .

- confidence_level:

  Confidence level for intervals

- likert_levels:

  Number of levels in Likert scale

- show_intercept:

  Include intercept in coefficient plots

- sort_coefficients:

  Sort coefficients by magnitude

- standardized:

  Display standardized coefficients

- show_statistics:

  Display statistical values

- show_significance:

  Add significance stars to plot

- color_palette:

  .

- theme_style:

  .

- plot_title:

  .

- plot_subtitle:

  .

- x_label:

  .

- y_label:

  .

- facet_var:

  Variable for creating plot facets

- facet_type:

  .

- show_model_summary:

  Display model fit statistics

- show_interpretation:

  Generate interpretation text

- output_format:

  .

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$plot`           |     |     |     |     | an image |
| `results$modeltab`       |     |     |     |     | a table  |
| `results$summary`        |     |     |     |     | a table  |
| `results$interpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modeltab$asDF`

`as.data.frame(results$modeltab)`
