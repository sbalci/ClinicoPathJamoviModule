# Forest Plot Visualization

Create professional forest plots from regression models including
linear, logistic, and survival models. Visualize coefficients,
confidence intervals, and effect sizes with customizable formatting and
layout options.

## Usage

``` r
jforestmodel(
  data,
  dependent_var,
  predictor_vars,
  model_type = "lm",
  time_var,
  event_var,
  family = "binomial",
  exponentiate = FALSE,
  show_p_values = TRUE,
  show_confidence_intervals = TRUE,
  confidence_level = 0.95,
  factor_separate_line = TRUE,
  covariates,
  sort_variables = "none",
  plot_title = "Forest Plot",
  x_axis_label = "",
  point_size = 2,
  line_size = 0.5,
  color_scheme = "default",
  custom_color = "#2E8B57",
  show_reference_line = TRUE,
  reference_value = 0,
  panel_width_ratio = "1:1:1",
  show_summary = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  Dataset containing variables for regression modeling

- dependent_var:

  Outcome variable for regression modeling

- predictor_vars:

  Independent variables to include in the model

- model_type:

  Type of regression model to fit

- time_var:

  Time-to-event variable for Cox regression

- event_var:

  Event indicator (0=censored, 1=event) for Cox regression

- family:

  Distribution family for GLM models

- exponentiate:

  Transform coefficients to odds ratios or hazard ratios

- show_p_values:

  Display p-values in the forest plot

- show_confidence_intervals:

  Display confidence intervals as horizontal lines

- confidence_level:

  Confidence level for intervals

- factor_separate_line:

  Display each factor level on a separate line

- covariates:

  Optional subset of variables to include in plot

- sort_variables:

  How to sort variables in the plot

- plot_title:

  Custom title for the forest plot

- x_axis_label:

  Custom label for x-axis (auto-generated if empty)

- point_size:

  Size of coefficient points

- line_size:

  Thickness of confidence interval lines

- color_scheme:

  Color scheme for the plot

- custom_color:

  Custom color (hex code) when using custom color scheme

- show_reference_line:

  Display vertical reference line at null effect

- reference_value:

  Value for reference line (0 for linear, 1 for exponentiated)

- panel_width_ratio:

  Ratio of panel widths (labels:plot:values)

- show_summary:

  Display model summary information

- show_interpretation:

  Provide interpretation guidance

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$model_summary`      |     |     |     |     | a table  |
| `results$coefficients_table` |     |     |     |     | a table  |
| `results$forest_plot`        |     |     |     |     | an image |
| `results$model_diagnostics`  |     |     |     |     | a table  |
| `results$interpretation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$model_summary$asDF`

`as.data.frame(results$model_summary)`
