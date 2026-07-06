# Coefficient Plots

Creates professional coefficient plots (forest plots) for regression
models. Visualizes coefficients and confidence intervals from linear,
logistic, and Cox regression models. Supports multiple models
comparison, custom coefficient selection, standardized coefficients, and
various styling options. Essential for presenting regression results in
clinical research and epidemiological studies.

## Usage

``` r
coefplot(
  data,
  dep = NULL,
  covs = NULL,
  model_type = "linear",
  time_var = NULL,
  include_intercept = FALSE,
  coef_selection = "all",
  specific_coefs = "",
  ci_level = 0.95,
  inner_ci_level = 0.8,
  sort_coefs = "natural",
  decreasing_sort = TRUE,
  horizontal_plot = TRUE,
  point_size = 3,
  line_thickness = 1,
  standardize = FALSE,
  robust_se = FALSE,
  exp_transform = FALSE,
  compare_models = FALSE,
  model2_covs = NULL,
  model3_covs = NULL,
  model_names = "Model 1, Model 2, Model 3",
  show_coefficient_plot = TRUE,
  show_model_summary = TRUE,
  show_coefficient_table = FALSE,
  custom_title = "",
  custom_x_label = ""
)
```

## Arguments

- data:

  The data as a data frame for regression analysis.

- dep:

  The outcome variable for regression analysis. Can be continuous
  (linear regression), binary (logistic regression), or time-to-event.

- covs:

  Independent variables (predictors) to include in the regression model.
  These will be displayed as coefficients in the plot.

- model_type:

  Type of regression model to fit: - Linear: For continuous outcomes -
  Logistic: For binary outcomes (odds ratios) - Cox: For survival
  analysis (hazard ratios) - Poisson: For count outcomes (rate ratios)

- time_var:

  Time-to-event variable for Cox regression. Only required when model
  type is set to Cox regression.

- include_intercept:

  Include the intercept term in the coefficient plot. Usually excluded
  as it's often not of primary interest.

- coef_selection:

  How to select which coefficients to display in the plot.

- specific_coefs:

  Comma-separated list of coefficient names to include or exclude
  (depending on selection method). Leave blank to use all coefficients.

- ci_level:

  Confidence level for coefficient confidence intervals (e.g., 0.95 for
  95 percent CI).

- inner_ci_level:

  Optional inner confidence interval for enhanced visualization. Set to
  0 to disable inner CI. Common values are 0.8 or 0.9.

- sort_coefs:

  How to order coefficients in the plot. Magnitude sorting can help
  identify the most important predictors.

- decreasing_sort:

  When sorting by magnitude or alphabetically, use decreasing order.

- horizontal_plot:

  Display coefficients horizontally (default) or vertically. Horizontal
  layout is typically preferred for readability.

- point_size:

  Size of the coefficient points in the plot.

- line_thickness:

  Thickness of the confidence interval lines.

- standardize:

  Standardize coefficients by scaling predictors to have mean 0 and
  SD 1. Useful for comparing effect sizes across variables with
  different scales.

- robust_se:

  Use robust (sandwich) standard errors for confidence intervals.
  Recommended when there are concerns about heteroscedasticity.

- exp_transform:

  Exponentiate coefficients to show odds ratios (logistic), hazard
  ratios (Cox), or rate ratios (Poisson). Automatically enabled for
  logistic and Cox models.

- compare_models:

  Create comparison plots for multiple model specifications. Useful for
  sensitivity analysis or model selection.

- model2_covs:

  Covariates for second model comparison. Only used when comparing
  models.

- model3_covs:

  Covariates for third model comparison. Only used when comparing
  models.

- model_names:

  Comma-separated names for models when comparing multiple models. Will
  be used in the legend.

- show_coefficient_plot:

  Display the main coefficient plot with confidence intervals.

- show_model_summary:

  Display statistical summary of the fitted model(s) including
  R-squared, AIC, and other fit statistics.

- show_coefficient_table:

  Display detailed table of coefficients, standard errors, and p-values.

- custom_title:

  Custom title for the coefficient plot. Leave blank for automatic
  title.

- custom_x_label:

  Custom label for x-axis. Leave blank for automatic label based on
  model type.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$instructions`      |     |     |     |     | a html   |
| `results$coefficient_plot`  |     |     |     |     | an image |
| `results$model_summary`     |     |     |     |     | a html   |
| `results$coefficient_table` |     |     |     |     | a html   |
