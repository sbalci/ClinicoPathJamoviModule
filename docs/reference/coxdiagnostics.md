# Cox Proportional Hazards Model Diagnostics

Cox proportional hazards model diagnostic plots using ggcoxdiagnostics
from survminer package for comprehensive model validation.

## Usage

``` r
coxdiagnostics(
  data,
  time = NULL,
  event = NULL,
  covariates = NULL,
  strata_var = NULL,
  show_martingale = TRUE,
  show_deviance = TRUE,
  show_score = FALSE,
  show_schoenfeld = FALSE,
  show_dfbeta = FALSE,
  ox_scale = "linear.predictions",
  add_smooth = TRUE,
  add_reference = TRUE,
  point_size = 1,
  alpha_level = 0.6,
  show_ph_test = TRUE,
  show_model_summary = TRUE,
  show_vif = TRUE,
  vif_threshold = 5,
  show_interpretation = TRUE,
  exclude_missing = TRUE,
  confidence_level = 0.95
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  The time-to-event variable for survival analysis.

- event:

  The event indicator variable (1=event, 0=censored).

- covariates:

  Variables to include as covariates in the Cox model.

- strata_var:

  Optional variable for stratified Cox regression.

- show_martingale:

  Whether to show martingale residual plots.

- show_deviance:

  Whether to show deviance residual plots.

- show_score:

  Whether to show score residual plots.

- show_schoenfeld:

  Whether to show Schoenfeld residual plots.

- show_dfbeta:

  Whether to show DFBeta influence diagnostic plots.

- ox_scale:

  Scale for the x-axis in residual plots.

- add_smooth:

  Whether to add a smooth line to residual plots.

- add_reference:

  Whether to add a reference line at y=0.

- point_size:

  Size of points in diagnostic plots.

- alpha_level:

  Transparency level for points in plots.

- show_ph_test:

  Whether to display results of proportional hazards test.

- show_model_summary:

  Whether to display Cox regression model summary.

- show_vif:

  Whether to calculate and display Variance Inflation Factor (VIF) for
  multicollinearity assessment.

- vif_threshold:

  VIF threshold for multicollinearity warning.

- show_interpretation:

  Whether to include interpretation of diagnostic results.

- exclude_missing:

  Whether to exclude observations with missing values.

- confidence_level:

  Confidence level for statistical tests and intervals.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$instructions`    |     |     |     |     | a html   |
| `results$model_summary`   |     |     |     |     | a html   |
| `results$ph_test_results` |     |     |     |     | a html   |
| `results$vif_results`     |     |     |     |     | a html   |
| `results$martingale_plot` |     |     |     |     | an image |
| `results$deviance_plot`   |     |     |     |     | an image |
| `results$score_plot`      |     |     |     |     | an image |
| `results$schoenfeld_plot` |     |     |     |     | an image |
| `results$dfbeta_plot`     |     |     |     |     | an image |
| `results$interpretation`  |     |     |     |     | a html   |
