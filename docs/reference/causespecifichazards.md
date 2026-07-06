# Cause-Specific Hazards Models

Implements cause-specific hazards modeling for competing risks analysis.
This approach models the hazard for each specific cause separately,
treating other causes as censoring events. Provides comprehensive
analysis including hazard ratios, cumulative incidence functions, and
model comparisons across different causes of failure.

## Usage

``` r
causespecifichazards(
  data,
  elapsedtime,
  outcome,
  covariates = NULL,
  cause_variable = NULL,
  reference_cause = "1",
  model_type = "cox",
  include_se = TRUE,
  include_ci = TRUE,
  conf_level = 0.95,
  cumulative_incidence = TRUE,
  model_comparison = FALSE,
  proportional_hazards_test = FALSE,
  plot_cumulative_incidence = FALSE,
  plot_hazards = FALSE,
  plot_diagnostics = FALSE,
  stratify_plots = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Survival time or follow-up duration variable. Should contain positive
  numeric values representing the time to event or censoring.

- outcome:

  Event indicator variable. Should contain values indicating the type of
  event or censoring (0 for censored, 1,2,3... for different causes).

- covariates:

  Vector of variable names for covariates/explanatory variables to
  include in the cause-specific hazards models.

- cause_variable:

  Optional separate variable identifying the cause of failure. If not
  specified, the outcome variable will be used to identify causes.

- reference_cause:

  The reference cause for model comparison and hazard ratio
  calculations. Should match one of the cause levels in the data.

- model_type:

  Type of survival model to fit for each cause-specific hazard.

- include_se:

  Whether to compute and display standard errors for parameter
  estimates.

- include_ci:

  Whether to compute and display confidence intervals for parameter
  estimates.

- conf_level:

  Confidence level for confidence intervals (between 0.5 and 0.99).

- cumulative_incidence:

  Whether to compute and display cumulative incidence functions for each
  cause.

- model_comparison:

  Whether to perform model comparison across different causes using
  likelihood ratio tests and information criteria.

- proportional_hazards_test:

  Whether to test the proportional hazards assumption for Cox models.

- plot_cumulative_incidence:

  Whether to create cumulative incidence function plots for each cause.

- plot_hazards:

  Whether to create hazard function plots for each cause.

- plot_diagnostics:

  Whether to create diagnostic plots for model assessment (residuals,
  etc.).

- stratify_plots:

  Whether to stratify plots by covariate levels for better
  visualization.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$overview` |  |  |  |  | Overview of the cause-specific hazards analysis |
| `results$cause_summary` |  |  |  |  | Summary of events by cause |
| `results$model_fit` |  |  |  |  | Model fit statistics for each cause-specific model |
| `results$coefficients` |  |  |  |  | Parameter estimates for each cause-specific model |
| `results$cumulative_incidence_table` |  |  |  |  | Cumulative incidence estimates by cause and time points |
| `results$model_comparison_table` |  |  |  |  | Comparison of model fit across different causes |
| `results$proportional_hazards_table` |  |  |  |  | Tests of proportional hazards assumption for each cause |
| `results$cumulative_incidence_plot` |  |  |  |  | Plot of cumulative incidence functions by cause |
| `results$hazards_plot` |  |  |  |  | Plot of hazard functions by cause |
| `results$diagnostics_plot` |  |  |  |  | Model diagnostic plots for cause-specific models |
| `results$model_summary` |  |  |  |  | Comprehensive summary and interpretation of cause-specific hazards analysis |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
