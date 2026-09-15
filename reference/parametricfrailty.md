# Parametric Frailty Models

Implements parametric frailty models for survival analysis with
clustered or correlated survival data. Uses the frailtySurv package for
efficient estimation of parametric baseline distributions combined with
frailty components to model unobserved heterogeneity between clusters or
individuals.

## Usage

``` r
parametricfrailty(
  data,
  elapsedtime,
  outcome,
  covariates = NULL,
  frailty_variable,
  baseline_distribution = "weibull",
  frailty_distribution = "gamma",
  estimation_method = "penalized_likelihood",
  include_se = TRUE,
  include_ci = TRUE,
  conf_level = 0.95,
  frailty_variance = TRUE,
  frailty_predictions = FALSE,
  gof_tests = FALSE,
  plot_hazard = FALSE,
  plot_survival = FALSE,
  plot_frailty = FALSE,
  plot_diagnostics = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Survival time or follow-up duration variable. Should contain positive
  numeric values representing the time to event or censoring.

- outcome:

  Binary event indicator variable. Should contain values like 0/1,
  FALSE/TRUE, or factor levels indicating whether event occurred.

- covariates:

  Vector of variable names for covariates/explanatory variables to
  include in the parametric frailty model.

- frailty_variable:

  Variable identifying clusters or groups for frailty modeling. Each
  unique value represents a different cluster/group with shared frailty.

- baseline_distribution:

  Baseline parametric distribution for survival times.

- frailty_distribution:

  Distribution of the frailty random effects.

- estimation_method:

  Method for parameter estimation in the frailty model.

- include_se:

  Whether to compute and display standard errors for parameter
  estimates.

- include_ci:

  Whether to compute and display confidence intervals for parameter
  estimates.

- conf_level:

  Confidence level for confidence intervals (between 0.5 and 0.99).

- frailty_variance:

  Whether to estimate and display frailty variance components.

- frailty_predictions:

  Whether to compute individual frailty predictions for each cluster.

- gof_tests:

  Whether to perform goodness-of-fit tests for the fitted model.

- plot_hazard:

  Whether to create plots of the estimated hazard function.

- plot_survival:

  Whether to create plots of the estimated survival function.

- plot_frailty:

  Whether to create plots of the frailty distribution.

- plot_diagnostics:

  Whether to create model diagnostic plots for residuals and fit
  assessment.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$overview` |  |  |  |  | Overview of the parametric frailty model analysis |
| `results$model_fit` |  |  |  |  | Model fit statistics for the parametric frailty model |
| `results$coefficients` |  |  |  |  | Parameter estimates for the parametric frailty model |
| `results$frailty_summary` |  |  |  |  | Summary statistics for frailty terms |
| `results$frailty_predictions_table` |  |  |  |  | Individual frailty predictions for each subject/group |
| `results$gof_table` |  |  |  |  | Goodness-of-fit tests for the parametric frailty model |
| `results$hazard_plot` |  |  |  |  | Plot of the estimated hazard function |
| `results$survival_plot` |  |  |  |  | Plot of the estimated survival function |
| `results$frailty_plot` |  |  |  |  | Plot of the frailty distribution |
| `results$diagnostics_plot` |  |  |  |  | Model diagnostic plots |
| `results$model_summary` |  |  |  |  | Comprehensive model summary and interpretation |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
