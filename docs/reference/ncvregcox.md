# Smoothly Clipped Absolute Deviation Cox Regression

Smoothly Clipped Absolute Deviation (SCAD) Cox regression for
high-dimensional survival data. SCAD provides automatic variable
selection with oracle properties, avoiding over-penalization of large
coefficients while maintaining sparsity for small coefficients.
Particularly useful for genomics and high-dimensional clinical data
where interpretable variable selection is crucial.

## Usage

``` r
ncvregcox(
  data,
  time,
  event,
  outcomeLevel,
  censorLevel,
  covariates,
  penalty = "SCAD",
  cv_folds = 10,
  lambda_type = "min",
  alpha = 1,
  gamma = 3.7,
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time:

  survival time variable

- event:

  event indicator (1=event, 0=censored)

- outcomeLevel:

  Level of `event` considered as the event (e.g., death, recurrence).
  For binary factor outcomes, if left empty the second observed level is
  used; for numeric binary outcomes, the larger observed value is used
  (or 1 for 0/1 coding).

- censorLevel:

  Level of `event` considered as censored (no event). Together with
  `outcomeLevel`, this defines a strict two-level encoding: rows whose
  event value matches neither level are treated as missing and excluded.

- covariates:

  predictor variables for high-dimensional analysis

- penalty:

  Type of penalty function for variable selection

- cv_folds:

  Number of folds for cross-validation

- lambda_type:

  Lambda selection criterion

- alpha:

  Mixing parameter for elastic net (1=pure penalty, values near
  0=ridge-like). ncvreg requires alpha \> 0; pure ridge (alpha=0) is not
  supported.

- gamma:

  Tuning parameter controlling concavity of the penalty. SCAD
  recommended default is 3.7 (Fan & Li 2001). MCP recommended default is
  3.0 (Zhang 2010). When MCP is selected and gamma is left at 3.7, the
  MCP-recommended default of 3.0 is used automatically.

- standardize:

  Standardize covariates before fitting

- plot_path:

  Display coefficient paths plot

- plot_cv:

  Display cross-validation error plot

- variable_importance:

  Calculate and display variable importance metrics

- suitabilityCheck:

  Run a comprehensive data suitability assessment before analysis.
  Checks sample size, events-per-variable ratio, multicollinearity, and
  whether regularization is needed.

## Value

A results object containing:

|                                    |     |     |     |     |                |
|------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                  |     |     |     |     | a preformatted |
| `results$instructions`             |     |     |     |     | a html         |
| `results$suitabilityReport`        |     |     |     |     | a html         |
| `results$model_summary`            |     |     |     |     | a table        |
| `results$selected_variables`       |     |     |     |     | a table        |
| `results$variable_importance`      |     |     |     |     | a table        |
| `results$cross_validation_results` |     |     |     |     | a table        |
| `results$model_comparison`         |     |     |     |     | a table        |
| `results$convergence_info`         |     |     |     |     | a table        |
| `results$regularization_path`      |     |     |     |     | an image       |
| `results$cv_error_plot`            |     |     |     |     | an image       |
| `results$variable_selection_plot`  |     |     |     |     | an image       |
| `results$model_interpretation`     |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$model_summary$asDF`

`as.data.frame(results$model_summary)`
