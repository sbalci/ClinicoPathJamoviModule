# Aalen's Additive Hazard Models

Implements Aalen's additive hazard regression models for survival data
where covariate effects are additive rather than multiplicative. This
approach is particularly useful when the proportional hazards assumption
is violated, allowing for time-varying covariate effects and
non-proportional hazards. The model estimates cumulative regression
functions that can reveal how covariate effects change over time.

## Usage

``` r
aalenhazard(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  model_type = "additive",
  constant_effects,
  test_constant_effects = TRUE,
  bandwidth = 1,
  robust_se = TRUE,
  show_model_summary = TRUE,
  show_coefficients_table = TRUE,
  show_test_results = TRUE,
  show_cumulative_plots = TRUE,
  show_hazard_plots = TRUE,
  show_diagnostics = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Survival time or follow-up duration variable

- outcome:

  Event indicator variable (0/1, FALSE/TRUE, or factor)

- covariates:

  Covariate variables for additive hazard modeling

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- model_type:

  Type of Aalen hazard model to fit

- constant_effects:

  Variables to be treated as having constant (time-independent) effects

- test_constant_effects:

  Test whether covariate effects are constant over time

- bandwidth:

  Bandwidth parameter for kernel smoothing of cumulative coefficients

- robust_se:

  Use robust sandwich estimator for standard errors

- show_model_summary:

  Display model summary table

- show_coefficients_table:

  Display table of cumulative regression coefficients

- show_test_results:

  Display statistical test results

- show_cumulative_plots:

  Display plots of cumulative regression coefficients over time

- show_hazard_plots:

  Display estimated hazard function plots

- show_diagnostics:

  Display model diagnostic plots and residuals

- showSummaries:

  Generate natural language summaries of results

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$modelSummary`        |     |     |     |     | a html   |
| `results$coefficientsTable`   |     |     |     |     | a table  |
| `results$constantEffectsTest` |     |     |     |     | a table  |
| `results$goodnessOfFit`       |     |     |     |     | a table  |
| `results$modelComparison`     |     |     |     |     | a table  |
| `results$cumulativePlots`     |     |     |     |     | an image |
| `results$hazardPlots`         |     |     |     |     | an image |
| `results$diagnosticPlots`     |     |     |     |     | an image |
| `results$residualPlots`       |     |     |     |     | an image |
| `results$analysisSummary`     |     |     |     |     | a html   |
| `results$methodExplanation`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficientsTable$asDF`

`as.data.frame(results$coefficientsTable)`
