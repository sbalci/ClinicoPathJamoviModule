# Smoothly Time-Varying Effects

Implements smooth estimation of time-varying covariate effects in
survival analysis using flexible spline-based methods. This approach
provides continuous modeling of how covariate effects change over time,
offering an alternative to step-function approaches in standard
time-varying Cox models and complementing Aalen's additive hazard
methodology.

## Usage

``` r
smoothtimevary(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  time_varying_covariates,
  smoothing_method = "spline",
  spline_df = 4,
  bandwidth = 1,
  confidence_level = 0.95,
  test_constancy = TRUE,
  residual_analysis = TRUE,
  show_model_summary = TRUE,
  show_effects_table = TRUE,
  show_constancy_tests = TRUE,
  show_smooth_plots = TRUE,
  show_diagnostic_plots = TRUE,
  show_comparison_plots = TRUE,
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

  Covariate variables for smooth time-varying effects modeling

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- time_varying_covariates:

  Covariates to model with smooth time-varying effects

- smoothing_method:

  Method for smooth estimation of time-varying effects

- spline_df:

  Degrees of freedom for spline-based smoothing

- bandwidth:

  Bandwidth parameter for kernel or LOESS smoothing

- confidence_level:

  Confidence level for interval estimation

- test_constancy:

  Test whether covariate effects are constant over time

- residual_analysis:

  Perform comprehensive residual analysis and diagnostics

- show_model_summary:

  Display comprehensive model summary

- show_effects_table:

  Display table of smooth time-varying effects

- show_constancy_tests:

  Display statistical tests for effect constancy

- show_smooth_plots:

  Display plots of smooth time-varying effects

- show_diagnostic_plots:

  Display model diagnostic and residual plots

- show_comparison_plots:

  Display comparison with constant effects models

- showSummaries:

  Generate natural language summaries of the analysis results

- showExplanations:

  Show detailed explanations of the methodology and interpretation

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$modelSummary`        |     |     |     |     | a html   |
| `results$effectsTable`        |     |     |     |     | a table  |
| `results$constancyTests`      |     |     |     |     | a table  |
| `results$modelComparison`     |     |     |     |     | a table  |
| `results$smoothingParameters` |     |     |     |     | a table  |
| `results$goodnessOfFit`       |     |     |     |     | a table  |
| `results$smoothEffectPlots`   |     |     |     |     | an image |
| `results$diagnosticPlots`     |     |     |     |     | an image |
| `results$residualPlots`       |     |     |     |     | an image |
| `results$comparisonPlots`     |     |     |     |     | an image |
| `results$smoothingPlots`      |     |     |     |     | an image |
| `results$analysisSummary`     |     |     |     |     | a html   |
| `results$methodExplanation`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$effectsTable$asDF`

`as.data.frame(results$effectsTable)`
