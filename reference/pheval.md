# Proportional Hazards Testing

Comprehensive testing of the proportional hazards assumption in Cox
regression models using multiple statistical approaches. This analysis
provides systematic validation of the fundamental assumption underlying
Cox proportional hazards models through statistical tests, graphical
diagnostics, and time-dependent coefficient analysis for robust model
assessment and assumption verification.

## Usage

``` r
pheval(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  test_schoenfeld = TRUE,
  test_scaled_schoenfeld = TRUE,
  test_global = TRUE,
  test_correlation = FALSE,
  test_logrank = FALSE,
  test_supremum = FALSE,
  time_transform = "identity",
  confidence_level = 0.95,
  rho_parameter = 0,
  global_test_method = "chisquare",
  stratify_variable,
  show_individual_tests = TRUE,
  show_global_tests = TRUE,
  show_residual_plots = TRUE,
  show_diagnostic_plots = TRUE,
  show_time_varying_plots = TRUE,
  show_model_summary = TRUE,
  show_recommendations = TRUE,
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

  Covariate variables for proportional hazards testing

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- test_schoenfeld:

  Perform Schoenfeld residuals test

- test_scaled_schoenfeld:

  Perform scaled Schoenfeld residuals test

- test_global:

  Perform global proportional hazards test

- test_correlation:

  Perform correlation test with time

- test_logrank:

  Perform log-rank trend test

- test_supremum:

  Perform supremum test

- time_transform:

  Transformation function for time in testing

- confidence_level:

  Confidence level for test statistics and intervals

- rho_parameter:

  Rho parameter for weighted residuals (0=unweighted)

- global_test_method:

  Method for global proportional hazards test

- stratify_variable:

  Optional variable for stratified analysis

- show_individual_tests:

  Display individual covariate test results

- show_global_tests:

  Display global proportional hazards tests

- show_residual_plots:

  Display Schoenfeld residual plots

- show_diagnostic_plots:

  Display comprehensive diagnostic plots

- show_time_varying_plots:

  Display plots of potential time-varying effects

- show_model_summary:

  Display fitted Cox model summary

- show_recommendations:

  Display interpretation recommendations

- showSummaries:

  Generate natural language summaries of the analysis results

- showExplanations:

  Show detailed explanations of the methodology and interpretation

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$modelSummary`      |     |     |     |     | a html   |
| `results$individualTests`   |     |     |     |     | a table  |
| `results$globalTests`       |     |     |     |     | a table  |
| `results$residualAnalysis`  |     |     |     |     | a table  |
| `results$stratifiedResults` |     |     |     |     | a table  |
| `results$powerAnalysis`     |     |     |     |     | a table  |
| `results$residualPlots`     |     |     |     |     | an image |
| `results$diagnosticPlots`   |     |     |     |     | an image |
| `results$timeVaryingPlots`  |     |     |     |     | an image |
| `results$globalTestPlots`   |     |     |     |     | an image |
| `results$stratifiedPlots`   |     |     |     |     | an image |
| `results$recommendations`   |     |     |     |     | a html   |
| `results$analysisSummary`   |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$individualTests$asDF`

`as.data.frame(results$individualTests)`
