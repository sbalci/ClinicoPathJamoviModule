# Stratified Parametric Models

Stratified Parametric Models

## Usage

``` r
stratifiedparametric(
  data,
  elapsedtime,
  outcome,
  strata_variable,
  covariates,
  outcomeLevel = "1",
  parametric_distribution = "weibull",
  baseline_specification = "separate_baselines",
  spline_df = 3,
  knot_placement = "quantiles",
  test_stratification = TRUE,
  confidence_level = 0.95,
  show_model_summary = TRUE,
  show_coefficients = TRUE,
  show_stratification_test = TRUE,
  show_survival_curves = TRUE,
  show_hazard_curves = TRUE,
  show_comparison_plot = FALSE,
  show_diagnostics = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  Time variable for survival analysis.

- outcome:

  Event indicator variable.

- strata_variable:

  Variable for stratifying the parametric models.

- covariates:

  Covariates to include in the stratified parametric model.

- outcomeLevel:

  Level of outcome variable indicating event.

- parametric_distribution:

  Parametric distribution for stratified modeling.

- baseline_specification:

  Method for specifying baseline functions across strata.

- spline_df:

  Degrees of freedom for spline-based baseline functions.

- knot_placement:

  Method for placing spline knots.

- test_stratification:

  Test whether stratification significantly improves model fit.

- confidence_level:

  Confidence level for parameter estimates and predictions.

- show_model_summary:

  Display comprehensive model summary information.

- show_coefficients:

  Display parameter estimates for each stratum.

- show_stratification_test:

  Display test results for stratification effect.

- show_survival_curves:

  Display stratified survival curves.

- show_hazard_curves:

  Display stratified hazard functions.

- show_comparison_plot:

  Compare stratified vs non-stratified models.

- show_diagnostics:

  Display diagnostic plots and statistics for each stratum.

- showSummaries:

  Generate natural language summaries of results.

- showExplanations:

  Show detailed methodology explanations.

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                       |     |     |     |     | a html   |
| `results$modelSummary`               |     |     |     |     | a html   |
| `results$coefficientsTable`          |     |     |     |     | a table  |
| `results$stratificationTestTable`    |     |     |     |     | a table  |
| `results$modelComparisonTable`       |     |     |     |     | a table  |
| `results$strataCharacteristicsTable` |     |     |     |     | a table  |
| `results$survivalPlot`               |     |     |     |     | an image |
| `results$hazardPlot`                 |     |     |     |     | an image |
| `results$comparisonPlot`             |     |     |     |     | an image |
| `results$diagnosticPlots`            |     |     |     |     | an image |
| `results$residualAnalysisTable`      |     |     |     |     | a table  |
| `results$predictionTable`            |     |     |     |     | a table  |
| `results$analysisSummary`            |     |     |     |     | a html   |
| `results$methodExplanation`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficientsTable$asDF`

`as.data.frame(results$coefficientsTable)`
