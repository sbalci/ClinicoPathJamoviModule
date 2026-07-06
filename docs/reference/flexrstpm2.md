# Comprehensive Flexible Parametric Survival Models

FLEXIBLE PARAMETRIC SURVIVAL MODELS (Royston-Parmar) CLINICAL USE: Model
survival data with smooth, flexible hazard functions that can capture
complex patterns (increasing, decreasing, or bathtub-shaped hazards).
Better than Cox models when you need to extrapolate beyond observed data
or estimate absolute risks. WHEN TO USE: • Cancer survival with complex
hazard patterns • Health economic evaluations requiring extrapolation •
Time-varying treatment effects • Relative survival analysis with
population mortality REQUIREMENTS: • Minimum 30 events (preferably
\>=50) • Follow-up covering the period of interest • For df\>4: need
\>=100 events to avoid overfitting KEY OUTPUTS: • Hazard ratios with
clinical interpretation • Survival predictions at specific time points •
Model fit statistics (AIC, concordance) • Time-varying effect plots •
Copy-ready clinical report sentences

## Usage

``` r
flexrstpm2(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  scale = "hazard",
  df = 4,
  knots = "",
  boundary_knots = "",
  time_varying_covariates,
  tvc_df = 3,
  cure_fraction = FALSE,
  bhazard,
  group_variable,
  link_function = "log",
  prediction_times = "1, 2, 5, 10",
  extrapolation_time = 20,
  confidence_level = 0.95,
  smooth_formula = "",
  robust_se = FALSE,
  time_ratio = FALSE,
  relative_survival = FALSE,
  model_comparison = TRUE,
  goodness_of_fit = TRUE,
  hazard_analysis = TRUE,
  derivative_analysis = FALSE,
  bootstrap_validation = FALSE,
  bootstrap_samples = 500,
  show_model_summary = TRUE,
  show_coefficients_table = TRUE,
  show_survival_curves = TRUE,
  show_hazard_curves = TRUE,
  show_time_varying_plots = FALSE,
  show_model_diagnostics = TRUE,
  show_residuals = FALSE,
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

  Covariate variables for flexible parametric modeling

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- scale:

  Scale for the flexible parametric model

- df:

  Degrees of freedom for baseline spline function (complexity of hazard
  curve). Clinical guide: df=3-4 for most analyses, df=5-6 for complex
  patterns. Need \>=30 events per df for reliable results.

- knots:

  Comma-separated knot positions (optional, overrides df)

- boundary_knots:

  Comma-separated boundary knot positions (optional)

- time_varying_covariates:

  Covariates with time-varying effects (spline interactions)

- tvc_df:

  Degrees of freedom for time-varying covariate effects

- cure_fraction:

  Include cure fraction in the model

- bhazard:

  Background hazard variable for relative survival analysis

- group_variable:

  Optional grouping variable for stratified analysis

- link_function:

  Link function for baseline transformation

- prediction_times:

  Comma-separated list of time points for survival predictions

- extrapolation_time:

  Maximum time for survival curve extrapolation

- confidence_level:

  Confidence level for confidence intervals

- smooth_formula:

  Custom smoothing formula for advanced specifications

- robust_se:

  Use robust sandwich estimator for standard errors

- time_ratio:

  Include time ratio (acceleration factor) analysis

- relative_survival:

  Perform relative survival analysis (requires background hazard)

- model_comparison:

  Compare flexible parametric models with standard parametric models

- goodness_of_fit:

  Perform goodness of fit tests and model diagnostics

- hazard_analysis:

  Generate hazard function estimates and plots

- derivative_analysis:

  Include analysis of hazard function derivatives
  (acceleration/deceleration)

- bootstrap_validation:

  Perform bootstrap validation of model predictions

- bootstrap_samples:

  Number of bootstrap samples for validation (when bootstrap enabled)

- show_model_summary:

  Display comprehensive model summary

- show_coefficients_table:

  Display table of model coefficients

- show_survival_curves:

  Display fitted survival curves

- show_hazard_curves:

  Display fitted hazard functions

- show_time_varying_plots:

  Display time-varying covariate effect plots

- show_model_diagnostics:

  Display model diagnostic plots

- show_residuals:

  Display residual analysis plots

- showSummaries:

  Generate natural language summaries of results

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                     |     |     |     |     | a html   |
| `results$modelSummary`             |     |     |     |     | a html   |
| `results$coefficientsTable`        |     |     |     |     | a table  |
| `results$timeVaryingTable`         |     |     |     |     | a table  |
| `results$modelFit`                 |     |     |     |     | a table  |
| `results$splineInfo`               |     |     |     |     | a table  |
| `results$parameterEstimates`       |     |     |     |     | a table  |
| `results$survivalPredictions`      |     |     |     |     | a table  |
| `results$timeRatioAnalysis`        |     |     |     |     | a table  |
| `results$relativeSurvivalAnalysis` |     |     |     |     | a table  |
| `results$modelComparison`          |     |     |     |     | a table  |
| `results$goodnessOfFitTests`       |     |     |     |     | a table  |
| `results$hazardAnalysis`           |     |     |     |     | a table  |
| `results$derivativeAnalysis`       |     |     |     |     | a table  |
| `results$bootstrapValidation`      |     |     |     |     | a table  |
| `results$survivalCurves`           |     |     |     |     | an image |
| `results$hazardCurves`             |     |     |     |     | an image |
| `results$timeVaryingPlots`         |     |     |     |     | an image |
| `results$modelDiagnostics`         |     |     |     |     | an image |
| `results$residualPlots`            |     |     |     |     | an image |
| `results$splineComponents`         |     |     |     |     | an image |
| `results$hazardFunctionPlot`       |     |     |     |     | an image |
| `results$splineBasisPlot`          |     |     |     |     | an image |
| `results$modelComparisonPlot`      |     |     |     |     | an image |
| `results$derivativePlot`           |     |     |     |     | an image |
| `results$clinicalSummary`          |     |     |     |     | a html   |
| `results$analysisSummary`          |     |     |     |     | a html   |
| `results$methodExplanation`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficientsTable$asDF`

`as.data.frame(results$coefficientsTable)`
