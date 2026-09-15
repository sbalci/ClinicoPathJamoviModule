# Flexible Parametric Survival Models (Enhanced)

ENHANCED: Comprehensive flexible parametric survival modeling combining
traditional parametric distributions (Generalized Gamma, Weibull, etc.)
with advanced spline-based approaches (Royston-Parmar, B-splines).
Provides maximum flexibility for modeling complex hazard shapes,
time-varying effects, and non-proportional hazards. CLINICAL USE: Model
survival data when standard Cox models are inadequate. Ideal for cancer
research, health economics, and comparative effectiveness studies. KEY
FEATURES: • Traditional parametric and spline-based models in one
function • Automatic model comparison and selection • Clinical
interpretation of parameters • Comprehensive diagnostic plots •
Copy-ready report templates

## Usage

``` r
flexparametric(
  data,
  elapsedtime,
  outcome,
  covariates,
  outcomeLevel = "1",
  model_approach = "automatic",
  distribution = "gengamma",
  spline_type = "hazard",
  spline_df = 4,
  knot_placement = "quantiles",
  manual_knots = "",
  confidence_level = 0.95,
  grouping_variable,
  model_comparison = TRUE,
  time_varying_effects = FALSE,
  show_parameters = TRUE,
  show_aic_bic = TRUE,
  show_survival_plot = TRUE,
  show_hazard_plot = FALSE,
  show_spline_plot = FALSE,
  show_diagnostics = TRUE,
  show_clinical_summary = TRUE,
  show_density_plot = FALSE,
  plot_time_max = 0,
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

- covariates:

  Covariates to include in the model.

- outcomeLevel:

  Level of outcome variable indicating event.

- model_approach:

  Choose modeling approach: Traditional uses standard distributions,
  Spline-based uses flexible baseline, Automatic selects best fit.

- distribution:

  Parametric distribution for traditional approach.

- spline_type:

  Type of spline transformation for flexible approach.

- spline_df:

  Degrees of freedom for spline flexibility (3-4 typical, 5-6 complex).

- knot_placement:

  Method for placing spline knots.

- manual_knots:

  Comma-separated knot positions (when manual placement selected).

- confidence_level:

  Confidence level for parameter estimates and survival curves.

- grouping_variable:

  Optional grouping variable for stratified analysis.

- model_comparison:

  Compare multiple models and select best based on AIC/BIC.

- time_varying_effects:

  Allow covariate effects to vary over time (spline models only).

- show_parameters:

  Display parameter estimates table.

- show_aic_bic:

  Display AIC and BIC for model comparison.

- show_survival_plot:

  Display parametric survival curves.

- show_hazard_plot:

  Display parametric hazard functions.

- show_spline_plot:

  Display spline basis functions and knot positions (spline models
  only).

- show_diagnostics:

  Display residual plots and goodness-of-fit diagnostics.

- show_clinical_summary:

  Display clinical interpretation and copy-ready report.

- show_density_plot:

  Display parametric density functions.

- plot_time_max:

  Maximum time for plots (0 = automatic).

- showSummaries:

  Generate natural language summaries.

- showExplanations:

  Show methodology explanations.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$modelSummary`      |     |     |     |     | a html   |
| `results$parametersTable`   |     |     |     |     | a table  |
| `results$splineDetails`     |     |     |     |     | a table  |
| `results$modelComparison`   |     |     |     |     | a table  |
| `results$fitStatistics`     |     |     |     |     | a table  |
| `results$survivalPlot`      |     |     |     |     | an image |
| `results$hazardPlot`        |     |     |     |     | an image |
| `results$densityPlot`       |     |     |     |     | an image |
| `results$splinePlot`        |     |     |     |     | an image |
| `results$diagnosticsPlot`   |     |     |     |     | an image |
| `results$clinicalSummary`   |     |     |     |     | a html   |
| `results$analysisSummary`   |     |     |     |     | a html   |
| `results$methodExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$parametersTable$asDF`

`as.data.frame(results$parametersTable)`
