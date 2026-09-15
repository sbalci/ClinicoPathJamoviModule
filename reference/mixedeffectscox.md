# Mixed-Effects Cox Models

Implements mixed-effects Cox proportional hazards models for
hierarchical survival data with random effects. Accounts for clustering,
repeated measurements, and unobserved heterogeneity using the coxme
framework for multi-level survival analysis in clinical research.

## Usage

``` r
mixedeffectscox(
  data,
  elapsedtime,
  outcome,
  fixed_effects,
  random_effects,
  outcomeLevel = "1",
  random_structure = "random_intercept",
  correlation_structure = "independent",
  variance_structure = "homoscedastic",
  estimation_method = "reml",
  sparse_matrix = TRUE,
  ties_method = "efron",
  offset_variable,
  weights_variable,
  confidence_level = 0.95,
  max_iterations = 100,
  convergence_tolerance = 1e-06,
  random_effects_prediction = TRUE,
  variance_components_test = TRUE,
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  show_model_summary = TRUE,
  show_fixed_effects = TRUE,
  show_random_effects = TRUE,
  show_diagnostics = TRUE,
  show_comparison = TRUE,
  show_residual_plots = TRUE,
  show_random_effects_plots = TRUE,
  show_survival_plots = TRUE,
  show_forest_plot = TRUE,
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

- fixed_effects:

  Fixed effect covariates for the Cox model

- random_effects:

  Random effect grouping variables (clusters, subjects)

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- random_structure:

  Structure of random effects in the model

- correlation_structure:

  Correlation structure for random effects

- variance_structure:

  Variance structure specification

- estimation_method:

  Method for estimating variance components

- sparse_matrix:

  Use sparse matrix methods for large datasets

- ties_method:

  Method for handling tied event times

- offset_variable:

  Optional offset variable for the model

- weights_variable:

  Optional case weights variable

- confidence_level:

  Confidence level for intervals

- max_iterations:

  Maximum number of iterations

- convergence_tolerance:

  Convergence tolerance for estimation

- random_effects_prediction:

  Compute and display random effects predictions (BLUPs)

- variance_components_test:

  Test significance of variance components

- bootstrap_ci:

  Compute bootstrap confidence intervals

- bootstrap_samples:

  Number of bootstrap samples

- show_model_summary:

  Display comprehensive model summary

- show_fixed_effects:

  Display fixed effects coefficients table

- show_random_effects:

  Display random effects and variance components

- show_diagnostics:

  Display model diagnostics

- show_comparison:

  Compare mixed-effects vs standard Cox models

- show_residual_plots:

  Display residual diagnostic plots

- show_random_effects_plots:

  Display random effects distribution plots

- show_survival_plots:

  Display survival curves by groups

- show_forest_plot:

  Display forest plot of hazard ratios

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$modelSummary`             |     |     |     |     | a table  |
| `results$fixedEffects`             |     |     |     |     | a table  |
| `results$randomEffects`            |     |     |     |     | a table  |
| `results$varianceComponents`       |     |     |     |     | a table  |
| `results$randomEffectsPredictions` |     |     |     |     | a table  |
| `results$diagnostics`              |     |     |     |     | a table  |
| `results$modelComparison`          |     |     |     |     | a table  |
| `results$convergenceInfo`          |     |     |     |     | a table  |
| `results$hierarchicalStructure`    |     |     |     |     | a table  |
| `results$iccAnalysis`              |     |     |     |     | a table  |
| `results$residualPlots`            |     |     |     |     | an image |
| `results$randomEffectsPlots`       |     |     |     |     | an image |
| `results$survivalPlots`            |     |     |     |     | an image |
| `results$forestPlot`               |     |     |     |     | an image |
| `results$clusterAnalysisPlots`     |     |     |     |     | an image |
| `results$summaryTable`             |     |     |     |     | a html   |
| `results$methodExplanation`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
