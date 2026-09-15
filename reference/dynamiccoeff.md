# Dynamic Coefficient Models

Implements dynamic coefficient models for survival data where regression
coefficients evolve continuously over time through adaptive updating
mechanisms. This approach provides real-time modeling of changing
covariate effects using Bayesian frameworks and dynamic linear models,
offering sophisticated alternatives to static coefficient approaches in
survival analysis.

## Usage

``` r
dynamiccoeff(
  data,
  elapsedtime,
  outcome,
  covariates,
  dynamic_covariates,
  outcomeLevel = "1",
  updating_method = "kalman",
  state_dimension = 2,
  process_variance = 0.1,
  observation_variance = 0.1,
  forgetting_factor = 0.99,
  burn_in_period = 10,
  confidence_level = 0.95,
  smoothing_parameter = 0.2,
  adaptation_rate = 0.1,
  show_model_summary = TRUE,
  show_coefficient_paths = TRUE,
  show_state_evolution = TRUE,
  show_adaptation_metrics = TRUE,
  show_dynamic_plots = TRUE,
  show_state_plots = TRUE,
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

  Covariate variables with constant effects over time

- dynamic_covariates:

  Covariate variables with dynamically updating coefficients

- outcomeLevel:

  Level of outcome variable indicating event occurrence

- updating_method:

  Method for dynamic coefficient updating

- state_dimension:

  Dimension of the state space for dynamic modeling

- process_variance:

  Variance of the process noise in state evolution

- observation_variance:

  Variance of the observation noise

- forgetting_factor:

  Forgetting factor for exponential discounting of past observations

- burn_in_period:

  Number of initial observations for model initialization

- confidence_level:

  Confidence level for dynamic coefficient intervals

- smoothing_parameter:

  Smoothing parameter for coefficient trajectory smoothing

- adaptation_rate:

  Rate of adaptation for dynamic coefficient updates

- show_model_summary:

  Display comprehensive dynamic model summary

- show_coefficient_paths:

  Display table of dynamic coefficient trajectories

- show_state_evolution:

  Display state space evolution results

- show_adaptation_metrics:

  Display adaptation and convergence metrics

- show_dynamic_plots:

  Display plots of dynamic coefficient evolution

- show_state_plots:

  Display state space visualization plots

- show_diagnostic_plots:

  Display model diagnostic and residual plots

- show_comparison_plots:

  Display comparison with static coefficient models

- showSummaries:

  Generate natural language summaries of the analysis results

- showExplanations:

  Show detailed explanations of the methodology and interpretation

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$todo`               |     |     |     |     | a html   |
| `results$modelSummary`       |     |     |     |     | a html   |
| `results$coefficientPaths`   |     |     |     |     | a table  |
| `results$stateEvolution`     |     |     |     |     | a table  |
| `results$adaptationMetrics`  |     |     |     |     | a table  |
| `results$modelComparison`    |     |     |     |     | a table  |
| `results$convergenceMetrics` |     |     |     |     | a table  |
| `results$dynamicPlots`       |     |     |     |     | an image |
| `results$statePlots`         |     |     |     |     | an image |
| `results$diagnosticPlots`    |     |     |     |     | an image |
| `results$comparisonPlots`    |     |     |     |     | an image |
| `results$adaptationPlots`    |     |     |     |     | an image |
| `results$analysisSummary`    |     |     |     |     | a html   |
| `results$methodExplanation`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficientPaths$asDF`

`as.data.frame(results$coefficientPaths)`
