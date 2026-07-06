# Continuous-Time Markov Models

Continuous-time Markov models for analyzing longitudinal data with
discrete states. Handles irregularly observed data and estimates
transition intensities between states.

## Usage

``` r
continuousmarkov(
  data,
  subject,
  time,
  state,
  covariates,
  time_covariates,
  model_structure = "full",
  baseline_hazard = "piecewise",
  n_intervals = 5,
  absorbing_states = "",
  initial_values = FALSE,
  optimization_method = "BFGS",
  calculate_sojourn = TRUE,
  transition_probabilities = TRUE,
  prevalence_estimates = TRUE,
  prediction_times = "1, 5, 10, 20",
  confidence_intervals = TRUE,
  bootstrap_se = FALSE,
  n_bootstrap = 100,
  plot_intensities = TRUE,
  plot_probabilities = TRUE,
  plot_prevalence = TRUE,
  model_selection = FALSE,
  goodness_of_fit = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- subject:

  Variable identifying individual subjects

- time:

  Time variable for observations

- state:

  Discrete state variable

- covariates:

  Variables affecting transition intensities

- time_covariates:

  Covariates that change over time

- model_structure:

  Defines which state transitions are possible

- baseline_hazard:

  Baseline hazard specification

- n_intervals:

  Time intervals for piecewise baseline

- absorbing_states:

  States from which no transitions are possible

- initial_values:

  Use custom initial parameter values

- optimization_method:

  Method for maximum likelihood estimation

- calculate_sojourn:

  Compute mean sojourn times

- transition_probabilities:

  Compute P(t) matrices over time

- prevalence_estimates:

  Estimate prevalence in each state

- prediction_times:

  Time points for probability predictions

- confidence_intervals:

  Include uncertainty estimates

- bootstrap_se:

  Bootstrap-based confidence intervals

- n_bootstrap:

  Bootstrap sample size

- plot_intensities:

  Generate intensity plots

- plot_probabilities:

  Generate probability plots

- plot_prevalence:

  Generate prevalence plots

- model_selection:

  Perform model comparison using AIC/BIC

- goodness_of_fit:

  Test model adequacy

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                    |     |     |     |     | a html   |
| `results$errors`                  |     |     |     |     | a html   |
| `results$warnings`                |     |     |     |     | a html   |
| `results$summary`                 |     |     |     |     | a html   |
| `results$dataStructure`           |     |     |     |     | a table  |
| `results$transitionMatrix`        |     |     |     |     | a table  |
| `results$intensityMatrix`         |     |     |     |     | a table  |
| `results$covariateEffects`        |     |     |     |     | a table  |
| `results$sojournTimes`            |     |     |     |     | a table  |
| `results$transitionProbabilities` |     |     |     |     | a table  |
| `results$prevalenceTable`         |     |     |     |     | a table  |
| `results$modelFit`                |     |     |     |     | a table  |
| `results$intensityPlot`           |     |     |     |     | an image |
| `results$probabilityPlot`         |     |     |     |     | an image |
| `results$prevalencePlot`          |     |     |     |     | an image |
| `results$modelComparison`         |     |     |     |     | a table  |
| `results$goodnessOfFit`           |     |     |     |     | a html   |
| `results$clinicalInterpretation`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataStructure$asDF`

`as.data.frame(results$dataStructure)`
