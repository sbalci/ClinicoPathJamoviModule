# Multistate Survival Models

Multistate models for analyzing disease progression through multiple
states. Implements transition probabilities, state occupation
probabilities, and competing transitions for complex disease pathways in
clinical research.

## Usage

``` r
multistatesurvival(
  data,
  id,
  time_start,
  time_stop,
  state_from,
  state_to,
  covariates,
  model_type = "markov",
  transition_matrix = "auto",
  state_names = "",
  initial_state = "1",
  absorbing_states = "",
  transition_probabilities = TRUE,
  state_probabilities = TRUE,
  sojourn_times = TRUE,
  hazard_ratios = TRUE,
  prediction_times = "6,12,24,60",
  plot_transitions = TRUE,
  plot_probabilities = TRUE,
  plot_cumhazard = TRUE,
  plot_individual = FALSE,
  competing_risks = TRUE,
  time_varying = FALSE,
  stratified,
  confidence_level = 0.95,
  bootstrap_ci = FALSE,
  n_bootstrap = 1000
)
```

## Arguments

- data:

  The data as a data frame.

- id:

  Patient ID variable for tracking individuals

- time_start:

  Start time of observation period

- time_stop:

  End time of observation period

- state_from:

  State at beginning of interval

- state_to:

  State at end of interval

- covariates:

  Variables influencing state transitions

- model_type:

  Multistate model type specification

- transition_matrix:

  Transition matrix specification

- state_names:

  Custom names for model states

- initial_state:

  Starting state specification

- absorbing_states:

  States from which no transitions occur

- transition_probabilities:

  Compute transition probability matrix

- state_probabilities:

  Compute state occupation probabilities

- sojourn_times:

  Compute mean sojourn times

- hazard_ratios:

  Compute transition-specific hazard ratios

- prediction_times:

  Times at which to evaluate probabilities

- plot_transitions:

  Generate transition diagram plot

- plot_probabilities:

  Generate probability curves

- plot_cumhazard:

  Generate cumulative hazard plots

- plot_individual:

  Create individual-level predictions

- competing_risks:

  Use competing risks framework

- time_varying:

  Include time-varying effects

- stratified:

  Stratification variable

- confidence_level:

  Confidence interval level

- bootstrap_ci:

  Bootstrap-based inference

- n_bootstrap:

  Bootstrap iterations

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                  |     |     |     |     | a html   |
| `results$summary`               |     |     |     |     | a html   |
| `results$transitionMatrix`      |     |     |     |     | a table  |
| `results$stateTable`            |     |     |     |     | a table  |
| `results$hazardTable`           |     |     |     |     | a table  |
| `results$sojournTable`          |     |     |     |     | a table  |
| `results$transitionPlot`        |     |     |     |     | an image |
| `results$probabilityPlot`       |     |     |     |     | an image |
| `results$cumhazardPlot`         |     |     |     |     | an image |
| `results$individualPredictions` |     |     |     |     | a table  |
| `results$modelFit`              |     |     |     |     | a table  |
| `results$stratifiedResults`     |     |     |     |     | a html   |
| `results$interpretation`        |     |     |     |     | a html   |
| `results$warnings`              |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$transitionMatrix$asDF`

`as.data.frame(results$transitionMatrix)`
