# Flexible Multi-State Survival Models

Flexible parametric multi-state survival models for complex disease
progression. Models transition hazards using splines and handles
competing risks and intermediate states.

## Usage

``` r
flexmultistate(
  data,
  time,
  status,
  start_state = NULL,
  predictors,
  model_type = "illness_death",
  hazard_distribution = "royston_parmar",
  spline_knots = 3,
  time_scale = "hazard",
  transition_specific = TRUE,
  shared_effects,
  time_varying_effects,
  prediction_times = "12, 24, 36, 60",
  transition_probabilities = TRUE,
  state_probabilities = TRUE,
  expected_sojourn = TRUE,
  plot_hazards = TRUE,
  plot_cumhaz = TRUE,
  plot_survival = TRUE,
  microsimulation = FALSE,
  n_simulation = 10000,
  confidence_intervals = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time variable for transitions

- status:

  State or transition status variable

- start_state:

  Variable indicating starting state

- predictors:

  Variables to include in transition models

- model_type:

  Multi-state model configuration

- hazard_distribution:

  Hazard function specification

- spline_knots:

  Spline complexity parameter

- time_scale:

  Transformation scale for splines

- transition_specific:

  Fit separate models for each transition type

- shared_effects:

  Predictors with constrained effects

- time_varying_effects:

  Predictors with time-dependent coefficients

- prediction_times:

  Time points for state occupancy predictions

- transition_probabilities:

  Compute state transition probabilities

- state_probabilities:

  Compute state occupancy probabilities

- expected_sojourn:

  Compute expected sojourn times

- plot_hazards:

  Generate hazard function plots

- plot_cumhaz:

  Generate cumulative hazard plots

- plot_survival:

  Generate state probability plots

- microsimulation:

  Use simulation for state predictions

- n_simulation:

  Sample size for microsimulation

- confidence_intervals:

  Compute uncertainty estimates

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                   |     |     |     |     | a html   |
| `results$summary`                |     |     |     |     | a html   |
| `results$transitionMatrix`       |     |     |     |     | a table  |
| `results$transitionModels`       |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$transitionProbTable`    |     |     |     |     | a table  |
| `results$stateProbTable`         |     |     |     |     | a table  |
| `results$sojournTable`           |     |     |     |     | a table  |
| `results$hazardPlot`             |     |     |     |     | an image |
| `results$cumhazPlot`             |     |     |     |     | an image |
| `results$survivalPlot`           |     |     |     |     | an image |
| `results$transitionDiagram`      |     |     |     |     | an image |
| `results$microsimResults`        |     |     |     |     | a html   |
| `results$goodnessOfFit`          |     |     |     |     | a html   |
| `results$clinicalInterpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$transitionMatrix$asDF`

`as.data.frame(results$transitionMatrix)`
