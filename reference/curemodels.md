# Cure Models for Long-term Survivors

Cure models for analyzing survival data with a cured fraction of
long-term survivors. Implements mixture cure models and non-mixture cure
models for oncology research.

## Usage

``` r
curemodels(
  data,
  time,
  status,
  predictors,
  model_type = "mixture",
  smcure_model_type = "ph",
  cure_link = "logit",
  survival_dist = "weibull",
  bootstrap_ci = FALSE,
  n_bootstrap = 1000,
  cure_threshold = 60,
  plot_cure_fraction = FALSE,
  plot_survival = FALSE,
  goodness_of_fit = FALSE,
  sensitivity_analysis = FALSE,
  use_background_mortality = FALSE,
  background_hazard_var = NULL,
  npcure_covariate = NULL,
  npcure_bandwidth = "auto",
  npcure_time_points = 100
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Follow-up time variable

- status:

  Event status variable

- predictors:

  Predictor variables for the model

- model_type:

  Type of cure model: mixture (smcure), non-mixture (flexsurvcure),
  cuRe, npcure, or all

- smcure_model_type:

  Survival model type for smcure: 'ph' (proportional hazards) or 'aft'
  (accelerated failure time)

- cure_link:

  Link function for the cure probability model

- survival_dist:

  Distribution for modeling survival of uncured patients

- bootstrap_ci:

  Use bootstrap for confidence interval estimation

- n_bootstrap:

  Number of bootstrap samples

- cure_threshold:

  Time threshold for defining cured patients

- plot_cure_fraction:

  Generate cure fraction plot

- plot_survival:

  Generate survival curve plots

- goodness_of_fit:

  Conduct goodness of fit assessment

- sensitivity_analysis:

  Conduct sensitivity analysis

- use_background_mortality:

  Enable background mortality integration using cuRe package

- background_hazard_var:

  Background hazard variable for cuRe models

- npcure_covariate:

  Continuous covariate for npcure model (limited to one variable)

- npcure_bandwidth:

  Bandwidth parameter for npcure kernel smoothing

- npcure_time_points:

  Number of time points for testimate in npcure

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$warnings`            |     |     |     |     | a html   |
| `results$errors`              |     |     |     |     | a html   |
| `results$summary`             |     |     |     |     | a html   |
| `results$modelTable`          |     |     |     |     | a table  |
| `results$cureTable`           |     |     |     |     | a table  |
| `results$cureFractionPlot`    |     |     |     |     | an image |
| `results$survivalPlot`        |     |     |     |     | an image |
| `results$goodnessOfFit`       |     |     |     |     | a table  |
| `results$sensitivityAnalysis` |     |     |     |     | a html   |
| `results$modelComparison`     |     |     |     |     | a table  |
| `results$interpretation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelTable$asDF`

`as.data.frame(results$modelTable)`
