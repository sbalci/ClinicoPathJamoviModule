# Bayesian Model Averaging

Bayesian Model Averaging for survival analysis

## Usage

``` r
bayesianma(
  data,
  time_var,
  event_var,
  pred_vars,
  prior_type = "uniform",
  prior_inclusion_prob = 0.5,
  beta_alpha = 1,
  beta_beta = 1,
  complexity_penalty = 1,
  mcmc_method = "mc3",
  mcmc_chains = 3,
  mcmc_iterations = 5000,
  burn_in = 1000,
  thinning = 1,
  convergence_diagnostic = "gelman_rubin",
  model_selection_method = "highest_posterior",
  occam_ratio = 20,
  temperature_ladder = "1.0,1.5,2.0,3.0",
  proposal_variance = 1,
  variable_selection_threshold = 0.5,
  uncertainty_quantification = TRUE,
  prediction_intervals = TRUE,
  credible_level = 0.95,
  model_diagnostics = TRUE,
  sensitivity_analysis = FALSE,
  cross_validation = FALSE,
  cv_folds = 5,
  parallel_processing = FALSE,
  seed_value = 42,
  show_summary = TRUE,
  show_model_space = TRUE,
  show_coefficients = TRUE,
  show_inclusion_probs = TRUE,
  show_top_models = TRUE,
  show_diagnostics = TRUE,
  show_predictions = FALSE,
  plot_model_probs = TRUE,
  plot_inclusion_probs = TRUE,
  plot_coefficients = TRUE,
  plot_convergence = TRUE,
  plot_model_space = FALSE,
  plot_predictions = FALSE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time_var:

  the time-to-event variable

- event_var:

  the event indicator variable (0/1 or FALSE/TRUE)

- pred_vars:

  the predictor variables for model averaging

- prior_type:

  type of prior distribution for model space

- prior_inclusion_prob:

  prior probability of variable inclusion (for uniform/beta-binomial
  priors)

- beta_alpha:

  alpha parameter for beta-binomial prior

- beta_beta:

  beta parameter for beta-binomial prior

- complexity_penalty:

  penalty parameter for complexity prior

- mcmc_method:

  MCMC sampling method for model space exploration

- mcmc_chains:

  number of MCMC chains

- mcmc_iterations:

  number of MCMC iterations per chain

- burn_in:

  number of burn-in iterations

- thinning:

  thinning interval for MCMC samples

- convergence_diagnostic:

  convergence diagnostic method

- model_selection_method:

  method for selecting representative model

- occam_ratio:

  ratio for Occam's window model selection

- temperature_ladder:

  temperature ladder for MC³ (comma-separated values). Higher
  temperatures enable better mixing

- proposal_variance:

  variance for parameter proposal distributions

- variable_selection_threshold:

  posterior probability threshold for variable selection

- uncertainty_quantification:

  whether to perform comprehensive uncertainty quantification

- prediction_intervals:

  whether to compute prediction intervals

- credible_level:

  level for credible intervals

- model_diagnostics:

  whether to perform comprehensive model diagnostics

- sensitivity_analysis:

  whether to perform prior sensitivity analysis

- cross_validation:

  whether to perform cross-validation assessment

- cv_folds:

  number of folds for cross-validation

- parallel_processing:

  whether to use parallel processing for MCMC

- seed_value:

  random seed for reproducible results

- show_summary:

  show model averaging summary

- show_model_space:

  show model space exploration results

- show_coefficients:

  show model-averaged coefficient estimates

- show_inclusion_probs:

  show posterior inclusion probabilities

- show_top_models:

  show highest probability models

- show_diagnostics:

  show MCMC convergence diagnostics

- show_predictions:

  show model-averaged predictions

- plot_model_probs:

  plot model posterior probabilities

- plot_inclusion_probs:

  plot variable inclusion probabilities

- plot_coefficients:

  plot posterior coefficient distributions

- plot_convergence:

  plot MCMC convergence diagnostics

- plot_model_space:

  plot model space exploration

- plot_predictions:

  plot model-averaged predictions

- showExplanations:

  show explanations for the analysis

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`              |     |     |     |     | a html   |
| `results$todo`                      |     |     |     |     | a html   |
| `results$summary`                   |     |     |     |     | a table  |
| `results$modelSpace`                |     |     |     |     | a table  |
| `results$averagedCoefficients`      |     |     |     |     | a table  |
| `results$inclusionProbabilities`    |     |     |     |     | a table  |
| `results$topModels`                 |     |     |     |     | a table  |
| `results$mcmcDiagnostics`           |     |     |     |     | a table  |
| `results$selectedModel`             |     |     |     |     | a table  |
| `results$uncertaintyQuantification` |     |     |     |     | a table  |
| `results$sensitivityAnalysis`       |     |     |     |     | a table  |
| `results$crossValidation`           |     |     |     |     | a table  |
| `results$modelProbsPlot`            |     |     |     |     | an image |
| `results$inclusionProbsPlot`        |     |     |     |     | an image |
| `results$coefficientsPlot`          |     |     |     |     | an image |
| `results$convergencePlot`           |     |     |     |     | an image |
| `results$modelSpacePlot`            |     |     |     |     | an image |
| `results$predictionsPlot`           |     |     |     |     | an image |
| `results$explanations`              |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
