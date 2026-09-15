# Spike-and-Slab Variable Selection

Advanced Bayesian variable selection using spike-and-slab priors for
high-dimensional data analysis. Supports regression, classification, and
survival analysis with automatic variable selection, model averaging,
and uncertainty quantification for clinical biomarker discovery.

## Usage

``` r
spikeslabpriors(
  data,
  outcome,
  predictors,
  outcomeLevel,
  time_variable = NULL,
  status_variable = NULL,
  model_type = "regression",
  spike_slab_type = "binary",
  prior_inclusion_prob = 0.5,
  expected_model_size = 5,
  spike_variance = 0.001,
  slab_variance = 1,
  hyperprior_type = "empirical_bayes",
  hyperprior_alpha = 1,
  hyperprior_beta = 1,
  selection_criterion = "median_probability",
  inclusion_threshold = 0.5,
  bayes_factor_threshold = 3,
  mcmc_samples = 10000,
  mcmc_burnin = 5000,
  mcmc_thin = 1,
  mcmc_chains = 3,
  model_averaging = TRUE,
  prediction_method = "bma",
  cross_validation = FALSE,
  cv_folds = 10,
  standardize_predictors = TRUE,
  center_predictors = TRUE,
  max_model_size = 50,
  dimension_reduction = "none",
  prescreening_threshold = 0.1,
  show_variable_selection = TRUE,
  show_inclusion_probabilities = TRUE,
  show_model_probabilities = TRUE,
  show_coefficient_estimates = TRUE,
  show_prediction_performance = TRUE,
  show_convergence_diagnostics = TRUE,
  show_variable_importance = TRUE,
  show_model_comparison = FALSE,
  show_interpretation = TRUE,
  interaction_terms = FALSE,
  max_interactions = 2,
  heredity_constraint = "weak",
  group_variables = FALSE,
  group_structure = NULL,
  clinical_context = "biomarker_discovery",
  confidence_level = 0.95,
  set_seed = TRUE,
  seed_value = 42,
  parallel_processing = TRUE,
  n_cores = 2
)
```

## Arguments

- data:

  The data as a data frame for spike-and-slab variable selection.

- outcome:

  .

- predictors:

  .

- outcomeLevel:

  .

- time_variable:

  For survival analysis

- status_variable:

  For survival analysis

- model_type:

  .

- spike_slab_type:

  .

- prior_inclusion_prob:

  Prior probability that each variable is included

- expected_model_size:

  Expected number of active predictors

- spike_variance:

  Variance for spike component (near-zero effects)

- slab_variance:

  Variance for slab component (non-zero effects)

- hyperprior_type:

  .

- hyperprior_alpha:

  .

- hyperprior_beta:

  .

- selection_criterion:

  .

- inclusion_threshold:

  .

- bayes_factor_threshold:

  .

- mcmc_samples:

  .

- mcmc_burnin:

  .

- mcmc_thin:

  .

- mcmc_chains:

  .

- model_averaging:

  .

- prediction_method:

  .

- cross_validation:

  .

- cv_folds:

  .

- standardize_predictors:

  .

- center_predictors:

  .

- max_model_size:

  Maximum number of variables to include

- dimension_reduction:

  .

- prescreening_threshold:

  .

- show_variable_selection:

  .

- show_inclusion_probabilities:

  .

- show_model_probabilities:

  .

- show_coefficient_estimates:

  .

- show_prediction_performance:

  .

- show_convergence_diagnostics:

  .

- show_variable_importance:

  .

- show_model_comparison:

  .

- show_interpretation:

  .

- interaction_terms:

  .

- max_interactions:

  .

- heredity_constraint:

  .

- group_variables:

  .

- group_structure:

  .

- clinical_context:

  .

- confidence_level:

  .

- set_seed:

  .

- seed_value:

  .

- parallel_processing:

  .

- n_cores:

  .

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$variableSelection`          |     |     |     |     | a table  |
| `results$inclusionProbabilities`     |     |     |     |     | a table  |
| `results$modelProbabilities`         |     |     |     |     | a table  |
| `results$coefficientEstimates`       |     |     |     |     | a table  |
| `results$predictionPerformance`      |     |     |     |     | a table  |
| `results$convergenceDiagnostics`     |     |     |     |     | a table  |
| `results$variableImportance`         |     |     |     |     | a table  |
| `results$modelComparison`            |     |     |     |     | a table  |
| `results$dimensionSummary`           |     |     |     |     | a table  |
| `results$groupSelection`             |     |     |     |     | a table  |
| `results$clinicalInterpretation`     |     |     |     |     | a html   |
| `results$methodsExplanation`         |     |     |     |     | a html   |
| `results$inclusionProbabilityPlot`   |     |     |     |     | an image |
| `results$modelSizePlot`              |     |     |     |     | an image |
| `results$coefficientPlot`            |     |     |     |     | an image |
| `results$variableImportancePlot`     |     |     |     |     | an image |
| `results$convergencePlots`           |     |     |     |     | an image |
| `results$posteriorDistributionPlots` |     |     |     |     | an image |
| `results$modelComparisonPlot`        |     |     |     |     | an image |
| `results$predictionPerformancePlot`  |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$variableSelection$asDF`

`as.data.frame(results$variableSelection)`
