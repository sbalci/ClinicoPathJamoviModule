# Adaptive Trial Design Methods

Advanced adaptive trial design methods for clinical research including
Bayesian interim analysis, adaptive sample size re-estimation, futility
stopping rules, and group sequential designs. Designed for efficient
clinical trial conduct with ethical early stopping capabilities.

## Usage

``` r
adaptivetrialdesign(
  data,
  outcome,
  treatment,
  outcomeLevel,
  stratification_variables = NULL,
  time_variable = NULL,
  adaptation_type = "sample_size",
  design_framework = "bayesian",
  interim_analysis = TRUE,
  interim_timing = "information",
  interim_fractions = "0.25,0.50,0.75",
  max_interim_analyses = 3,
  efficacy_boundary = 0.95,
  futility_boundary = 0.1,
  boundary_type = "obrien_fleming",
  planned_sample_size = 100,
  minimum_effect_size = 0.5,
  target_power = 0.8,
  type1_error_rate = 0.05,
  max_sample_size_inflation = 2,
  prior_type = "weakly_informative",
  historical_data_weight = 0.1,
  prior_parameters = "",
  decision_criterion = "posterior_probability",
  bayes_factor_threshold = 3,
  predictive_power_threshold = 0.8,
  run_simulations = TRUE,
  n_simulations = 1000,
  true_effect_scenarios = "0,0.3,0.5,0.8",
  mcmc_samples = 5000,
  mcmc_burnin = 2000,
  mcmc_chains = 3,
  show_design_summary = TRUE,
  show_interim_results = TRUE,
  show_stopping_boundaries = TRUE,
  show_sample_size_evolution = TRUE,
  show_operating_characteristics = TRUE,
  show_posterior_evolution = TRUE,
  show_decision_analysis = TRUE,
  show_interpretation = TRUE,
  alpha_spending_function = "obf_type",
  spending_parameter = 1,
  blinded_sample_size_reestimation = FALSE,
  conditional_power_threshold = 0.2,
  clinical_context = "general",
  set_seed = TRUE,
  seed_value = 42,
  regulatory_framework = "general",
  dmb_recommendations = TRUE
)
```

## Arguments

- data:

  The data as a data frame for adaptive trial design analysis.

- outcome:

  .

- treatment:

  .

- outcomeLevel:

  .

- stratification_variables:

  .

- time_variable:

  For time-to-event endpoints

- adaptation_type:

  .

- design_framework:

  .

- interim_analysis:

  .

- interim_timing:

  .

- interim_fractions:

  Comma-separated fractions of planned information/sample size

- max_interim_analyses:

  .

- efficacy_boundary:

  Posterior probability threshold for efficacy stopping

- futility_boundary:

  Posterior probability threshold for futility stopping

- boundary_type:

  .

- planned_sample_size:

  .

- minimum_effect_size:

  Standardized or raw effect size

- target_power:

  .

- type1_error_rate:

  .

- max_sample_size_inflation:

  Maximum fold increase from planned sample size

- prior_type:

  .

- historical_data_weight:

  Weight given to historical data in informative priors

- prior_parameters:

  Comma-separated prior parameters (mean,sd for normal)

- decision_criterion:

  .

- bayes_factor_threshold:

  .

- predictive_power_threshold:

  .

- run_simulations:

  .

- n_simulations:

  .

- true_effect_scenarios:

  Comma-separated effect sizes for simulation

- mcmc_samples:

  .

- mcmc_burnin:

  .

- mcmc_chains:

  .

- show_design_summary:

  .

- show_interim_results:

  .

- show_stopping_boundaries:

  .

- show_sample_size_evolution:

  .

- show_operating_characteristics:

  .

- show_posterior_evolution:

  .

- show_decision_analysis:

  .

- show_interpretation:

  .

- alpha_spending_function:

  .

- spending_parameter:

  .

- blinded_sample_size_reestimation:

  .

- conditional_power_threshold:

  .

- clinical_context:

  .

- set_seed:

  .

- seed_value:

  .

- regulatory_framework:

  .

- dmb_recommendations:

  Generate templates for Data Monitoring Board recommendations

## Value

A results object containing:

|                                        |     |     |     |     |          |
|----------------------------------------|-----|-----|-----|-----|----------|
| `results$designSummary`                |     |     |     |     | a table  |
| `results$interimResults`               |     |     |     |     | a table  |
| `results$stoppingBoundaries`           |     |     |     |     | a table  |
| `results$sampleSizeEvolution`          |     |     |     |     | a table  |
| `results$operatingCharacteristics`     |     |     |     |     | a table  |
| `results$posteriorEvolution`           |     |     |     |     | a table  |
| `results$decisionAnalysis`             |     |     |     |     | a table  |
| `results$bayesFactors`                 |     |     |     |     | a table  |
| `results$predictivePower`              |     |     |     |     | a table  |
| `results$regulatoryConsiderations`     |     |     |     |     | a table  |
| `results$dmbRecommendations`           |     |     |     |     | a table  |
| `results$clinicalInterpretation`       |     |     |     |     | a html   |
| `results$methodsExplanation`           |     |     |     |     | a html   |
| `results$stoppingBoundaryPlot`         |     |     |     |     | an image |
| `results$sampleSizeEvolutionPlot`      |     |     |     |     | an image |
| `results$posteriorEvolutionPlot`       |     |     |     |     | an image |
| `results$operatingCharacteristicsPlot` |     |     |     |     | an image |
| `results$decisionAnalysisPlot`         |     |     |     |     | an image |
| `results$predictivePowerPlot`          |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$designSummary$asDF`

`as.data.frame(results$designSummary)`
