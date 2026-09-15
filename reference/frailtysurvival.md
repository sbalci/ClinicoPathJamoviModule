# Frailty & Random Effects Survival Models

Frailty and random effects survival models for clustered data analysis.
Includes gamma frailty, shared frailty, and correlated frailty models
for multi-center studies, family studies, and recurrent events.

## Usage

``` r
frailtysurvival(
  time_var,
  status_var,
  cluster_var,
  covariates = NULL,
  strata_vars = NULL,
  frailty_type = "shared",
  frailty_distribution = "gamma",
  estimation_method = "penalized_likelihood",
  confidence_level = 0.95,
  baseline_hazard = "cox",
  random_effects = NULL,
  test_frailty = TRUE,
  variance_components = TRUE,
  cluster_diagnostics = TRUE,
  prediction_intervals = FALSE,
  survival_curves = TRUE,
  residual_analysis = TRUE,
  model_comparison = TRUE,
  mcmc_iterations = 10000,
  burnin_samples = 2000,
  convergence_diagnostics = TRUE
)
```

## Arguments

- time_var:

  Survival time variable

- status_var:

  Event status indicator variable

- cluster_var:

  Variable defining clusters for frailty modeling

- covariates:

  Covariates to include in the survival model

- strata_vars:

  Variables for stratifying the baseline hazard

- frailty_type:

  Frailty model type for cluster dependencies

- frailty_distribution:

  Probability distribution for frailty effects

- estimation_method:

  Statistical estimation approach

- confidence_level:

  Confidence level for parameter estimates

- baseline_hazard:

  Baseline hazard function choice

- random_effects:

  Additional random effects variables

- test_frailty:

  Whether to test for significant frailty

- variance_components:

  Whether to analyze variance components

- cluster_diagnostics:

  Whether to generate cluster diagnostics

- prediction_intervals:

  Whether to compute prediction intervals

- survival_curves:

  Whether to plot cluster-specific curves

- residual_analysis:

  Whether to include residual analysis

- model_comparison:

  Whether to compare with standard models

- mcmc_iterations:

  MCMC sampling iterations

- burnin_samples:

  MCMC burn-in period

- convergence_diagnostics:

  Whether to assess MCMC convergence

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`              |     |     |     |     | a html   |
| `results$model_summary`             |     |     |     |     | a html   |
| `results$frailty_effects`           |     |     |     |     | a html   |
| `results$variance_components_table` |     |     |     |     | a html   |
| `results$covariate_effects`         |     |     |     |     | a html   |
| `results$frailty_test`              |     |     |     |     | a html   |
| `results$model_comparison_table`    |     |     |     |     | a html   |
| `results$cluster_summary`           |     |     |     |     | a html   |
| `results$convergence_summary`       |     |     |     |     | a html   |
| `results$survival_curves_plot`      |     |     |     |     | an image |
| `results$frailty_distribution_plot` |     |     |     |     | an image |
| `results$cluster_effects_plot`      |     |     |     |     | an image |
| `results$residual_plots`            |     |     |     |     | an image |
| `results$variance_components_plot`  |     |     |     |     | an image |
| `results$mcmc_diagnostics_plot`     |     |     |     |     | an image |
