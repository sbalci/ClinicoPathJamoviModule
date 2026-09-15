# Interval-Censored Survival Analysis

Interval-censored survival analysis for data where the exact event time
is unknown but falls within an interval. Common in periodic follow-up
studies, screening programs, and clinical trials with scheduled visits.

## Usage

``` r
intervalsurvival(
  left_time,
  right_time,
  status_var = NULL,
  covariates = NULL,
  stratification_vars = NULL,
  model_type = "aft_weibull",
  estimation_method = "em_algorithm",
  confidence_level = 0.95,
  convergence_tolerance = 1e-04,
  max_iterations = 500,
  baseline_hazard_smooth = FALSE,
  smoothing_bandwidth = 1,
  survival_curves = TRUE,
  hazard_function = FALSE,
  model_diagnostics = TRUE,
  residual_analysis = TRUE,
  goodness_of_fit = TRUE,
  model_comparison = FALSE,
  prediction_times = "12,24,36,60",
  bootstrap_samples = 200,
  mcmc_iterations = 5000,
  mcmc_burnin = 1000,
  imputation_method = "midpoint"
)
```

## Arguments

- left_time:

  Left boundary of the censoring interval

- right_time:

  Right boundary of the censoring interval

- status_var:

  Censoring status indicator variable

- covariates:

  Covariates to include in the survival model

- stratification_vars:

  Variables for stratifying the baseline hazard

- model_type:

  Model specification for interval-censored survival

- estimation_method:

  Method for parameter estimation

- confidence_level:

  Confidence level for parameter estimates

- convergence_tolerance:

  Convergence criterion for iterative algorithms

- max_iterations:

  Maximum number of algorithm iterations

- baseline_hazard_smooth:

  Whether to smooth the baseline hazard function

- smoothing_bandwidth:

  Bandwidth for baseline hazard smoothing

- survival_curves:

  Whether to plot survival curves

- hazard_function:

  Whether to plot hazard functions

- model_diagnostics:

  Whether to include model diagnostics

- residual_analysis:

  Whether to perform residual analysis

- goodness_of_fit:

  Whether to test model goodness-of-fit

- model_comparison:

  Whether to compare multiple models

- prediction_times:

  Time points for survival probability predictions

- bootstrap_samples:

  Number of bootstrap resamples

- mcmc_iterations:

  MCMC sampling iterations

- mcmc_burnin:

  MCMC burn-in period

- imputation_method:

  Imputation approach for incomplete intervals

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`              |     |     |     |     | a html   |
| `results$data_summary`              |     |     |     |     | a html   |
| `results$model_results`             |     |     |     |     | a html   |
| `results$covariate_effects`         |     |     |     |     | a html   |
| `results$survival_estimates`        |     |     |     |     | a html   |
| `results$hazard_estimates`          |     |     |     |     | a html   |
| `results$model_diagnostics_summary` |     |     |     |     | a html   |
| `results$goodness_of_fit_results`   |     |     |     |     | a html   |
| `results$model_comparison_table`    |     |     |     |     | a html   |
| `results$residual_summary`          |     |     |     |     | a html   |
| `results$survival_curves_plot`      |     |     |     |     | an image |
| `results$hazard_plot`               |     |     |     |     | an image |
| `results$interval_plot`             |     |     |     |     | an image |
| `results$residual_plots`            |     |     |     |     | an image |
| `results$model_comparison_plot`     |     |     |     |     | an image |
| `results$convergence_plot`          |     |     |     |     | an image |
