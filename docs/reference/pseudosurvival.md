# Pseudo-Observations Survival Methods

Pseudo-observations methods for direct modeling of survival
probabilities and restricted mean survival time (RMST). Enables
regression analysis of survival probability at specific time points and
RMST with standard statistical methods.

## Usage

``` r
pseudosurvival(
  time_var,
  status_var,
  covariates = NULL,
  stratification_vars = NULL,
  analysis_type = "survival_probability",
  time_points = "12,24,36,60",
  tau_rmst = 60,
  quantile_probs = "0.25,0.5,0.75",
  jackknife_method = "standard",
  regression_method = "ols",
  cluster_var = NULL,
  confidence_level = 0.95,
  bootstrap_samples = 500,
  robust_se = FALSE,
  model_diagnostics = TRUE,
  prediction_intervals = FALSE,
  survival_curves = TRUE,
  rmst_comparison = TRUE,
  sensitivity_analysis = FALSE,
  competing_risks = FALSE,
  weighted_analysis = FALSE
)
```

## Arguments

- time_var:

  Survival time variable

- status_var:

  Event status indicator variable

- covariates:

  Covariates to include in the pseudo-regression model

- stratification_vars:

  Variables for stratifying the analysis

- analysis_type:

  Analysis approach using pseudo-observations

- time_points:

  Time points for pseudo-observation calculation

- tau_rmst:

  Maximum follow-up time for RMST analysis

- quantile_probs:

  Survival quantiles for pseudo-observation analysis

- jackknife_method:

  Method for computing pseudo-observations

- regression_method:

  Statistical method for pseudo-observation regression

- cluster_var:

  Variable defining clusters for GEE or cluster jackknife

- confidence_level:

  Confidence level for parameter estimates

- bootstrap_samples:

  Bootstrap resamples for standard error estimation

- robust_se:

  Whether to compute robust standard errors

- model_diagnostics:

  Whether to include model diagnostics

- prediction_intervals:

  Whether to compute prediction intervals

- survival_curves:

  Whether to plot survival probability curves

- rmst_comparison:

  Whether to perform RMST group comparisons

- sensitivity_analysis:

  Whether to perform sensitivity analysis

- competing_risks:

  Whether to include competing risks modeling

- weighted_analysis:

  Whether to apply inverse probability weighting

## Value

A results object containing:

|                                        |     |     |     |     |          |
|----------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                 |     |     |     |     | a html   |
| `results$pseudo_summary`               |     |     |     |     | a html   |
| `results$regression_results`           |     |     |     |     | a html   |
| `results$covariate_effects`            |     |     |     |     | a html   |
| `results$rmst_analysis`                |     |     |     |     | a html   |
| `results$survival_probability_results` |     |     |     |     | a html   |
| `results$group_comparisons`            |     |     |     |     | a html   |
| `results$model_diagnostics_summary`    |     |     |     |     | a html   |
| `results$sensitivity_results`          |     |     |     |     | a html   |
| `results$pseudo_observations_plot`     |     |     |     |     | an image |
| `results$regression_plots`             |     |     |     |     | an image |
| `results$survival_curves_plot`         |     |     |     |     | an image |
| `results$rmst_comparison_plot`         |     |     |     |     | an image |
| `results$diagnostics_plots`            |     |     |     |     | an image |
| `results$sensitivity_plot`             |     |     |     |     | an image |
