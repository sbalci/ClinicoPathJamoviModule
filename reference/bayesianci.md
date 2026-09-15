# Bayesian Confidence Intervals (Credible Intervals)

Bayesian confidence intervals (credible intervals) as alternative to
frequentist confidence intervals. This analysis provides credible
intervals using Beta-posterior distributions, Monte Carlo sampling, and
various prior specifications for continuous and binary outcome data.
Essential for Bayesian statistical inference in clinical research.

## Usage

``` r
bayesianci(
  data,
  outcome,
  group_var,
  covariates,
  outcome_type = "continuous",
  credible_level = 0.95,
  additional_levels = "0.50,0.80,0.90",
  prior_type = "uniform",
  beta_alpha = 1,
  beta_beta = 1,
  normal_mean = 0,
  normal_sd = 1,
  gamma_shape = 1,
  gamma_rate = 1,
  mcmc_method = "analytical",
  mcmc_samples = 10000,
  burnin_samples = 2000,
  thinning = 1,
  chains = 3,
  posterior_summary = TRUE,
  convergence_diagnostics = TRUE,
  prior_posterior_comparison = TRUE,
  credible_vs_confidence = TRUE,
  hpd_intervals = TRUE,
  sensitivity_analysis = TRUE,
  robustness_check = TRUE,
  clinical_interpretation = TRUE,
  posterior_plots = TRUE,
  trace_plots = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- outcome:

  Outcome variable for credible interval estimation

- group_var:

  Grouping variable for stratified analysis

- covariates:

  Covariate variables for adjusted analysis

- outcome_type:

  Type of outcome variable for appropriate modeling

- credible_level:

  Level for Bayesian credible intervals

- additional_levels:

  Comma-separated additional credible levels to compute

- prior_type:

  Prior distribution specification for Bayesian analysis

- beta_alpha:

  Alpha parameter for Beta prior distribution

- beta_beta:

  Beta parameter for Beta prior distribution

- normal_mean:

  Mean parameter for Normal prior distribution

- normal_sd:

  Standard deviation for Normal prior distribution

- gamma_shape:

  Shape parameter for Gamma prior distribution

- gamma_rate:

  Rate parameter for Gamma prior distribution

- mcmc_method:

  Method for posterior sampling and credible interval computation

- mcmc_samples:

  Number of MCMC samples for posterior estimation

- burnin_samples:

  Number of burn-in samples to discard

- thinning:

  Thinning interval for MCMC chains

- chains:

  Number of parallel MCMC chains

- posterior_summary:

  Compute posterior mean, median, mode, and credible intervals

- convergence_diagnostics:

  Assess MCMC convergence using R-hat and effective sample size

- prior_posterior_comparison:

  Compare prior and posterior distributions

- credible_vs_confidence:

  Compare Bayesian credible intervals with frequentist confidence
  intervals

- hpd_intervals:

  Compute highest posterior density (HPD) credible intervals

- sensitivity_analysis:

  Assess sensitivity to prior specification

- robustness_check:

  Perform robustness checks with different prior specifications

- clinical_interpretation:

  Provide clinical interpretation of credible intervals

- posterior_plots:

  Create posterior distribution and credible interval visualizations

- trace_plots:

  Generate MCMC trace plots for convergence assessment

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$notices`                |     |     |     |     | a html   |
| `results$dataInfo`               |     |     |     |     | a table  |
| `results$priorSpecification`     |     |     |     |     | a table  |
| `results$posteriorSummary`       |     |     |     |     | a table  |
| `results$credibleIntervals`      |     |     |     |     | a table  |
| `results$hpdIntervals`           |     |     |     |     | a table  |
| `results$comparisonTable`        |     |     |     |     | a table  |
| `results$convergenceDiagnostics` |     |     |     |     | a table  |
| `results$sensitivityAnalysis`    |     |     |     |     | a table  |
| `results$robustnessCheck`        |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a table  |
| `results$posteriorPlot`          |     |     |     |     | an image |
| `results$priorPosteriorPlot`     |     |     |     |     | an image |
| `results$tracePlots`             |     |     |     |     | an image |
| `results$credibleComparisonPlot` |     |     |     |     | an image |
| `results$sensitivityPlot`        |     |     |     |     | an image |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
