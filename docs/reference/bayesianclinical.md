# Bayesian Clinical Analysis & Decision Making

Comprehensive Bayesian analysis for clinical research and medical
decision making using tabular clinical data. Implements Bayesian
hypothesis testing, parameter estimation, predictive modeling, and
decision analysis. Provides intuitive interpretation of evidence,
uncertainty quantification, and clinical decision support. Essential for
evidence-based medicine, clinical guidelines, and personalized treatment
decisions with proper uncertainty communication.

## Usage

``` r
bayesianclinical(
  data,
  outcome_var,
  treatment_var,
  covariates,
  patient_id,
  time_var,
  center_var,
  analysis_type = "treatment_effect",
  outcome_type = "continuous",
  prior_type = "weakly_informative",
  prior_mean = 0,
  prior_sd = 1,
  historical_data_weight = 0.1,
  mcmc_chains = 4,
  mcmc_iterations = 2000,
  mcmc_warmup = 1000,
  mcmc_thinning = 1,
  credible_interval = 0.95,
  rope_lower = -0.1,
  rope_upper = 0.1,
  minimum_effect_size = 0.2,
  decision_analysis = FALSE,
  benefit_utility = 1,
  harm_utility = -0.5,
  cost_consideration = FALSE,
  model_diagnostics = TRUE,
  convergence_diagnostics = TRUE,
  posterior_predictive_checks = TRUE,
  leave_one_out_cv = FALSE,
  hierarchical_modeling = FALSE,
  mixture_modeling = FALSE,
  nonparametric_methods = FALSE,
  adaptive_design = FALSE,
  model_comparison = FALSE,
  bayes_factor_analysis = TRUE,
  evidence_thresholds = TRUE,
  probability_statements = TRUE,
  prediction_intervals = TRUE,
  sensitivity_to_priors = FALSE,
  posterior_plots = TRUE,
  trace_plots = TRUE,
  forest_plots = FALSE,
  decision_plots = FALSE,
  comprehensive_report = TRUE,
  clinical_interpretation = TRUE,
  regulatory_documentation = FALSE,
  layman_summary = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- outcome_var:

  Primary clinical outcome of interest

- treatment_var:

  Treatment group or intervention variable

- covariates:

  Clinical covariates and baseline characteristics

- patient_id:

  Patient identifier for hierarchical models

- time_var:

  Time variable for longitudinal Bayesian analysis

- center_var:

  Center or site variable for hierarchical models

- analysis_type:

  Type of Bayesian analysis to perform

- outcome_type:

  Type of outcome variable

- prior_type:

  Type of prior distributions to use

- prior_mean:

  Prior mean for treatment effect or main parameter

- prior_sd:

  Prior standard deviation for main parameter

- historical_data_weight:

  Weight for historical data in prior (0=no weight, 1=full weight)

- mcmc_chains:

  Number of MCMC chains

- mcmc_iterations:

  Number of MCMC iterations per chain

- mcmc_warmup:

  Number of warmup (burn-in) iterations

- mcmc_thinning:

  Thinning interval for MCMC samples

- credible_interval:

  Level for Bayesian credible intervals

- rope_lower:

  Lower bound for Region of Practical Equivalence (ROPE)

- rope_upper:

  Upper bound for Region of Practical Equivalence (ROPE)

- minimum_effect_size:

  Minimum effect size considered clinically important

- decision_analysis:

  Perform Bayesian decision analysis with utilities

- benefit_utility:

  Utility value for treatment benefit

- harm_utility:

  Utility value for treatment harm (negative)

- cost_consideration:

  Include cost considerations in decision analysis

- model_diagnostics:

  Perform MCMC diagnostics and model validation

- convergence_diagnostics:

  Check MCMC convergence (R-hat, ESS, etc.)

- posterior_predictive_checks:

  Perform posterior predictive model checking

- leave_one_out_cv:

  Perform LOO-CV for model comparison

- hierarchical_modeling:

  Use hierarchical/multilevel Bayesian models

- mixture_modeling:

  Use Bayesian mixture models for heterogeneity

- nonparametric_methods:

  Use nonparametric Bayesian approaches

- adaptive_design:

  Perform adaptive trial design analysis

- model_comparison:

  Compare multiple Bayesian models

- bayes_factor_analysis:

  Calculate Bayes factors for hypothesis testing

- evidence_thresholds:

  Classify evidence strength using standard thresholds

- probability_statements:

  Generate clinically relevant probability statements

- prediction_intervals:

  Calculate Bayesian prediction intervals

- sensitivity_to_priors:

  Assess sensitivity to prior specifications

- posterior_plots:

  Generate posterior distribution visualizations

- trace_plots:

  Generate MCMC trace plots for diagnostics

- forest_plots:

  Generate Bayesian forest plots for meta-analysis

- decision_plots:

  Generate decision analysis visualization

- comprehensive_report:

  Generate comprehensive Bayesian analysis report

- clinical_interpretation:

  Provide clinical interpretation of Bayesian results

- regulatory_documentation:

  Include regulatory-compliant documentation

- layman_summary:

  Provide plain language summary of results

## Value

A results object containing:

|                                          |     |     |     |     |          |
|------------------------------------------|-----|-----|-----|-----|----------|
| `results$bayesian_overview`              |     |     |     |     | a table  |
| `results$posterior_summary`              |     |     |     |     | a table  |
| `results$treatment_effect_analysis`      |     |     |     |     | a table  |
| `results$bayes_factors`                  |     |     |     |     | a table  |
| `results$model_diagnostics_summary`      |     |     |     |     | a table  |
| `results$prior_posterior_comparison`     |     |     |     |     | a table  |
| `results$decision_analysis_results`      |     |     |     |     | a table  |
| `results$predictive_intervals`           |     |     |     |     | a table  |
| `results$sensitivity_to_priors`          |     |     |     |     | a table  |
| `results$probability_statements`         |     |     |     |     | a table  |
| `results$model_comparison`               |     |     |     |     | a table  |
| `results$posterior_distribution_plot`    |     |     |     |     | an image |
| `results$trace_plots`                    |     |     |     |     | an image |
| `results$prior_posterior_plot`           |     |     |     |     | an image |
| `results$bayesian_forest_plot`           |     |     |     |     | an image |
| `results$decision_analysis_plot`         |     |     |     |     | an image |
| `results$predictive_checks_plot`         |     |     |     |     | an image |
| `results$comprehensive_report`           |     |     |     |     | a html   |
| `results$clinical_interpretation_report` |     |     |     |     | a html   |
| `results$layman_summary_report`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$bayesian_overview$asDF`

`as.data.frame(results$bayesian_overview)`
