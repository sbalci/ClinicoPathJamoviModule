# Bayesian Diagnostic Test Evaluation

Comprehensive Bayesian evaluation of diagnostic test performance
including sensitivity, specificity, predictive values, likelihood
ratios, and diagnostic odds ratios with full uncertainty quantification.
Supports meta-analysis, hierarchical modeling, and comparative test
evaluation.

## Usage

``` r
bayesiandiagnostic(
  data,
  test_results,
  gold_standard,
  test_positive_level,
  disease_positive_level,
  study_id = NULL,
  patient_id = NULL,
  covariates,
  analysis_type = "single_test",
  comparison_test = NULL,
  comparison_positive_level,
  prior_type = "informative",
  prior_sensitivity_mean = 0.8,
  prior_sensitivity_precision = 10,
  prior_specificity_mean = 0.9,
  prior_specificity_precision = 10,
  correlation_prior = 0,
  bivariate_model = TRUE,
  hierarchical_model = FALSE,
  covariate_effects = FALSE,
  heterogeneity_model = "random",
  mcmc_samples = 10000,
  mcmc_burnin = 5000,
  mcmc_thin = 1,
  mcmc_chains = 3,
  mcmc_adapt = TRUE,
  convergence_criterion = 1.1,
  compute_sensitivity = TRUE,
  compute_specificity = TRUE,
  compute_ppv = TRUE,
  compute_npv = TRUE,
  compute_likelihood_ratios = TRUE,
  compute_diagnostic_odds_ratio = TRUE,
  compute_accuracy = TRUE,
  compute_auc = FALSE,
  prevalence_prior = 0.5,
  decision_analysis = FALSE,
  cost_fn = 1,
  cost_fp = 1,
  utility_tp = 1,
  utility_tn = 1,
  threshold_analysis = FALSE,
  threshold_metric = "youden",
  show_summary_statistics = TRUE,
  show_posterior_distributions = TRUE,
  show_credible_intervals = TRUE,
  show_convergence_diagnostics = TRUE,
  show_model_comparison = FALSE,
  show_predictive_checks = FALSE,
  show_clinical_interpretation = TRUE,
  credible_interval = 0.95,
  hpdi = TRUE,
  set_seed = TRUE,
  seed_value = 42,
  parallel_chains = TRUE,
  n_cores = 2,
  robust_estimation = FALSE,
  outlier_detection = FALSE,
  publication_bias = FALSE,
  sensitivity_analysis = FALSE
)
```

## Arguments

- data:

  The data as a data frame for Bayesian diagnostic test evaluation.

- test_results:

  .

- gold_standard:

  .

- test_positive_level:

  .

- disease_positive_level:

  .

- study_id:

  .

- patient_id:

  .

- covariates:

  .

- analysis_type:

  .

- comparison_test:

  .

- comparison_positive_level:

  .

- prior_type:

  .

- prior_sensitivity_mean:

  .

- prior_sensitivity_precision:

  .

- prior_specificity_mean:

  .

- prior_specificity_precision:

  .

- correlation_prior:

  .

- bivariate_model:

  .

- hierarchical_model:

  .

- covariate_effects:

  .

- heterogeneity_model:

  .

- mcmc_samples:

  .

- mcmc_burnin:

  .

- mcmc_thin:

  .

- mcmc_chains:

  .

- mcmc_adapt:

  .

- convergence_criterion:

  .

- compute_sensitivity:

  .

- compute_specificity:

  .

- compute_ppv:

  .

- compute_npv:

  .

- compute_likelihood_ratios:

  .

- compute_diagnostic_odds_ratio:

  .

- compute_accuracy:

  .

- compute_auc:

  .

- prevalence_prior:

  .

- decision_analysis:

  .

- cost_fn:

  .

- cost_fp:

  .

- utility_tp:

  .

- utility_tn:

  .

- threshold_analysis:

  .

- threshold_metric:

  .

- show_summary_statistics:

  .

- show_posterior_distributions:

  .

- show_credible_intervals:

  .

- show_convergence_diagnostics:

  .

- show_model_comparison:

  .

- show_predictive_checks:

  .

- show_clinical_interpretation:

  .

- credible_interval:

  .

- hpdi:

  .

- set_seed:

  .

- seed_value:

  .

- parallel_chains:

  .

- n_cores:

  .

- robust_estimation:

  .

- outlier_detection:

  .

- publication_bias:

  .

- sensitivity_analysis:

  .

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$summaryStatistics`          |     |     |     |     | a table  |
| `results$diagnosticPerformance`      |     |     |     |     | a table  |
| `results$sensitivitySpecificity`     |     |     |     |     | a table  |
| `results$predictiveValues`           |     |     |     |     | a table  |
| `results$likelihoodRatios`           |     |     |     |     | a table  |
| `results$diagnosticOddsRatio`        |     |     |     |     | a table  |
| `results$convergenceDiagnostics`     |     |     |     |     | a table  |
| `results$posteriorSummary`           |     |     |     |     | a table  |
| `results$modelComparison`            |     |     |     |     | a table  |
| `results$metaAnalysisResults`        |     |     |     |     | a table  |
| `results$heterogeneityAssessment`    |     |     |     |     | a table  |
| `results$covariateEffects`           |     |     |     |     | a table  |
| `results$thresholdAnalysis`          |     |     |     |     | a table  |
| `results$decisionAnalysis`           |     |     |     |     | a table  |
| `results$posteriorPredictiveChecks`  |     |     |     |     | a table  |
| `results$sensitivityAnalysis`        |     |     |     |     | a table  |
| `results$clinicalInterpretation`     |     |     |     |     | a html   |
| `results$methodsExplanation`         |     |     |     |     | a html   |
| `results$rocCurve`                   |     |     |     |     | an image |
| `results$posteriorDistributionsPlot` |     |     |     |     | an image |
| `results$sensitivitySpecificityPlot` |     |     |     |     | an image |
| `results$predictiveValuesPlot`       |     |     |     |     | an image |
| `results$likelihoodRatioPlot`        |     |     |     |     | an image |
| `results$convergencePlots`           |     |     |     |     | an image |
| `results$forestPlot`                 |     |     |     |     | an image |
| `results$heterogeneityPlot`          |     |     |     |     | an image |
| `results$thresholdOptimizationPlot`  |     |     |     |     | an image |
| `results$decisionCurvePlot`          |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryStatistics$asDF`

`as.data.frame(results$summaryStatistics)`
