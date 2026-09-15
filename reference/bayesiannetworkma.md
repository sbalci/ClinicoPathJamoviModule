# Bayesian Network Meta-Analysis

Advanced Bayesian network meta-analysis for multiple treatment
comparisons using network geometry. Supports direct and indirect
comparisons, network coherence assessment, and ranking of multiple
interventions with uncertainty quantification for clinical decision
making.

## Usage

``` r
bayesiannetworkma(
  data,
  study_id,
  treatment,
  outcome,
  sample_size,
  outcomeLevel,
  standard_error = NULL,
  control_rate = NULL,
  network_type = "mixed_evidence",
  outcome_type = "binary_or",
  reference_treatment,
  baseline_model = "exchangeable",
  random_effects_model = "random_univariate",
  heterogeneity_model = "common",
  correlation_structure = "unstructured",
  treatment_effect_prior = "vague_normal",
  heterogeneity_prior = "half_normal",
  prior_mean_effect = 0,
  prior_sd_effect = 10,
  prior_heterogeneity_sd = 1,
  assess_coherence = TRUE,
  coherence_method = "node_splitting",
  inconsistency_threshold = 0.05,
  mcmc_samples = 10000,
  mcmc_burnin = 5000,
  mcmc_thin = 2,
  mcmc_chains = 3,
  ranking_analysis = TRUE,
  ranking_measure = "sucra",
  pairwise_probabilities = TRUE,
  superiority_threshold = 0,
  model_selection = TRUE,
  model_comparison_criteria = "dic",
  network_plot = TRUE,
  network_layout = "spring",
  edge_weights = "n_studies",
  show_network_summary = TRUE,
  show_treatment_effects = TRUE,
  show_heterogeneity_analysis = TRUE,
  show_coherence_results = TRUE,
  show_ranking_results = TRUE,
  show_forest_plots = TRUE,
  show_league_table = TRUE,
  show_convergence_diagnostics = TRUE,
  show_interpretation = TRUE,
  meta_regression_covariates = NULL,
  treatment_classes = NULL,
  multi_arm_correction = TRUE,
  zero_events_correction = 0.5,
  clinical_context = "therapeutic",
  confidence_level = 0.95,
  set_seed = TRUE,
  seed_value = 42,
  parallel_processing = TRUE,
  n_cores = 2
)
```

## Arguments

- data:

  The data as a data frame for Bayesian network meta-analysis.

- study_id:

  .

- treatment:

  .

- outcome:

  .

- sample_size:

  .

- outcomeLevel:

  .

- standard_error:

  For continuous outcomes

- control_rate:

  Baseline event rate for binary outcomes

- network_type:

  .

- outcome_type:

  .

- reference_treatment:

  Reference treatment for network comparisons

- baseline_model:

  .

- random_effects_model:

  .

- heterogeneity_model:

  .

- correlation_structure:

  .

- treatment_effect_prior:

  .

- heterogeneity_prior:

  .

- prior_mean_effect:

  .

- prior_sd_effect:

  .

- prior_heterogeneity_sd:

  .

- assess_coherence:

  .

- coherence_method:

  .

- inconsistency_threshold:

  .

- mcmc_samples:

  .

- mcmc_burnin:

  .

- mcmc_thin:

  .

- mcmc_chains:

  .

- ranking_analysis:

  .

- ranking_measure:

  .

- pairwise_probabilities:

  .

- superiority_threshold:

  Threshold for clinically meaningful superiority

- model_selection:

  .

- model_comparison_criteria:

  .

- network_plot:

  .

- network_layout:

  .

- edge_weights:

  .

- show_network_summary:

  .

- show_treatment_effects:

  .

- show_heterogeneity_analysis:

  .

- show_coherence_results:

  .

- show_ranking_results:

  .

- show_forest_plots:

  .

- show_league_table:

  .

- show_convergence_diagnostics:

  .

- show_interpretation:

  .

- meta_regression_covariates:

  .

- treatment_classes:

  For class-effect models

- multi_arm_correction:

  .

- zero_events_correction:

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
| `results$networkSummary`             |     |     |     |     | a table  |
| `results$networkCharacteristics`     |     |     |     |     | a table  |
| `results$treatmentEffects`           |     |     |     |     | a table  |
| `results$heterogeneityAnalysis`      |     |     |     |     | a table  |
| `results$coherenceAssessment`        |     |     |     |     | a table  |
| `results$treatmentRankings`          |     |     |     |     | a table  |
| `results$pairwiseComparisons`        |     |     |     |     | a table  |
| `results$leagueTable`                |     |     |     |     | a table  |
| `results$modelComparison`            |     |     |     |     | a table  |
| `results$convergenceDiagnostics`     |     |     |     |     | a table  |
| `results$metaRegressionResults`      |     |     |     |     | a table  |
| `results$sensitivityAnalysis`        |     |     |     |     | a table  |
| `results$clinicalInterpretation`     |     |     |     |     | a html   |
| `results$methodsExplanation`         |     |     |     |     | a html   |
| `results$networkPlot`                |     |     |     |     | an image |
| `results$forestPlots`                |     |     |     |     | an image |
| `results$rankingPlots`               |     |     |     |     | an image |
| `results$heterogeneityPlots`         |     |     |     |     | an image |
| `results$coherencePlots`             |     |     |     |     | an image |
| `results$posteriorDistributionPlots` |     |     |     |     | an image |
| `results$convergencePlots`           |     |     |     |     | an image |
| `results$comparisonMatrix`           |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$networkSummary$asDF`

`as.data.frame(results$networkSummary)`
