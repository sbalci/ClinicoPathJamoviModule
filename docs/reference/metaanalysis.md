# Meta-Analysis & Evidence Synthesis

Comprehensive meta-analysis and evidence synthesis including forest
plots, heterogeneity testing, publication bias assessment, diagnostic
test accuracy meta-analysis, and network meta-analysis.

## Usage

``` r
metaanalysis(
  data,
  effect_size,
  variance,
  study_id,
  sample_size,
  year,
  analysis_type = "generic",
  model_type = "random_effects",
  effect_measure = "odds_ratio",
  heterogeneity_method = "dersimonian_laird",
  true_positives,
  false_positives,
  false_negatives,
  true_negatives,
  dta_model_type = "bivariate",
  treatment_arm,
  comparison_arm,
  network_method = "frequentist",
  publication_bias = TRUE,
  bias_tests = "all_tests",
  subgroup_var,
  moderator_vars,
  meta_regression = FALSE,
  sensitivity_analysis = TRUE,
  outlier_detection = TRUE,
  forest_plot_options = TRUE,
  prediction_interval = TRUE,
  confidence_level = 0.95,
  robust_methods = FALSE,
  small_sample_correction = TRUE,
  knha_adjustment = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- effect_size:

  Effect size variable (e.g., log odds ratio, Cohen's d, log hazard
  ratio)

- variance:

  Variance or standard error of the effect size

- study_id:

  Study identifier for each effect size (required for meta-analysis)

- sample_size:

  Sample size for each study (optional, used for sensitivity analysis)

- year:

  Publication year for temporal trend analysis and publication bias
  assessment

- analysis_type:

  Type of meta-analysis to perform: Standard for combining effect sizes,
  Diagnostic for test accuracy studies, Network for multiple treatment
  comparisons

- model_type:

  Random-effects is usually preferred as it accounts for differences
  between studies

- effect_measure:

  Choose based on your outcome type: continuous (mean difference),
  binary (odds/risk ratio), time-to-event (hazard ratio), or association
  (correlation)

- heterogeneity_method:

  Method for estimating between-study heterogeneity (tau-squared)

- true_positives:

  True positives for diagnostic test accuracy meta-analysis

- false_positives:

  False positives for diagnostic test accuracy meta-analysis

- false_negatives:

  False negatives for diagnostic test accuracy meta-analysis

- true_negatives:

  True negatives for diagnostic test accuracy meta-analysis

- dta_model_type:

  Model type for diagnostic test accuracy meta-analysis

- treatment_arm:

  Treatment arm identifier for network meta-analysis

- comparison_arm:

  Comparison arm identifier for network meta-analysis

- network_method:

  Statistical approach for network meta-analysis

- publication_bias:

  Perform publication bias assessment using funnel plots and statistical
  tests

- bias_tests:

  Statistical tests for publication bias assessment

- subgroup_var:

  Categorical variable for subgroup analysis

- moderator_vars:

  Variables for meta-regression analysis

- meta_regression:

  Perform meta-regression analysis with moderator variables

- sensitivity_analysis:

  Perform sensitivity analysis including leave-one-out and influence
  diagnostics

- outlier_detection:

  Detect outlying studies using standardized residuals and influence
  measures

- forest_plot_options:

  Enable forest plot customization options

- prediction_interval:

  Include prediction intervals in forest plots and results

- confidence_level:

  Confidence level for confidence and prediction intervals

- robust_methods:

  Use robust methods for outlier-resistant meta-analysis

- small_sample_correction:

  Apply small sample corrections (Hartung-Knapp adjustment)

- knha_adjustment:

  Apply Knapp-Hartung adjustment for random-effects models

## Value

A results object containing:

|                                     |     |     |     |     |          |
|-------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`              |     |     |     |     | a html   |
| `results$studySummary`              |     |     |     |     | a table  |
| `results$overallResults`            |     |     |     |     | a table  |
| `results$heterogeneityAssessment`   |     |     |     |     | a table  |
| `results$publicationBiasResults`    |     |     |     |     | a table  |
| `results$subgroupAnalysis`          |     |     |     |     | a table  |
| `results$metaRegressionResults`     |     |     |     |     | a table  |
| `results$diagnosticAccuracyResults` |     |     |     |     | a table  |
| `results$networkResults`            |     |     |     |     | a table  |
| `results$sensitivityAnalysis`       |     |     |     |     | a table  |
| `results$outlierAnalysis`           |     |     |     |     | a table  |
| `results$modelFitStatistics`        |     |     |     |     | a table  |
| `results$forestPlot`                |     |     |     |     | an image |
| `results$funnelPlot`                |     |     |     |     | an image |
| `results$heterogeneityPlot`         |     |     |     |     | an image |
| `results$metaRegressionPlot`        |     |     |     |     | an image |
| `results$srocPlot`                  |     |     |     |     | an image |
| `results$networkPlot`               |     |     |     |     | an image |
| `results$methodExplanation`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$studySummary$asDF`

`as.data.frame(results$studySummary)`
