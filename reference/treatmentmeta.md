# Treatment Effect Meta-Analysis

Comprehensive meta-analysis for treatment effects in clinical research.
Supports continuous outcomes (mean differences, standardized mean
differences), binary outcomes (risk ratios, odds ratios, risk
differences), and time-to-event outcomes (hazard ratios). Includes
fixed-effect, random-effects, and Bayesian meta-analysis approaches with
comprehensive heterogeneity assessment and publication bias evaluation.

## Usage

``` r
treatmentmeta(
  data,
  study_id = NULL,
  year = NULL,
  outcome_type = "continuous",
  mean_treatment = NULL,
  sd_treatment = NULL,
  n_treatment = NULL,
  mean_control = NULL,
  sd_control = NULL,
  n_control = NULL,
  events_treatment = NULL,
  events_control = NULL,
  correlation = NULL,
  sample_size = NULL,
  effect_size = NULL,
  standard_error = NULL,
  ci_lower = NULL,
  ci_upper = NULL,
  hazard_ratio = NULL,
  log_hr_se = NULL,
  effect_measure = "SMD",
  model_type = "random",
  heterogeneity_test = TRUE,
  prediction_interval = TRUE,
  subgroup_var = NULL,
  subgroup_test = TRUE,
  moderator_vars = NULL,
  sensitivity_analysis = TRUE,
  influence_diagnostics = TRUE,
  publication_bias = TRUE,
  funnel_plot = TRUE,
  eggers_test = TRUE,
  trim_fill = TRUE,
  pcurve = FALSE,
  quality_score = NULL,
  weight_by_quality = FALSE,
  confidence_level = 0.95,
  small_study_correction = TRUE,
  bayesian_analysis = FALSE,
  forest_plot = TRUE,
  baujat_plot = FALSE,
  radial_plot = FALSE,
  cumulative_meta = FALSE,
  show_weights = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- study_id:

  Variable containing study names or identifiers

- year:

  Year of publication for chronological analyses

- outcome_type:

  Type of outcome measure for meta-analysis

- mean_treatment:

  Mean outcome in treatment group (continuous outcomes)

- sd_treatment:

  Standard deviation in treatment group

- n_treatment:

  Sample size in treatment group

- mean_control:

  Mean outcome in control group (continuous outcomes)

- sd_control:

  Standard deviation in control group

- n_control:

  Sample size in control group

- events_treatment:

  Number of events in treatment group (binary outcomes)

- events_control:

  Number of events in control group

- correlation:

  Correlation coefficient (r) for correlation meta-analysis

- sample_size:

  Total sample size for correlation studies

- effect_size:

  Pre-calculated effect size (generic meta-analysis)

- standard_error:

  Standard error of effect size

- ci_lower:

  Lower bound of confidence interval

- ci_upper:

  Upper bound of confidence interval

- hazard_ratio:

  Hazard ratio for survival outcomes

- log_hr_se:

  Standard error of log hazard ratio

- effect_measure:

  Effect size measure for meta-analysis

- model_type:

  Statistical model for pooling effect sizes

- heterogeneity_test:

  Perform heterogeneity tests (Q, I², τ²)

- prediction_interval:

  Calculate prediction interval for future studies

- subgroup_var:

  Categorical variable for subgroup analysis

- subgroup_test:

  Test for differences between subgroups

- moderator_vars:

  Variables for meta-regression analysis

- sensitivity_analysis:

  Perform leave-one-out sensitivity analysis

- influence_diagnostics:

  Calculate influence measures and outlier detection

- publication_bias:

  Assess publication bias using multiple methods

- funnel_plot:

  Generate funnel plot for visual bias assessment

- eggers_test:

  Perform Egger's regression test for asymmetry

- trim_fill:

  Apply trim and fill method for bias adjustment

- pcurve:

  Perform p-curve analysis for evidential value

- quality_score:

  Quality assessment score (e.g., Jadad, Cochrane RoB)

- weight_by_quality:

  Weight studies by quality score in analysis

- confidence_level:

  Confidence level for intervals

- small_study_correction:

  Apply corrections for small study effects

- bayesian_analysis:

  Perform Bayesian meta-analysis (requires additional packages)

- forest_plot:

  Generate forest plot of results

- baujat_plot:

  Generate Baujat plot for heterogeneity contribution

- radial_plot:

  Generate radial (Galbraith) plot

- cumulative_meta:

  Perform cumulative meta-analysis by year

- show_weights:

  Display individual study weights in output

- show_interpretation:

  Include clinical interpretation and recommendations

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions for treatment effect meta-analysis |
| `results$data_summary` |  |  |  |  | Overview of included studies and data quality |
| `results$pooled_effects` |  |  |  |  | Overall meta-analysis results with confidence and prediction intervals |
| `results$individual_studies` |  |  |  |  | Effect sizes and weights for each study |
| `results$heterogeneity` |  |  |  |  | Tests and measures of between-study heterogeneity |
| `results$subgroup_analysis` |  |  |  |  | Meta-analysis results by subgroup |
| `results$subgroup_test` |  |  |  |  | Statistical test for differences between subgroups |
| `results$meta_regression` |  |  |  |  | Moderator analysis using meta-regression |
| `results$sensitivity` |  |  |  |  | Leave-one-out sensitivity analysis results |
| `results$influence` |  |  |  |  | Identification of influential studies and outliers |
| `results$publication_bias_tests` |  |  |  |  | Statistical tests for publication bias |
| `results$trim_fill_results` |  |  |  |  | Adjusted estimates after trim and fill correction |
| `results$bayesian_results` |  |  |  |  | Parameter estimates from Bayesian meta-analysis |
| `results$pcurve_results` |  |  |  |  | Statistical tests for evidential value |
| `results$quality_assessment` |  |  |  |  | Impact of study quality on effect sizes |
| `results$forest_plot` |  |  |  |  | Forest plot showing individual and pooled effect sizes |
| `results$funnel_plot` |  |  |  |  | Funnel plot for assessing publication bias |
| `results$baujat_plot` |  |  |  |  | Contribution to heterogeneity vs influence on pooled estimate |
| `results$radial_plot` |  |  |  |  | Galbraith radial plot for heterogeneity assessment |
| `results$cumulative_results` |  |  |  |  | Sequential pooled estimates as studies are added |
| `results$cumulative_plot` |  |  |  |  | Cumulative meta-analysis plot over time |
| `results$pcurve_plot` |  |  |  |  | Distribution of significant p-values |
| `results$clinical_interpretation` |  |  |  |  | Clinical context and evidence synthesis |
| `results$methods_section` |  |  |  |  | Template text for publication methods section |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$data_summary$asDF`

`as.data.frame(results$data_summary)`
