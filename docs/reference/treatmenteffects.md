# Causal Inference & Treatment Effects Analysis

Comprehensive causal inference analysis for estimating treatment effects
in observational studies. Implements propensity score methods, inverse
probability of treatment weighting (IPTW), matching techniques, and
doubly robust estimation. Includes covariate balance assessment,
sensitivity analysis, and causal effect estimation with confidence
intervals. Essential for comparative effectiveness research, real-world
evidence studies, and observational research where randomized controlled
trials are not feasible.

## Usage

``` r
treatmenteffects(
  data,
  treatment_var,
  outcome_var,
  covariates,
  patient_id,
  time_var,
  event_var,
  cluster_var,
  causal_method = "propensity_score",
  outcome_type = "continuous",
  estimand = "ate",
  ps_method = "logistic",
  ps_specification = "main_effects",
  balance_threshold = 0.1,
  matching_method = "nearest_neighbor",
  matching_ratio = "1to1",
  caliper_width = 0.2,
  replacement_matching = FALSE,
  weight_trimming = TRUE,
  trim_quantiles = 0.05,
  stabilized_weights = TRUE,
  weight_normalization = "sum_to_n",
  outcome_model = "linear",
  include_ps_in_outcome = TRUE,
  balance_assessment = TRUE,
  balance_methods = "standardized_differences",
  overlap_assessment = TRUE,
  sensitivity_analysis = TRUE,
  sensitivity_method = "rosenbaum_bounds",
  gamma_values = "1.1,1.2,1.3,1.5,2.0",
  heterogeneous_effects = FALSE,
  effect_modifiers,
  causal_tree = FALSE,
  causal_forest = FALSE,
  instrument_var,
  iv_method = "two_stage_least_squares",
  model_diagnostics = TRUE,
  bootstrap_inference = TRUE,
  n_bootstrap = 1000,
  cross_validation = FALSE,
  comprehensive_report = TRUE,
  individual_effects = FALSE,
  save_weights = FALSE,
  save_matched_data = FALSE,
  regulatory_documentation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- treatment_var:

  Binary treatment variable (treated vs. control)

- outcome_var:

  Primary outcome variable of interest

- covariates:

  Confounding variables to adjust for in causal analysis

- patient_id:

  Patient identifier for tracking matched pairs

- time_var:

  Time to event variable for survival outcomes

- event_var:

  Event indicator for survival outcomes

- cluster_var:

  Clustering variable (e.g., hospital, physician) for robust standard
  errors

- causal_method:

  Primary method for causal inference

- outcome_type:

  Type of outcome variable

- estimand:

  Target causal estimand

- ps_method:

  Method for estimating propensity scores

- ps_specification:

  Complexity of propensity score model

- balance_threshold:

  Maximum standardized mean difference for adequate balance

- matching_method:

  Specific matching algorithm to use

- matching_ratio:

  Ratio of controls to treated units

- caliper_width:

  Maximum distance for matching (in standard deviations)

- replacement_matching:

  Allow controls to be matched to multiple treated units

- weight_trimming:

  Trim extreme propensity score weights

- trim_quantiles:

  Quantiles for weight trimming (e.g., 0.05 = trim top/bottom 5 percent)

- stabilized_weights:

  Use stabilized inverse probability weights

- weight_normalization:

  Method for normalizing weights

- outcome_model:

  Model for outcome regression

- include_ps_in_outcome:

  Include propensity score in outcome model (doubly robust)

- balance_assessment:

  Assess covariate balance before and after adjustment

- balance_methods:

  Methods for assessing covariate balance

- overlap_assessment:

  Assess propensity score overlap between treatment groups

- sensitivity_analysis:

  Perform sensitivity analysis for unmeasured confounding

- sensitivity_method:

  Method for sensitivity analysis

- gamma_values:

  Gamma values for Rosenbaum bounds (comma-separated)

- heterogeneous_effects:

  Analyze treatment effect heterogeneity

- effect_modifiers:

  Variables for subgroup analysis and effect modification

- causal_tree:

  Use causal trees to identify effect heterogeneity

- causal_forest:

  Use causal random forests for individualized treatment effects

- instrument_var:

  Instrumental variable for IV analysis

- iv_method:

  Method for instrumental variable estimation

- model_diagnostics:

  Perform comprehensive model diagnostics

- bootstrap_inference:

  Use bootstrap for confidence intervals

- n_bootstrap:

  Number of bootstrap samples

- cross_validation:

  Use cross-validation for model selection

- comprehensive_report:

  Generate comprehensive causal inference report

- individual_effects:

  Estimate individual treatment effects

- save_weights:

  Save propensity scores and weights to dataset

- save_matched_data:

  Save matched dataset for further analysis

- regulatory_documentation:

  Include regulatory compliance documentation

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$overview`                   |     |     |     |     | a table  |
| `results$treatment_effect_estimates` |     |     |     |     | a table  |
| `results$balance_assessment`         |     |     |     |     | a table  |
| `results$propensity_summary`         |     |     |     |     | a table  |
| `results$matching_summary`           |     |     |     |     | a table  |
| `results$weight_summary`             |     |     |     |     | a table  |
| `results$model_diagnostics`          |     |     |     |     | a table  |
| `results$sensitivity_analysis`       |     |     |     |     | a table  |
| `results$heterogeneity_analysis`     |     |     |     |     | a table  |
| `results$individual_effects`         |     |     |     |     | a table  |
| `results$overlap_plot`               |     |     |     |     | an image |
| `results$balance_plot`               |     |     |     |     | an image |
| `results$effect_plot`                |     |     |     |     | an image |
| `results$sensitivity_plot`           |     |     |     |     | an image |
| `results$causal_report`              |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
