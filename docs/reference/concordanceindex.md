# Concordance Index (Harrell's C-index)

Harrell's concordance index (C-index) for evaluating discrimination
ability of survival prediction models. The C-index measures the
proportion of all comparable pairs of patients in which the model
correctly ranks their survival times - a generalization of the ROC
curve's AUC to censored survival data. Values range from 0.5 (no
discrimination) to 1.0 (perfect discrimination), with 0.7-0.8 considered
good and \>0.8 considered excellent. Unlike time-dependent ROC which
evaluates at specific time points, the C-index provides a global measure
across the entire follow-up period. Supports time-dependent concordance
for dynamic predictions, competing risks extensions, stratified
analysis, and comparison between models. Essential for validating Cox
regression models, machine learning survival predictions, and prognostic
scores. The C-index accounts for censoring through appropriate pair
weighting and can be decomposed to identify which risk groups contribute
most to discrimination.

## Usage

``` r
concordanceindex(
  data,
  time,
  event,
  event_code,
  predictor,
  reverse_direction = FALSE,
  predictor_formula = "",
  cindex_method = "harrell",
  time_dependent = FALSE,
  evaluation_times = "12, 36, 60",
  confidence_intervals = FALSE,
  ci_method = "bootstrap",
  bootstrap_samples = 500,
  confidence_level = 0.95,
  compare_models = FALSE,
  additional_predictors,
  model_names = "Model 1, Model 2, Model 3",
  compare_test = FALSE,
  competing_risks = FALSE,
  cause_specific = FALSE,
  decompose_cindex = FALSE,
  risk_groups = 3,
  somers_d = FALSE,
  goodman_kruskal_gamma = FALSE,
  stratified_cindex = FALSE,
  stratify_by,
  plot_cindex_over_time = FALSE,
  plot_model_comparison = FALSE,
  plot_risk_group_kaplan_meier = FALSE,
  plot_decomposition = FALSE,
  clinical_application = "general",
  show_interpretation = TRUE,
  external_validation = FALSE,
  restricted_time = FALSE,
  max_time = 60,
  handle_ties = "average",
  missing_handling = "complete",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time-to-event or censoring variable (days, months, years). For
  time-dependent concordance, this is the follow-up time.

- event:

  Binary event indicator (1 = event, 0 = censored). For competing risks,
  can be multi-level factor with different event types.

- event_code:

  Which level represents the event of interest (e.g., death,
  progression).

- predictor:

  Continuous predictor variable used for ranking patients. Can be a risk
  score, linear predictor from Cox model, or predicted probability.
  Higher values should indicate higher risk (or use reverse_direction
  option).

- reverse_direction:

  If predictor is protective (lower values = higher risk), reverse the
  direction for concordance calculation. Example: survival probability
  where lower values indicate worse prognosis.

- predictor_formula:

  Formula for Cox model if predictor not directly provided. Example: "~
  age + stage + grade". Model will be fitted and linear predictor used.

- cindex_method:

  Method for C-index calculation. Harrell's is standard, Uno's uses
  inverse probability weighting for censoring, Gönen-Heller is bias-free
  estimator for proportional hazards models (doesn't require follow-up
  information).

- time_dependent:

  Calculate time-dependent C-index at specific time horizons. Evaluates
  discrimination for predictions at particular time points rather than
  globally across follow-up.

- evaluation_times:

  Comma-separated time points for time-dependent C-index evaluation.
  Example: "12, 24, 36, 60" for 1, 2, 3, and 5 years in months.

- confidence_intervals:

  Calculate confidence intervals for C-index using bootstrap or
  asymptotic standard errors.

- ci_method:

  Method for confidence interval estimation. Bootstrap is more accurate
  but computationally intensive.

- bootstrap_samples:

  Number of bootstrap samples for CI estimation.

- confidence_level:

  Confidence level for interval estimation.

- compare_models:

  Compare C-index across multiple prediction models to identify best
  discriminating model.

- additional_predictors:

  Additional predictor variables (risk scores) for model comparison.

- model_names:

  Comma-separated names for models being compared.

- compare_test:

  Perform statistical test for differences in C-index between models.

- competing_risks:

  Use competing risks framework for C-index calculation when multiple
  event types can occur. Requires cause-specific hazard approach.

- cause_specific:

  For competing risks, calculate cause-specific C-index for event of
  interest treating other events as censoring.

- decompose_cindex:

  Decompose C-index to show contribution from different risk strata.
  Identifies which patient groups contribute most to discrimination.

- risk_groups:

  Number of risk groups for C-index decomposition (e.g., 3 =
  low/medium/high, 4 = quartiles, 10 = deciles).

- somers_d:

  Calculate Somers' D rank correlation (D = 2\*(C-index - 0.5)). Ranges
  from -1 to +1, interpretable as correlation between predictor and
  outcome.

- goodman_kruskal_gamma:

  Calculate Goodman-Kruskal gamma statistic for ordinal association.
  Related to C-index but uses different pair weighting.

- stratified_cindex:

  Calculate C-index stratified by important subgroups to assess
  consistency across populations.

- stratify_by:

  Variable for stratified analysis (e.g., treatment arm, center, age
  group).

- plot_cindex_over_time:

  Plot time-dependent C-index across follow-up period. Shows how
  discrimination changes with prediction horizon.

- plot_model_comparison:

  Bar plot comparing C-index across multiple models with confidence
  intervals.

- plot_risk_group_kaplan_meier:

  Display Kaplan-Meier curves stratified by risk groups defined by
  predictor quantiles. Visual assessment of separation = discrimination.

- plot_decomposition:

  Visualize contribution of different risk strata to overall C-index.

- clinical_application:

  Clinical application context for interpretation guidance.

- show_interpretation:

  Provide interpretation of C-index with clinical context and
  recommendations.

- external_validation:

  Indicate this is external validation (model from different cohort).

- restricted_time:

  Restrict C-index calculation to specific follow-up period to avoid
  issues with long-term censoring.

- max_time:

  Maximum follow-up time for restricted C-index calculation. Pairs
  beyond this time are not considered.

- handle_ties:

  Method for handling tied predictor values in concordance calculation.

- missing_handling:

  Method for handling missing predictor or outcome data.

- random_seed:

  Random seed for bootstrap and other stochastic procedures.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$instructions` |  |  |  |  | a html |
| `results$cindexSummary` |  |  |  |  | Overall C-index with confidence intervals |
| `results$somersD` |  |  |  |  | Somers' D rank correlation |
| `results$timeDependentCindex` |  |  |  |  | C-index evaluated at specific time points |
| `results$modelComparison` |  |  |  |  | Comparison of C-index across multiple models |
| `results$pairwiseTests` |  |  |  |  | Statistical tests comparing C-index between models |
| `results$decomposition` |  |  |  |  | Contribution of risk strata to overall C-index |
| `results$stratifiedCindex` |  |  |  |  | C-index by subgroup |
| `results$cindexOverTimePlot` |  |  |  |  | Time-dependent C-index across follow-up |
| `results$modelComparisonPlot` |  |  |  |  | Bar plot comparing C-index across models |
| `results$riskGroupKMPlot` |  |  |  |  | Survival curves stratified by predictor quantiles |
| `results$decompositionPlot` |  |  |  |  | Contribution of risk strata to discrimination |
| `results$interpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$cindexSummary$asDF`

`as.data.frame(results$cindexSummary)`
