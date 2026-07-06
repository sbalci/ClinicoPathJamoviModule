# Brier Score & Integrated Brier Score

IMPORTANT LIMITATIONS: This is a BASIC implementation for single
time-point Brier score evaluation. It requires PRE-COMPUTED survival
probabilities (not raw Cox models). Many features listed below are NOT
IMPLEMENTED or STATISTICALLY INVALID. Brier Score analysis evaluates
calibration accuracy by measuring the mean squared difference between
predicted survival probabilities and observed outcomes at a SINGLE
specified time point. Lower Brier scores indicate better prediction
accuracy (perfect prediction = 0, worst = 1). Includes scaled Brier
scores relative to null model (Kaplan-Meier curve). NOT SUPPORTED:
Multiple time points, Integrated Brier Score (IBS), competing risks,
linear predictor inputs, formula inputs, temporal/external validation
flags. These features may appear in the UI but will generate errors if
used. For rigorous clinical validation, use established packages:
riskRegression::Score() or pec::pec(). This implementation has NOT been
validated against those standards.

## Usage

``` r
brierscore(
  data,
  time,
  event,
  event_code,
  predicted_survival,
  linear_predictor,
  prediction_formula = "",
  prediction_time = 60,
  multiple_time_points = FALSE,
  time_points = "12, 36, 60",
  calculate_ibs = FALSE,
  ibs_start_time = 0,
  ibs_end_time = 60,
  scaled_brier = TRUE,
  reference_model = FALSE,
  reference_predictions,
  reference_formula = "",
  confidence_intervals = FALSE,
  ci_method = "bootstrap",
  bootstrap_samples = 500,
  confidence_level = 0.95,
  compare_multiple_models = FALSE,
  additional_predictions,
  model_names = "Model 1, Model 2, Model 3",
  competing_risks = FALSE,
  cause_specific = TRUE,
  plot_brier_over_time = TRUE,
  plot_calibration_curve = FALSE,
  calibration_groups = 10,
  plot_model_comparison = FALSE,
  plot_integrated_brier = FALSE,
  stratified_brier = FALSE,
  stratify_by,
  inverse_probability_weighting = TRUE,
  external_validation = FALSE,
  temporal_validation = FALSE,
  development_period = "",
  missing_handling = "complete",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time-to-event or censoring variable. For survival analysis, this is
  the follow-up time in days, months, or years.

- event:

  Binary event indicator variable (1 = event occurred, 0 = censored).
  For competing risks, can be multi-level factor.

- event_code:

  Which level of the event variable represents the event of interest
  (e.g., death, progression, recurrence).

- predicted_survival:

  Pre-computed survival probabilities at the specified prediction time.
  Should be between 0 and 1. For example, predicted 5-year survival
  probability from a Cox model or machine learning algorithm.

- linear_predictor:

  Linear predictor from Cox model (log hazard ratio). If provided
  instead of predicted_survival, survival probabilities will be computed
  using Breslow estimator and baseline hazard.

- prediction_formula:

  Formula for prediction model if neither predicted_survival nor
  linear_predictor is provided. Example: "~ age + stage + grade". Cox
  model will be fitted internally and predictions generated.

- prediction_time:

  Time point at which to evaluate Brier score (in same units as time
  variable). For example, 60 months for 5-year predictions, 1825 days
  for 5-year predictions in days.

- multiple_time_points:

  Calculate Brier scores at multiple time points to show how calibration
  changes over time.

- time_points:

  Comma-separated list of additional time points for evaluation.
  Example: "12, 24, 36, 60" for 1, 2, 3, and 5 years (in months).

- calculate_ibs:

  Calculate Integrated Brier Score by integrating time-dependent Brier
  score across follow-up period. Provides single summary measure of
  overall prediction performance across time. Computationally expensive.

- ibs_start_time:

  Start time for IBS integration. Usually 0 to include entire follow-up
  period.

- ibs_end_time:

  End time for IBS integration. Should not exceed maximum follow-up
  time. Determines the time horizon over which prediction performance is
  summarized.

- scaled_brier:

  Calculate scaled (standardized) Brier score relative to null model
  (Kaplan-Meier survival curve). Scaled Brier = 1 - (Brier_model /
  Brier_null). Values near 0 indicate no improvement over KM, positive
  values indicate better predictions than KM.

- reference_model:

  Compare Brier score to a reference prediction model to assess
  incremental value of new predictors.

- reference_predictions:

  Pre-computed survival probabilities from reference model for
  comparison.

- reference_formula:

  Formula for reference model if reference_predictions not provided.
  Example: "~ age + stage" (simpler than full model).

- confidence_intervals:

  Calculate confidence intervals for Brier scores using bootstrap or
  influence function methods. Bootstrap method is computationally
  expensive.

- ci_method:

  Method for confidence interval estimation. Bootstrap is more accurate
  but computationally intensive, influence function is faster.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation. More
  samples provide more stable estimates.

- confidence_level:

  Confidence level for interval estimation.

- compare_multiple_models:

  Compare Brier scores across multiple prediction models to identify the
  best calibrated model.

- additional_predictions:

  Additional prediction variables (survival probabilities) from
  competing models for comparison.

- model_names:

  Comma-separated names for prediction models being compared. Used for
  labeling output tables and plots.

- competing_risks:

  Use competing risks framework when multiple event types can occur.
  Brier score accounts for cumulative incidence rather than survival.

- cause_specific:

  For competing risks, calculate cause-specific Brier score for the
  event of interest.

- plot_brier_over_time:

  Plot time-dependent Brier score across follow-up period. Shows how
  prediction accuracy changes over time.

- plot_calibration_curve:

  Display calibration curve showing agreement between predicted and
  observed survival probabilities. Perfect calibration appears as 45°
  line.

- calibration_groups:

  Number of risk groups for calibration curve (e.g., deciles = 10). More
  groups provide finer resolution but require larger sample size.

- plot_model_comparison:

  Bar plot comparing Brier scores across multiple models with confidence
  intervals.

- plot_integrated_brier:

  Visualize IBS as area under the Brier score curve over time.

- stratified_brier:

  Calculate Brier scores stratified by important subgroups to assess
  calibration consistency across populations.

- stratify_by:

  Variable for stratified analysis (e.g., treatment arm, age group,
  center).

- inverse_probability_weighting:

  Use inverse probability of censoring weighting (IPCW) to handle
  censored observations. Recommended for proper Brier score estimation.

- external_validation:

  Indicate this is external validation (model developed on different
  dataset). Affects interpretation and may trigger additional
  diagnostics.

- temporal_validation:

  Indicate this is temporal validation (more recent patients than
  development cohort). May assess calibration drift over time.

- development_period:

  End date of development period for temporal validation assessment
  (format: YYYY-MM-DD).

- missing_handling:

  Method for handling missing predictor or outcome data.

- random_seed:

  Random seed for bootstrap sampling and other stochastic procedures.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$brierSummary` |  |  |  |  | Summary of Brier score at specified time point(s) |
| `results$integratedBrier` |  |  |  |  | Integrated Brier score over time period |
| `results$modelComparison` |  |  |  |  | Comparison of Brier scores across multiple models |
| `results$pairwiseComparisons` |  |  |  |  | Statistical tests comparing Brier scores between models |
| `results$calibrationTable` |  |  |  |  | Predicted vs observed survival by risk group |
| `results$stratifiedBrier` |  |  |  |  | Brier scores by subgroup |
| `results$brierOverTimePlot` |  |  |  |  | Time-dependent Brier score across follow-up period |
| `results$calibrationPlot` |  |  |  |  | Predicted vs observed survival probabilities |
| `results$modelComparisonPlot` |  |  |  |  | Bar plot comparing Brier scores across models |
| `results$integratedBrierPlot` |  |  |  |  | Area under Brier score curve |
| `results$interpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$brierSummary$asDF`

`as.data.frame(results$brierSummary)`
