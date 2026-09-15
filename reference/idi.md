# Integrated Discrimination Improvement (IDI)

Integrated Discrimination Improvement (IDI) analysis for assessing the
incremental value of new biomarkers or risk factors in prediction
models. IDI quantifies the improvement in model discrimination by
measuring the difference in predicted probabilities between events and
non-events for the new model compared to the baseline model. Unlike NRI
which focuses on reclassification, IDI provides a numeric measure of
discrimination improvement without requiring risk categories. The
analysis includes confidence intervals through bootstrap methods,
statistical significance testing, and decomposition into event and
non-event components. Essential for biomarker validation, model
enhancement assessment, and demonstrating clinical utility of new
predictors in medical research. Particularly valuable when numeric risk
improvement is more relevant than categorical reclassification, such as
in precision medicine applications.

## Usage

``` r
idi(
  data,
  outcome,
  baseline_risk,
  new_risk,
  time_var,
  followup_time = 5,
  idi_type = "standard",
  discrimination_measure = "mean_diff",
  trim_proportion = 0.1,
  confidence_level = 0.95,
  bootstrap_samples = 1000,
  bootstrap_method = "bca",
  hypothesis_test = TRUE,
  test_method = "bootstrap",
  decompose_idi = TRUE,
  risk_distribution = TRUE,
  discrimination_slope = TRUE,
  relative_improvement = TRUE,
  cross_validation = FALSE,
  cv_folds = 5,
  sensitivity_analysis = FALSE,
  outlier_detection = FALSE,
  outlier_method = "iqr",
  show_summary = TRUE,
  show_distributions = TRUE,
  show_discrimination = TRUE,
  show_validation = FALSE,
  plot_risk_distributions = TRUE,
  plot_discrimination = TRUE,
  plot_scatter = TRUE,
  plot_bootstrap = FALSE,
  plot_sensitivity = FALSE,
  plot_outliers = FALSE,
  competing_risks = FALSE,
  stratified_analysis = FALSE,
  subgroup_var,
  missing_handling = "complete",
  calibration_aware = FALSE,
  time_dependent = FALSE,
  alpha_level = 0.05,
  random_seed = 123,
  relative_idi = FALSE,
  idi_competing_risks = FALSE,
  visual_discrimination_slopes = TRUE,
  idi_trajectory = FALSE,
  prediction_error_curves = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome variable (0/1) indicating event occurrence. For
  time-to-event data, this should be event status at the specified time
  point.

- baseline_risk:

  Predicted risks or probabilities from the baseline (reference) model.
  Should be on probability scale (0-1) for optimal IDI interpretation.

- new_risk:

  Predicted risks or probabilities from the new (enhanced) model that
  includes additional predictors or biomarkers.

- time_var:

  Time to event variable for survival data analysis. Required when using
  time-to-event outcomes with IDI.

- followup_time:

  Time point for survival analysis (years). Events occurring after this
  time are censored for IDI calculation.

- idi_type:

  Type of IDI calculation. Standard uses absolute differences, relative
  scales by baseline discrimination, scaled normalizes by outcome
  prevalence, all provides comprehensive assessment.

- discrimination_measure:

  Method for calculating discrimination differences. Mean difference is
  standard, median is robust to outliers, trimmed mean balances both
  approaches, robust uses M-estimators.

- trim_proportion:

  Proportion of extreme values to trim when using trimmed mean. 0.1
  trims 10 percent from each tail, providing robust estimation.

- confidence_level:

  Confidence level for IDI confidence intervals and hypothesis testing.
  0.95 provides 95 percent confidence intervals.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation. More
  samples provide more stable estimates but increase computation time.

- bootstrap_method:

  Bootstrap confidence interval method. BCa provides bias-corrected
  intervals with better coverage, studentized accounts for variance
  changes, percentile is simple and robust.

- hypothesis_test:

  Perform hypothesis test for IDI = 0. Tests whether the new model
  provides significant discrimination improvement.

- test_method:

  Method for hypothesis testing. Bootstrap is recommended for most
  situations, asymptotic assumes normality, permutation is
  distribution-free, robust handles outliers.

- decompose_idi:

  Decompose IDI into contributions from events and non-events. Provides
  insight into which population drives the improvement.

- risk_distribution:

  Analyze and compare risk score distributions between baseline and new
  models for events and non-events separately.

- discrimination_slope:

  Calculate discrimination slopes (difference in means) for baseline and
  new models with confidence intervals.

- relative_improvement:

  Calculate relative improvement as percentage increase in
  discrimination over baseline model performance.

- cross_validation:

  Perform k-fold cross-validation to assess stability of IDI estimates
  and reduce optimism bias in model comparisons.

- cv_folds:

  Number of folds for cross-validation when enabled. 5-fold CV provides
  good balance of bias and variance.

- sensitivity_analysis:

  Perform sensitivity analysis by varying follow-up times and examining
  IDI stability across different time horizons.

- outlier_detection:

  Detect and analyze outliers in risk predictions that may
  disproportionately influence IDI calculations.

- outlier_method:

  Method for outlier detection. IQR is simple and robust, Z-score
  assumes normality, modified Z-score is more robust, isolation forest
  handles complex patterns.

- show_summary:

  Display comprehensive IDI summary including overall IDI, decomposed
  components, and statistical significance.

- show_distributions:

  Display risk distribution summaries for baseline and new models
  separately for events and non-events.

- show_discrimination:

  Display discrimination slopes and related measures for both baseline
  and new models with improvements.

- show_validation:

  Display cross-validation and sensitivity analysis results when these
  options are enabled.

- plot_risk_distributions:

  Create plots showing risk score distributions for baseline and new
  models separated by outcome status.

- plot_discrimination:

  Visualize discrimination improvement showing mean differences and IDI
  components with confidence intervals.

- plot_scatter:

  Create scatter plot of baseline vs new model risk scores colored by
  outcome status to visualize improvement patterns.

- plot_bootstrap:

  Plot bootstrap distribution of IDI estimates showing sampling
  variability and confidence interval construction.

- plot_sensitivity:

  Create plots showing IDI variation across different analysis
  parameters and time points.

- plot_outliers:

  Visualize detected outliers and their influence on IDI calculations
  with and without outliers.

- competing_risks:

  Account for competing risks in survival analysis. Modifies IDI
  calculation for settings with multiple event types.

- stratified_analysis:

  Perform stratified IDI analysis by important subgroups to assess
  consistency of improvement across populations.

- subgroup_var:

  Variable defining subgroups for stratified analysis. Each level will
  receive separate IDI calculation.

- missing_handling:

  Method for handling missing data in risk predictions. Multiple
  imputation provides most robust results.

- calibration_aware:

  Adjust IDI calculation to account for model calibration. Uses
  calibrated probabilities for more accurate assessment.

- time_dependent:

  Calculate time-dependent IDI for survival models showing how
  discrimination improvement varies over time.

- alpha_level:

  Type I error rate for hypothesis testing and confidence intervals.
  Standard value is 0.05 for 95 percent confidence.

- random_seed:

  Random seed for bootstrap sampling and cross-validation. Ensures
  reproducible results across analyses.

- relative_idi:

  Calculate relative IDI as percentage improvement over baseline
  discrimination. More interpretable for comparing models across
  different populations.

- idi_competing_risks:

  Calculate cause-specific IDI for competing risks data. Accounts for
  multiple event types in discrimination improvement.

- visual_discrimination_slopes:

  Create enhanced visualizations of discrimination slopes with density
  plots. Shows separation between events and non-events for both models.

- idi_trajectory:

  Analyze how IDI changes over time for time-to-event outcomes. Useful
  for understanding when discrimination improvement is largest.

- prediction_error_curves:

  Plot prediction error curves alongside IDI to visualize improvement.
  Shows integrated Brier score differences over time.

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$todo`                   |     |     |     |     | a html   |
| `results$summary`                |     |     |     |     | a table  |
| `results$distributionSummary`    |     |     |     |     | a table  |
| `results$discriminationAnalysis` |     |     |     |     | a table  |
| `results$decomposition`          |     |     |     |     | a table  |
| `results$validationResults`      |     |     |     |     | a table  |
| `results$subgroupAnalysis`       |     |     |     |     | a table  |
| `results$outlierAnalysis`        |     |     |     |     | a table  |
| `results$sensitivityTable`       |     |     |     |     | a table  |
| `results$riskDistributionsPlot`  |     |     |     |     | an image |
| `results$discriminationPlot`     |     |     |     |     | an image |
| `results$scatterPlot`            |     |     |     |     | an image |
| `results$bootstrapPlot`          |     |     |     |     | an image |
| `results$sensitivityPlot`        |     |     |     |     | an image |
| `results$outliersPlot`           |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
