# Net Reclassification Improvement (NRI)

Net Reclassification Improvement (NRI) analysis for assessing the
clinical utility of adding new biomarkers or risk factors to existing
prediction models. NRI quantifies how well a new model reclassifies
individuals into more appropriate risk categories compared to a baseline
model. This implementation supports both categorical NRI (with
predefined risk thresholds) and numeric NRI (risk-free approach). The
analysis includes confidence intervals via bootstrap, statistical
significance testing, and comprehensive visualization of
reclassification patterns. Essential for biomarker validation studies,
clinical decision making, and demonstrating incremental value of new
predictors in medical research. Particularly valuable for risk
prediction model development and validation in cardiovascular, oncology,
and other clinical domains where risk stratification drives treatment
decisions.

## Usage

``` r
netreclassification(
  data,
  outcome,
  baseline_risk,
  new_risk,
  time_var,
  followup_time = 5,
  nri_type = "categorical",
  risk_thresholds = "0.05, 0.10, 0.20",
  custom_categories = FALSE,
  category_labels = "Low, Intermediate, High, Very High",
  confidence_level = 0.95,
  bootstrap_samples = 1000,
  bootstrap_method = "bca",
  hypothesis_test = TRUE,
  decompose_nri = TRUE,
  direction_analysis = TRUE,
  show_transitions = TRUE,
  clinical_cutoffs = FALSE,
  treatment_threshold = 0.075,
  cost_effectiveness = FALSE,
  cross_validation = FALSE,
  cv_folds = 5,
  sensitivity_analysis = FALSE,
  show_summary = TRUE,
  show_reclassification = TRUE,
  show_performance = TRUE,
  plot_reclassification = TRUE,
  plot_risk_distribution = TRUE,
  plot_improvement = TRUE,
  plot_sensitivity = FALSE,
  competing_risks = FALSE,
  missing_handling = "complete",
  stratified_analysis = FALSE,
  subgroup_var,
  alpha_level = 0.05,
  random_seed = 123,
  category_free_nri = FALSE,
  event_specific_nri = FALSE,
  clinical_nri_custom = FALSE,
  reclassification_visualization = TRUE,
  relative_nri = FALSE,
  weighted_nri = FALSE,
  utility_weights = "1, 2, 4, 8"
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
  Should be on probability scale (0-1) for risk predictions.

- new_risk:

  Predicted risks or probabilities from the new (enhanced) model that
  includes additional predictors or biomarkers.

- time_var:

  Time to event variable for survival data analysis. Required when using
  time-to-event outcomes.

- followup_time:

  Time point for survival analysis (years). Events occurring after this
  time are considered censored for NRI calculation.

- nri_type:

  Type of NRI analysis. Categorical uses predefined risk thresholds,
  numeric (risk-free) compares all risk changes, both provides
  comprehensive assessment.

- risk_thresholds:

  Comma-separated risk thresholds for categorical NRI (e.g., 5 percent,
  10 percent, 20 percent). These define clinically meaningful risk
  categories for decision making.

- custom_categories:

  Use custom category labels instead of automatic threshold-based
  naming. Allows for more clinically meaningful category descriptions.

- category_labels:

  Comma-separated labels for risk categories when using custom
  categories. Should match the number of categories defined by
  thresholds.

- confidence_level:

  Confidence level for NRI confidence intervals and hypothesis testing.
  0.95 provides 95 percent confidence intervals.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation. More
  samples provide more stable estimates but increase computation time.

- bootstrap_method:

  Bootstrap confidence interval method. BCa provides bias-corrected and
  accelerated intervals with better coverage properties.

- hypothesis_test:

  Perform hypothesis test for NRI = 0. Tests whether the new model
  provides significant reclassification improvement.

- decompose_nri:

  Decompose total NRI into contributions from events and non-events.
  Provides insight into which population drives the improvement.

- direction_analysis:

  Analyze upward vs downward reclassification patterns separately. Shows
  whether improvement comes from better risk stratification.

- show_transitions:

  Display detailed reclassification transition matrix showing movement
  between all risk categories.

- clinical_cutoffs:

  Use clinically established risk cutoffs (e.g., ACC/AHA guidelines)
  instead of data-driven thresholds for cardiovascular risk.

- treatment_threshold:

  Risk threshold for treatment initiation. Used to calculate clinical
  decision metrics and treatment reclassification.

- cost_effectiveness:

  Include cost-effectiveness considerations in reclassification
  analysis. Requires additional cost and utility parameters.

- cross_validation:

  Perform k-fold cross-validation to assess stability of NRI estimates
  and reduce optimism bias in model comparisons.

- cv_folds:

  Number of folds for cross-validation when enabled. 5-fold CV provides
  good balance of bias and variance.

- sensitivity_analysis:

  Perform sensitivity analysis by varying risk thresholds to assess
  robustness of NRI conclusions.

- show_summary:

  Display comprehensive NRI summary including overall NRI, decomposed
  components, and statistical significance.

- show_reclassification:

  Display detailed reclassification cross-tabulation showing movement
  patterns between baseline and new model categories.

- show_performance:

  Display comparative performance metrics (C-statistic, calibration) for
  baseline and new models alongside NRI results.

- plot_reclassification:

  Create reclassification plot showing risk distribution changes and
  movement patterns between models.

- plot_risk_distribution:

  Plot risk distributions for baseline and new models separately for
  events and non-events with category boundaries.

- plot_improvement:

  Visualize NRI decomposition showing contributions from events and
  non-events with confidence intervals.

- plot_sensitivity:

  Create plots showing how NRI varies with different risk thresholds to
  assess robustness of conclusions.

- competing_risks:

  Account for competing risks in survival analysis. Modifies NRI
  calculation for settings with multiple event types.

- missing_handling:

  Method for handling missing data in risk predictions. Multiple
  imputation provides most robust results.

- stratified_analysis:

  Perform stratified NRI analysis by important subgroups (age, sex,
  baseline risk level) to assess consistency.

- subgroup_var:

  Variable defining subgroups for stratified analysis. Each level will
  receive separate NRI calculation.

- alpha_level:

  Type I error rate for hypothesis testing and confidence intervals.
  Standard value is 0.05 for 95 percent confidence.

- random_seed:

  Random seed for bootstrap sampling and cross-validation. Ensures
  reproducible results across analyses.

- category_free_nri:

  Calculate category-free (continuous) NRI without predefined risk
  categories. Provides unbiased assessment of reclassification across
  entire risk spectrum.

- event_specific_nri:

  Calculate NRI separately for events and non-events with detailed
  breakdowns. Essential for understanding which population benefits most
  from new model.

- clinical_nri_custom:

  Use clinically validated custom risk categories instead of statistical
  thresholds. More appropriate for guideline-based risk assessment.

- reclassification_visualization:

  Create comprehensive visualization including Sankey diagrams and
  heatmaps for reclassification patterns.

- relative_nri:

  Calculate relative NRI metrics normalized by baseline model
  performance. Useful for comparing improvements across different
  populations.

- weighted_nri:

  Weight reclassification by clinical importance using utility weights.
  Accounts for differential clinical impact of risk categories.

- utility_weights:

  Comma-separated weights for each risk category (low to high). Higher
  weights emphasize importance of correctly classifying high-risk
  individuals.

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`         |     |     |     |     | a html   |
| `results$todo`                 |     |     |     |     | a html   |
| `results$summary`              |     |     |     |     | a table  |
| `results$reclassification`     |     |     |     |     | a table  |
| `results$decomposition`        |     |     |     |     | a table  |
| `results$performance`          |     |     |     |     | a table  |
| `results$transitions`          |     |     |     |     | a table  |
| `results$subgroupAnalysis`     |     |     |     |     | a table  |
| `results$sensitivityTable`     |     |     |     |     | a table  |
| `results$reclassificationPlot` |     |     |     |     | an image |
| `results$riskDistributionPlot` |     |     |     |     | an image |
| `results$improvementPlot`      |     |     |     |     | an image |
| `results$sensitivityPlot`      |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
