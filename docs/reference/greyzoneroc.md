# Grey-zone (Fuzzy) ROC Analysis

Grey-zone ROC analysis for diagnostic tests where uncertainty around
decision thresholds requires a "don't know" or "defer decision" option.
Unlike traditional binary ROC that forces every case into positive or
negative, grey-zone ROC acknowledges diagnostic uncertainty by creating
an indeterminate zone around the threshold where additional testing or
expert review is recommended. Essential for AI model deployment where
uncertain predictions should not drive clinical decisions, cytology with
atypical findings requiring repeat sampling, and biomarker cutoffs where
borderline values need confirmation. The analysis determines optimal
grey-zone boundaries that maximize diagnostic certainty while minimizing
inconclusive results, calculates performance metrics excluding vs
including grey-zone cases, and provides clinical decision rules for
handling uncertain classifications. Particularly valuable for real-world
implementation of diagnostic tests where "I don't know" is a valid and
often safer response than forcing a potentially incorrect binary
classification.

## Usage

``` r
greyzoneroc(
  data,
  predictor,
  outcome,
  positive_level,
  grey_zone_method = "fixed_width",
  grey_zone_width = 0.1,
  lower_grey_boundary = 0.45,
  upper_grey_boundary = 0.55,
  confidence_threshold = 0.8,
  cost_false_positive = 1,
  cost_false_negative = 1,
  cost_grey_zone = 0.3,
  calculate_definite_performance = TRUE,
  calculate_all_cases_performance = TRUE,
  grey_zone_characteristics = TRUE,
  optimal_threshold = "youden",
  clinical_threshold = 0.5,
  prediction_intervals = FALSE,
  bootstrap_grey_zone = TRUE,
  bootstrap_samples = 1000,
  confidence_level = 0.95,
  grey_zone_action = "reflex",
  reflex_test_name = "Confirmatory test",
  plot_grey_zone_roc = TRUE,
  plot_threshold_distributions = TRUE,
  plot_grey_zone_size = TRUE,
  plot_uncertainty_map = FALSE,
  plot_cost_surface = FALSE,
  fuzzy_membership = FALSE,
  bayesian_grey_zone = FALSE,
  stratified_grey_zone = FALSE,
  stratify_by,
  clinical_scenario = "general",
  missing_handling = "complete",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- predictor:

  Continuous predictor variable or probability score from diagnostic
  test. For AI models, this is typically the predicted probability. For
  biomarkers, this is the measured concentration or expression level.

- outcome:

  Binary gold standard outcome variable defining true disease status.
  Must have exactly 2 levels (positive/negative or diseased/healthy).

- positive_level:

  Level of the outcome variable representing the positive/diseased
  state.

- grey_zone_method:

  Method for defining the grey-zone boundaries. Fixed width creates
  equal margins around the optimal threshold, confidence uses prediction
  uncertainty from the model, cost-benefit minimizes expected costs,
  Youden creates symmetric zone maximizing certainty, custom allows
  manual boundary specification.

- grey_zone_width:

  Width of the grey zone when using fixed_width method. For probability
  scores (0-1), 0.1 creates a ±0.05 margin around the threshold. Larger
  values increase the indeterminate region but improve certainty for
  definite classifications.

- lower_grey_boundary:

  Lower boundary of grey zone when using custom method. Values below
  this are classified as negative. Should be less than
  upper_grey_boundary.

- upper_grey_boundary:

  Upper boundary of grey zone when using custom method. Values above
  this are classified as positive. Should be greater than
  lower_grey_boundary.

- confidence_threshold:

  Minimum confidence level required for definite classification when
  using confidence-based method. Predictions with lower confidence are
  assigned to grey zone. Higher values increase grey-zone size but
  improve reliability of definite calls.

- cost_false_positive:

  Relative cost of false positive classification. Used in cost-benefit
  optimization to determine grey-zone boundaries that minimize expected
  costs.

- cost_false_negative:

  Relative cost of false negative classification. Higher values widen
  the grey zone for low scores to avoid missing positive cases.

- cost_grey_zone:

  Relative cost of classifying a case as grey zone (requiring additional
  testing). Typically lower than misclassification costs but higher than
  correct classification. Reflects the burden of deferred decisions and
  additional testing.

- calculate_definite_performance:

  Calculate sensitivity, specificity, and AUC using only cases with
  definite classifications (excluding grey-zone). Shows the reliability
  of the test when it makes a definite call.

- calculate_all_cases_performance:

  Calculate performance treating grey-zone as misclassifications.
  Provides worst-case scenario where all deferred decisions are
  considered failures.

- grey_zone_characteristics:

  Analyze characteristics of cases falling in the grey zone including
  prevalence distribution, risk factors, and suggestions for resolution.

- optimal_threshold:

  Method for selecting the central threshold around which the grey zone
  is defined.

- clinical_threshold:

  Prespecified clinical threshold when using clinical threshold
  selection. Common values: 0.5 for balanced classification, varies by
  clinical context.

- prediction_intervals:

  Calculate prediction intervals around ROC curve to visualize
  uncertainty. Particularly useful for AI models with calibrated
  probabilities.

- bootstrap_grey_zone:

  Use bootstrap to assess stability of grey-zone boundaries and
  performance metrics. Provides confidence intervals for grey-zone size
  and definite classification rates.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation.

- confidence_level:

  Confidence level for intervals around performance metrics and
  grey-zone boundaries.

- grey_zone_action:

  Recommended clinical action for cases in the grey zone. This affects
  interpretation of diagnostic performance and workflow design.

- reflex_test_name:

  Name of the reflex/confirmatory test used for grey-zone cases. For
  example: FISH for HER2 2+, HPV testing for ASCUS cytology.

- plot_grey_zone_roc:

  Display ROC curve with grey-zone boundaries highlighted and
  performance metrics for definite vs all classifications shown.

- plot_threshold_distributions:

  Show distribution of predictor scores for positive and negative cases
  with grey-zone boundaries marked. Visualizes overlap and uncertainty.

- plot_grey_zone_size:

  Plot showing how varying grey-zone width affects the trade-off between
  percentage of definite classifications and their accuracy.

- plot_uncertainty_map:

  Create heatmap showing prediction uncertainty across the score range.
  Useful for AI models with calibrated confidence estimates.

- plot_cost_surface:

  Visualize expected costs across different grey-zone boundary
  configurations. Helps identify cost-optimal grey-zone width.

- fuzzy_membership:

  Use fuzzy set theory to model gradual transition between definite
  positive, grey zone, and definite negative. Provides soft boundaries
  instead of hard cutoffs.

- bayesian_grey_zone:

  Use Bayesian approach to define grey zone based on posterior
  probability intervals. Incorporates prior information and uncertainty
  quantification.

- stratified_grey_zone:

  Perform grey-zone analysis stratified by important covariates to
  assess whether grey-zone boundaries should vary by subpopulation.

- stratify_by:

  Variable for stratified analysis. Each level gets separate grey-zone
  analysis.

- clinical_scenario:

  Clinical scenario for context-specific interpretation and
  recommendations.

- missing_handling:

  Method for handling missing predictor or outcome data.

- random_seed:

  Random seed for bootstrap and other stochastic procedures.

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`               |     |     |     |     | a html   |
| `results$greyZoneSummary`            |     |     |     |     | a table  |
| `results$greyZoneBoundaries`         |     |     |     |     | a table  |
| `results$definitePerformanceTable`   |     |     |     |     | a table  |
| `results$allCasesPerformanceTable`   |     |     |     |     | a table  |
| `results$greyZoneCharacteristics`    |     |     |     |     | a table  |
| `results$classificationBreakdown`    |     |     |     |     | a table  |
| `results$costAnalysisTable`          |     |     |     |     | a table  |
| `results$tradeoffAnalysisTable`      |     |     |     |     | a table  |
| `results$stratifiedGreyZoneTable`    |     |     |     |     | a table  |
| `results$greyZoneROCPlot`            |     |     |     |     | an image |
| `results$thresholdDistributionsPlot` |     |     |     |     | an image |
| `results$greyZoneSizePlot`           |     |     |     |     | an image |
| `results$uncertaintyMapPlot`         |     |     |     |     | an image |
| `results$costSurfacePlot`            |     |     |     |     | an image |
| `results$clinicalRecommendations`    |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$greyZoneSummary$asDF`

`as.data.frame(results$greyZoneSummary)`
