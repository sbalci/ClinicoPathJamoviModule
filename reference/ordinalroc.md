# Ordinal ROC Analysis

Ordinal ROC analysis for diagnostic tests predicting ordered categorical
outcomes such as tumor differentiation grade (well/moderate/poor),
fibrosis stage (F0-F4), inflammation severity
(none/mild/moderate/severe), or cancer stage (I/II/III/IV). Unlike
binary ROC which dichotomizes outcomes, ordinal ROC preserves the
natural ordering of categories and accounts for the severity/progression
inherent in ordinal scales. The analysis uses cumulative probability
models and proportional odds assumptions to generate ROC curves that
reflect the ordinal nature of the outcome. Provides cumulative AUC
across all thresholds, partial proportional odds models, and
ordinal-specific performance metrics. Essential for biomarker validation
in grading systems, staging algorithms, and any scenario where the
outcome has more than 2 ordered levels. Particularly valuable in
pathology where grading and staging are fundamental to diagnosis and
prognosis (Gleason score, Nottingham grade, FIGO stage, etc.).

## Usage

``` r
ordinalroc(
  data,
  predictor,
  ordinal_outcome,
  outcome_order,
  roc_method = "empirical",
  cumulative_direction = "geq",
  calculate_auc = TRUE,
  auc_ci_method = "delong",
  bootstrap_samples = 1000,
  confidence_level = 0.95,
  category_specific_auc = TRUE,
  test_proportional_odds = TRUE,
  show_coefficients = TRUE,
  partial_proportional_odds = FALSE,
  optimal_thresholds = TRUE,
  threshold_method = "youden",
  sensitivity_target = 0.8,
  specificity_target = 0.8,
  plot_ordinal_roc = TRUE,
  plot_all_cumulative = TRUE,
  plot_category_distributions = TRUE,
  plot_cumulative_probabilities = FALSE,
  plot_confusion_matrix = FALSE,
  clinical_context = "general",
  show_interpretation = TRUE,
  stratified_analysis = FALSE,
  stratify_by,
  covariate_adjustment = FALSE,
  covariates,
  compare_predictors = FALSE,
  additional_predictors,
  auc_comparison_test = TRUE,
  missing_handling = "complete",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- predictor:

  Continuous predictor variable (biomarker level, risk score, imaging
  feature) used to predict the ordinal outcome. Should increase or
  decrease monotonically with outcome severity.

- ordinal_outcome:

  Ordered categorical outcome variable with 3 or more levels
  representing increasing severity or progression (e.g.,
  well/moderate/poor differentiation, stage I/II/III/IV, FIGO stage,
  fibrosis score F0-F4).

- outcome_order:

  Specify the correct ordering of outcome levels from lowest to highest
  severity. This ensures proper cumulative probability calculation. For
  example, for tumor differentiation, order should be: well, moderate,
  poor.

- roc_method:

  Method for constructing ordinal ROC curves. Empirical uses observed
  cumulative distributions (non-parametric), binormal assumes normal
  distributions within categories, proportional odds uses logistic
  regression framework.

- cumulative_direction:

  Direction for cumulative probabilities. Use \>= if higher predictor
  values indicate higher severity (e.g., higher biomarker = worse
  grade). Use \<= if lower predictor values indicate higher severity.

- calculate_auc:

  Calculate area under the ordinal ROC curve. For ordinal outcomes, this
  is often called the concordance probability or generalized AUC. Values
  \>0.5 indicate discriminatory ability.

- auc_ci_method:

  Method for calculating confidence intervals around AUC. DeLong is
  faster, bootstrap is more accurate for small samples.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation when
  using bootstrap method.

- confidence_level:

  Confidence level for interval estimation around AUC and other metrics.

- category_specific_auc:

  Calculate AUC for each cumulative dichotomization (e.g., for grades
  1/2/3, calculate AUC for 1 vs 2+3, 1+2 vs 3). Shows discriminatory
  ability at each threshold.

- test_proportional_odds:

  Test whether the proportional odds assumption holds. This assumption
  states that the effect of the predictor is the same across all
  category thresholds. Violation suggests non-proportional odds model
  may be needed.

- show_coefficients:

  Display coefficients from the proportional odds logistic regression
  model. Shows the log-odds ratio for predictor and threshold
  intercepts.

- partial_proportional_odds:

  If proportional odds assumption is violated, fit partial proportional
  odds model where predictor effects can vary across thresholds.

- optimal_thresholds:

  Identify optimal predictor thresholds for each cumulative
  dichotomization using Youden index or other criteria.

- threshold_method:

  Method for selecting optimal threshold at each cumulative split.

- sensitivity_target:

  Target sensitivity for threshold selection if using
  sensitivity-constrained approach.

- specificity_target:

  Target specificity for threshold selection if using
  specificity-constrained approach.

- plot_ordinal_roc:

  Display ordinal ROC curve(s). For empirical method, shows cumulative
  ROC for each dichotomization. For proportional odds, shows model-based
  curve.

- plot_all_cumulative:

  Display separate ROC curves for each cumulative dichotomization on the
  same plot. Helps visualize how discrimination varies across severity
  levels.

- plot_category_distributions:

  Show distribution of predictor values stratified by outcome category.
  Visualizes separation between ordinal levels.

- plot_cumulative_probabilities:

  Display cumulative probability curves as a function of predictor
  value. Shows how predicted probabilities change with predictor level.

- plot_confusion_matrix:

  Show confusion matrix using optimal thresholds to classify
  observations into ordinal categories.

- clinical_context:

  Clinical context for interpretation guidance. Provides
  context-specific recommendations for threshold selection and clinical
  application.

- show_interpretation:

  Provide clinical interpretation of results including guidance on
  biomarker utility for distinguishing between ordinal categories.

- stratified_analysis:

  Perform stratified ordinal ROC analysis by important subgroups to
  assess consistency of biomarker performance across populations.

- stratify_by:

  Variable defining strata for stratified analysis (e.g., age group,
  sex, institution). Each level receives separate ordinal ROC analysis.

- covariate_adjustment:

  Adjust ordinal ROC analysis for covariates (confounders) using
  regression framework. Useful for scanner effects, batch effects, or
  demographic variables.

- covariates:

  Variables to adjust for in covariate-adjusted ordinal ROC analysis.

- compare_predictors:

  Compare multiple predictor variables on the same ordinal outcome to
  determine which biomarker has superior discrimination across ordinal
  categories.

- additional_predictors:

  Additional continuous predictors to compare with the primary
  predictor. Each will receive ordinal ROC analysis and AUC comparison
  tests.

- auc_comparison_test:

  When comparing multiple predictors, perform statistical test for
  differences in AUC values (DeLong test or bootstrap comparison).

- missing_handling:

  Method for handling missing data in predictor or outcome variables.

- random_seed:

  Random seed for bootstrap sampling and other stochastic procedures.
  Ensures reproducible results.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$overallSummary` |  |  |  |  | Summary of ordinal ROC analysis with AUC and confidence intervals |
| `results$cumulativeAUC` |  |  |  |  | AUC for each cumulative split of ordinal categories |
| `results$proportionalOddsTest` |  |  |  |  | Test of proportional odds assumption |
| `results$modelCoefficients` |  |  |  |  | Coefficients from proportional odds logistic regression |
| `results$optimalThresholds` |  |  |  |  | Optimal predictor thresholds for each cumulative split |
| `results$predictorComparison` |  |  |  |  | Comparison of AUC across multiple predictors |
| `results$aucComparisonTests` |  |  |  |  | Statistical tests comparing AUC between predictors |
| `results$stratifiedResults` |  |  |  |  | Ordinal ROC results stratified by subgroup |
| `results$confusionMatrix` |  |  |  |  | Confusion matrix using optimal thresholds to classify into ordinal categories |
| `results$ordinalROCPlot` |  |  |  |  | Ordinal ROC curve showing discrimination across ordinal categories |
| `results$cumulativeROCPlot` |  |  |  |  | Separate ROC curves for each cumulative dichotomization |
| `results$categoryDistPlot` |  |  |  |  | Distribution of predictor values stratified by ordinal outcome category |
| `results$cumulativeProbPlot` |  |  |  |  | Cumulative probabilities as a function of predictor value |
| `results$interpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overallSummary$asDF`

`as.data.frame(results$overallSummary)`
