# Trichotomous (Three-way) ROC Analysis

Trichotomous ROC analysis for three-category diagnostic tests with
positive, indeterminate (grey zone), and negative outcomes. Essential
for biomarker scoring systems like HER2 (0-1+/2+/3+), PD-L1 expression
levels, and cytology classifications with "atypical" results. Unlike
binary ROC which only considers positive vs negative, trichotomous ROC
accounts for the clinically important indeterminate zone where
additional testing may be required. The analysis provides 3×3 confusion
matrices, category-specific sensitivities and specificities, volume
under the ROC surface (VUS) as a 3D extension of AUC, and optimal
threshold determination for each decision boundary. Particularly
valuable for establishing clinical cutoffs in molecular pathology where
grey-zone results trigger reflex testing (e.g., HER2 2+ requires FISH
confirmation).

## Usage

``` r
trichotomousroc(
  data,
  predictor,
  outcome,
  positive_level,
  indeterminate_level,
  negative_level,
  threshold_method = "youden",
  lower_threshold = 0.33,
  upper_threshold = 0.67,
  cost_fn = 1,
  cost_fp = 1,
  cost_indeterminate = 0.5,
  confidence_level = 0.95,
  bootstrap_samples = 1000,
  bootstrap_method = "bca",
  calculate_vus = TRUE,
  category_sensitivities = TRUE,
  pairwise_comparisons = TRUE,
  confusion_matrix_3x3 = TRUE,
  plot_3d_surface = TRUE,
  plot_threshold_analysis = TRUE,
  plot_category_distributions = TRUE,
  plot_pairwise_rocs = FALSE,
  clinical_context = "general",
  show_clinical_interpretation = TRUE,
  indeterminate_action = "test",
  stratified_analysis = FALSE,
  stratify_by,
  missing_handling = "complete",
  random_seed = 123
)
```

## Arguments

- data:

  The data as a data frame.

- predictor:

  Continuous predictor variable (e.g., IHC H-score, gene expression
  level, AI probability score) used to classify cases into three
  categories.

- outcome:

  Three-level gold standard outcome variable defining the true
  classification (positive/indeterminate/negative or
  diseased/borderline/healthy).

- positive_level:

  Level of the outcome variable representing definite positive/diseased
  cases.

- indeterminate_level:

  Level of the outcome variable representing
  indeterminate/borderline/grey zone cases.

- negative_level:

  Level of the outcome variable representing definite
  negative/non-diseased cases.

- threshold_method:

  Method for determining the two thresholds that separate the three
  categories. Youden maximizes overall correct classification, clinical
  uses cost-benefit analysis, fixed allows manual threshold
  specification, tertile divides into equal thirds.

- lower_threshold:

  Lower threshold value when using fixed threshold method. Values below
  this are classified as negative. Required when threshold_method =
  'fixed'.

- upper_threshold:

  Upper threshold value when using fixed threshold method. Values above
  this are classified as positive. Required when threshold_method =
  'fixed'.

- cost_fn:

  Relative cost of misclassifying a positive case as negative. Higher
  values increase sensitivity at expense of specificity.

- cost_fp:

  Relative cost of misclassifying a negative case as positive. Higher
  values increase specificity at expense of sensitivity.

- cost_indeterminate:

  Relative cost of classifying a case as indeterminate (requiring
  additional testing). Typically lower than definite misclassification
  but higher than correct classification.

- confidence_level:

  Confidence level for intervals around VUS and performance metrics.

- bootstrap_samples:

  Number of bootstrap samples for confidence interval estimation. More
  samples provide more stable estimates but increase computation time.

- bootstrap_method:

  Bootstrap confidence interval method. BCa provides bias-corrected
  intervals.

- calculate_vus:

  Calculate Volume Under the ROC Surface, the 3D extension of AUC for
  three categories. VUS ranges from 0 to 1, where 0.167 represents
  random classification and 1 represents perfect classification. Values
  \> 0.167 indicate discriminatory ability.

- category_sensitivities:

  Calculate sensitivity for each category (ability to correctly identify
  positive, indeterminate, and negative cases respectively).

- pairwise_comparisons:

  Perform binary ROC analysis for all three pairwise comparisons:
  positive vs negative (ignoring indeterminate), positive vs
  indeterminate, and indeterminate vs negative.

- confusion_matrix_3x3:

  Display full 3×3 confusion matrix showing all possible classification
  outcomes. Rows represent true categories, columns represent predicted
  categories.

- plot_3d_surface:

  Create 3D visualization of the ROC surface. The surface extends the 2D
  ROC curve to three dimensions, showing classification performance
  across all threshold combinations.

- plot_threshold_analysis:

  Plot showing how classification metrics vary with different threshold
  combinations. Helps visualize the trade-off between definite and
  indeterminate classifications.

- plot_category_distributions:

  Display distribution of predictor scores for each outcome category
  with threshold lines showing decision boundaries.

- plot_pairwise_rocs:

  Show the three pairwise binary ROC curves for comparison with the
  trichotomous analysis.

- clinical_context:

  Clinical context for interpretation assistance. Provides
  context-specific guidance on threshold selection and clinical
  implications.

- show_clinical_interpretation:

  Provide clinical interpretation of results including recommendations
  for threshold use and implications for patient management.

- indeterminate_action:

  Specify the clinical action for cases classified as indeterminate.
  This affects the interpretation of diagnostic performance metrics.

- stratified_analysis:

  Perform stratified trichotomous ROC analysis by important subgroups to
  assess consistency across populations.

- stratify_by:

  Variable defining strata for stratified analysis. Each level will
  receive separate trichotomous ROC analysis.

- missing_handling:

  Method for handling missing data in predictor or outcome variables.

- random_seed:

  Random seed for bootstrap sampling. Ensures reproducible results.

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`               |     |     |     |     | a html   |
| `results$performanceSummary`         |     |     |     |     | a table  |
| `results$vusTable`                   |     |     |     |     | a table  |
| `results$thresholdsTable`            |     |     |     |     | a table  |
| `results$confusionMatrix3x3`         |     |     |     |     | a table  |
| `results$categorySensitivitiesTable` |     |     |     |     | a table  |
| `results$pairwiseROCTable`           |     |     |     |     | a table  |
| `results$clinicalImpactTable`        |     |     |     |     | a table  |
| `results$stratifiedAnalysisTable`    |     |     |     |     | a table  |
| `results$surface3DPlot`              |     |     |     |     | an image |
| `results$thresholdAnalysisPlot`      |     |     |     |     | an image |
| `results$categoryDistributionPlot`   |     |     |     |     | an image |
| `results$pairwiseROCPlot`            |     |     |     |     | an image |
| `results$clinicalInterpretation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performanceSummary$asDF`

`as.data.frame(results$performanceSummary)`
