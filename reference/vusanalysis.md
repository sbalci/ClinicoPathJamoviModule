# Volume Under ROC Surface (VUS) Analysis

Volume Under ROC Surface (VUS) analysis extends the traditional Area
Under the ROC Curve (AUC) to diagnostic tests with three or more ordered
classes. While binary ROC analysis calculates the area under a 2D curve,
VUS calculates the volume under a 3D surface for multi-class problems.
The VUS represents the probability that a randomly selected triple of
observations (one from each class) will be correctly ordered by the
diagnostic marker. VUS ranges from 0 to 1, where 0.5 indicates random
classification (no discrimination), and 1 indicates perfect
discrimination across all classes. This method is particularly valuable
for tumor grading (Grade 1/2/3), disease staging (Stage I/II/III/IV),
severity classification (mild/moderate/severe), and any diagnostic
scenario where outcomes naturally fall into three or more ordered
categories. The analysis provides pairwise AUC comparisons between
adjacent classes, overall VUS with confidence intervals, and hypothesis
testing against chance performance. Essential for validating biomarkers,
imaging features, and AI models in multi-class clinicopathological
applications where binary classification is insufficient.

## Usage

``` r
vusanalysis(
  data,
  predictor,
  multiclass_outcome,
  outcome_order = "",
  vus_method = "mann_whitney",
  confidence_intervals = TRUE,
  ci_method = "bootstrap",
  bootstrap_samples = 1000,
  confidence_level = 0.95,
  hypothesis_test = TRUE,
  test_method = "bootstrap",
  pairwise_auc = TRUE,
  pairwise_ci = TRUE,
  pairwise_tests = FALSE,
  min_class_size = 10,
  covariate_adjustment = FALSE,
  covariates,
  stratified_analysis = FALSE,
  stratify_by,
  plot_3d_surface = FALSE,
  plot_pairwise_rocs = TRUE,
  plot_class_distributions = TRUE,
  plot_type = "boxplot",
  handle_ties = "average",
  random_seed = 12345
)
```

## Arguments

- data:

  The data as a data frame.

- predictor:

  Continuous predictor variable (biomarker, risk score, imaging
  feature). Should be numeric with higher values generally indicating
  higher class membership.

- multiclass_outcome:

  Outcome variable with 3 or more ordered classes. Examples: tumor grade
  (1/2/3), disease stage (I/II/III/IV), severity (mild/moderate/severe).

- outcome_order:

  Comma-separated list specifying the order of outcome classes from low
  to high. Example: "Grade 1, Grade 2, Grade 3". If not specified,
  factor levels are used.

- vus_method:

  Method for VUS estimation. Mann-Whitney is most robust and recommended
  for general use. Normal theory assumes multivariate normality.
  Bootstrap provides non-parametric confidence intervals.

- confidence_intervals:

  Calculate confidence intervals for VUS using specified method.

- ci_method:

  Method for confidence interval calculation. Bootstrap is recommended
  for most applications. BCa provides bias-corrected intervals.

- bootstrap_samples:

  Number of bootstrap resamples for confidence intervals and hypothesis
  testing.

- confidence_level:

  Confidence level for interval estimation.

- hypothesis_test:

  Test null hypothesis: VUS = 1/6 (random classification for 3 classes).
  For k classes, null VUS = 1/k!

- test_method:

  Method for hypothesis testing. Bootstrap and permutation are robust to
  distributional assumptions.

- pairwise_auc:

  Calculate AUC for all pairwise class comparisons. Helps identify which
  class transitions are well-discriminated.

- pairwise_ci:

  Calculate confidence intervals for pairwise AUC comparisons.

- pairwise_tests:

  Test whether each pairwise AUC differs significantly from 0.5.

- min_class_size:

  Minimum number of observations required per class for analysis.
  Smaller classes may produce unstable estimates.

- covariate_adjustment:

  Adjust VUS for covariates (experimental feature). Useful for
  controlling confounding factors.

- covariates:

  Covariates for adjustment (age, sex, site, etc.). Requires
  covariate_adjustment = TRUE.

- stratified_analysis:

  Calculate VUS stratified by subgroups to assess consistency across
  populations.

- stratify_by:

  Variable for stratified analysis (e.g., age group, disease subtype).

- plot_3d_surface:

  Generate 3D visualization of ROC surface (requires additional
  packages). Warning: May be computationally intensive for large
  datasets.

- plot_pairwise_rocs:

  Plot ROC curves for all pairwise class comparisons.

- plot_class_distributions:

  Display predictor distributions by outcome class (box plots or density
  plots).

- plot_type:

  Type of plot for showing predictor distributions across classes.

- handle_ties:

  Method for handling tied predictor values. Average ranks is standard
  for Mann-Whitney based calculations.

- random_seed:

  Random seed for bootstrap and permutation procedures.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$vusSummary` |  |  |  |  | Overall VUS with confidence intervals |
| `results$hypothesisTest` |  |  |  |  | Test whether VUS differs from random classification |
| `results$classBreakdown` |  |  |  |  | Sample sizes and summary statistics by class |
| `results$pairwiseAUC` |  |  |  |  | AUC for all pairwise class comparisons |
| `results$stratifiedVUS` |  |  |  |  | VUS by subgroup |
| `results$pairwiseROCPlot` |  |  |  |  | ROC curves for all pairwise class comparisons |
| `results$classDistributionPlot` |  |  |  |  | Distribution of predictor values across outcome classes |
| `results$surface3DPlot` |  |  |  |  | Three-dimensional ROC surface visualization |
| `results$interpretation` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$vusSummary$asDF`

`as.data.frame(results$vusSummary)`
