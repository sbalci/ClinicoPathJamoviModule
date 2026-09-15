# Optimal Cutpoint Determination

Determines optimal cutpoints for continuous biomarkers to maximize
diagnostic or prognostic performance. Essential for converting
continuous measurements to clinically actionable thresholds and
preventing arbitrary cutpoint selection in pathology studies.

## Usage

``` r
optimalcutpoint(
  data,
  biomarker,
  analysis_type = "binary",
  outcome,
  time_var,
  status_var,
  cutpoint_method = "youden",
  bootstrap_validation = FALSE,
  bootstrap_runs = 1000,
  cross_validation = FALSE,
  cv_folds = 10,
  performance_metrics = TRUE,
  survival_analysis = TRUE,
  confidence_level = 0.95,
  multiple_testing = "miller_siegmund",
  nperm = 2000,
  plot_roc = TRUE,
  plot_distribution = TRUE,
  plot_survival = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- biomarker:

  Continuous biomarker variable for cutpoint optimization

- analysis_type:

  Type of analysis based on outcome variable

- outcome:

  Outcome variable for binary or continuous analysis

- time_var:

  Time-to-event variable for survival analysis

- status_var:

  Event status variable for survival analysis (0=censored, 1=event)

- cutpoint_method:

  Method for cutpoint optimization

- bootstrap_validation:

  Perform bootstrap validation of cutpoint stability

- bootstrap_runs:

  Number of bootstrap runs for validation

- cross_validation:

  Perform cross-validation for cutpoint assessment

- cv_folds:

  Number of folds for cross-validation

- performance_metrics:

  Calculate comprehensive performance metrics

- survival_analysis:

  Perform survival analysis for optimal groups

- confidence_level:

  Confidence level for intervals and tests

- multiple_testing:

  Multiple-testing correction for cut-point selection. For survival
  cut-points the article default Miller-Siegmund uses the
  Lausen-Schumacher / Hothorn-Lausen asymptotic correction
  (maxstat::maxstat.test pmethod="HL"); selecting "monte_carlo" performs
  a permutation-based correction analogous to X-tile (Camp 2004, Clin
  Cancer Res), which is more robust for small samples or non-Gaussian
  biomarker distributions.

- nperm:

  Number of permutations used when multiple_testing = "monte_carlo".
  Increase for more stable corrected p-values at the cost of runtime.

- plot_roc:

  Generate ROC curve with optimal cutpoint

- plot_distribution:

  Show biomarker distribution with cutpoint

- plot_survival:

  Generate Kaplan-Meier curves for optimal groups

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions for optimal cutpoint determination |
| `results$cutpointresults` |  |  |  |  | Optimal cutpoint values and associated metrics |
| `results$performance` |  |  |  |  | Diagnostic performance at optimal cutpoint |
| `results$survival` |  |  |  |  | Survival statistics for optimal groups |
| `results$validation` |  |  |  |  | Bootstrap validation of cutpoint stability |
| `results$crossvalidation` |  |  |  |  | Cross-validation performance assessment |
| `results$rocplot` |  |  |  |  | ROC curve showing optimal cutpoint |
| `results$distributionplot` |  |  |  |  | Distribution of biomarker with optimal cutpoint |
| `results$survivalplot` |  |  |  |  | Survival curves for optimal groups |
| `results$cutpointplot` |  |  |  |  | Visualization of cutpoint search process |
| `results$interpretation` |  |  |  |  | Clinical context and interpretation for optimal cutpoints |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$cutpointresults$asDF`

`as.data.frame(results$cutpointresults)`
