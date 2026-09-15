# Diagnostic Performance Analysis

Comprehensive diagnostic performance analysis including ROC curves,
cross-validation, and statistical comparison of diagnostic tests.
Supports both single and multiple biomarker evaluation with robust
validation methods.

## Usage

``` r
diagnosticperformance(
  data,
  outcome,
  outcomeLevel,
  predictors,
  validationMethod = "none",
  cvFolds = 10,
  cvRepeats = 1,
  bootstrapN = 1000,
  holdoutProp = 0.3,
  comparisonMethod = "delong",
  optimalCutoff = "youden",
  costRatio = 1,
  confidenceLevel = 0.95,
  plotROC = TRUE,
  plotPR = FALSE,
  plotCalibration = FALSE,
  showMetrics = TRUE,
  showCutoffAnalysis = TRUE,
  stratifyBy
)
```

## Arguments

- data:

  .

- outcome:

  Binary outcome variable indicating disease/condition status (0/1 or
  factor).

- outcomeLevel:

  Level of outcome variable representing positive cases (events).

- predictors:

  Continuous predictor variables (biomarkers, risk scores) to evaluate.

- validationMethod:

  Method for validating diagnostic performance to avoid overfitting.

- cvFolds:

  Number of folds for cross-validation (typically 5 or 10).

- cvRepeats:

  Number of times to repeat cross-validation for more stable results.

- bootstrapN:

  Number of bootstrap samples for bootstrap validation and confidence
  intervals.

- holdoutProp:

  Proportion of data to hold out for validation (0.1 to 0.5).

- comparisonMethod:

  Statistical method for comparing multiple ROC curves.

- optimalCutoff:

  Method for determining optimal diagnostic cutoff point.

- costRatio:

  Ratio of cost of false positive to false negative (for cost-weighted
  cutoff).

- confidenceLevel:

  Confidence level for confidence intervals (0.80 to 0.99).

- plotROC:

  .

- plotPR:

  Generate precision-recall curves (useful for imbalanced datasets).

- plotCalibration:

  Generate calibration plots to assess prediction calibration.

- showMetrics:

  Show comprehensive diagnostic performance metrics table.

- showCutoffAnalysis:

  Show optimal cutoff analysis with sensitivity/specificity trade-offs.

- stratifyBy:

  Optional variable for stratified analysis (subgroup analysis).

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$text`              |     |     |     |     | a html   |
| `results$performanceTable`  |     |     |     |     | a table  |
| `results$cutoffTable`       |     |     |     |     | a table  |
| `results$comparisonTable`   |     |     |     |     | a table  |
| `results$validationTable`   |     |     |     |     | a table  |
| `results$rocPlot`           |     |     |     |     | an image |
| `results$prPlot`            |     |     |     |     | an image |
| `results$calibrationPlot`   |     |     |     |     | an image |
| `results$stratifiedResults` |     |     |     |     | a table  |
| `results$interpretation`    |     |     |     |     | a html   |
| `results$recommendations`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performanceTable$asDF`

`as.data.frame(results$performanceTable)`

## Examples

``` r
# Single biomarker diagnostic performance
diagnosticperformance(
    data = dataset,
    outcome = "disease_status",
    predictors = "biomarker_level",
    validation_method = "crossval",
    cv_folds = 10
)
#> Error in diagnosticperformance(data = dataset, outcome = "disease_status",     predictors = "biomarker_level", validation_method = "crossval",     cv_folds = 10): unused arguments (validation_method = "crossval", cv_folds = 10)

# Multiple biomarker comparison
diagnosticperformance(
    data = dataset,
    outcome = "disease_status",
    predictors = c("marker1", "marker2", "marker3"),
    comparison_method = "delong",
    validation_method = "bootstrap",
    bootstrap_n = 1000
)
#> Error in diagnosticperformance(data = dataset, outcome = "disease_status",     predictors = c("marker1", "marker2", "marker3"), comparison_method = "delong",     validation_method = "bootstrap", bootstrap_n = 1000): unused arguments (comparison_method = "delong", validation_method = "bootstrap", bootstrap_n = 1000)
```
