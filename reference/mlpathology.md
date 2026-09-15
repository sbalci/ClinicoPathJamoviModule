# Classification Performance Metrics for Digital Pathology

Comprehensive evaluation of machine learning models and algorithms for
digital pathology applications. Provides classification metrics, ROC
analysis, segmentation quality assessment, and statistical model
comparison for AI validation and algorithm comparison studies.

## Usage

``` r
mlpathology(
  data,
  analysis_type = "classification",
  actual_labels,
  predicted_labels,
  predicted_probabilities,
  reference_segmentation,
  predicted_segmentation,
  model1_predictions,
  model2_predictions,
  model1_probabilities,
  model2_probabilities,
  roc_analysis = TRUE,
  roc_comparison = FALSE,
  confidence_level = 0.95,
  confusion_matrix_plot = TRUE,
  roc_plot = TRUE,
  bootstrap_validation = FALSE,
  bootstrap_runs = 1000
)
```

## Arguments

- data:

  the data as a data frame

- analysis_type:

  Type of performance analysis to conduct

- actual_labels:

  True/actual classification labels

- predicted_labels:

  Predicted classification labels from model

- predicted_probabilities:

  Predicted probabilities for ROC analysis (binary classification)

- reference_segmentation:

  Reference/ground truth segmentation masks (binary)

- predicted_segmentation:

  Predicted segmentation masks from model (binary)

- model1_predictions:

  Predictions from first model for comparison

- model2_predictions:

  Predictions from second model for comparison

- model1_probabilities:

  Probabilities from first model for ROC comparison

- model2_probabilities:

  Probabilities from second model for ROC comparison

- roc_analysis:

  Perform ROC curve analysis for binary classification

- roc_comparison:

  Compare ROC curves between two models using DeLong's test

- confidence_level:

  Confidence level for performance metrics

- confusion_matrix_plot:

  Generate confusion matrix heatmap

- roc_plot:

  Generate ROC curve plot

- bootstrap_validation:

  Use bootstrap sampling for confidence intervals

- bootstrap_runs:

  Number of bootstrap replicates for validation

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`        |     |     |     |     | a html   |
| `results$confusionmatrix`     |     |     |     |     | a table  |
| `results$performancemetrics`  |     |     |     |     | a table  |
| `results$rocanalysis`         |     |     |     |     | a table  |
| `results$segmentationmetrics` |     |     |     |     | a table  |
| `results$modelcomparison`     |     |     |     |     | a table  |
| `results$roccomparison`       |     |     |     |     | a table  |
| `results$confusionmatrixplot` |     |     |     |     | an image |
| `results$rocplot`             |     |     |     |     | an image |
| `results$interpretation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$confusionmatrix$asDF`

`as.data.frame(results$confusionmatrix)`
