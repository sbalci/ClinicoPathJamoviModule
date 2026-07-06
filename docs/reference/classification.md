# Clinical Classification

Clinical Classification

## Usage

``` r
classification(
  data,
  dep,
  indep,
  testSize = 0.33,
  noOfFolds = 10,
  testing,
  reporting = list("classifMetrices"),
  classifier,
  minSplit = 20,
  complexity = 0.01,
  maxCompete = 4,
  maxSurrogate = 5,
  maxDepth = 30,
  noOfTrees = 10,
  maxDepthRandFor = 30,
  sampleFraction = 1,
  splitRule,
  knnNeighbors = 5,
  knnDistance = "euclidean",
  svmKernel = "radial",
  svmCost = 1,
  svmGamma = 1,
  plotDecisionTree = FALSE,
  predictedFreq = FALSE,
  printRandForest = FALSE,
  predictedFreqRF = FALSE,
  balancingMethod = "none",
  validateMethod = "holdout",
  bootstrapSamples = 1000,
  reportClinicalMetrics = FALSE,
  reportConfidenceIntervals = FALSE,
  reportMCC = FALSE,
  positiveClass = "",
  seed = 42,
  thresholdMethod = "youden",
  thresholdValue = 0.5,
  showSummary = FALSE,
  showAbout = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- data:

  .

- dep:

  .

- indep:

  .

- testSize:

  .

- noOfFolds:

  .

- testing:

  .

- reporting:

  .

- classifier:

  .

- minSplit:

  .

- complexity:

  .

- maxCompete:

  .

- maxSurrogate:

  .

- maxDepth:

  .

- noOfTrees:

  .

- maxDepthRandFor:

  .

- sampleFraction:

  .

- splitRule:

  .

- knnNeighbors:

  Number of nearest neighbors for KNN classification.

- knnDistance:

  .

- svmKernel:

  .

- svmCost:

  Regularization parameter for SVM.

- svmGamma:

  Kernel coefficient for SVM (used in RBF, polynomial, sigmoid kernels).

- plotDecisionTree:

  .

- predictedFreq:

  .

- printRandForest:

  .

- predictedFreqRF:

  .

- balancingMethod:

  Method for handling class imbalance in medical datasets.

- validateMethod:

  Validation method for clinical model assessment.

- bootstrapSamples:

  Number of bootstrap samples for confidence intervals.

- reportClinicalMetrics:

  Report sensitivity, specificity, PPV, NPV, and likelihood ratios.

- reportConfidenceIntervals:

  Include 95 percent confidence intervals for clinical metrics.

- reportMCC:

  Calculate Matthews Correlation Coefficient (MCC), a balanced metric
  for imbalanced datasets. MCC ranges from -1 (perfect disagreement) to
  +1 (perfect agreement), with 0 indicating random prediction.

- positiveClass:

  Specify which class should be considered "positive" for clinical
  metrics (sensitivity, specificity, PPV, NPV, likelihood ratios). If
  left empty, the second factor level will be used as positive. This is
  critical for ensuring correct interpretation of diagnostic performance
  metrics.

- seed:

  Set random seed for reproducibility. Using the same seed will produce
  identical results across runs, which is critical for validating
  analyses.

- thresholdMethod:

  Method for determining classification threshold for binary outcomes.
  Youden J maximizes sensitivity + specificity. Manual allows custom
  cutoff.

- thresholdValue:

  Custom probability threshold when using manual threshold method. Only
  used when Decision Threshold Method is set to Manual Cutoff.

- showSummary:

  Display a plain-language summary of classification results suitable
  for copying to clinical reports.

- showAbout:

  Display information about classification methodology and
  interpretation guidance.

- showGlossary:

  Display glossary of machine learning and clinical metrics terms.

## Value

A results object containing:

|                                                 |     |     |     |     |          |
|-------------------------------------------------|-----|-----|-----|-----|----------|
| `results$warnings`                              |     |     |     |     | a html   |
| `results$modelSettings`                         |     |     |     |     | a html   |
| `results$confusion$matrix`                      |     |     |     |     | a table  |
| `results$classificationMetrics$general`         |     |     |     |     | a table  |
| `results$classificationMetrics$clinicalMetrics` |     |     |     |     | a table  |
| `results$classificationMetrics$mccTable`        |     |     |     |     | a table  |
| `results$classificationMetrics$class`           |     |     |     |     | a table  |
| `results$rocCurvePlot`                          |     |     |     |     | an image |
| `results$decisionTreeModel`                     |     |     |     |     | an image |
| `results$predictedFreqPlot`                     |     |     |     |     | an image |
| `results$printRandForest$randomForestModel`     |     |     |     |     | a table  |
| `results$text`                                  |     |     |     |     | a html   |
| `results$naturalSummary`                        |     |     |     |     | a html   |
| `results$aboutAnalysis`                         |     |     |     |     | a html   |
| `results$glossaryPanel`                         |     |     |     |     | a html   |
