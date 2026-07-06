# IHC Diagnostic Prediction

Predicts diagnoses for cases with unknown diagnoses based on IHC marker
expression patterns, using a trained reference model.

Predicts diagnoses for cases with unknown diagnoses based on IHC marker
expression patterns, using a trained reference model from previously
analyzed cases with known diagnoses.

## Usage

``` r
ihcpredict(
  data,
  useStoredModel = "stored",
  trainingFile = "",
  trainingDiagnosis = "Diagnosis",
  catVars = NULL,
  contVars = NULL,
  caseId = NULL,
  predictionMethod = "hybrid",
  confidenceThreshold = 0.5,
  highConfidenceThreshold = 0.75,
  lowConfidenceThreshold = 0.25,
  panelWeight = 0.5,
  distanceWeight = 0.3,
  silhouetteWeight = 0.2,
  showAlternatives = TRUE,
  nAlternatives = 3,
  showEvidence = TRUE,
  flagLowConfidence = TRUE,
  showMarkerComparison = TRUE,
  validateMarkers = TRUE,
  requireAllMarkers = TRUE,
  scaleContVars = TRUE,
  handleMissing = "pairwise",
  exportPredictions = FALSE,
  predictionVarName = "Predicted_Diagnosis",
  confidenceVarName = "Prediction_Confidence",
  showPredictionPlot = TRUE,
  showConfidencePlot = TRUE,
  colorPalette = "colorblind",
  fontSize = "medium",
  plotContrast = FALSE
)

ihcpredict(
  data,
  useStoredModel = "stored",
  trainingFile = "",
  trainingDiagnosis = "Diagnosis",
  catVars = NULL,
  contVars = NULL,
  caseId = NULL,
  predictionMethod = "hybrid",
  confidenceThreshold = 0.5,
  highConfidenceThreshold = 0.75,
  lowConfidenceThreshold = 0.25,
  panelWeight = 0.5,
  distanceWeight = 0.3,
  silhouetteWeight = 0.2,
  showAlternatives = TRUE,
  nAlternatives = 3,
  showEvidence = TRUE,
  flagLowConfidence = TRUE,
  showMarkerComparison = TRUE,
  validateMarkers = TRUE,
  requireAllMarkers = TRUE,
  scaleContVars = TRUE,
  handleMissing = "pairwise",
  exportPredictions = FALSE,
  predictionVarName = "Predicted_Diagnosis",
  confidenceVarName = "Prediction_Confidence",
  showPredictionPlot = TRUE,
  showConfidencePlot = TRUE,
  colorPalette = "colorblind",
  fontSize = "medium",
  plotContrast = FALSE
)
```

## Arguments

- data:

  The query data as a data frame (cases with unknown diagnoses).

- useStoredModel:

  Source of the trained reference model

- trainingFile:

  Path to CSV file with training cases (must include Diagnosis column)

- trainingDiagnosis:

  Name of the diagnosis variable in training dataset

- catVars:

  Binary (pos/neg) or ordinal (0/1/2/3) stain results - must match
  training markers

- contVars:

  H-scores (0-300), percent positivity (0-100) - must match training
  markers

- caseId:

  Case identifier for tracking predictions

- predictionMethod:

  Algorithm for predicting diagnoses

- confidenceThreshold:

  Minimum confidence score to accept prediction (0-1)

- highConfidenceThreshold:

  Threshold for "High Confidence" classification

- lowConfidenceThreshold:

  Threshold below which cases are flagged as very low confidence

- panelWeight:

  Weight for rule-based panel matches (hybrid method only)

- distanceWeight:

  Weight for distance to centroid (hybrid method only)

- silhouetteWeight:

  Weight for silhouette score (hybrid method only)

- showAlternatives:

  Display differential diagnosis with top 3 alternatives

- nAlternatives:

  Number of alternative diagnoses to report

- showEvidence:

  Display detailed evidence supporting each prediction

- flagLowConfidence:

  Create separate table for cases requiring review

- showMarkerComparison:

  Compare query case markers to cluster centroids

- validateMarkers:

  Check that query markers match training markers

- requireAllMarkers:

  Error if any training markers are missing from query data

- scaleContVars:

  Z-score continuous markers using training set parameters

- handleMissing:

  How to handle missing marker values in query data

- exportPredictions:

  Save predicted diagnoses and confidence scores to dataset

- predictionVarName:

  Name for exported prediction variable

- confidenceVarName:

  Name for exported confidence score variable

- showPredictionPlot:

  Display PCA/MCA plot with query cases and training clusters

- showConfidencePlot:

  Display histogram of prediction confidence scores

- colorPalette:

  Color palette for plots

- fontSize:

  Base font size for all text elements

- plotContrast:

  Enable high contrast mode for better visibility

## Value

A results object containing prediction tables and plots

A results object containing:

|                                 |     |     |     |     |                |
|---------------------------------|-----|-----|-----|-----|----------------|
| `results$instructions`          |     |     |     |     | a html         |
| `results$modelSummary`          |     |     |     |     | a html         |
| `results$validationResults`     |     |     |     |     | a table        |
| `results$predictions`           |     |     |     |     | a table        |
| `results$alternativeDiagnoses`  |     |     |     |     | a table        |
| `results$predictionEvidence`    |     |     |     |     | a table        |
| `results$lowConfidenceCases`    |     |     |     |     | a table        |
| `results$markerComparison`      |     |     |     |     | a table        |
| `results$predictionSummary`     |     |     |     |     | a table        |
| `results$confidenceByDiagnosis` |     |     |     |     | a table        |
| `results$predictionPlot`        |     |     |     |     | an image       |
| `results$confidencePlot`        |     |     |     |     | an image       |
| `results$markerHeatmap`         |     |     |     |     | an image       |
| `results$clinicalGuidance`      |     |     |     |     | a html         |
| `results$technicalDetails`      |     |     |     |     | a html         |
| `results$exportSummary`         |     |     |     |     | a preformatted |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$validationResults$asDF`

`as.data.frame(results$validationResults)`

## Details

**Requirements:**

- Training dataset analyzed with `ihccluster` (with known diagnoses)

- Query dataset with same IHC markers (unknown diagnoses to predict)

- Marker names and data types must match between datasets

**Output:**

- Predicted diagnosis per case with confidence level

- Alternative diagnoses (differential)

- Evidence supporting each prediction

- Flags for atypical/low-confidence cases
