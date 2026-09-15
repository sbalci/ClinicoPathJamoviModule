# Imaging Findings Correlation

Imaging Findings Correlation provides comprehensive multi-modal
diagnostic data integration by correlating imaging findings with
laboratory results, clinical presentations, and pathological data for
enhanced diagnostic accuracy and clinical decision support through
evidence-based pattern recognition and cross-modality validation.

## Usage

``` r
imagingcorrelation(
  data,
  imagingFindings,
  labResults,
  clinicalVars,
  pathologyData,
  imagingModality,
  correlation_analysis = TRUE,
  concordance_assessment = TRUE,
  pattern_recognition = TRUE,
  sensitivity_specificity = TRUE,
  integration_method = "weighted_fusion",
  correlation_method = "spearman",
  confidence_level = 0.95,
  minimum_correlation = 0.3,
  radiomics_analysis = FALSE,
  texture_analysis = FALSE,
  lesion_characterization = TRUE,
  temporal_analysis = FALSE,
  anatomical_correlation = TRUE,
  staging_correlation = TRUE,
  treatment_response = FALSE,
  ai_assisted = FALSE,
  correlation_plots = TRUE,
  concordance_plots = TRUE,
  heatmap_visualization = TRUE,
  network_diagram = FALSE,
  roc_curves = TRUE,
  clinical_guidelines = TRUE,
  diagnostic_confidence = TRUE,
  report_generation = TRUE
)
```

## Arguments

- data:

  .

- imagingFindings:

  Imaging findings from various modalities (CT, MRI, PET, ultrasound,
  X-ray)

- labResults:

  Laboratory test results and biomarkers for correlation analysis
  (optional)

- clinicalVars:

  Clinical symptoms, signs, and examination findings (optional)

- pathologyData:

  Pathological findings and histopathology results for validation
  (optional)

- imagingModality:

  Type of imaging modality for each finding (CT, MRI, PET, etc.)
  (optional)

- correlation_analysis:

  Perform comprehensive correlation analysis across imaging, lab, and
  clinical data

- concordance_assessment:

  Evaluate concordance between different diagnostic modalities

- pattern_recognition:

  Identify diagnostic patterns across multiple data modalities

- sensitivity_specificity:

  Calculate sensitivity, specificity, PPV, NPV for imaging findings

- integration_method:

  Method for integrating multi-modal diagnostic data

- correlation_method:

  Statistical method for correlation analysis

- confidence_level:

  Confidence level for statistical calculations and intervals

- minimum_correlation:

  Minimum correlation coefficient to report as significant

- radiomics_analysis:

  Perform advanced radiomics feature extraction and analysis

- texture_analysis:

  Analyze texture features from imaging data for pattern detection

- lesion_characterization:

  Comprehensive lesion characterization across imaging modalities

- temporal_analysis:

  Analyze temporal changes in imaging findings over time

- anatomical_correlation:

  Correlate findings based on anatomical location and distribution

- staging_correlation:

  Correlate imaging findings with disease staging and progression

- treatment_response:

  Evaluate treatment response using imaging criteria (RECIST, mRECIST,
  etc.)

- ai_assisted:

  Use AI/ML models for enhanced pattern recognition and prediction

- correlation_plots:

  Create comprehensive correlation visualization plots

- concordance_plots:

  Generate concordance plots between diagnostic modalities

- heatmap_visualization:

  Create heatmaps showing multi-modal correlations

- network_diagram:

  Generate network diagrams showing relationships between findings

- roc_curves:

  Generate ROC curves for diagnostic performance assessment

- clinical_guidelines:

  Integrate evidence-based imaging guidelines and recommendations

- diagnostic_confidence:

  Calculate and report diagnostic confidence levels for integrated
  findings

- report_generation:

  Generate comprehensive integrated diagnostic report with
  recommendations

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                        |     |     |     |     | a html   |
| `results$correlationTable`            |     |     |     |     | a table  |
| `results$concordanceTable`            |     |     |     |     | a table  |
| `results$diagnosticPerformanceTable`  |     |     |     |     | a table  |
| `results$patternRecognitionTable`     |     |     |     |     | a table  |
| `results$lesionCharacterizationTable` |     |     |     |     | a table  |
| `results$stagingCorrelationTable`     |     |     |     |     | a table  |
| `results$treatmentResponseTable`      |     |     |     |     | a table  |
| `results$radiomicsTable`              |     |     |     |     | a table  |
| `results$integratedDiagnosticSummary` |     |     |     |     | a html   |
| `results$clinicalRecommendations`     |     |     |     |     | a html   |
| `results$confidenceAssessment`        |     |     |     |     | a html   |
| `results$correlationPlot`             |     |     |     |     | an image |
| `results$concordancePlot`             |     |     |     |     | an image |
| `results$heatmapPlot`                 |     |     |     |     | an image |
| `results$networkPlot`                 |     |     |     |     | an image |
| `results$rocPlot`                     |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$correlationTable$asDF`

`as.data.frame(results$correlationTable)`
