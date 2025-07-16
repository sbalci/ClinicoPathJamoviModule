# Time-Dependent ROC Analysis Documentation

This document provides a comprehensive overview of the Time-Dependent ROC Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Time-Dependent ROC Analysis module is designed for the comprehensive evaluation of biomarker predictive performance over time in survival analysis. It supports various ROC estimation methods, provides bootstrap confidence intervals for robust inference, calculates optimal cutoff values, and offers extensive visualization and clinical interpretation of results. It also includes capabilities for comparing model performance against a baseline.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis Setup**          |                                |                                        |                                     |                                      |
| Time Elapsed                     | `elapsedtime`                  | Time Elapsed                           | `text`, `aucTable`, `rocPlot`, `aucPlot` | `.cleanData`, `.run`                 |
| Outcome                          | `outcome`                      | Outcome                                | `text`, `aucTable`, `rocPlot`, `aucPlot` | `.cleanData`, `.run`                 |
| Event Level                      | `outcomeLevel`                 | Event Level                            | `text`, `aucTable`, `rocPlot`, `aucPlot` | `.cleanData`, `.run`                 |
| Marker Variable                  | `marker`                       | Marker Variable                        | `text`, `aucTable`, `rocPlot`, `aucPlot` | `.cleanData`, `.run`                 |
| Evaluation Timepoints            | `timepoints`                   | Evaluation Timepoints                  | `text`, `aucTable`, `rocPlot`, `aucPlot`, `cutoffTable` | `.parseTimepoints`, `.run`           |
| ROC Estimation Method            | `method`                       | ROC Estimation Method                  | `text`, `aucTable`, `aucPlot`       | `.run`                               |
| Bootstrap Confidence Intervals   | `bootstrapCI`                  | Bootstrap Confidence Intervals         | `text`, `aucTable`                  | `.run`                               |
| Number of Bootstrap Samples      | `nboot`                        | Number of Bootstrap Samples            | `text`                              | `.run`                               |
| Time Units                       | `timetypeoutput`               | Time Units                             | `text`, `aucPlot`                   | `.createInterpretation`, `.createClinicalInterpretation`, `.compareToBaseline` |
| **Output & Visualization**       |                                |                                        |                                     |                                      |
| Show Optimal Cutoff Values       | `showOptimalCutoff`            | Show Optimal Cutoff Values             | `cutoffTable`                       | `.calculateOptimalCutoffs`           |
| Show Marker Statistics           | `showMarkerStats`              | Show Marker Statistics                 | `markerStats`                       | `.calculateMarkerStats`              |
| Compare to Baseline Model        | `compareBaseline`              | Compare to Baseline Model              | `modelComparison`                   | `.compareToBaseline`                 |
| Smooth AUC Curve                 | `smoothAUC`                    | Smooth AUC Curve                       | `aucPlot`                           | `.plotAUC`                           |
| Plot ROC Curves                  | `plotROC`                      | Plot ROC Curves                        | `rocPlot`                           | `.plotROC`                           |
| Plot AUC Over Time               | `plotAUC`                      | Plot AUC Over Time                     | `aucPlot`                           | `.plotAUC`                           |
| **Interpretation**               |                                |                                        |                                        |                                      |
| Clinical Interpretation          | `clinicalInterpretation`       | Clinical Interpretation                | `clinicalInterpretation`            | `.createClinicalInterpretation`      |
