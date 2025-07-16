# Classification Analysis Documentation

This document provides a comprehensive overview of the Classification Analysis module (classification), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The classification module is a versatile tool for building and evaluating classification models. It supports various classification algorithms and provides functionalities to assess model performance, identify important predictors, and visualize classification results.

The module's features can be broadly categorized as follows:

*   **Core Classification Algorithms:** Implement common classification methods (e.g., logistic regression, decision trees, random forest).
*   **Model Training and Evaluation:** Train classification models and assess their performance using metrics like accuracy, precision, recall, and F1-score.
*   **Feature Importance:** Identify the most influential variables in the classification model.
*   **Prediction and Visualization:** Generate predictions and visualize classification boundaries or probabilities.
*   **Export Options:** Capabilities to save model results, predictions, and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Outcome Variable                 | `outcomeVar`                   | Outcome Variable                       | `classificationOverview`            | `.trainClassificationModel`          |
| Predictor Variables              | `predictorVars`                | Predictor Variables                    | `classificationOverview`            | `.trainClassificationModel`          |
| Classification Method            | `classificationMethod`         | Classification Method                  | `classificationOverview`            | `.trainClassificationModel`          |
| **Model Evaluation**             |                                |                                        |                                     |                                      |
| Show Confusion Matrix            | `showConfusionMatrix`          | Show Confusion Matrix                  | `confusionMatrixResults`            | `.calculateConfusionMatrix`          |
| Show Performance Metrics         | `showPerformanceMetrics`       | Show Performance Metrics               | `performanceMetrics`                | `.calculatePerformanceMetrics`       |
| Show ROC Curve                   | `showROCCurve`                 | Show ROC Curve                         | `rocCurvePlot`                      | `.plotROCCurve`                      |
| Show AUC                         | `showAUC`                      | Show Area Under Curve (AUC)            | `rocCurvePlot`                      | `.calculateAUC`                      |
| **Feature Importance**           |                                |                                        |                                     |                                      |
| Show Feature Importance          | `showFeatureImportance`        | Show Feature Importance                | `featureImportanceResults`          | `.calculateFeatureImportance`        |
| **Prediction & Visualization**   |                                |                                        |                                     |                                      |
| Generate Predictions             | `generatePredictions`          | Generate Predictions                   | `predictions`                       | `.generatePredictions`               |
| Show Decision Boundary Plot      | `showDecisionBoundaryPlot`     | Show Decision Boundary Plot            | `decisionBoundaryPlot`              | `.plotDecisionBoundary`              |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Cross-Validation                 | `crossValidation`              | Cross-Validation                       | `advancedOptions`                   | `.performCrossValidation`            |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportClassificationResults`       |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportClassificationPlot`          |