# Decision Panel Analysis Documentation

This document provides a comprehensive overview of the Decision Panel Analysis module (decisionpanel), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The decisionpanel module is a powerful tool for evaluating the performance of diagnostic tests or prediction models. It provides a comprehensive set of metrics and visualizations to assess accuracy, discrimination, and clinical utility across various decision thresholds.

The module's features can be broadly categorized as follows:

*   **Core Performance Metrics:** Calculate sensitivity, specificity, positive predictive value (PPV), negative predictive value (NPV), accuracy, and likelihood ratios.
*   **Threshold Analysis:** Evaluate performance across a range of decision thresholds.
*   **ROC Curve Analysis:** Generate and interpret Receiver Operating Characteristic (ROC) curves.
*   **Decision Curve Analysis (DCA):** Assess the clinical net benefit of a diagnostic strategy.
*   **Visualization:** Create various plots to illustrate test performance and clinical utility.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Outcome Variable                 | `outcomeVar`                   | Outcome Variable                       | `decisionPanelOverview`             | `.calculatePerformance`              |
| Predictor Variable               | `predictorVar`                 | Predictor Variable                     | `decisionPanelOverview`             | `.calculatePerformance`              |
| Outcome Event Level              | `outcomeEventLevel`            | Outcome Event Level                    | `decisionPanelOverview`             | `.calculatePerformance`              |
| **Performance Metrics**          |                                |                                        |                                     |                                      |
| Show Sensitivity                 | `showSensitivity`              | Show Sensitivity                       | `performanceMetrics`                | `.calculateSensitivity`              |
| Show Specificity                 | `showSpecificity`              | Show Specificity                       | `performanceMetrics`                | `.calculateSpecificity`              |
| Show PPV                         | `showPPV`                      | Show Positive Predictive Value (PPV)   | `performanceMetrics`                | `.calculatePPV`                      |
| Show NPV                         | `showNPV`                      | Show Negative Predictive Value (NPV)   | `performanceMetrics`                | `.calculateNPV`                      |
| Show Accuracy                    | `showAccuracy`                 | Show Accuracy                          | `performanceMetrics`                | `.calculateAccuracy`                 |
| Show Likelihood Ratios           | `showLikelihoodRatios`         | Show Likelihood Ratios                 | `performanceMetrics`                | `.calculateLikelihoodRatios`         |
| **Threshold Analysis**           |                                |                                        |                                     |                                      |
| Custom Thresholds                | `customThresholds`             | Custom Thresholds                      | `thresholdAnalysis`                 | `.analyzeThresholds`                 |
| **ROC Curve Analysis**           |                                |                                        |                                     |                                      |
| Show ROC Curve                   | `showROCCurve`                 | Show ROC Curve                         | `rocCurvePlot`                      | `.plotROCCurve`                      |
| Show AUC                         | `showAUC`                      | Show Area Under Curve (AUC)            | `rocCurvePlot`                      | `.calculateAUC`                      |
| **Decision Curve Analysis**      |                                |                                        |                                     |                                      |
| Show Decision Curve              | `showDecisionCurve`            | Show Decision Curve                    | `decisionCurvePlot`                 | `.plotDecisionCurve`                 |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportPerformanceResults`          |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportPerformancePlot`             |