# Comprehensive ROC Analysis with Advanced Features Documentation

This document provides a comprehensive overview of the Comprehensive ROC Analysis with Advanced Features module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs sophisticated Receiver Operating Characteristic (ROC) curve analysis with optimal cutpoint determination, multiple comparison methods, and advanced statistical features. It includes IDI (Integrated Discrimination Index) and NRI (Net Reclassification Index) calculations, DeLong test for AUC comparison, partial AUC calculations, and comprehensive visualization options. The module also supports subgroup analysis, cost-benefit optimization, and interactive plots, making it a powerful tool for diagnostic and prognostic research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Data Input**                   |                                |                                        |                                     |                                      |
| Test Variables                   | `dependentVars`                | Test Variables                         | `simpleResultsTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `idiTable`, `nriTable` | `.run`, `.prepareVarData`, `.runCutpointrMetrics`, `.generateTables`, `.calculatePartialAUC`, `.createSmoothedROC`, `.calculateBootstrapCI`, `.calculatePrecisionRecall`, `.calculateClassifierMetrics`, `.deLongTest`, `.enhancedDelongTest` |
| Class Variable (Gold Standard)   | `classVar`                     | Class Variable (Gold Standard)         | `simpleResultsTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `idiTable`, `nriTable` | `.run`, `.prepareVarData`, `.runCutpointrMetrics`, `.generateTables`, `.calculatePartialAUC`, `.createSmoothedROC`, `.calculateBootstrapCI`, `.calculatePrecisionRecall`, `.calculateClassifierMetrics`, `.deLongTest`, `.enhancedDelongTest` |
| Positive Class                   | `positiveClass`                | Positive Class                         | `simpleResultsTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `idiTable`, `nriTable` | `.run`, `.prepareVarData`, `.runCutpointrMetrics`, `.generateTables`, `.calculatePartialAUC`, `.createSmoothedROC`, `.calculateBootstrapCI`, `.calculatePrecisionRecall`, `.calculateClassifierMetrics`, `.deLongTest`, `.enhancedDelongTest` |
| Subgroup Variable (Optional)     | `subGroup`                     | Subgroup Variable (Optional)           | `simpleResultsTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `aucSummaryTable`, `plotROC`, `interactivePlot`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable` | `.run`, `.prepareVarData`            |
| **Cutpoint Optimization**        |                                |                                        |                                     |                                      |
| Cutpoint Method                  | `method`                       | Cutpoint Method                        | `resultsTable`, `sensSpecTable`, `thresholdTable` | `.run`, `.runCutpointrMetrics`       |
| Optimization Metric              | `metric`                       | Optimization Metric                    | `resultsTable`, `thresholdTable`    | `.run`, `.runCutpointrMetrics`       |
| Classification Direction         | `direction`                    | Classification Direction               | `resultsTable`, `thresholdTable`    | `.run`, `.runCutpointrMetrics`       |
| Manual Cutpoint Value            | `specifyCutScore`              | Manual Cutpoint Value                  | `resultsTable`                      | `.run`                               |
| Metric Tolerance                 | `tol_metric`                   | Metric Tolerance                       | `resultsTable`                      | `.run`                               |
| Tie Breaking Method              | `break_ties`                   | Tie Breaking Method                    | `resultsTable`                      | `.run`                               |
| **Analysis Options**             |                                |                                        |                                     |                                      |
| Show All Observed Cutpoints      | `allObserved`                  | Show All Observed Cutpoints            | `resultsTable`                      | `.run`                               |
| Bootstrap Iterations             | `boot_runs`                    | Bootstrap Iterations                   | `resultsTable`                      | `.run`                               |
| Use Prior Prevalence             | `usePriorPrev`                 | Use Prior Prevalence                   | `prevalencePlot`                    | `.run`                               |
| Prior Prevalence Value           | `priorPrev`                    | Prior Prevalence Value                 | `prevalencePlot`                    | `.run`                               |
| Cost Ratio (FP:FN)               | `costratioFP`                  | Cost Ratio (FP:FN)                     | `resultsTable`                      | `.run`                               |
| **Basic Output Options**         |                                |                                        |                                     |                                      |
| Show Confusion Matrices          | `sensSpecTable`                | Show Confusion Matrices                | `sensSpecTable`                     | `.run`                               |
| Show Threshold Table             | `showThresholdTable`           | Show Threshold Table                   | `thresholdTable`                    | `.run`                               |
| Maximum Thresholds to Display    | `maxThresholds`                | Maximum Thresholds to Display          | `thresholdTable`                    | `.run`                               |
| Compare AUCs (DeLong's Test)     | `delongTest`                   | Compare AUCs (DeLong's Test)          | `delongComparisonTable`, `delongTest` | `.run`, `.deLongTest`, `.enhancedDelongTest` |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show ROC Curves                  | `plotROC`                      | Show ROC Curves                        | `plotROC`                           | `.run`                               |
| Combine Multiple ROC Curves      | `combinePlots`                 | Combine Multiple ROC Curves            | `plotROC`                           | `.run`                               |
| Publication-Ready Plot           | `cleanPlot`                    | Publication-Ready Plot                 | `plotROC`                           | Not implemented in `.b.R`            |
| Mark Optimal Cutpoint            | `showOptimalPoint`             | Mark Optimal Cutpoint                  | `plotROC`                           | Not implemented in `.b.R`            |
| Show Standard Error Bands        | `displaySE`                    | Show Standard Error Bands              | `plotROC`                           | Not implemented in `.b.R`            |
| Apply LOESS Smoothing            | `smoothing`                    | Apply LOESS Smoothing                  | `plotROC`                           | Not implemented in `.b.R`            |
| Show Confidence Bands            | `showConfidenceBands`          | Show Confidence Bands                  | `plotROC`                           | Not implemented in `.b.R`            |
| Legend Position                  | `legendPosition`               | Legend Position                        | `plotROC`                           | Not implemented in `.b.R`            |
| Direct Curve Labels              | `directLabel`                  | Direct Curve Labels                    | `plotROC`                           | Not implemented in `.b.R`            |
| Create Interactive Plot          | `interactiveROC`               | Create Interactive Plot                | `interactivePlot`                   | Not implemented in `.b.R`            |
| **Additional Plots**             |                                |                                        |                                     |                                      |
| Sensitivity/Specificity vs Threshold| `showCriterionPlot`            | Sensitivity/Specificity vs Threshold   | `criterionPlot`                     | `.run`                               |
| Predictive Values vs Prevalence  | `showPrevalencePlot`           | Predictive Values vs Prevalence        | `prevalencePlot`                    | `.run`                               |
| Test Value Distribution          | `showDotPlot`                  | Test Value Distribution                | `dotPlot`                           | `.run`                               |
| Precision-Recall Curve           | `precisionRecallCurve`         | Precision-Recall Curve                 | `precisionRecallPlot`               | `.run`                               |
| **Advanced Analyses**            |                                |                                        |                                     |                                      |
| Calculate Partial AUC            | `partialAUC`                   | Calculate Partial AUC                  | `partialAUCTable`                   | `.run`                               |
| Partial AUC From (Specificity)   | `partialAUCfrom`               | Partial AUC From (Specificity)         | `partialAUCTable`                   | `.run`                               |
| Partial AUC To (Specificity)     | `partialAUCto`                 | Partial AUC To (Specificity)           | `partialAUCTable`                   | `.run`                               |
| ROC Smoothing Method             | `rocSmoothingMethod`           | ROC Smoothing Method                   | `plotROC`                           | `.run`                               |
| Bootstrap Confidence Intervals   | `bootstrapCI`                  | Bootstrap Confidence Intervals         | `bootstrapCITable`                  | `.run`                               |
| Bootstrap Replications           | `bootstrapReps`                | Bootstrap Replications                 | `bootstrapCITable`                  | `.run`                               |
| Show CIs at Quantiles            | `quantileCIs`                  | Show CIs at Quantiles                  | `plotROC`                           | Not implemented in `.b.R`            |
| Quantile Positions               | `quantiles`                    | Quantile Positions                     | `plotROC`                           | Not implemented in `.b.R`            |
| **Model Comparison**             |                                |                                        |                                     |                                      |
| Compare Classifier Performance   | `compareClassifiers`           | Compare Classifier Performance         | `rocComparisonTable`                | `.run`                               |
| Calculate IDI                    | `calculateIDI`                 | Calculate IDI                          | `idiTable`                          | `.run`                               |
| Calculate NRI                    | `calculateNRI`                 | Calculate NRI                          | `nriTable`                          | `.run`                               |
| Reference Variable               | `refVar`                       | Reference Variable                     | `idiTable`, `nriTable`              | `.run`                               |
| NRI Risk Categories              | `nriThresholds`                | NRI Risk Categories                    | `nriTable`                          | `.run`                               |
| IDI/NRI Bootstrap Iterations     | `idiNriBootRuns`               | IDI/NRI Bootstrap Iterations           | `idiTable`, `nriTable`              | `.run`                               |
