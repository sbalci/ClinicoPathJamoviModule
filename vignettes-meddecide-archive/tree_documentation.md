# Medical Decision Tree Analysis Documentation

This document provides a comprehensive overview of the Medical Decision Tree Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Medical Decision Tree Analysis module is designed for building and evaluating decision trees specifically for medical diagnosis and prognosis. It offers a robust set of features including clinical performance metrics, advanced data handling for medical data (missing values, class imbalance), feature importance analysis, and clinical interpretation guidelines. It also supports comparison with alternative models and spatial analysis using `autocart`.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Model Building**          |                                |                                        |                                     |                                      |
| Continuous Clinical Variables    | `vars`                         | Continuous Clinical Variables          | `modelSummary`                      | `.prepareData`                       |
| Categorical Clinical Variables   | `facs`                         | Categorical Clinical Variables         | `modelSummary`                      | `.prepareData`                       |
| Target Outcome                   | `target`                       | Target Outcome                         | `modelSummary`                      | `.prepareData`                       |
| Disease/Positive Level           | `targetLevel`                  | Disease/Positive Level                 | `modelSummary`                      | `.prepareData`                       |
| Training/Validation Cohort       | `train`                        | Training/Validation Cohort             | `modelSummary`                      | `.prepareData`                       |
| Training Cohort Level            | `trainLevel`                   | Training Cohort Level                  | `modelSummary`                      | `.prepareData`                       |
| **Data Handling**                |                                |                                        |                                     |                                      |
| Handle Missing Data              | `imputeMissing`                | Handle Missing Data                    | `missingDataReport`                 | `.imputeMedicalData`                 |
| Balance Disease Classes          | `balanceClasses`               | Balance Disease Classes                | `modelSummary`                      | `.balanceData`                       |
| Standardize Biomarkers           | `scaleFeatures`                | Standardize Biomarkers                 | `modelSummary`                      | `.prepareData`                       |
| **Performance Metrics**          |                                |                                        |                                     |                                      |
| Show Clinical Performance Metrics| `clinicalMetrics`              | Show Clinical Performance Metrics      | `clinicalMetrics`                   | `.calculateClinicalMetrics`          |
| Analyze Feature Importance       | `featureImportance`            | Analyze Feature Importance             | `featureImportance`                 | `.analyzeFeatureImportance`          |
| Risk Stratification Analysis     | `riskStratification`           | Risk Stratification Analysis           | `riskStratification`                | `.performRiskStratification`         |
| Adjust for Population Prevalence | `prevalenceAdjustment`         | Adjust for Population Prevalence       | `adjustedMetrics`                   | `.adjustForPrevalence`               |
| Expected Population Prevalence (%)| `expectedPrevalence`           | Expected Population Prevalence (%)     | `adjustedMetrics`                   | `.adjustForPrevalence`               |
| **Validation**                   |                                |                                        |                                     |                                      |
| Perform Cross-Validation         | `crossValidation`              | Perform Cross-Validation               | `crossValidationResults`            | `.performCrossValidation`            |
| Number of CV Folds               | `cvFolds`                      | Number of CV Folds                     | `crossValidationResults`            | `.performCrossValidation`            |
| Bootstrap Validation             | `bootstrapValidation`          | Bootstrap Validation                   | `bootstrapResults`                  | `.performBootstrapValidation`        |
| Bootstrap Samples                | `bootstrapSamples`             | Bootstrap Samples                      | `bootstrapResults`                  | `.performBootstrapValidation`        |
| **Model Comparison**             |                                |                                        |                                     |                                      |
| Compare with Alternative Models  | `compareModels`                | Compare with Alternative Models        | `modelComparison`                   | `.compareModels`                     |
| Primary Comparison Metric        | `modelComparisonMetric`        | Primary Comparison Metric              | `modelComparison`                   | `.compareModels`                     |
| **Spatial Analysis (Autocart)**  |                                |                                        |                                     |                                      |
| Spatial Coordinates (X, Y)       | `spatialCoords`                | Spatial Coordinates (X, Y)             | `spatialAnalysis`, `spatialInterpretation` | `.performAutocartAnalysis`           |
| Enable Spatial Autocart Analysis | `useAutocart`                  | Enable Spatial Autocart Analysis       | `spatialAnalysis`, `spatialInterpretation` | `.performAutocartAnalysis`           |
| Spatial Autocorrelation Weight (α)| `spatialAlpha`                 | Spatial Autocorrelation Weight (α)     | `spatialAnalysis`                   | `.performAutocartAnalysis`           |
| Spatial Compactness Weight (β)   | `spatialBeta`                  | Spatial Compactness Weight (β)         | `spatialAnalysis`                   | `.performAutocartAnalysis`           |
| **Visualization**                |                                |                                        |                                     |                                      |
| Show Decision Tree Plot          | `showPlot`                     | Show Decision Tree Plot                | `plot`                              | `.plot`                              |
| Show Partition Plot (2D Variables Only)| `showPartitionPlot`            | Show Partition Plot (2D Variables Only)| `partitionPlot`                     | `.plotPartition`                     |
| Show ROC Curve                   | `showROCCurve`                 | Show ROC Curve                         | `rocPlot`                           | `.plotROC`                           |
| Show Calibration Plot            | `showCalibrationPlot`          | Show Calibration Plot                  | `calibrationPlot`                   | `.plotCalibration`                   |
| Show Clinical Utility Curve      | `showClinicalUtility`          | Show Clinical Utility Curve            | `clinicalUtilityPlot`               | `.plotClinicalUtility`               |
| **Reporting & Interpretation**   |                                |                                        |                                     |                                      |
| Clinical Interpretation          | `showInterpretation`           | Clinical Interpretation                | `clinicalInterpretation`            | `.generateClinicalInterpretation`    |
| Clinical Context                 | `clinicalContext`              | Clinical Context                       | `clinicalInterpretation`, `deploymentGuidelines` | `.generateClinicalInterpretation`, `.generateDeploymentGuidelines` |
| Data Quality Report              | `dataQuality`                  | Data Quality Report                    | `dataQuality`                       | `.generateDataQualityReport`         |
| Model Summary                    | `modelSummary`                 | Decision Tree Model Summary            | `modelSummary`                      | `.run`                               |
| Confusion Matrix & Clinical Outcomes| `confusionMatrix`              | Confusion Matrix & Clinical Outcomes   | `confusionMatrix`                   | `.run`                               |
| Clinical Implementation Guidelines| `deploymentGuidelines`         | Clinical Implementation Guidelines     | `deploymentGuidelines`              | `.generateDeploymentGuidelines`      |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Minimum Cases per Node           | `minCases`                     | Minimum Cases per Node                 | `modelSummary`                      | `.run`                               |
| Maximum Tree Depth               | `maxDepth`                     | Maximum Tree Depth                     | `modelSummary`                      | `.run`                               |
| Show Confidence Intervals        | `confidenceInterval`           | Show Confidence Intervals              | `clinicalMetrics`                   | `.calculateClinicalMetrics`          |
| Export Predictions               | `exportPredictions`            | Export Predictions                     | `predictions`, `probabilities`      | `.run`                               |
| Variable Importance Method       | `variableImportanceMethod`     | Variable Importance Method             | `featureImportance`                 | `.analyzeFeatureImportance`          |
| Use Custom Performance Thresholds| `customThresholds`             | Use Custom Performance Thresholds      | `clinicalMetrics`                   | Not implemented in `.b.R`            |
| Minimum Acceptable Sensitivity   | `sensitivityThreshold`         | Minimum Acceptable Sensitivity         | `clinicalMetrics`                   | Not implemented in `.b.R`            |
| Minimum Acceptable Specificity   | `specificityThreshold`         | Minimum Acceptable Specificity         | `clinicalMetrics`                   | Not implemented in `.b.R`            |
| Tree Visualization Style         | `treeVisualization`            | Tree Visualization Style               | `plot`                              | Not implemented in `.b.R`            |
| Show Node Statistics             | `showNodeStatistics`           | Show Node Statistics                   | `plot`                              | Not implemented in `.b.b.R`          |
