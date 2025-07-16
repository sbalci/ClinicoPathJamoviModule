# Prediction Model Builder for Clinical Decision Analysis Documentation

This document provides a comprehensive overview of the Prediction Model Builder for Clinical Decision Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module is a comprehensive clinical prediction model builder with advanced validation and performance assessment. It creates multiple logistic regression models optimized for integration with Decision Curve Analysis. The module provides robust error handling, comprehensive validation (cross-validation, bootstrap), and clinical interpretation guidance. It supports various modeling approaches, including basic clinical models, enhanced clinical models, biomarker models, and custom models, along with advanced options for missing data handling, variable transformations, and interaction terms.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Outcome & Basic Setup**        |                                |                                        |                                     |                                      |
| Outcome Variable                 | `outcome`                      | Outcome Variable                       | `dataSummary`, `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary`, `modelComparisonTable`, `validationResults`, `nriTable`, `idiTable`, `rocCurvesPlot`, `calibrationPlotsArray`, `modelComparisonPlot`, `validationPlot` | `.run`, `.validateInputs`, `.splitData`, `.createDataSummary`, `.buildAllModels`, `.calculateAllPerformance`, `.performAllCrossValidation`, `.performBootstrapValidation`, `.calculateAdvancedMetrics`, `.addPredictionsToDataset`, `.prepareDCAOutput`, `.generateAllRiskScores` |
| Positive Outcome Level           | `outcomePositive`              | Positive Outcome Level                 | `dataSummary`, `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary`, `modelComparisonTable`, `validationResults`, `nriTable`, `idiTable`, `rocCurvesPlot`, `calibrationPlotsArray`, `modelComparisonPlot`, `validationPlot` | `.run`, `.validateInputs`, `.splitData`, `.createDataSummary`, `.buildAllModels`, `.calculateAllPerformance`, `.performAllCrossValidation`, `.performBootstrapValidation`, `.calculateAdvancedMetrics`, `.addPredictionsToDataset`, `.prepareDCAOutput`, `.generateAllRiskScores` |
| Data Splitting                   | `splitData`                    | Data Splitting                         | `dataSummary`, `modelComparisonTable`, `validationResults`, `rocCurvesPlot`, `calibrationPlotsArray`, `modelComparisonPlot`, `validationPlot` | `.run`, `.splitData`                 |
| Random Seed                      | `randomSeed`                   | Random Seed                            | `dataSummary`, `validationResults`  | `.run`, `.splitData`                 |
| **Model Definitions**            |                                |                                        |                                     |                                      |
| Build Basic Clinical Model       | `buildBasicModel`              | Build Basic Clinical Model             | `basicModelSummary`, `modelComparisonTable` | `.run`, `.buildAllModels`            |
| Basic Model Predictors           | `basicPredictors`              | Basic Model Predictors                 | `basicModelSummary`                 | `.run`, `.buildAllModels`            |
| Basic Model Name                 | `basicModelName`               | Basic Model Name                       | `dcaReadyMessage`                   | `.run`                               |
| Build Enhanced Clinical Model    | `buildEnhancedModel`           | Build Enhanced Clinical Model          | `enhancedModelSummary`, `modelComparisonTable` | `.run`, `.buildAllModels`            |
| Enhanced Model Predictors        | `enhancedPredictors`           | Enhanced Model Predictors              | `enhancedModelSummary`              | `.run`, `.buildAllModels`            |
| Enhanced Model Name              | `enhancedModelName`            | Enhanced Model Name                    | `dcaReadyMessage`                   | `.run`                               |
| Build Biomarker Model            | `buildBiomarkerModel`          | Build Biomarker Model                  | `biomarkerModelSummary`, `modelComparisonTable` | `.run`, `.buildAllModels`            |
| Biomarker Model Predictors       | `biomarkerPredictors`          | Biomarker Model Predictors             | `biomarkerModelSummary`             | `.run`, `.buildAllModels`            |
| Biomarker Model Name             | `biomarkerModelName`           | Biomarker Model Name                   | `dcaReadyMessage`                   | `.run`                               |
| Build Custom Model               | `buildCustomModel`             | Build Custom Model                     | `customModelSummary`, `modelComparisonTable` | `.run`, `.buildAllModels`            |
| Custom Model Predictors          | `customPredictors`             | Custom Model Predictors                | `customModelSummary`                | `.run`, `.buildAllModels`            |
| Custom Model Name                | `customModelName`              | Custom Model Name                      | `dcaReadyMessage`                   | `.run`                               |
| **Advanced Modeling Options**    |                                |                                        |                                     |                                      |
| Include Interactions             | `includeInteractions`          | Include Interactions                   | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.createInteractions`        |
| Specific Interactions            | `interactionTerms`             | Specific Interactions                  | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.createInteractions`        |
| Stepwise Selection               | `useStepwise`                  | Stepwise Selection                     | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.applyStepwiseSelection`    |
| Stepwise Direction               | `stepwiseDirection`            | Stepwise Direction                     | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.applyStepwiseSelection`    |
| Selection Criterion              | `selectionCriterion`           | Selection Criterion                    | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.applyStepwiseSelection`    |
| Variable Transformations         | `transformVariables`           | Variable Transformations               | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.transformVariables`        |
| Transformation Method            | `transformMethod`              | Transformation Method                  | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`, `.transformVariables`        |
| **Missing Data Handling**        |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataMethod`            | Missing Data Handling                  | `dataSummary`                       | `.run`, `.handleMissingData`         |
| Multiple Imputation Sets         | `imputationSets`               | Multiple Imputation Sets               | `dataSummary`                       | `.run`, `.handleMissingData`         |
| **Validation Options**           |                                |                                        |                                     |                                      |
| Cross-Validation                 | `crossValidation`              | Cross-Validation                       | `validationResults`                 | `.run`, `.performCrossValidation`    |
| CV Folds                         | `cvFolds`                      | CV Folds                               | `validationResults`                 | `.run`, `.performCrossValidation`    |
| Bootstrap Validation             | `bootstrapValidation`          | Bootstrap Validation                   | `validationResults`                 | `.run`, `.performBootstrapValidation` |
| Bootstrap Replications           | `bootstrapReps`                | Bootstrap Replications                 | `validationResults`                 | `.run`, `.performBootstrapValidation` |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Model Summaries             | `showModelSummary`             | Show Model Summaries                   | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | `.run`                               |
| Show Performance Metrics         | `showPerformanceMetrics`       | Show Performance Metrics               | `modelComparisonTable`              | `.run`, `.calculateAllPerformance`   |
| Show Calibration Plots           | `showCalibrationPlots`         | Show Calibration Plots                 | `calibrationPlotsArray`             | `.plotCalibration`                   |
| Show ROC Curves                  | `showROCCurves`                | Show ROC Curves                        | `rocCurvesPlot`                     | `.plotROCCurves`                     |
| Model Comparison Table           | `compareModels`                | Model Comparison Table                 | `modelComparisonTable`              | `.run`, `.calculateAllPerformance`   |
| Add Predictions to Data          | `createPredictions`            | Add Predictions to Data                | `dcaReadyMessage`                   | `.run`, `.addPredictionsToDataset`   |
| Prepare for Decision Curve Analysis| `exportForDCA`                 | Prepare for Decision Curve Analysis    | `dcaReadyMessage`                   | `.run`, `.prepareDCAOutput`          |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Calculate Net Reclassification Index| `calculateNRI`                 | Calculate Net Reclassification Index   | `nriTable`                          | `.run`, `.calculateAdvancedMetrics` |
| NRI Risk Thresholds              | `nriThresholds`                | NRI Risk Thresholds                    | `nriTable`                          | `.run`, `.calculateAdvancedMetrics` |
| Calculate Integrated Discrimination Index| `calculateIDI`                 | Calculate Integrated Discrimination Index| `idiTable`                          | `.run`, `.calculateAdvancedMetrics` |
| Penalized Regression             | `penalizedRegression`          | Penalized Regression                   | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | Not implemented in `.b.R`            |
| Penalty Type                     | `penaltyType`                  | Penalty Type                           | `basicModelSummary`, `enhancedModelSummary`, `biomarkerModelSummary`, `customModelSummary` | Not implemented in `.b.R`            |
| Generate Risk Score              | `generateRiskScore`            | Generate Risk Score                    | `riskScoreTable`                    | `.run`, `.generateAllRiskScores`     |
| Risk Score Point System          | `riskScorePoints`              | Risk Score Point System                | `riskScoreTable`                    | `.run`, `.generateAllRiskScores`     |
