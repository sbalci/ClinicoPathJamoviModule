# Simon-Makuch Analysis Documentation

This document provides a comprehensive overview of the Simon-Makuch Time-Dependent Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Simon-Makuch analysis module is a specialized tool for survival analysis when a patient's exposure status (e.g., treatment, biomarker status) can change over time. It correctly handles time-dependent covariates, avoiding common pitfalls like immortal time bias. The module provides robust statistical analysis and visualizations to assess the impact of time-dependent exposures on survival outcomes.

The module's features can be broadly categorized as follows:

*   **Core Time-Dependent Analysis:** Prepares data into a counting process format and provides basic survival estimates.
*   **Advanced Modeling:** Performs time-dependent Cox proportional hazards regression to estimate hazard ratios.
*   **Specialized Analyses:** Includes landmark analysis and methods to assess immortal time bias.
*   **Statistical Tests:** Offers appropriate statistical tests for comparing survival curves with time-dependent exposures.
*   **Visualizations:** Generates Simon-Makuch plots (modified Kaplan-Meier curves) and other relevant visualizations.
*   **Validation and Diagnostics:** Provides options for bootstrap validation and model diagnostics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Survival Time                    | `survivalTime`                 | Survival Time                          | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Event Indicator                  | `event`                        | Event Indicator                        | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Event Level                      | `eventLevel`                   | Event Level                            | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Time-Dependent Variable          | `timeDepVariable`              | Time-Dependent Variable                | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Time of Change                   | `timeDepTime`                  | Time-Dependent Change Time             | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Time-Dependent Status            | `timeDepStatus`                | Time-Dependent Status                  | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Exposed Level                    | `exposedLevel`                 | Exposed Level                          | (Used in all calculations)          | `.prepareTimeDepenentData`           |
| Analysis Type                    | `analysisType`                 | Analysis Type                          | (Controls visibility of other sections) | `.run`                               |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | (Used in CI calculations)           | (Implicitly used by `survival` pkg)  |
| **Exposure & Survival Estimates** |                                |                                        |                                     |                                      |
| Exposure Pattern Summary         | `showExposurePatterns`         | Exposure Pattern Summary               | `exposurePatterns`                  | `.populateExposurePatterns`          |
| Survival Estimates Table         | `showSurvivalEstimates`        | Survival Estimates Table               | `survivalEstimates`                 | `.populateSurvivalEstimates`         |
| **Time-Dependent Cox Regression** |                                |                                        |                                     |                                      |
| Perform Cox Regression           | `performTimeDependentCox`      | Time-Dependent Cox Regression          | `timeDependentCox`                  | `.performTimeDependentCox`           |
| Additional Covariates            | `timeDependentCovariates`      | Additional Covariates                  | `timeDependentCox`                  | `.performTimeDependentCox`           |
| Hazard Ratios Table              | `showHazardRatios`             | Hazard Ratios Table                    | `hazardRatios`                      | `.populateHazardRatios`              |
| Test Time-Varying Effects        | `testTimeVaryingEffect`        | Test Time-Varying Effects              | `timeVaryingEffects`                | `.testTimeVaryingEffects`            |
| **Landmark Analysis**            |                                |                                        |                                     |                                      |
| Perform Landmark Analysis        | `performLandmarkAnalysis`      | Landmark Analysis                      | `landmarkResults`                   | `.performLandmarkAnalysis`           |
| Landmark Time Points             | `landmarkTimes`                | Landmark Time Points                   | `landmarkResults`                   | `.performLandmarkAnalysis`           |
| Landmark Results Table           | `showLandmarkResults`          | Landmark Analysis Results              | `landmarkResults`                   | `.performLandmarkAnalysis`           |
| **Bias Assessment**              |                                |                                        |                                     |                                      |
| Assess Immortal Time Bias        | `assessImmortalTimeBias`       | Assess Immortal Time Bias              | `immortalTimeBias`                  | `.assessImmortalTimeBias`            |
| **Statistical Tests**            |                                |                                        |                                     |                                      |
| Log-Rank Test                    | `performLogRankTest`           | Log-Rank Test                          | `statisticalTests`                  | `.performStatisticalTests`           |
| Mantel-Byar Test                 | `performMantelByarTest`        | Mantel-Byar Test                       | `statisticalTests`                  | `.performStatisticalTests`           |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Simon-Makuch Plot                | `showSimonMakuchPlot`          | Simon-Makuch Plot                      | `simonMakuchPlot`                   | `.plotSimonMakuch`                   |
| Landmark Survival Plots          | `showLandmarkPlots`            | Landmark Survival Plots                | `landmarkPlots`                     | `.plotLandmarkAnalysis`              |
| Cumulative Incidence Plot        | `showCumulativeIncidencePlot`  | Cumulative Incidence Plot              | `cumulativeIncidencePlot`           | `.plotCumulativeIncidence`           |
| Plot Exposure Status             | `plotExposureStatus`           | Plot Exposure Status Over Time         | `exposureStatusPlot`                | `.plotExposureStatus`                |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `simonMakuchPlot`                   | `.plotSimonMakuch`                   |
| Show Risk Tables                 | `showRiskTables`               | Show Risk Tables                       | `simonMakuchPlot`                   | `.plotSimonMakuch`                   |
| **Validation & Diagnostics**     |                                |                                        |                                     |                                      |
| Model Diagnostics                | `showModelDiagnostics`         | Model Diagnostics                      | `modelDiagnostics`, `diagnosticPlots` | `.performModelDiagnostics`, `.plotModelDiagnostics` |
| Bootstrap Validation             | `performBootstrapValidation`   | Bootstrap Validation                   | `bootstrapValidation`               | `.performBootstrapValidation`        |
| Bootstrap Samples                | `bootstrapSamples`             | Bootstrap Samples                      | `bootstrapValidation`               | `.performBootstrapValidation`        |
| Sensitivity Analysis             | `performSensitivityAnalysis`   | Sensitivity Analysis                   | `sensitivityAnalysis`               | `.performSensitivityAnalysis`        |
| **Explanatory Output**           |                                |                                        |                                     |                                      |
| Show Explanations                | `showExplanations`             | Show Explanations                      | `simonMakuchExplanation`, etc.      | `.populateExplanations`              |
