
# Stage Migration Analysis Documentation

This document provides a comprehensive overview of the Stage Migration Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Stage Migration Analysis module is a powerful tool for comparing two staging systems (e.g., different editions of the TNM staging system). It provides a wide range of statistical analyses and visualizations to assess the impact of the new staging system on patient classification and prognosis.

The module's features can be broadly categorized as follows:

*   **Core Migration Analysis:** Basic statistics and visualizations to understand how patients are re-classified under the new staging system.
*   **Advanced Discrimination Metrics:** In-depth statistical tests to compare the prognostic accuracy of the two staging systems.
*   **Clinical Utility Assessment:** Analyses to evaluate the clinical relevance and usefulness of the new staging system.
*   **Validation Framework:** Tools for internal validation of the analysis results.
*   **Advanced Visualizations:** A variety of plots to visually represent the analysis results.
*   **Multifactorial Analysis:** Options to include covariates in the analysis for adjusted comparisons.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Old Staging System               | `oldStage`                     | Old Staging System                     | `migrationOverview`                 | `.calculateBasicMigration`           |
| New Staging System               | `newStage`                     | New Staging System                     | `migrationOverview`                 | `.calculateBasicMigration`           |
| Survival Time                    | `survivalTime`                 | Survival Time                          | `concordanceComparison`             | `.calculateAdvancedMetrics`          |
| Event                            | `event`                        | Event                                  | `concordanceComparison`             | `.calculateAdvancedMetrics`          |
| Event Level                      | `eventLevel`                   | Event Level                            | `concordanceComparison`             | `.calculateAdvancedMetrics`          |
| Analysis Type                    | `analysisType`                 | Analysis Type                          | (Controls visibility of other sections) | `.run`                               |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `concordanceComparison`             | `.calculateAdvancedMetrics`          |
| **Migration Analysis**           |                                |                                        |                                     |                                      |
| Migration Overview               | `showMigrationOverview`        | Show Migration Overview                | `migrationOverview`                 | `.populateMigrationOverview`         |
| Migration Summary                | `showMigrationSummary`         | Show Migration Summary                 | `migrationSummary`                  | `.populateMigrationSummary`          |
| Stage Distribution               | `showStageDistribution`        | Show Stage Distribution                | `stageDistribution`                 | `.populateStageDistribution`         |
| Migration Matrix                 | `showMigrationMatrix`          | Show Migration Matrix                  | `migrationMatrix`                   | `.populateMigrationMatrix`           |
| Migration Heatmap                | `showMigrationHeatmap`         | Show Migration Heatmap                 | `migrationHeatmap`                  | `.plotMigrationHeatmap`              |
| **Advanced Metrics**             |                                |                                        |                                     |                                      |
| Net Reclassification Improvement | `calculateNRI`                 | Calculate Net Reclassification Improvement (NRI) | `nriResults`                        | `.calculateNRI`                      |
| NRI Time Points                  | `nriTimePoints`                | NRI Time Points                        | `nriResults`                        | `.calculateNRI`                      |
| Integrated Discrimination Improvement | `calculateIDI`                 | Calculate Integrated Discrimination Improvement (IDI) | `idiResults`                        | `.calculateIDI`                      |
| Time-dependent ROC Analysis      | `performROCAnalysis`           | Perform Time-dependent ROC Analysis    | `rocAnalysis`                       | `.performTimeROCAnalysis`            |
| ROC Time Points                  | `rocTimePoints`                | ROC Time Points                        | `rocAnalysis`                       | `.performTimeROCAnalysis`            |
| Decision Curve Analysis          | `performDCA`                   | Perform Decision Curve Analysis (DCA)  | `dcaResults`                        | `.performDCA`                        |
| Likelihood Ratio Tests           | `performLikelihoodTests`       | Perform Likelihood Ratio Tests         | `likelihoodTests`                   | `.calculateAdvancedMetrics`          |
| Pseudo R-squared                 | `calculatePseudoR2`            | Calculate Pseudo R-squared             | `statisticalComparison`             | `.calculatePseudoR2`                 |
| **Validation**                   |                                |                                        |                                     |                                      |
| Bootstrap Validation             | `performBootstrap`             | Perform Bootstrap Validation           | `bootstrapResults`                  | `.performBootstrapValidation`        |
| Bootstrap Replications           | `bootstrapReps`                | Bootstrap Replications                 | `bootstrapResults`                  | `.performBootstrapValidation`        |
| Cross-Validation                 | `performCrossValidation`       | Perform Cross-Validation               | Not directly in results table       | Not implemented in `.b.R`            |
| CV Folds                         | `cvFolds`                      | Folds for Cross-Validation             | Not directly in results table       | Not implemented in `.b.R`            |
| **Clinical Utility**             |                                |                                        |                                     |                                      |
| Clinical Significance Threshold  | `clinicalSignificanceThreshold` | Clinical Significance Threshold        | `clinicalInterpretation`            | `.generateClinicalInterpretation`    |
| NRI Clinical Threshold           | `nriClinicalThreshold`         | NRI Clinical Threshold                 | `clinicalInterpretation`            | `.generateClinicalInterpretation`    |
| Will Rogers Phenomenon           | `showWillRogersAnalysis`       | Show Will Rogers Phenomenon Analysis   | `willRogersAnalysis`                | `.analyzeWillRogers`                 |
| Clinical Interpretation          | `showClinicalInterpretation`   | Show Clinical Interpretation           | `clinicalInterpretation`            | `.generateClinicalInterpretation`    |
| Executive Summary                | `generateExecutiveSummary`     | Generate Executive Summary             | `executiveSummary`                  | `.populateExecutiveSummary`          |
| **Homogeneity and Trend**        |                                |                                        |                                     |                                      |
| Homogeneity Tests                | `performHomogeneityTests`      | Perform Stage Homogeneity Tests        | `homogeneityTests`                  | `.performHomogeneityTests`           |
| Trend Tests                      | `performTrendTests`            | Perform Trend Tests                    | `homogeneityTests`                  | `.calculateTrendTest`                |
| **Visualizations**               |                                |                                        |                                     |                                      |
| ROC Comparison Plot              | `showROCComparison`            | Show ROC Comparison Plot               | `rocComparisonPlot`                 | `.plotROCComparison`                 |
| Calibration Plots                | `showCalibrationPlots`         | Show Calibration Plots                 | `calibrationPlots`                  | `.plotCalibration`                   |
| Decision Curves                  | `showDecisionCurves`           | Show Decision Curves                   | `decisionCurves`                    | `.plotDecisionCurves`                |
| Forest Plot                      | `showForestPlot`               | Show Forest Plot                       | `forestPlot`                        | `.plotForest`                        |
| Survival Curves                  | `showSurvivalCurves`           | Show Survival Curves                   | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Survival Plot Type               | `survivalPlotType`             | Survival Plot Type                     | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Show Risk Tables                 | `showRiskTables`               | Show Risk Tables                       | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Plot Time Range                  | `plotTimeRange`                | Plot Time Range                        | `survivalCurves`                    | `.plotSurvivalCurves`                |
| **Multifactorial Analysis**      |                                |                                        |                                     |                                      |
| Enable Multifactorial Analysis   | `enableMultifactorialAnalysis` | Enable Multifactorial Analysis         | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Continuous Covariates            | `continuousCovariates`         | Continuous Covariates                  | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Categorical Covariates           | `categoricalCovariates`        | Categorical Covariates                 | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Multifactorial Comparison Type   | `multifactorialComparisonType` | Multifactorial Comparison Type         | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Baseline Model                   | `baselineModel`                | Baseline Model                         | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Interaction Tests                | `performInteractionTests`      | Perform Interaction Tests              | `interactionTests`                  | `.performInteractionTestsOnly`       |
| Stratified Analysis              | `stratifiedAnalysis`           | Perform Stratified Analysis            | `stratifiedAnalysis`                | `.performMultifactorialAnalysis`     |
| Show Multifactorial Tables       | `showMultifactorialTables`     | Show Multifactorial Tables             | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Show Adjusted C-Index Comparison | `showAdjustedCIndexComparison` | Show Adjusted C-Index Comparison       | `adjustedCIndexComparison`          | `.performMultifactorialAnalysis`     |
| Show Nested Model Tests          | `showNestedModelTests`         | Show Nested Model Tests                | `nestedModelTests`                  | `.performMultifactorialAnalysis`     |
| Show Stepwise Results            | `showStepwiseResults`          | Show Stepwise Results                  | `stepwiseResults`                   | `.performMultifactorialAnalysis`     |
