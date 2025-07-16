# Comparing Survival Analysis Documentation

This document provides a comprehensive overview of the Comparing Survival Analysis module (comparingSurvival), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The comparingSurvival module is a powerful tool for comparing survival outcomes between two or more groups. It provides various statistical tests and visualizations to assess differences in survival curves, hazard ratios, and other relevant metrics.

The module's features can be broadly categorized as follows:

*   **Core Survival Comparison:** Perform log-rank tests and calculate hazard ratios to compare survival curves.
*   **Kaplan-Meier Plotting:** Generate Kaplan-Meier survival curves with customizable aesthetics.
*   **Cox Regression:** Conduct Cox proportional hazards regression for univariable and multivariable analysis.
*   **Risk Table Generation:** Display the number of subjects at risk over time.
*   **Export Options:** Capabilities to save analysis results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Survival Time                    | `survivalTime`                 | Survival Time                          | `survivalComparisonOverview`        | `.calculateSurvivalComparison`       |
| Event                            | `event`                        | Event                                  | `survivalComparisonOverview`        | `.calculateSurvivalComparison`       |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `survivalComparisonOverview`        | `.calculateSurvivalComparison`       |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `survivalComparisonOverview`        | `.calculateSurvivalComparison`       |
| **Statistical Tests**            |                                |                                        |                                     |                                      |
| Perform Log-Rank Test            | `performLogRank`               | Perform Log-Rank Test                  | `logRankResults`                    | `.performLogRankTest`                |
| Show Hazard Ratio                | `showHazardRatio`              | Show Hazard Ratio                      | `hazardRatioResults`                | `.calculateHazardRatio`              |
| **Kaplan-Meier Plotting**        |                                |                                        |                                     |                                      |
| Show Kaplan-Meier Plot           | `showKaplanMeierPlot`          | Show Kaplan-Meier Plot                 | `kaplanMeierPlot`                   | `.plotKaplanMeier`                   |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `kaplanMeierPlot`                   | `.plotKaplanMeier`                   |
| Show Risk Table                  | `showRiskTable`                | Show Risk Table                        | `kaplanMeierPlot`                   | `.plotKaplanMeier`                   |
| **Cox Regression**               |                                |                                        |                                     |                                      |
| Perform Univariable Cox          | `performUnivariableCox`        | Perform Univariable Cox Regression     | `univariableCoxResults`             | `.performUnivariableCox`             |
| Perform Multivariable Cox        | `performMultivariableCox`      | Perform Multivariable Cox Regression   | `multivariableCoxResults`           | `.performMultivariableCox`           |
| Covariates                       | `covariates`                   | Covariates                             | `multivariableCoxResults`           | `.performMultivariableCox`           |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportSurvivalComparisonResults`   |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportSurvivalComparisonPlot`      |