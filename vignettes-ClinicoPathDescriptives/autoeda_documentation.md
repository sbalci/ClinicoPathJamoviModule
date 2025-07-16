# Automated Exploratory Data Analysis (EDA) Documentation

This document provides a comprehensive overview of the Automated Exploratory Data Analysis (EDA) module (autoeda), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The autoeda module is designed to automate and streamline the process of exploratory data analysis. It provides a quick and efficient way to generate summary statistics, visualize data distributions, and identify potential issues or patterns within a dataset, reducing the manual effort typically involved in initial data exploration.

The module's features can be broadly categorized as follows:

*   **Comprehensive Data Summaries:** Automatically generate descriptive statistics for all variables.
*   **Automated Visualizations:** Create a range of plots (e.g., histograms, bar plots, scatter plots) based on variable types.
*   **Missing Data and Outlier Reporting:** Identify and visualize missing values and potential outliers.
*   **Correlation Analysis:** Automatically compute and visualize correlations between numerical variables.
*   **Interactive Reports:** Generate interactive HTML reports for easy sharing and exploration.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Data Input                       | `dataInput`                    | Data Input                             | `edaOverview`                       | `.performAutoEDA`                    |
| **Summary Statistics**           |                                |                                        |                                     |                                      |
| Show Descriptive Summary         | `showDescriptiveSummary`       | Show Descriptive Summary               | `descriptiveSummary`                | `.generateDescriptiveSummary`        |
| Show Variable Types              | `showVariableTypes`            | Show Variable Types                    | `variableTypes`                     | `.identifyVariableTypes`             |
| **Automated Visualizations**     |                                |                                        |                                     |                                      |
| Show Histograms                  | `showHistograms`               | Show Histograms (Numerical)            | `histograms`                        | `.plotHistograms`                    |
| Show Bar Plots                   | `showBarPlots`                 | Show Bar Plots (Categorical)           | `barPlots`                          | `.plotBarPlots`                      |
| Show Scatter Plots               | `showScatterPlots`             | Show Scatter Plots (Numerical Pairs)   | `scatterPlots`                      | `.plotScatterPlots`                  |
| **Data Quality Reports**         |                                |                                        |                                     |                                      |
| Show Missing Data Report         | `showMissingDataReport`        | Show Missing Data Report               | `missingDataReport`                 | `.reportMissingData`                 |
| Show Outlier Report              | `showOutlierReport`            | Show Outlier Report                    | `outlierReport`                     | `.reportOutliers`                    |
| **Correlation Analysis**         |                                |                                        |                                     |                                      |
| Show Correlation Matrix          | `showCorrelationMatrix`        | Show Correlation Matrix                | `correlationMatrix`                 | `.plotCorrelationMatrix`             |
| **Reporting & Export**           |                                |                                        |                                     |                                      |
| Generate HTML Report             | `generateHtmlReport`           | Generate HTML Report                   | `htmlReport`                        | `.generateHtmlReport`                |
| Export Plots                     | `exportPlots`                  | Export Plots                           | `exportOptions`                     | `.exportPlots`                       |
| Export Summaries                 | `exportSummaries`              | Export Summaries                       | `exportOptions`                     | `.exportSummaries`                   |