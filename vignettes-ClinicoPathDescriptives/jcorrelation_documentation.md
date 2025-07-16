# Correlation Analysis Documentation

This document provides a comprehensive overview of the Correlation Analysis module (jcorrelation), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jcorrelation module is a versatile tool for assessing the statistical relationship between two or more variables. It provides various methods for calculating correlation coefficients, visualizing correlation matrices, and performing significance tests.

The module's features can be broadly categorized as follows:

*   **Core Correlation Calculation:** Compute different types of correlation coefficients (e.g., Pearson, Spearman, Kendall).
*   **Correlation Matrix Visualization:** Generate heatmaps or other graphical representations of correlation matrices.
*   **Significance Testing:** Perform hypothesis tests for correlation coefficients and adjust for multiple comparisons.
*   **Pairwise Analysis:** Analyze relationships between pairs of variables.
*   **Export Options:** Capabilities to save correlation results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variables                        | `variables`                    | Variables                              | `correlationOverview`               | `.calculateCorrelation`              |
| Method                           | `method`                       | Method                                 | `correlationOverview`               | `.calculateCorrelation`              |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `correlationOverview`               | `.calculateCorrelation`              |
| **Correlation Results**          |                                |                                        |                                     |                                      |
| Show Correlation Matrix          | `showCorrelationMatrix`        | Show Correlation Matrix                | `correlationMatrix`                 | `.populateCorrelationMatrix`         |
| Show P-values                    | `showPValues`                  | Show P-values                          | `correlationMatrix`                 | `.populateCorrelationMatrix`         |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `correlationMatrix`                 | `.populateCorrelationMatrix`         |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Heatmap                     | `showHeatmap`                  | Show Heatmap                           | `correlationHeatmap`                | `.plotCorrelationHeatmap`            |
| Show Scatter Plot Matrix         | `showScatterPlotMatrix`        | Show Scatter Plot Matrix               | `scatterPlotMatrix`                 | `.plotScatterPlotMatrix`             |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| P-value Adjustment               | `pValueAdjustment`             | P-value Adjustment                     | `advancedOptions`                   | `.adjustPValues`                     |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportCorrelationResults`          |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportCorrelationPlot`             |