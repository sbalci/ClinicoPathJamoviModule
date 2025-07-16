# General Statistical Analysis and Visualization Documentation

This document provides a comprehensive overview of the General Statistical Analysis and Visualization module (jggstats), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jggstats module is a versatile tool for performing a wide range of statistical analyses and generating high-quality visualizations. It aims to provide a user-friendly interface for common statistical tasks and customizable graphical outputs.

The module's features can be broadly categorized as follows:

*   **Descriptive Statistics:** Summarize and explore data distributions.
*   **Inferential Statistics:** Perform hypothesis testing and estimate parameters.
*   **Data Visualization:** Create various types of plots for data exploration and presentation.
*   **Data Transformation:** Basic data manipulation and preparation utilities.
*   **Customization Options:** Extensive options for tailoring analyses and plots.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variables                        | `variables`                    | Variables                              | `summaryTable`                      | `.calculateSummary`                  |
| Grouping Variable                | `groupingVariable`             | Grouping Variable                      | `summaryTable`                      | `.calculateSummary`                  |
| Statistical Test                 | `statisticalTest`              | Statistical Test                       | `testResults`                       | `.performStatisticalTest`            |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `testResults`                       | `.performStatisticalTest`            |
| **Descriptive Statistics**       |                                |                                        |                                     |                                      |
| Show Summary Table               | `showSummaryTable`             | Show Summary Table                     | `summaryTable`                      | `.populateSummaryTable`              |
| Show Frequencies                 | `showFrequencies`              | Show Frequencies                       | `frequencyTable`                    | `.populateFrequencyTable`            |
| **Inferential Statistics**       |                                |                                        |                                     |                                      |
| T-Test                           | `performTTest`                 | Perform T-Test                         | `tTestResults`                      | `.performTTest`                      |
| ANOVA                            | `performANOVA`                 | Perform ANOVA                          | `anovaResults`                      | `.performANOVA`                      |
| Chi-squared Test                 | `performChiSq`                 | Perform Chi-squared Test               | `chiSqResults`                      | `.performChiSq`                      |
| Correlation Analysis             | `performCorrelation`           | Perform Correlation Analysis           | `correlationResults`                | `.performCorrelation`                |
| Regression Analysis              | `performRegression`            | Perform Regression Analysis            | `regressionResults`                 | `.performRegression`                 |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Bar Plot                    | `showBarPlot`                  | Show Bar Plot                          | `barPlot`                           | `.plotBarPlot`                       |
| Show Box Plot                    | `showBoxPlot`                  | Show Box Plot                          | `boxPlot`                           | `.plotBoxPlot`                       |
| Show Histogram                   | `showHistogram`                | Show Histogram                         | `histogram`                         | `.plotHistogram`                     |
| Show Scatter Plot                | `showScatterPlot`              | Show Scatter Plot                      | `scatterPlot`                       | `.plotScatterPlot`                   |
| Show Line Plot                   | `showLinePlot`                 | Show Line Plot                         | `linePlot`                          | `.plotLinePlot`                      |
| Plot Type                        | `plotType`                     | Plot Type                              | `generalPlot`                       | `.plotGeneral`                       |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `generalPlot`                       | `.plotGeneral`                       |
| Y-axis Label                     | `yAxisLabel`                   | Y-axis Label                           | `generalPlot`                       | `.plotGeneral`                       |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `generalPlot`                       | `.plotGeneral`                       |
| Show Legend                      | `showLegend`                   | Show Legend                            | `generalPlot`                       | `.plotGeneral`                       |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `generalPlot`                       | `.plotGeneral`                       |
| **Data Transformation**          |                                |                                        |                                     |                                      |
| Recode Variable                  | `recodeVariable`               | Recode Variable                        | `dataTransformation`                | `.recodeVariable`                    |
| Create New Variable              | `createNewVariable`            | Create New Variable                    | `dataTransformation`                | `.createNewVariable`                 |
| Filter Data                      | `filterData`                   | Filter Data                            | `dataTransformation`                | `.filterData`                        |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| P-value Adjustment               | `pValueAdjustment`             | P-value Adjustment                     | `advancedOptions`                   | `.adjustPValues`                     |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportResults`                     |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportPlot`                        |