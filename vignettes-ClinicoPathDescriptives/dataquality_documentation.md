# Data Quality Analysis Documentation

This document provides a comprehensive overview of the Data Quality Analysis module (dataquality), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The dataquality module is a crucial tool for assessing and improving the quality of datasets. It provides functionalities to identify common data issues such as missing values, outliers, inconsistencies, and incorrect data types, which are essential steps before any statistical analysis.

The module's features can be broadly categorized as follows:

*   **Missing Data Analysis:** Identify and quantify missing values across variables.
*   **Outlier Detection:** Detect and visualize outliers in numerical data.
*   **Data Type Validation:** Check and report on variable data types.
*   **Consistency Checks:** Identify inconsistencies or errors in data entries.
*   **Reporting and Visualization:** Generate summaries and plots to highlight data quality issues.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Data Input                       | `dataInput`                    | Data Input                             | `dataQualityOverview`               | `.analyzeDataQuality`                |
| **Missing Data**                 |                                |                                        |                                     |                                      |
| Show Missing Data Summary        | `showMissingSummary`           | Show Missing Data Summary              | `missingDataSummary`                | `.summarizeMissingData`              |
| Show Missing Data Plot           | `showMissingPlot`              | Show Missing Data Plot                 | `missingDataPlot`                   | `.plotMissingData`                   |
| **Outlier Detection**            |                                |                                        |                                     |                                      |
| Show Outlier Summary             | `showOutlierSummary`           | Show Outlier Summary                   | `outlierSummary`                    | `.summarizeOutliers`                 |
| Show Outlier Plot                | `showOutlierPlot`              | Show Outlier Plot                      | `outlierPlot`                       | `.plotOutliers`                      |
| Outlier Method                   | `outlierMethod`                | Outlier Detection Method               | `outlierSummary`                    | `.detectOutliers`                    |
| **Data Type & Consistency**      |                                |                                        |                                     |                                      |
| Show Data Type Summary           | `showDataTypeSummary`          | Show Data Type Summary                 | `dataTypeSummary`                   | `.summarizeDataTypes`                |
| Show Consistency Report          | `showConsistencyReport`        | Show Consistency Report                | `consistencyReport`                 | `.checkConsistency`                  |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Export Report                    | `exportReport`                 | Export Data Quality Report             | `exportOptions`                     | `.exportDataQualityReport`           |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportDataQualityPlot`             |