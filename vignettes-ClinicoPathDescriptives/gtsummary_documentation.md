# gtsummary Documentation

This document provides a comprehensive overview of the gtsummary module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The gtsummary module is a powerful tool for creating publication-ready analytical and descriptive tables. It is designed to summarize data and model results in a clear, concise, and customizable manner, making it ideal for clinical and scientific reporting.

The module's features can be broadly categorized as follows:

*   **Descriptive Statistics Tables:** Generate tables summarizing characteristics of variables (e.g., mean, median, frequencies).
*   **Regression Model Summary Tables:** Present results from various regression models (e.g., logistic, linear, Cox).
*   **Customization and Formatting:** Extensive options for styling, labeling, and formatting tables.
*   **Table Merging and Stacking:** Combine multiple tables for comprehensive reporting.
*   **Export Options:** Capabilities to save tables in various formats (e.g., Word, PDF, HTML).

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variables                        | `variables`                    | Variables to Summarize                 | `summaryTable`                      | `.createSummaryTable`                |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `summaryTable`                      | `.createSummaryTable`                |
| **Descriptive Tables**           |                                |                                        |                                     |                                      |
| Show Summary Table               | `showSummaryTable`             | Show Summary Table                     | `summaryTable`                      | `.populateSummaryTable`              |
| Statistics to Display            | `statistics`                   | Statistics to Display                  | `summaryTable`                      | `.createSummaryTable`                |
| Digits                           | `digits`                       | Number of Decimal Places               | `summaryTable`                      | `.createSummaryTable`                |
| Add Total Column                 | `addTotalColumn`               | Add Total Column                       | `summaryTable`                      | `.createSummaryTable`                |
| **Model Summary Tables**         |                                |                                        |                                     |                                      |
| Model Object                     | `modelObject`                  | Model Object                           | `modelTable`                        | `.createModelTable`                  |
| Show Coefficients                | `showCoefficients`             | Show Coefficients                      | `modelTable`                        | `.createModelTable`                  |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `modelTable`                        | `.createModelTable`                  |
| Show P-values                    | `showPValues`                  | Show P-values                          | `modelTable`                        | `.createModelTable`                  |
| **Customization & Export**       |                                |                                        |                                     |                                      |
| Table Title                      | `tableTitle`                   | Table Title                            | `summaryTable`                      | `.customizeTable`                    |
| Footnote                         | `footnote`                     | Footnote                               | `summaryTable`                      | `.customizeTable`                    |
| Export Table                     | `exportTable`                  | Export Table                           | `exportOptions`                     | `.exportTable`                       |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportTable`                       |