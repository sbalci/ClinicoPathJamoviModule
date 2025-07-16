# Descriptive Statistics Tools Documentation

This document provides a comprehensive overview of the Descriptive Statistics Tools module (desctools), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The desctools module is a collection of functions designed for efficient calculation and presentation of descriptive statistics. It aims to provide a user-friendly interface for summarizing key characteristics of data, including measures of central tendency, dispersion, and distribution.

The module's features can be broadly categorized as follows:

*   **Core Descriptive Statistics:** Calculate basic statistics for numerical and categorical variables.
*   **Grouped Summaries:** Generate descriptive statistics stratified by one or more grouping variables.
*   **Frequency Tables:** Create frequency and proportion tables for categorical data.
*   **Data Distribution Analysis:** Tools for assessing the shape and characteristics of data distributions.
*   **Export Options:** Capabilities to save summary tables and results in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variables                        | `variables`                    | Variables to Analyze                   | `descriptiveOverview`               | `.calculateDescriptives`             |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `descriptiveOverview`               | `.calculateDescriptives`             |
| **Numerical Descriptives**       |                                |                                        |                                     |                                      |
| Show Mean                        | `showMean`                     | Show Mean                              | `numericalSummary`                  | `.calculateMean`                     |
| Show Median                      | `showMedian`                   | Show Median                            | `numericalSummary`                  | `.calculateMedian`                   |
| Show Standard Deviation          | `showSD`                       | Show Standard Deviation                | `numericalSummary`                  | `.calculateSD`                       |
| Show Interquartile Range         | `showIQR`                      | Show Interquartile Range               | `numericalSummary`                  | `.calculateIQR`                      |
| Show Min/Max                     | `showMinMax`                   | Show Min/Max                           | `numericalSummary`                  | `.calculateMinMax`                   |
| **Categorical Descriptives**     |                                |                                        |                                     |                                      |
| Show Frequencies                 | `showFrequencies`              | Show Frequencies                       | `categoricalSummary`                | `.calculateFrequencies`              |
| Show Percentages                 | `showPercentages`              | Show Percentages                       | `categoricalSummary`                | `.calculateFrequencies`              |
| **Table Customization**          |                                |                                        |                                     |                                      |
| Digits                           | `digits`                       | Number of Decimal Places               | `descriptiveTable`                  | `.formatDescriptiveTable`            |
| Transpose Table                  | `transposeTable`               | Transpose Table                        | `descriptiveTable`                  | `.transposeTable`                    |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportDescriptiveResults`          |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportDescriptiveResults`          |