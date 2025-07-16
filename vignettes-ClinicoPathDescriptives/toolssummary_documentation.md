# Data Summary Analysis Documentation

This document provides a comprehensive overview of the Data Summary Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Data Summary Analysis module offers comprehensive tools for summarizing data, with enhanced capabilities through integration with the `summarytools` package. It provides basic variable information, missing value analysis, frequency distributions, and descriptive statistics for both categorical and numeric variables.

The module's features can be broadly categorized as follows:

*   **Basic Summary:** Provides fundamental statistics and information for selected variables.
*   **Frequency Analysis:** Generates frequency tables for categorical and low-cardinality numeric variables.
*   **Descriptive Statistics:** Calculates detailed descriptive statistics for numeric variables.
*   **`summarytools` Integration:** Leverages the `summarytools` package for advanced data frame summaries, enhanced descriptive statistics, professional frequency tables, and cross-tabulations.
*   **Grouping:** Supports stratified summaries by a grouping variable.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Basic Summary**                |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `summary`                           | `.run`                               |
| Exclude Missing Values           | `excludeNA`                    | Exclude Missing Values                 | `summary`, `numericStats`, `frequencies`, `dfSummary`, `descrStats`, `summaryToolsFreq`, `crosstabs` | `.run`, `.generateSummaryToolsOutput` |
| **Frequency Analysis**           |                                |                                        |                                     |                                      |
| Show Frequencies                 | `showFreq`                     | Show Frequencies                       | `frequencies`, `summaryToolsFreq`   | `.run`, `.generateSummaryToolsOutput` |
| **Descriptive Statistics**       |                                |                                        |                                     |                                      |
| Show Statistics                  | `showStats`                    | Show Statistics                        | `numericStats`                      | `.run`                               |
| **`summarytools` Integration**   |                                |                                        |                                     |                                      |
| Use summarytools package         | `useSummarytools`              | Use summarytools package               | `dfSummary`, `descrStats`, `summaryToolsFreq`, `crosstabs` | `.generateSummaryToolsOutput`        |
| Show Data Frame Summary          | `showDfSummary`                | Show Data Frame Summary                | `dfSummary`                         | `.generateSummaryToolsOutput`        |
| Show Descriptive Statistics      | `showDescr`                    | Show Descriptive Statistics            | `descrStats`                        | `.generateSummaryToolsOutput`        |
| Grouping Variable                | `groupVar`                     | Grouping Variable                      | `dfSummary`, `descrStats`, `summaryToolsFreq`, `crosstabs` | `.generateSummaryToolsOutput`        |
| Show Cross-tabulations           | `showCrosstabs`                | Show Cross-tabulations                 | `crosstabs`                         | `.generateSummaryToolsOutput`        |
