# Compare Groups Analysis Documentation

This document provides a comprehensive overview of the Compare Groups Analysis module (comparegroups), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The comparegroups module is a powerful tool for comparing characteristics of different groups within a dataset. It facilitates the generation of descriptive statistics and statistical tests to assess differences in numerical and categorical variables across predefined groups.

The module's features can be broadly categorized as follows:

*   **Descriptive Statistics by Group:** Calculate and present summary statistics for variables, stratified by grouping factors.
*   **Statistical Tests for Comparison:** Perform appropriate statistical tests (e.g., t-tests, ANOVA, chi-squared tests) to compare groups.
*   **Customizable Output:** Control the display of statistics, p-values, and formatting.
*   **Missing Data Handling:** Options for how missing values are treated during analysis.
*   **Export Options:** Capabilities to save the comparison tables in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variables to Compare             | `variables`                    | Variables to Compare                   | `comparisonOverview`                | `.calculateGroupComparison`          |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `comparisonOverview`                | `.calculateGroupComparison`          |
| **Descriptive Output**           |                                |                                        |                                     |                                      |
| Show Summary Table               | `showSummaryTable`             | Show Summary Table                     | `summaryTable`                      | `.populateSummaryTable`              |
| Statistics to Display            | `statistics`                   | Statistics to Display                  | `summaryTable`                      | `.populateSummaryTable`              |
| **Statistical Tests**            |                                |                                        |                                     |                                      |
| Perform T-test                   | `performTTest`                 | Perform T-test                         | `tTestResults`                      | `.performTTest`                      |
| Perform ANOVA                    | `performANOVA`                 | Perform ANOVA                          | `anovaResults`                      | `.performANOVA`                      |
| Perform Chi-squared Test         | `performChiSq`                 | Perform Chi-squared Test               | `chiSqResults`                      | `.performChiSq`                      |
| Show P-values                    | `showPValues`                  | Show P-values                          | `statisticalTests`                  | `.populateStatisticalTests`          |
| **Table Customization**          |                                |                                        |                                     |                                      |
| Digits                           | `digits`                       | Number of Decimal Places               | `comparisonTable`                   | `.formatComparisonTable`             |
| Add Total Column                 | `addTotalColumn`               | Add Total Column                       | `comparisonTable`                   | `.addTotalColumn`                    |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportComparisonResults`           |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportComparisonResults`           |