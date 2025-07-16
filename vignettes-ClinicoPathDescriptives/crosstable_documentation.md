# Cross-Tabulation Analysis Documentation

This document provides a comprehensive overview of the Cross-Tabulation Analysis module (crosstable), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The crosstable module is a fundamental tool for examining the relationship between two or more categorical variables. It generates contingency tables, calculates various association statistics, and provides visualizations to explore patterns and dependencies within the data.

The module's features can be broadly categorized as follows:

*   **Core Cross-Tabulation:** Create two-way or multi-way contingency tables.
*   **Association Statistics:** Calculate chi-squared tests, Fisher's exact test, and other measures of association (e.g., Cramer's V, Phi coefficient).
*   **Proportion and Percentage Display:** Show row, column, and total percentages within the table.
*   **Visualization:** Generate bar plots or mosaic plots to visualize the cross-tabulated data.
*   **Export Options:** Capabilities to save the cross-tabulation results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Row Variable                     | `rowVar`                       | Row Variable                           | `crosstableOverview`                | `.calculateCrossTable`               |
| Column Variable                  | `colVar`                       | Column Variable                        | `crosstableOverview`                | `.calculateCrossTable`               |
| Layer Variable                   | `layerVar`                     | Layer Variable (Optional)              | `crosstableOverview`                | `.calculateCrossTable`               |
| **Table Content**                |                                |                                        |                                     |                                      |
| Show Counts                      | `showCounts`                   | Show Counts                            | `crosstableResults`                 | `.populateCrossTable`                |
| Show Row Percentages             | `showRowPercentages`           | Show Row Percentages                   | `crosstableResults`                 | `.populateCrossTable`                |
| Show Column Percentages          | `showColPercentages`           | Show Column Percentages                | `crosstableResults`                 | `.populateCrossTable`                |
| Show Total Percentages           | `showTotalPercentages`         | Show Total Percentages                 | `crosstableResults`                 | `.populateCrossTable`                |
| **Statistical Tests**            |                                |                                        |                                     |                                      |
| Perform Chi-squared Test         | `performChiSq`                 | Perform Chi-squared Test               | `chiSqResults`                      | `.performChiSqTest`                  |
| Perform Fisher's Exact Test      | `performFisher`                | Perform Fisher's Exact Test            | `fisherResults`                     | `.performFisherTest`                 |
| Show Association Measures        | `showAssociationMeasures`      | Show Association Measures              | `associationMeasures`               | `.calculateAssociationMeasures`      |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Bar Plot                    | `showBarPlot`                  | Show Bar Plot                          | `crosstableBarPlot`                 | `.plotCrossTableBar`                 |
| Show Mosaic Plot                 | `showMosaicPlot`               | Show Mosaic Plot                       | `crosstableMosaicPlot`              | `.plotCrossTableMosaic`              |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportCrossTableResults`           |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportCrossTablePlot`              |