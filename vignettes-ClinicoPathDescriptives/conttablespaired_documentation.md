# Paired Samples Contingency Tables Analysis Documentation

This document provides a comprehensive overview of the Paired Samples Contingency Tables module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Paired Samples Contingency Tables module is designed to perform McNemar's test for analyzing paired categorical data. It allows users to construct contingency tables from raw data or pre-summarized counts and calculate various percentages. The module provides both the uncorrected and continuity-corrected Chi-squared statistics for McNemar's test, along with an exact log odds ratio.

The module's features can be broadly categorized as follows:

*   **Contingency Table Generation:** Create 2x2 contingency tables from paired categorical variables.
*   **McNemar's Test:** Perform McNemar's test to assess the statistical significance of changes between paired observations.
*   **Percentage Options:** Display row and/or column percentages within the contingency table.
*   **Exact Test:** Option to calculate an exact log odds ratio (requires `exact2x2` package).
*   **Summary Statistics:** Provides basic information about the sample size (N).

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Data Input**                   |                                |                                        |                                     |                                      |
| Rows Variable                    | `rows`                         | Rows                                   | `freqs`                             | `.run`, `.cleanData`, `.init`        |
| Columns Variable                 | `cols`                         | Columns                                | `freqs`                             | `.run`, `.cleanData`, `.init`        |
| Counts (optional)                | `counts`                       | Counts (optional)                      | `freqs`                             | `.run`, `.cleanData`                 |
| **Statistics**                   |                                |                                        |                                     |                                      |
| Chi-squared                      | `chiSq`                        | χ²                                     | `test`                              | `.run`                               |
| Chi-squared continuity correction| `chiSqCorr`                    | χ² continuity correction               | `test`                              | `.run`                               |
| Log odds ratio exact             | `exact`                        | Log odds ratio exact                   | `test`                              | `.run`                               |
| **Cells**                        |                                |                                        |                                     |                                      |
| Row Percentages                  | `pcRow`                        | Row                                    | `freqs`                             | `.run`, `.init`                      |
| Column Percentages               | `pcCol`                        | Column                                 | `freqs`                             | `.run`, `.init`                      |
| **Results Tables**               |                                |                                        |                                     |                                      |
| Contingency Tables               | (N/A)                          | (N/A)                                  | `freqs`                             | `.run`, `.init`                      |
| McNemar Test Results             | (N/A)                          | (N/A)                                  | `test`                              | `.run`                               |
