# Benford's Law Analysis Documentation

This document provides a comprehensive overview of the Benford's Law Analysis module (benford2), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The benford2 module is a specialized tool for applying Benford's Law (also known as the First-Digit Law) to numerical datasets. Benford's Law describes the frequency distribution of leading digits in many real-life numerical data sets. This module is particularly useful for fraud detection, data integrity checks, and identifying anomalies in financial, scientific, or demographic data.

The module's features can be broadly categorized as follows:

*   **Core Benford's Law Test:** Compare the observed first-digit distribution with the expected Benford's Law distribution.
*   **Statistical Goodness-of-Fit Tests:** Perform statistical tests (e.g., Chi-squared, Kolmogorov-Smirnov) to assess the conformity of data to Benford's Law.
*   **Visualization:** Generate bar plots to visually compare observed and expected digit frequencies.
*   **Digit Analysis:** Analyze leading digits beyond the first (e.g., second digit, first two digits).
*   **Export Options:** Capabilities to save analysis results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Numerical Variable               | `numericalVar`                 | Numerical Variable                     | `benfordOverview`                   | `.calculateBenford`                  |
| **Benford's Law Tests**          |                                |                                        |                                     |                                      |
| Show First Digit Frequencies     | `showFirstDigitFreq`           | Show First Digit Frequencies           | `firstDigitResults`                 | `.calculateFirstDigitFreq`           |
| Show Chi-squared Test            | `showChiSqTest`                | Show Chi-squared Test                  | `chiSqTestResults`                  | `.performChiSqTest`                  |
| Show Kolmogorov-Smirnov Test     | `showKSTest`                   | Show Kolmogorov-Smirnov Test           | `ksTestResults`                     | `.performKSTest`                     |
| **Visualization**                |                                |                                        |                                     |                                      |
| Show Bar Plot                    | `showBarPlot`                  | Show Bar Plot                          | `benfordBarPlot`                    | `.plotBenfordBar`                    |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Analyze Second Digit             | `analyzeSecondDigit`           | Analyze Second Digit                   | `secondDigitResults`                | `.calculateSecondDigitFreq`          |
| Analyze First Two Digits         | `analyzeFirstTwoDigits`        | Analyze First Two Digits               | `firstTwoDigitsResults`             | `.calculateFirstTwoDigitsFreq`       |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportBenfordResults`              |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportBenfordPlot`                 |