# Intraclass Correlation Coefficient (ICC) Analysis Documentation

This document provides a comprehensive overview of the Intraclass Correlation Coefficient (ICC) Analysis module (icccoeff), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The icccoeff module is a specialized tool for assessing the reliability or consistency of measurements made by different observers or methods. It calculates various forms of Intraclass Correlation Coefficients, which are widely used in clinical and psychometric research to quantify agreement among raters.

The module's features can be broadly categorized as follows:

*   **Core ICC Calculation:** Compute different types of ICCs based on various models (e.g., one-way, two-way random, two-way mixed) and forms (e.g., single rater, average of k raters).
*   **Confidence Intervals:** Provide confidence intervals for ICC estimates.
*   **Hypothesis Testing:** Perform significance tests for ICC values.
*   **Visualization:** Generate plots to visualize agreement or disagreement among raters.
*   **Export Options:** Capabilities to save ICC results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Measurements                     | `measurements`                 | Measurements                           | `iccOverview`                       | `.calculateICC`                      |
| Subjects                         | `subjects`                     | Subjects                               | `iccOverview`                       | `.calculateICC`                      |
| Raters                           | `raters`                       | Raters                                 | `iccOverview`                       | `.calculateICC`                      |
| Type                             | `iccType`                      | Type                                   | `iccOverview`                       | `.calculateICC`                      |
| Model                            | `iccModel`                     | Model                                  | `iccOverview`                       | `.calculateICC`                      |
| Form                             | `iccForm`                      | Form                                   | `iccOverview`                       | `.calculateICC`                      |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `iccOverview`                       | `.calculateICC`                      |
| **ICC Results**                  |                                |                                        |                                     |                                      |
| Show ICC Table                   | `showICCTable`                 | Show ICC Table                         | `iccTable`                          | `.populateICCTable`                  |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `iccTable`                          | `.populateICCTable`                  |
| Show P-value                     | `showPValue`                   | Show P-value                           | `iccTable`                          | `.populateICCTable`                  |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Bland-Altman Plot           | `showBlandAltmanPlot`          | Show Bland-Altman Plot                 | `blandAltmanPlot`                   | `.plotBlandAltman`                   |
| Show Scatter Plot                | `showScatterPlot`              | Show Scatter Plot                      | `scatterPlot`                       | `.plotScatter`                       |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportICCResults`                  |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportICCPlot`                     |