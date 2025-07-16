# Concordance Test Analysis Documentation

This document provides a comprehensive overview of the Concordance Test Analysis module (cotest), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The cotest module is a specialized tool for assessing the agreement or concordance between two continuous measurements or ratings. It is particularly useful in clinical research to evaluate the consistency of different measurement methods or observers.

The module's features can be broadly categorized as follows:

*   **Core Concordance Calculation:** Compute concordance correlation coefficient (CCC) and other agreement metrics.
*   **Bias Assessment:** Analyze systematic and random differences between measurements.
*   **Visualization:** Generate plots to visually represent agreement (e.g., Bland-Altman plots, scatter plots).
*   **Confidence Intervals:** Provide confidence intervals for agreement metrics.
*   **Export Options:** Capabilities to save concordance results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Measurement 1                    | `measurement1`                 | Measurement 1                          | `concordanceOverview`               | `.calculateConcordance`              |
| Measurement 2                    | `measurement2`                 | Measurement 2                          | `concordanceOverview`               | `.calculateConcordance`              |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `concordanceOverview`               | `.calculateConcordance`              |
| **Concordance Metrics**          |                                |                                        |                                     |                                      |
| Show Concordance Coefficient     | `showCCC`                      | Show Concordance Correlation Coefficient | `cccResults`                        | `.calculateCCC`                      |
| Show Bias Measures               | `showBiasMeasures`             | Show Bias Measures                     | `biasResults`                       | `.calculateBias`                     |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Bland-Altman Plot           | `showBlandAltmanPlot`          | Show Bland-Altman Plot                 | `blandAltmanPlot`                   | `.plotBlandAltman`                   |
| Show Scatter Plot                | `showScatterPlot`              | Show Scatter Plot                      | `scatterPlot`                       | `.plotScatter`                       |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportConcordanceResults`          |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportConcordancePlot`             |