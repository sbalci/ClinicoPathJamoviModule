# Statistical Extraction and Visualization Documentation

This document provides a comprehensive overview of the Statistical Extraction and Visualization module (jextractggstats), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jextractggstats module is designed to facilitate the extraction, presentation, and customization of statistical results and visualizations, particularly those generated from `ggstatsplot` or other statistical models. It aims to streamline the process of reporting complex statistical findings.

The module's features can be broadly categorized as follows:

*   **Core Extraction:** Extract key statistical metrics (e.g., p-values, effect sizes, confidence intervals) from model objects.
*   **Plot Customization:** Modify and enhance existing `ggplot2` or `ggstatsplot` objects.
*   **Table Generation:** Create publication-ready tables of statistical results.
*   **Reporting Integration:** Tools for integrating extracted results into reports or documents.
*   **Export Options:** Capabilities to save extracted data, tables, and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Statistical Object               | `statObject`                   | Statistical Object                     | `extractionOverview`                | `.extractStats`                      |
| Plot Object                      | `plotObject`                   | Plot Object                            | `extractionOverview`                | `.extractPlotData`                   |
| **Extraction Options**           |                                |                                        |                                     |                                      |
| Extract P-values                 | `extractPValues`               | Extract P-values                       | `pValuesTable`                      | `.extractPValues`                    |
| Extract Effect Sizes             | `extractEffectSizes`           | Extract Effect Sizes                   | `effectSizesTable`                  | `.extractEffectSizes`                |
| Extract Model Coefficients       | `extractCoefficients`          | Extract Model Coefficients             | `coefficientsTable`                 | `.extractCoefficients`               |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Add Custom Title                 | `addCustomTitle`               | Add Custom Title                       | `customPlot`                        | `.customizePlot`                     |
| Change Axis Labels               | `changeAxisLabels`             | Change Axis Labels                     | `customPlot`                        | `.customizePlot`                     |
| Modify Theme                     | `modifyTheme`                  | Modify Plot Theme                      | `customPlot`                        | `.customizePlot`                     |
| **Table Generation**             |                                |                                        |                                     |                                      |
| Generate Summary Table           | `generateSummaryTable`         | Generate Summary Table                 | `summaryTable`                      | `.generateSummaryTable`              |
| Generate ANOVA Table             | `generateAnovaTable`           | Generate ANOVA Table                   | `anovaTable`                        | `.generateAnovaTable`                |
| **Reporting & Export**           |                                |                                        |                                     |                                      |
| Export Data                      | `exportData`                   | Export Extracted Data                  | `exportOptions`                     | `.exportExtractedData`               |
| Export Table                     | `exportTable`                  | Export Table                           | `exportOptions`                     | `.exportTable`                       |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportPlot`                        |