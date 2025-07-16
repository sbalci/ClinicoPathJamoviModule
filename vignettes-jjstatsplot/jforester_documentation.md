# Forester Plot Analysis Documentation

This document provides a comprehensive overview of the Forester Plot Analysis module (jforester), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jforester module is designed to create highly customizable forest plots, primarily used for visualizing effect sizes and their confidence intervals from various studies or subgroups. It is particularly useful in meta-analysis, subgroup analysis, and presenting results from regression models in a clear, graphical format.

The module's features can be broadly categorized as follows:

*   **Core Forest Plot Generation:** Create forest plots from diverse data inputs (e.g., effect sizes, confidence intervals, p-values).
*   **Customization of Aesthetics:** Extensive options for controlling plot appearance, including colors, shapes, labels, and overall layout.
*   **Subgroup and Overall Effects:** Display individual study/subgroup effects alongside pooled or overall effects.
*   **Heterogeneity Metrics:** Include statistical measures of heterogeneity (e.g., I-squared, Tau-squared).
*   **Export Options:** Capabilities to save the generated plots in various high-quality formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Effect Size                      | `effectSize`                   | Effect Size                            | `foresterOverview`                  | `.calculateForesterData`             |
| Lower CI                         | `lowerCI`                      | Lower Confidence Interval              | `foresterOverview`                  | `.calculateForesterData`             |
| Upper CI                         | `upperCI`                      | Upper Confidence Interval              | `foresterOverview`                  | `.calculateForesterData`             |
| Study/Group Labels               | `labels`                       | Study/Group Labels                     | `foresterOverview`                  | `.calculateForesterData`             |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Forest Plot                 | `showForestPlot`               | Show Forest Plot                       | `foresterPlot`                      | `.plotForester`                      |
| Reference Line                   | `referenceLine`                | Reference Line                         | `foresterPlot`                      | `.plotForester`                      |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `foresterPlot`                      | `.plotForester`                      |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `foresterPlot`                      | `.plotForester`                      |
| Point Shape                      | `pointShape`                   | Point Shape                            | `foresterPlot`                      | `.plotForester`                      |
| Point Color                      | `pointColor`                   | Point Color                            | `foresterPlot`                      | `.plotForester`                      |
| Line Type                        | `lineType`                     | Line Type                              | `foresterPlot`                      | `.plotForester`                      |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Show Overall Effect              | `showOverallEffect`            | Show Overall Effect                    | `foresterPlot`                      | `.plotForester`                      |
| Overall Effect Label             | `overallEffectLabel`           | Overall Effect Label                   | `foresterPlot`                      | `.plotForester`                      |
| Heterogeneity Statistics         | `showHeterogeneityStats`       | Show Heterogeneity Statistics          | `heterogeneityResults`              | `.calculateHeterogeneity`            |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportForesterPlot`                |