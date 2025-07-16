# Forest Model Analysis Documentation

This document provides a comprehensive overview of the Forest Model Analysis module (jforestmodel), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jforestmodel module is a powerful tool for visualizing and interpreting results from regression models, particularly in the context of meta-analysis or subgroup analysis. It generates forest plots, which are effective for displaying effect sizes and their confidence intervals across different studies or subgroups.

The module's features can be broadly categorized as follows:

*   **Core Forest Plot Generation:** Create forest plots from model outputs or summary statistics.
*   **Subgroup Analysis:** Display results for different subgroups within the data.
*   **Customization Options:** Control plot aesthetics such as labels, colors, and markers.
*   **Statistical Summaries:** Include overall effect estimates and heterogeneity statistics.
*   **Export Options:** Capabilities to save the generated plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Model Output                     | `modelOutput`                  | Model Output                           | `forestPlotOverview`                | `.calculateForestData`               |
| Effect Estimate                  | `effectEstimate`               | Effect Estimate                        | `forestPlotOverview`                | `.calculateForestData`               |
| Lower Confidence Interval        | `lowerCI`                      | Lower Confidence Interval              | `forestPlotOverview`                | `.calculateForestData`               |
| Upper Confidence Interval        | `upperCI`                      | Upper Confidence Interval              | `forestPlotOverview`                | `.calculateForestData`               |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Forest Plot                 | `showForestPlot`               | Show Forest Plot                       | `forestPlot`                        | `.plotForest`                        |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `forestPlot`                        | `.plotForest`                        |
| Reference Line                   | `referenceLine`                | Reference Line                         | `forestPlot`                        | `.plotForest`                        |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `forestPlot`                        | `.plotForest`                        |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `forestPlot`                        | `.plotForest`                        |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Heterogeneity Statistics         | `showHeterogeneity`            | Show Heterogeneity Statistics          | `heterogeneityResults`              | `.calculateHeterogeneity`            |
| Overall Effect                   | `showOverallEffect`            | Show Overall Effect                    | `forestPlot`                        | `.plotForest`                        |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportForestPlot`                  |