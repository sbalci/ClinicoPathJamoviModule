# Grouped Forest Plot Analysis Documentation

This document provides a comprehensive overview of the Grouped Forest Plot Analysis module (groupedforest), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The groupedforest module is a specialized tool for creating forest plots that display effect sizes and confidence intervals, particularly useful for visualizing results across multiple subgroups or studies. It enhances the standard forest plot by allowing for hierarchical grouping and comparison of effects.

The module's features can be broadly categorized as follows:

*   **Core Grouped Forest Plot Generation:** Create forest plots with hierarchical grouping of effects.
*   **Subgroup Analysis Visualization:** Clearly display effect estimates and confidence intervals for various subgroups.
*   **Customization Options:** Control plot aesthetics such as colors, shapes, labels, and overall layout.
*   **Overall Effect Summaries:** Include pooled or overall effect estimates for each main group.
*   **Export Options:** Capabilities to save the generated plots in various high-quality formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Effect Estimate                  | `effectEstimate`               | Effect Estimate                        | `groupedForestOverview`             | `.calculateGroupedForestData`        |
| Lower CI                         | `lowerCI`                      | Lower Confidence Interval              | `groupedForestOverview`             | `.calculateGroupedForestData`        |
| Upper CI                         | `upperCI`                      | Upper Confidence Interval              | `groupedForestOverview`             | `.calculateGroupedForestData`        |
| Grouping Variable 1              | `groupVar1`                    | Grouping Variable 1                    | `groupedForestOverview`             | `.calculateGroupedForestData`        |
| Grouping Variable 2              | `groupVar2`                    | Grouping Variable 2                    | `groupedForestOverview`             | `.calculateGroupedForestData`        |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Grouped Forest Plot         | `showGroupedForestPlot`        | Show Grouped Forest Plot               | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Reference Line                   | `referenceLine`                | Reference Line                         | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Point Shape                      | `pointShape`                   | Point Shape                            | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Point Color                      | `pointColor`                   | Point Color                            | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Line Type                        | `lineType`                     | Line Type                              | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Show Overall Effect              | `showOverallEffect`            | Show Overall Effect                    | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Overall Effect Label             | `overallEffectLabel`           | Overall Effect Label                   | `groupedForestPlot`                 | `.plotGroupedForest`                 |
| Heterogeneity Statistics         | `showHeterogeneityStats`       | Show Heterogeneity Statistics          | `heterogeneityResults`              | `.calculateHeterogeneity`            |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportGroupedForestPlot`           |