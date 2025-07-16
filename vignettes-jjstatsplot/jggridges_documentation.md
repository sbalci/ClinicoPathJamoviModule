# Ridge Plot Analysis Documentation

This document provides a comprehensive overview of the Ridge Plot Analysis module (jggridges), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jggridges module is a specialized tool for creating ridge plots (also known as joyplots), which are useful for visualizing the distribution of a quantitative variable for several groups. It allows for easy comparison of distributions across different categories.

The module's features can be broadly categorized as follows:

*   **Core Ridge Plot Generation:** Create ridge plots to visualize density distributions.
*   **Grouping and Faceting:** Organize plots by multiple categorical variables.
*   **Customization Options:** Control plot aesthetics such as colors, fills, and overlaps.
*   **Statistical Overlays:** Add statistical summaries or elements like mean markers.
*   **Export Options:** Capabilities to save the generated plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Quantitative Variable            | `quantitativeVar`              | Quantitative Variable                  | `ridgePlotOverview`                 | `.calculateRidgeData`                |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `ridgePlotOverview`                 | `.calculateRidgeData`                |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Ridge Plot                  | `showRidgePlot`                | Show Ridge Plot                        | `ridgePlot`                         | `.plotRidgePlot`                     |
| Fill By Group                    | `fillByGroup`                  | Fill By Group                          | `ridgePlot`                         | `.plotRidgePlot`                     |
| Overlap Scale                    | `overlapScale`                 | Overlap Scale                          | `ridgePlot`                         | `.plotRidgePlot`                     |
| Show Mean Line                   | `showMeanLine`                 | Show Mean Line                         | `ridgePlot`                         | `.plotRidgePlot`                     |
| Show Quantiles                   | `showQuantiles`                | Show Quantiles                         | `ridgePlot`                         | `.plotRidgePlot`                     |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `ridgePlot`                         | `.plotRidgePlot`                     |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `ridgePlot`                         | `.plotRidgePlot`                     |
| Y-axis Label                     | `yAxisLabel`                   | Y-axis Label                           | `ridgePlot`                         | `.plotRidgePlot`                     |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `ridgePlot`                         | `.plotRidgePlot`                     |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Bandwidth Adjustment             | `bandwidthAdjust`              | Bandwidth Adjustment                   | `ridgePlot`                         | `.plotRidgePlot`                     |
| Kernel Type                      | `kernelType`                   | Kernel Type                            | `ridgePlot`                         | `.plotRidgePlot`                     |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportRidgePlot`                   |