# ggprism Documentation

This document provides a comprehensive overview of the ggprism module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The ggprism module is a powerful extension for `ggplot2` that provides themes, color palettes, and other functionalities inspired by GraphPad Prism. It aims to help users create publication-ready plots with a consistent and professional look, often preferred in scientific and clinical research.

The module's features can be broadly categorized as follows:

*   **Prism-like Themes:** Apply themes that mimic the visual style of GraphPad Prism.
*   **Customizable Axis Breaks:** Control the appearance and placement of axis ticks and labels.
*   **Enhanced Error Bars:** Add various types of error bars to plots.
*   **Color Palettes:** Utilize colorblind-friendly and aesthetically pleasing color palettes.
*   **Export Options:** Capabilities to save the generated plots in various high-resolution formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Plot Object                      | `plotObject`                   | Input ggplot Object                    | `prismPlotOverview`                 | `.processPrismPlot`                  |
| **Customization Options**        |                                |                                        |                                     |                                      |
| Add Prism Theme                  | `addPrismTheme`                | Add Prism Theme                        | `prismPlot`                         | `.applyPrismTheme`                   |
| Add Axis Breaks                  | `addAxisBreaks`                | Add Axis Breaks                        | `prismPlot`                         | `.customizeAxisBreaks`               |
| Add Error Bars                   | `addErrorBars`                 | Add Error Bars                         | `prismPlot`                         | `.addErrorBars`                      |
| Set Color Palette                | `setColorPalette`              | Set Color Palette                      | `prismPlot`                         | `.setColorPalette`                   |
| Set Line Types                   | `setLineTypes`                 | Set Line Types                         | `prismPlot`                         | `.setLineTypes`                      |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportPrismPlot`                   |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportPrismPlot`                   |