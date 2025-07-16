# Economist Plot Analysis Documentation

This document provides a comprehensive overview of the Economist Plot Analysis module (economistplot), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The economistplot module is a specialized tool for creating plots with the distinctive style of The Economist magazine. It aims to produce clean, minimalist, and visually impactful graphics, often characterized by specific color palettes, fonts, and grid lines.

The module's features can be broadly categorized as follows:

*   **Economist Theme Application:** Apply a `ggplot2` theme that mimics The Economist's visual style.
*   **Custom Color Palettes:** Utilize color palettes inspired by The Economist's publications.
*   **Font Customization:** Apply specific fonts for a consistent look.
*   **Grid and Axis Control:** Fine-tune grid lines, axis labels, and ticks for clarity.
*   **Export Options:** Capabilities to save the generated plots in various high-quality formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Plot Object                      | `plotObject`                   | Input ggplot Object                    | `economistPlotOverview`             | `.processEconomistPlot`              |
| **Customization Options**        |                                |                                        |                                     |                                      |
| Apply Economist Theme            | `applyEconomistTheme`          | Apply Economist Theme                  | `economistPlot`                     | `.applyEconomistTheme`               |
| Set Economist Palette            | `setEconomistPalette`          | Set Economist Color Palette            | `economistPlot`                     | `.setEconomistPalette`               |
| Set Font                         | `setFont`                      | Set Font                               | `economistPlot`                     | `.setFont`                           |
| Show Major Gridlines             | `showMajorGridlines`           | Show Major Gridlines                   | `economistPlot`                     | `.setGridlines`                      |
| Show Minor Gridlines             | `showMinorGridlines`           | Show Minor Gridlines                   | `economistPlot`                     | `.setGridlines`                      |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportEconomistPlot`               |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportEconomistPlot`               |