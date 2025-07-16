# BBC-style Plots Documentation

This document provides a comprehensive overview of the BBC-style Plots module (bbcplots), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The bbcplots module is a specialized tool for creating data visualizations that adhere to the distinctive style guidelines of the BBC (British Broadcasting Corporation). It aims to produce clean, clear, and impactful graphics, often characterized by specific color palettes, fonts, and minimalist design principles.

The module's features can be broadly categorized as follows:

*   **BBC Theme Application:** Apply a `ggplot2` theme that mimics the BBC's visual style.
*   **Custom Color Palettes:** Utilize color palettes inspired by BBC data journalism.
*   **Font Customization:** Apply specific fonts for a consistent look.
*   **Grid and Axis Control:** Fine-tune grid lines, axis labels, and ticks for clarity and readability.
*   **Export Options:** Capabilities to save the generated plots in various high-quality formats suitable for publication.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Plot Object                      | `plotObject`                   | Input ggplot Object                    | `bbcPlotOverview`                   | `.processBBCPlot`                    |
| **Customization Options**        |                                |                                        |                                     |                                      |
| Apply BBC Theme                  | `applyBBCTheme`                | Apply BBC Theme                        | `bbcPlot`                           | `.applyBBCTheme`                     |
| Set BBC Palette                  | `setBBCPalette`                | Set BBC Color Palette                  | `bbcPlot`                           | `.setBBCPalette`                     |
| Set Font                         | `setFont`                      | Set Font                               | `bbcPlot`                           | `.setFont`                           |
| Show Major Gridlines             | `showMajorGridlines`           | Show Major Gridlines                   | `bbcPlot`                           | `.setGridlines`                      |
| Show Minor Gridlines             | `showMinorGridlines`           | Show Minor Gridlines                   | `bbcPlot`                           | `.setGridlines`                      |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportBBCPlot`                     |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportBBCPlot`                     |