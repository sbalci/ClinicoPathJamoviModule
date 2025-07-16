# Grafify Plot Analysis Documentation

This document provides a comprehensive overview of the Grafify Plot Analysis module (grafify), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The grafify module is a comprehensive tool for creating a wide variety of publication-quality plots, primarily focused on statistical data visualization. It aims to simplify the process of generating complex and aesthetically pleasing graphs for scientific and clinical research.

The module's features can be broadly categorized as follows:

*   **Diverse Plot Types:** Support for various plot types including scatter plots, bar plots, box plots, violin plots, and more.
*   **Statistical Overlays:** Option to add statistical summaries, error bars, and significance indicators directly to plots.
*   **Customization and Theming:** Extensive control over plot aesthetics, colors, fonts, and overall themes.
*   **Faceting and Grouping:** Ability to create multi-panel plots based on grouping variables.
*   **Export Options:** Capabilities to save the generated plots in various high-resolution formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| X Variable                       | `xVar`                         | X Variable                             | `grafifyOverview`                   | `.calculateGrafifyData`              |
| Y Variable                       | `yVar`                         | Y Variable                             | `grafifyOverview`                   | `.calculateGrafifyData`              |
| Grouping Variable                | `groupVar`                     | Grouping Variable                      | `grafifyOverview`                   | `.calculateGrafifyData`              |
| **Plot Types**                   |                                |                                        |                                     |                                      |
| Show Scatter Plot                | `showScatterPlot`              | Show Scatter Plot                      | `grafifyPlot`                       | `.plotScatter`                       |
| Show Bar Plot                    | `showBarPlot`                  | Show Bar Plot                          | `grafifyPlot`                       | `.plotBar`                           |
| Show Box Plot                    | `showBoxPlot`                  | Show Box Plot                          | `grafifyPlot`                       | `.plotBox`                           |
| Show Violin Plot                 | `showViolinPlot`               | Show Violin Plot                       | `grafifyPlot`                       | `.plotViolin`                        |
| Show Line Plot                   | `showLinePlot`                 | Show Line Plot                         | `grafifyPlot`                       | `.plotLine`                          |
| **Statistical Overlays**         |                                |                                        |                                     |                                      |
| Show Error Bars                  | `showErrorBars`                | Show Error Bars                        | `grafifyPlot`                       | `.addErrorBars`                      |
| Show Mean                        | `showMean`                     | Show Mean                              | `grafifyPlot`                       | `.addMean`                           |
| Show Statistical Test            | `showStatisticalTest`          | Show Statistical Test                  | `grafifyPlot`                       | `.addStats`                          |
| **Customization**                |                                |                                        |                                     |                                      |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `grafifyPlot`                       | `.applyColorPalette`                 |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `grafifyPlot`                       | `.addPlotTitle`                      |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `grafifyPlot`                       | `.setXLabel`                         |
| Y-axis Label                     | `yAxisLabel`                   | Y-axis Label                           | `grafifyPlot`                       | `.setYLabel`                         |
| Theme                            | `plotTheme`                    | Plot Theme                             | `grafifyPlot`                       | `.applyTheme`                        |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Faceting Variable                | `facetVar`                     | Faceting Variable                      | `grafifyPlot`                       | `.addFaceting`                       |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportGrafifyPlot`                 |