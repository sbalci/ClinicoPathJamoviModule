# High-Performance Scatter Plots Documentation

This document provides a comprehensive overview of the High-Performance Scatter Plots module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates high-performance scatter plots using the `scattermore` package, designed for plotting millions of points efficiently. It supports advanced customization options, including variable mapping for color and size, various plot types (base R or ggplot2 integration), and options for smoothing, density contours, and faceting. The module also provides detailed performance information and correlation analysis, making it ideal for visualizing large datasets in clinical and research settings.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| X Variable                       | `x_var`                        | X Variable                             | `plot`, `summary`                   | `.run`, `.prepareData`, `.createScatterPlot`, `.generateSummary` |
| Y Variable                       | `y_var`                        | Y Variable                             | `plot`, `summary`                   | `.run`, `.prepareData`, `.createScatterPlot`, `.generateSummary` |
| Color Variable                   | `color_var`                    | Color Variable                         | `plot`                              | `.createScatterPlot`, `.createBaseRPlot`, `.createGgplotPlot`, `.getColorPalette` |
| Size Variable                    | `size_var`                     | Size Variable                          | `plot`                              | `.createGgplotPlot`                  |
| Facet Variable                   | `facet_var`                    | Facet Variable                         | `plot`                              | `.createGgplotPlot`                  |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Plot Type                        | `plot_type`                    | Plot Type                              | `plot`                              | `.createScatterPlot`, `.createBaseRPlot`, `.createGgplotPlot` |
| Point Size                       | `point_size`                   | Point Size                             | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Transparency (Alpha)             | `alpha`                        | Transparency (Alpha)                   | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Raster Resolution (pixels)       | `pixels`                       | Raster Resolution (pixels)             | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Raster Point Size                | `pointsize`                    | Raster Point Size                      | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Interpolate Raster               | `interpolate`                  | Interpolate Raster                     | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.createGgplotPlot`, `.getColorPalette` |
| Add Smooth Line                  | `show_smooth`                  | Add Smooth Line                        | `plot`                              | `.createGgplotPlot`                  |
| Smooth Method                    | `smooth_method`                | Smooth Method                          | `plot`                              | `.createGgplotPlot`                  |
| Show Density Contours            | `show_density`                 | Show Density Contours                  | `plot`                              | `.createGgplotPlot`                  |
| Log Transform X                  | `log_transform_x`              | Log Transform X                        | `plot`                              | `.createScatterPlot`                 |
| Log Transform Y                  | `log_transform_y`              | Log Transform Y                        | `plot`                              | `.createScatterPlot`                 |
| X-Axis Label                     | `x_label`                      | X-Axis Label                           | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Y-Axis Label                     | `y_label`                      | Y-Axis Label                           | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Plot Title                       | `plot_title`                   | Plot Title                             | `plot`                              | `.createBaseRPlot`, `.createGgplotPlot` |
| Show Correlation                 | `show_correlation`             | Show Correlation                       | `plot`, `summary`                   | `.createBaseRPlot`, `.createGgplotPlot`, `.generateSummary` |
| Show Performance Info            | `show_performance`             | Show Performance Info                  | `performance`                       | `.run`, `.generatePerformanceInfo`   |
| Theme                            | `theme_style`                  | Theme                                  | `plot`                              | `.createGgplotPlot`, `.applyTheme`   |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Plot Summary & Statistics        | `summary`                      | Plot Summary & Statistics              | `summary`                           | `.generateSummary`                   |
| Performance Analysis             | `performance`                  | Performance Analysis                   | `performance`                       | `.generatePerformanceInfo`           |
