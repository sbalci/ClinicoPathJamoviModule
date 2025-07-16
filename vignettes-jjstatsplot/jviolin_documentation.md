# Violin Plot Analysis Documentation

This document provides a comprehensive overview of the Violin Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates professional violin plots for visualizing the distribution of continuous data across groups. Violin plots combine density estimation with boxplot elements to show both distribution shape and summary statistics, making them ideal for exploring data distributions, identifying patterns, and comparing groups. The module offers extensive customization options for plot appearance, including color palettes, themes, and various overlays like boxplots, data points, and mean indicators.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| Dependent Variable               | `dep`                          | Dependent Variable                     | `plot`                              | `.run`, `.plot`                      |
| Grouping Variable                | `group`                        | Grouping Variable                      | `plot`                              | `.run`, `.plot`                      |
| Color Variable                   | `col`                          | Color Variable                         | `plot`                              | `.plot`                              |
| Fill Variable                    | `fill`                         | Fill Variable                          | `plot`                              | `.plot`                              |
| Exclude Missing Values           | `excl`                         | Exclude Missing Values                 | `plot`                              | `.run`, `.prepareData`               |
| Flip Coordinates                 | `flip`                         | Flip Coordinates                       | `plot`                              | `.plot`                              |
| **Advanced Violin Options**      |                                |                                        |                                     |                                      |
| Add Boxplot Overlay              | `add_boxplot`                  | Add Boxplot Overlay                    | `plot`                              | `.plot`                              |
| Add Data Points                  | `add_points`                   | Add Data Points                        | `plot`                              | `.plot`                              |
| Add Mean Points                  | `add_mean`                     | Add Mean Points                        | `plot`                              | `.plot`                              |
| Draw Quantile Lines              | `draw_quantiles`               | Draw Quantile Lines                    | `plot`                              | `.plot`                              |
| Quantile Values                  | `quantile_lines`               | Quantile Values                        | `plot`                              | `.plot`                              |
| Trim Violin Tails                | `trim_violin`                  | Trim Violin Tails                      | `plot`                              | `.plot`                              |
| Violin Scaling                   | `scale_violin`                 | Violin Scaling                         | `plot`                              | `.plot`                              |
| Violin Width                     | `violin_width`                 | Violin Width                           | `plot`                              | `.plot`                              |
| Violin Transparency              | `violin_alpha`                 | Violin Transparency                    | `plot`                              | `.plot`                              |
| Boxplot Width                    | `boxplot_width`                | Boxplot Width                          | `plot`                              | `.plot`                              |
| Boxplot Transparency             | `boxplot_alpha`                | Boxplot Transparency                   | `plot`                              | `.plot`                              |
| Point Size                       | `point_size`                   | Point Size                             | `plot`                              | `.plot`                              |
| Point Transparency               | `point_alpha`                  | Point Transparency                     | `plot`                              | `.plot`                              |
| Jitter Points                    | `point_jitter`                 | Jitter Points                          | `plot`                              | `.plot`                              |
| **Color Options**                |                                |                                        |                                     |                                      |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.plot`                              |
| Manual Colors                    | `manual_colors`                | Manual Colors                          | `plot`                              | `.plot`                              |
| **Theme Options**                |                                |                                        |                                     |                                      |
| Plot Theme                       | `themex`                       | Plot Theme                             | `plot`                              | `.plot`                              |
| **Axis Labels**                  |                                |                                        |                                     |                                      |
| Use Custom X-axis Label          | `usexlabel`                    | Use Custom X-axis Label                | `plot`                              | `.plot`                              |
| X-axis Label                     | `xlabel`                       | X-axis Label                           | `plot`                              | `.plot`                              |
| Use Custom Y-axis Label          | `useylabel`                    | Use Custom Y-axis Label                | `plot`                              | `.plot`                              |
| Y-axis Label                     | `ylabel`                       | Y-axis Label                           | `plot`                              | `.plot`                              |
