# Waffle Charts Analysis Documentation

This document provides a comprehensive overview of the Waffle Charts Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates comprehensive waffle charts to visualize distributions and proportions. Waffle charts are particularly effective for displaying categorical data with a focus on individual units, making them ideal for representing proportions, survey responses, or resource allocation in a visually intuitive grid format. The module offers customizable rows, color palettes, and options for faceting and flipping the chart, ensuring a professional and publication-ready appearance.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| Counts (Optional)                | `counts`                       | Counts (Optional)                      | `plot`                              | `.run`, `.plot`                      |
| Groups                           | `groups`                       | Groups                                 | `plot`                              | `.run`, `.plot`                      |
| Facet By (Optional)              | `facet`                        | Facet By (Optional)                    | `plot`                              | `.run`, `.plot`                      |
| **Chart Customization**          |                                |                                        |                                     |                                      |
| Number of Rows                   | `rows`                         | Number of Rows                         | `plot`                              | `.run`, `.plot`                      |
| Flip Chart                       | `flip`                         | Flip Chart                             | `plot`                              | `.run`, `.plot`                      |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.run`, `.plot`, `.generateColorPalette` |
| Show Legend                      | `show_legend`                  | Show Legend                            | `plot`                              | `.plot`                              |
| Title                            | `mytitle`                      | Title                                  | `plot`                              | `.plot`                              |
| Legend Title                     | `legendtitle`                  | Legend Title                           | `plot`                              | `.plot`                              |
