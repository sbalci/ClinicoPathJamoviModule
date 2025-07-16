# StreamGraphs (jjstreamgraph) Documentation

This document provides a comprehensive overview of the `jjstreamgraph` module, detailing its features, user interface elements, and the underlying R functions. This module is designed to create interactive stream graphs for time series data.

## Feature Summary

The `jjstreamgraph` module is a powerful tool for visualizing changes in data over time across different categories. It generates interactive stream graphs, which are stacked area charts that show the evolution of different categories over a continuous variable (typically time). The module leverages the `streamgraph` R package for its functionality.

The module's features can be broadly categorized as follows:

*   **Data Input:** Requires time, value, and grouping variables to construct the stream graph.
*   **Layout and Appearance:** Offers options for offset type (silhouette, zero, expand), interpolation method (smooth, linear, step), and color palettes.
*   **Sizing:** Allows customization of the plot's width and height.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| :------------------------------- | :----------------------------- | :------------------------------------- | :---------------------------------- | :----------------------------------- |
| **Data Input**                   |                                |                                        |                                     |                                      |
| Time Variable                    | `timeVar`                      | Time Variable                          | `StreamGraph`                       | `.run`, `.prepareData`               |
| Value Variable                   | `valueVar`                     | Value Variable                         | `StreamGraph`                       | `.run`, `.prepareData`               |
| Grouping Variable                | `groupVar`                     | Grouping Variable                      | `StreamGraph`                       | `.run`, `.prepareData`               |
| **Layout and Appearance**        |                                |                                        |                                     |                                      |
| Offset Type                      | `offset`                       | Offset Type                            | `StreamGraph`                       | `.run`                               |
| Interpolation                    | `interpolate`                  | Interpolation                          | `StreamGraph`                       | `.run`                               |
| Color Palette                    | `palette`                      | Color Palette                          | `StreamGraph`                       | `.run`                               |
| **Sizing**                       |                                |                                        |                                     |                                      |
| Width                            | `width`                        | Width                                  | `StreamGraph`                       | `.run`                               |
| Height                           | `height`                       | Height                                 | `StreamGraph`                       | `.run`                               |
