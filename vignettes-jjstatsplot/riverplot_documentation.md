# River Plots Analysis Documentation

This document provides a comprehensive overview of the River Plots Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The River Plots module generates river plots (also known as alluvial diagrams, Sankey diagrams, or stream graphs) to visualize flows and transitions over time or between categorical stages. It supports various plot types and offers extensive customization options for aesthetics, labels, and data representation. This tool is ideal for tracking patient journeys, changes in disease states, or any sequential categorical data in clinical and research settings.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| ID Variable (optional)           | `id`                           | ID Variable (optional)                 | `plot`                              | `.plot`                              |
| Time/Sequence Variable           | `time`                         | Time/Sequence Variable                 | `plot`                              | `.run`, `.plot`                      |
| Strata Variables                 | `strata`                       | Strata Variables                       | `plot`                              | `.run`, `.plot`                      |
| Weight Variable (optional)       | `weight`                       | Weight Variable (optional)             | `plot`                              | `.plot`                              |
| **Plot Type & Aesthetics**       |                                |                                        |                                     |                                      |
| Plot Type                        | `plotType`                     | Plot Type                              | `plot`                              | `.plot`                              |
| Fill Pattern                     | `fillType`                     | Fill Pattern                           | `plot`                              | `.plot`                              |
| Sort Streams                     | `sortStreams`                  | Sort Streams                           | `plot`                              | `.plot`                              |
| Label Nodes                      | `labelNodes`                   | Label Nodes                            | `plot`                              | `.plot`                              |
| Curve Type                       | `curveType`                    | Curve Type                             | `plot`                              | `.plot`                              |
| Show Counts                      | `showCounts`                   | Show Counts                            | `plot`                              | `.plot`                              |
| Show Legend                      | `showLegend`                   | Show Legend                            | `plot`                              | `.plot`                              |
| Title                            | `mytitle`                      | Title                                  | `plot`                              | `.plot`                              |
| X-Title                          | `xtitle`                       | X-Title                                | `plot`                              | `.plot`                              |
| Y-Title                          | `ytitle`                       | Y-Title                                | `plot`                              | `.plot`                              |
| Use ggStatsPlot Theme            | `originaltheme`                | Use ggStatsPlot Theme                  | `plot`                              | `.plot`                              |
