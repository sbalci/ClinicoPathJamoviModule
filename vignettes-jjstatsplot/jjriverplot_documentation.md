# River Plot Analysis Documentation

This document provides a comprehensive overview of the River Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The River Plot Analysis module is a versatile tool for creating river plots (also known as alluvial diagrams or Sankey diagrams). These plots are effective for visualizing flows, transitions, and changes in categorical data over time or across different stages. The module allows for significant customization of the plot's appearance and the underlying data representation.

The module's features can be broadly categorized as follows:

*   **Core Data Input:** Defining the fundamental components of the river plot, such as the time or sequence variable, the categorical strata, and optional weights.
*   **Plot Configuration:** A wide range of options to control the visual representation of the data, including the plot type, color schemes, and labeling.
*   **Titles and Theming:** Customizing titles and selecting different visual themes for the plot.
*   **Output:** Generating the final river plot visualization.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| ID Variable | `id` | ID Variable (optional) | (Not in results) | `.prepareOptions` |
| Time/Sequence Variable | `time` | Time/Sequence Variable | `plot` | `.prepareData`, `.plot` |
| Strata Variables | `strata` | Strata Variables | `plot` | `.prepareData`, `.plot` |
| Weight Variable | `weight` | Weight Variable (optional) | `plot` | `.prepareOptions`, `.plot` |
| **Plot Configuration** | | | | |
| Plot Type | `plotType` | Plot Type | `plot` | `.plot` |
| Fill Pattern | `fillType` | Fill Pattern | `plot` | `.plot` |
| Sort Streams | `sortStreams` | Sort Streams | `plot` | `.prepareOptions` |
| Label Nodes | `labelNodes` | Label Nodes | `plot` | `.plot` |
| Curve Type | `curveType` | Curve Type | `plot` | `.prepareOptions` |
| Show Counts | `showCounts` | Show Counts | `plot` | `.plot` |
| Show Legend | `showLegend` | Show Legend | `plot` | `.plot` |
| **Titles and Theme** | | | | |
| Title | `mytitle` | Title | `plot` | `.prepareOptions` |
| X-Title | `xtitle` | X-Title | `plot` | `.prepareOptions` |
| Y-Title | `ytitle` | Y-Title | `plot` | `.prepareOptions` |
| Use ggStatsPlot Theme | `originaltheme` | Use ggStatsPlot Theme | `plot` | `.plot` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| River Plot | `plot` | River Plot | `plot` | `.plot` |
