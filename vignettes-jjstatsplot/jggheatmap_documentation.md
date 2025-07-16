# Heatmap Analysis Documentation

This document provides a comprehensive overview of the Heatmap Analysis module (jggheatmap), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jggheatmap module is a powerful tool for visualizing data in a matrix format, where values are represented by colors. It is particularly useful for displaying correlations, gene expression patterns, or any data that can be organized into a two-dimensional grid.

The module's features can be broadly categorized as follows:

*   **Core Heatmap Generation:** Create heatmaps from numerical data.
*   **Clustering Options:** Perform hierarchical clustering on rows and/or columns.
*   **Color Mapping and Legends:** Customize color scales and display legends.
*   **Annotation Features:** Add side annotations for rows and columns.
*   **Export Options:** Capabilities to save the generated heatmaps in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Data Matrix                      | `dataMatrix`                   | Data Matrix                            | `heatmapOverview`                   | `.calculateHeatmapData`              |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Heatmap                     | `showHeatmap`                  | Show Heatmap                           | `heatmapPlot`                       | `.plotHeatmap`                       |
| Row Clustering                   | `rowClustering`                | Row Clustering                         | `heatmapPlot`                       | `.plotHeatmap`                       |
| Column Clustering                | `colClustering`                | Column Clustering                      | `heatmapPlot`                       | `.plotHeatmap`                       |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `heatmapPlot`                       | `.plotHeatmap`                       |
| Show Row Names                   | `showRowNames`                 | Show Row Names                         | `heatmapPlot`                       | `.plotHeatmap`                       |
| Show Column Names                | `showColumnNames`              | Show Column Names                      | `heatmapPlot`                       | `.plotHeatmap`                       |
| Show Color Key                   | `showColorKey`                 | Show Color Key                         | `heatmapPlot`                       | `.plotHeatmap`                       |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Row Annotations                  | `rowAnnotations`               | Row Annotations                        | `heatmapPlot`                       | `.plotHeatmap`                       |
| Column Annotations               | `colAnnotations`               | Column Annotations                     | `heatmapPlot`                       | `.plotHeatmap`                       |
| Scaling                          | `scaling`                      | Scaling                                | `heatmapPlot`                       | `.plotHeatmap`                       |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportHeatmap`                     |