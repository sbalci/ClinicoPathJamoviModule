# Dendrogram Analysis Documentation

This document provides a comprehensive overview of the Dendrogram module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Dendrogram module is a powerful tool for performing hierarchical clustering and visualizing the results as dendrograms. It offers various clustering and distance methods, along with flexible plotting options including linear, circular, and basic R plots. Users can customize labels, color groups, and highlight clusters to enhance visualization and interpretation.

The module's features can be broadly categorized as follows:

*   **Clustering Configuration:** Select variables, clustering methods, and distance methods.
*   **Plot Customization:** Choose plot type (linear, circular, base), edge type, color schemes, and label visibility.
*   **Cluster Highlighting:** Option to highlight a specified number of clusters with distinct colors.
*   **Group Coloring:** Color dendrogram branches based on a grouping variable.
*   **Summary Statistics:** Provides basic summary statistics for the variables used in clustering.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Clustering Configuration**     |                                |                                        |                                     |                                      |
| Variables for Clustering         | `vars`                         | Variables for Clustering               | `plot`, `summary`                   | `.run`                               |
| Clustering Method                | `clusterMethod`                | Clustering Method                      | `clusterInfo`                       | `.run`                               |
| Distance Method                  | `distanceMethod`               | Distance Method                        | `clusterInfo`                       | `.run`                               |
| **Plot Options**                 |                                |                                        |                                     |                                      |
| Plot Type                        | `plotType`                     | Plot Type                              | `plot`                              | `.plot`, `.plotBaseDendrogram`, `.plotGgraphDendrogram` |
| Edge Type                        | `edgeType`                     | Edge Type                              | `plot`                              | `.plotGgraphDendrogram`              |
| Color Scheme                     | `colorScheme`                  | Color Scheme                           | `plot`                              | `.getColors`, `.getClusterColors`    |
| Show sample labels               | `showLabels`                   | Show sample labels                     | `plot`                              | `.plotBaseDendrogram`, `.plotGgraphDendrogram` |
| Highlight clusters               | `highlightClusters`            | Highlight clusters                     | `plot`, `clusterInfo`               | `.plotBaseDendrogram`, `.plotGgraphDendrogram` |
| Number of clusters to highlight  | `nClusters`                    | Number of clusters to highlight        | `plot`, `clusterInfo`               | `.plotBaseDendrogram`, `.plotGgraphDendrogram` |
| Color by groups                  | `colorGroups`                  | Color by groups                        | `plot`                              | `.plotBaseDendrogram`, `.plotGgraphDendrogram` |
| Grouping Variable                | `group`                        | Grouping Variable                      | `plot`                              | `.createVertices`                    |
| Plot Width                       | `plotWidth`                    | Plot Width                             | `plot`                              | (Image rendering)                    |
| Plot Height                      | `plotHeight`                   | Plot Height                            | `plot`                              | (Image rendering)                    |
| **Results**                      |                                |                                        |                                     |                                      |
| Cluster Information              | (N/A)                          | (N/A)                                  | `clusterInfo`                       | `.run`                               |
| Summary Statistics               | (N/A)                          | (N/A)                                  | `summary`                           | `.run`                               |
