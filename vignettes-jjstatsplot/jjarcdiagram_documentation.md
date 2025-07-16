# Arc Diagram Analysis Documentation

This document provides a comprehensive overview of the Arc Diagram Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Arc Diagram Analysis module is a powerful tool for visualizing relationships and connections between entities. It provides a wide range of statistical analyses and visualizations to explore network structures and patterns.

The module's features can be broadly categorized as follows:

*   **Core Arc Diagram Analysis:** Basic statistics and visualizations to understand the structure of the network.
*   **Advanced Layout Options:** Customization options for the visual representation of the arc diagram.
*   **Node and Edge Properties:** Analysis of attributes associated with nodes and edges in the network.
*   **Interactive Features:** Tools for exploring and manipulating the arc diagram.
*   **Export Options:** Capabilities to save the generated diagrams in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Nodes                            | `nodes`                        | Nodes                                  | `arcDiagramOverview`                | `.calculateArcDiagram`               |
| Edges                            | `edges`                        | Edges                                  | `arcDiagramOverview`                | `.calculateArcDiagram`               |
| Node Labels                      | `nodeLabels`                   | Node Labels                            | `arcDiagramOverview`                | `.calculateArcDiagram`               |
| Edge Weights                     | `edgeWeights`                  | Edge Weights                           | `arcDiagramOverview`                | `.calculateArcDiagram`               |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Arc Diagram                 | `showArcDiagram`               | Show Arc Diagram                       | `arcDiagramPlot`                    | `.plotArcDiagram`                    |
| Layout Type                      | `layoutType`                   | Layout Type                            | `arcDiagramPlot`                    | `.plotArcDiagram`                    |
| Node Color By                    | `nodeColorBy`                  | Node Color By                          | `arcDiagramPlot`                    | `.plotArcDiagram`                    |
| Edge Color By                    | `edgeColorBy`                  | Edge Color By                          | `arcDiagramPlot`                    | `.plotArcDiagram`                    |
| Node Size By                     | `nodeSizeBy`                   | Node Size By                           | `arcDiagramPlot`                    | `.plotArcDiagram`                    |
| Edge Thickness By                | `edgeThicknessBy`              | Edge Thickness By                      | `arcDiagramPlot`                    | `.plotArcDiagram`                    |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Centrality Measures              | `calculateCentrality`          | Calculate Centrality Measures          | `centralityResults`                 | `.calculateCentrality`               |
| Community Detection              | `performCommunityDetection`    | Perform Community Detection            | `communityDetectionResults`         | `.performCommunityDetection`         |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportArcDiagram`                  |