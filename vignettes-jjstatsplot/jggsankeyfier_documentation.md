# Sankey Diagram Analysis Documentation

This document provides a comprehensive overview of the Sankey Diagram Analysis module (jggsankeyfier), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jggsankeyfier module is a powerful tool for visualizing flows and relationships between different categories or stages. It is particularly useful for illustrating transitions, allocations, or movements of quantities through a system.

The module's features can be broadly categorized as follows:

*   **Core Sankey Diagram Generation:** Create basic Sankey diagrams to show flows between nodes.
*   **Node and Link Customization:** Options for styling nodes and links (e.g., colors, widths, labels).
*   **Flow Aggregation and Disaggregation:** Control how flows are grouped or broken down.
*   **Interactive Features:** Tools for exploring and manipulating the Sankey diagram.
*   **Export Options:** Capabilities to save the generated diagrams in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Source Nodes                     | `sourceNodes`                  | Source Nodes                           | `sankeyOverview`                    | `.calculateSankey`                   |
| Target Nodes                     | `targetNodes`                  | Target Nodes                           | `sankeyOverview`                    | `.calculateSankey`                   |
| Flow Values                      | `flowValues`                   | Flow Values                            | `sankeyOverview`                    | `.calculateSankey`                   |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Sankey Diagram              | `showSankeyDiagram`            | Show Sankey Diagram                    | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Node Colors                      | `nodeColors`                   | Node Colors                            | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Link Colors                      | `linkColor`                    | Link Colors                            | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Node Width                       | `nodeWidth`                    | Node Width                             | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Link Transparency                | `linkTransparency`             | Link Transparency                      | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Node Padding                     | `nodePadding`                  | Node Padding                           | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Aggregation Level                | `aggregationLevel`             | Aggregation Level                      | `sankeyOverview`                    | `.calculateSankey`                   |
| Show Node Labels                 | `showNodeLabels`               | Show Node Labels                       | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Show Flow Labels                 | `showFlowLabels`               | Show Flow Labels                       | `sankeyPlot`                        | `.plotSankeyDiagram`                 |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportSankeyDiagram`               |