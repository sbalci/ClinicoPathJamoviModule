# Flowchart Analysis Documentation

This document provides a comprehensive overview of the Flowchart Analysis module (flowchart), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The flowchart module is a versatile tool for creating visual representations of processes, algorithms, or sequences of steps. It is particularly useful for illustrating patient pathways, decision-making processes, or experimental designs in a clear and structured manner.

The module's features can be broadly categorized as follows:

*   **Core Flowchart Generation:** Create flowcharts from defined nodes and edges.
*   **Node and Edge Customization:** Options for styling nodes (e.g., shapes, colors, labels) and edges (e.g., arrows, line types).
*   **Layout Algorithms:** Apply different layout algorithms to optimize the visual arrangement of the flowchart.
*   **Conditional Logic Representation:** Support for representing decision points and conditional pathways.
*   **Export Options:** Capabilities to save the generated flowcharts in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Nodes                            | `nodes`                        | Nodes                                  | `flowchartOverview`                 | `.createFlowchartData`               |
| Edges                            | `edges`                        | Edges                                  | `flowchartOverview`                 | `.createFlowchartData`               |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Flowchart                   | `showFlowchart`                | Show Flowchart                         | `flowchartPlot`                     | `.plotFlowchart`                     |
| Node Shape                       | `nodeShape`                    | Node Shape                             | `flowchartPlot`                     | `.plotFlowchart`                     |
| Node Color                       | `nodeColor`                    | Node Color                             | `flowchartPlot`                     | `.plotFlowchart`                     |
| Edge Color                       | `edgeColor`                    | Edge Color                             | `flowchartPlot`                     | `.plotFlowchart`                     |
| Layout Algorithm                 | `layoutAlgorithm`              | Layout Algorithm                       | `flowchartPlot`                     | `.plotFlowchart`                     |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Add Node Labels                  | `addNodeLabels`                | Add Node Labels                        | `flowchartPlot`                     | `.plotFlowchart`                     |
| Add Edge Labels                  | `addEdgeLabels`                | Add Edge Labels                        | `flowchartPlot`                     | `.plotFlowchart`                     |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportFlowchart`                   |