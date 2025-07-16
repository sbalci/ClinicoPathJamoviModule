# Decision Graph Analysis Documentation

This document provides a comprehensive overview of the Decision Graph Analysis module (decisiongraph), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The decisiongraph module is a specialized tool for visualizing decision trees and decision-making processes. It allows for the graphical representation of choices, outcomes, and probabilities, which is crucial for understanding and communicating complex decision pathways in clinical and other fields.

The module's features can be broadly categorized as follows:

*   **Core Decision Graph Generation:** Create visual representations of decision trees.
*   **Node and Branch Customization:** Options for styling decision nodes, chance nodes, terminal nodes, and branches.
*   **Probability and Utility Display:** Integrate probabilities and outcome utilities directly into the graph.
*   **Sensitivity Analysis Visualization:** Display results of sensitivity analyses on decision graphs.
*   **Export Options:** Capabilities to save the generated decision graphs in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Decision Tree Data               | `treeData`                     | Decision Tree Data                     | `decisionGraphOverview`             | `.prepareDecisionGraphData`          |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Decision Graph              | `showDecisionGraph`            | Show Decision Graph                    | `decisionGraphPlot`                 | `.plotDecisionGraph`                 |
| Node Shape                       | `nodeShape`                    | Node Shape                             | `decisionGraphPlot`                 | `.plotDecisionGraph`                 |
| Node Color                       | `nodeColor`                    | Node Color                             | `decisionGraphPlot`                 | `.plotDecisionGraph`                 |
| Branch Color                     | `branchColor`                  | Branch Color                           | `decisionGraphPlot`                 | `.plotDecisionGraph`                 |
| Show Probabilities               | `showProbabilities`            | Show Probabilities                     | `decisionGraphPlot`                 | `.plotDecisionGraph`                 |
| Show Utilities                   | `showUtilities`                | Show Utilities                         | `decisionGraphPlot`                 | `.plotDecisionGraph`                 |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Sensitivity Analysis Plot        | `sensitivityPlot`              | Sensitivity Analysis Plot              | `sensitivityPlot`                   | `.plotSensitivityAnalysis`           |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportDecisionGraph`               |