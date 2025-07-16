# Decision Analysis (Version 2) Documentation

This document provides a comprehensive overview of the Decision Analysis (Version 2) module (decision2), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The decision2 module is an enhanced tool for conducting decision analyses, building upon previous versions to offer more robust features for evaluating choices under uncertainty. It supports the construction, analysis, and visualization of decision trees and influence diagrams, aiding in optimal decision-making in various contexts, particularly in clinical and health economics.

The module's features can be broadly categorized as follows:

*   **Core Decision Tree Construction:** Build and analyze decision trees with improved flexibility.
*   **Probabilistic and Utility Assignment:** Assign probabilities to chance nodes and utilities/costs to terminal nodes.
*   **Expected Value Calculation:** Compute expected values for different decision pathways.
*   **Sensitivity Analysis:** Perform one-way, two-way, and probabilistic sensitivity analyses to assess parameter uncertainty.
*   **Visualization:** Generate clear and customizable decision trees and sensitivity plots.
*   **Export Options:** Capabilities to save analysis results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Decision Tree Structure          | `treeStructure`                | Decision Tree Structure                | `decisionOverview`                  | `.buildDecisionTree`                 |
| Probabilities                    | `probabilities`                | Probabilities                          | `decisionOverview`                  | `.assignProbabilities`               |
| Utilities/Costs                  | `utilitiesCosts`               | Utilities/Costs                        | `decisionOverview`                  | `.assignUtilitiesCosts`              |
| **Analysis Options**             |                                |                                        |                                     |                                      |
| Calculate Expected Values        | `calculateExpectedValues`      | Calculate Expected Values              | `expectedValueResults`              | `.calculateExpectedValues`           |
| Show Optimal Strategy            | `showOptimalStrategy`          | Show Optimal Strategy                  | `expectedValueResults`              | `.identifyOptimalStrategy`           |
| **Sensitivity Analysis**         |                                |                                        |                                     |                                      |
| One-way Sensitivity              | `oneWaySensitivity`            | One-way Sensitivity Analysis           | `oneWaySensitivityResults`          | `.performOneWaySensitivity`          |
| Two-way Sensitivity              | `twoWaySensitivity`            | Two-way Sensitivity Analysis           | `twoWaySensitivityResults`          | `.performTwoWaySensitivity`          |
| Probabilistic Sensitivity        | `probabilisticSensitivity`     | Probabilistic Sensitivity Analysis     | `probabilisticSensitivityResults`   | `.performProbabilisticSensitivity`   |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Decision Tree Plot          | `showDecisionTreePlot`         | Show Decision Tree Plot                | `decisionTreePlot`                  | `.plotDecisionTree`                  |
| Show Sensitivity Plot            | `showSensitivityPlot`          | Show Sensitivity Plot                  | `sensitivityPlot`                   | `.plotSensitivity`                   |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Discounting                      | `discounting`                  | Discounting Rate                       | `advancedOptions`                   | `.applyDiscounting`                  |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportDecisionResults`             |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportDecisionPlot`                |