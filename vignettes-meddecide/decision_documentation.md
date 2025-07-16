# Decision Analysis Documentation

This document provides a comprehensive overview of the Decision Analysis module (decision), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The decision module is a fundamental tool for evaluating choices and their potential outcomes under conditions of uncertainty. It provides a structured approach to decision-making by quantifying the value of different options and assessing the impact of various factors.

The module's features can be broadly categorized as follows:

*   **Core Decision Tree Construction:** Build simple decision trees to map out choices and consequences.
*   **Expected Value Calculation:** Compute expected values for different decision pathways.
*   **Sensitivity Analysis:** Perform basic sensitivity analyses to understand the robustness of decisions.
*   **Visualization:** Generate graphical representations of decision trees.
*   **Export Options:** Capabilities to save analysis results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Decision Nodes                   | `decisionNodes`                | Decision Nodes                         | `decisionOverview`                  | `.createDecisionTree`                |
| Chance Nodes                     | `chanceNodes`                  | Chance Nodes                           | `decisionOverview`                  | `.createDecisionTree`                |
| Outcomes                         | `outcomes`                     | Outcomes                               | `decisionOverview`                  | `.createDecisionTree`                |
| Probabilities                    | `probabilities`                | Probabilities                          | `decisionOverview`                  | `.assignProbabilities`               |
| Values                           | `values`                       | Values (e.g., utility, cost)           | `decisionOverview`                  | `.assignValues`                      |
| **Analysis Options**             |                                |                                        |                                     |                                      |
| Calculate Expected Values        | `calculateExpectedValues`      | Calculate Expected Values              | `expectedValueResults`              | `.calculateExpectedValues`           |
| Show Optimal Path                | `showOptimalPath`              | Show Optimal Path                      | `expectedValueResults`              | `.identifyOptimalPath`               |
| **Sensitivity Analysis**         |                                |                                        |                                     |                                      |
| One-way Sensitivity              | `oneWaySensitivity`            | One-way Sensitivity Analysis           | `oneWaySensitivityResults`          | `.performOneWaySensitivity`          |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Decision Tree Plot          | `showDecisionTreePlot`         | Show Decision Tree Plot                | `decisionTreePlot`                  | `.plotDecisionTree`                  |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportDecisionResults`             |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportDecisionPlot`                |