# Decision Combination Analysis Documentation

This document provides a comprehensive overview of the Decision Combination Analysis module (decisioncombine), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The decisioncombine module is a specialized tool for combining multiple diagnostic tests or prediction models to improve overall diagnostic or prognostic accuracy. It provides methods for integrating information from different sources to derive a more robust decision.

The module's features can be broadly categorized as follows:

### Data preparation

- The analysis operates on complete cases for the gold standard and any selected tests, automatically omitting rows with missing values in those variables while leaving the source dataset untouched.
- Both "Positive" and "Negative" levels are retained for the internal gold-standard factor even when the imported dataset contains only one of those states, ensuring downstream tables and plots keep a consistent two-column structure.
- If you plan to analyse excluded records separately, export the cleaned dataset alongside the original data before running additional workflows.

*   **Core Combination Methods:** Implement various strategies for combining test results (e.g., serial, parallel, weighted averaging).
*   **Performance Evaluation:** Assess the performance of the combined model using metrics like sensitivity, specificity, and AUC.
*   **Optimization:** Identify optimal combination strategies based on desired performance criteria.
*   **Visualization:** Generate plots to compare the performance of individual tests versus combined models.
*   **Export Options:** Capabilities to save combination results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Test Results                     | `testResults`                  | Test Results (e.g., probabilities)     | `combinationOverview`               | `.combineDecisions`                  |
| Outcome Variable                 | `outcomeVar`                   | Outcome Variable                       | `combinationOverview`               | `.combineDecisions`                  |
| Outcome Event Level              | `outcomeEventLevel`            | Outcome Event Level                    | `combinationOverview`               | `.combineDecisions`                  |
| Combination Method               | `combinationMethod`            | Combination Method                     | `combinationOverview`               | `.combineDecisions`                  |
| **Performance Metrics**          |                                |                                        |                                     |                                      |
| Show Combined Performance        | `showCombinedPerformance`      | Show Combined Performance              | `combinedPerformance`               | `.calculateCombinedPerformance`      |
| Show Individual Performance      | `showIndividualPerformance`    | Show Individual Test Performance       | `individualPerformance`             | `.calculateIndividualPerformance`    |
| **Optimization**                 |                                |                                        |                                     |                                      |
| Optimize Threshold               | `optimizeThreshold`            | Optimize Threshold                     | `optimizationResults`               | `.optimizeThreshold`                 |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show ROC Comparison Plot         | `showROCComparisonPlot`        | Show ROC Comparison Plot               | `rocComparisonPlot`                 | `.plotROCComparison`                 |
| Show Decision Curve Comparison   | `showDecisionCurveComparison`  | Show Decision Curve Comparison         | `dcaComparisonPlot`                 | `.plotDCAComparison`                 |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportCombinationResults`          |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportCombinationPlot`             |
