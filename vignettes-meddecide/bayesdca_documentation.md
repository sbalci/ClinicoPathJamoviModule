# Bayesian Decision Curve Analysis Documentation

This document provides a comprehensive overview of the Bayesian Decision Curve Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Bayesian Decision Curve Analysis module is a powerful tool for evaluating the clinical utility of prediction models and diagnostic tests across different decision thresholds. It allows for both frequentist and Bayesian approaches to decision curve analysis, providing net benefit curves, strategy comparisons, and (for Bayesian analysis) the Expected Value of Perfect Information (EVPI).

The module's features can be broadly categorized as follows:

*   **Outcome and Predictor Specification:** Define binary outcomes and continuous or binary predictors (models/tests).
*   **Threshold Range Definition:** Specify the range and number of decision thresholds for analysis.
*   **Prevalence Options:** Use sample prevalence or an external prevalence for calculations.
*   **Analysis Type:** Choose between frequentist (with optional bootstrap CIs) and Bayesian (with prior strength and posterior draws) analysis.
*   **Output Tables:** Display net benefit results for each model/test, strategy comparisons, and EVPI (for Bayesian).
*   **Visualizations:** Generate decision curves, net benefit difference plots, probability of superiority plots, and EVPI plots.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Data Input**                   |                                |                                        |                                     |                                      |
| Outcomes                         | `outcomes`                     | Outcomes                               | `summary`                           | `.run`, `.runBayesianDCA`, `.runFrequentistDCA` |
| Positive Outcome Level           | `outcomePos`                   | Positive Outcome Level                 | `summary`                           | `.run`                               |
| Models or Tests                  | `predictors`                   | Models or Tests                        | `summary`, `modelResults`           | `.run`, `.runBayesianDCA`, `.runFrequentistDCA` |
| **Threshold Settings**           |                                |                                        |                                     |                                      |
| Minimum Threshold                | `thresholdMin`                 | Minimum Threshold                      | `netBenefitTable`, `modelResults`, `comparisonTable`, `evpiTable`, `mainPlot`, `deltaPlot`, `probPlot`, `evpiPlot` | `.run`, `.runBayesianDCA`, `.runFrequentistDCA`, `.calculateComparisons`, `.calculateEVPI`, `.plotDCA`, `.plotDeltaNB`, `.plotProbability`, `.plotEVPI` |
| Maximum Threshold                | `thresholdMax`                 | Maximum Threshold                      | `netBenefitTable`, `modelResults`, `comparisonTable`, `evpiTable`, `mainPlot`, `deltaPlot`, `probPlot`, `evpiPlot` | `.run`, `.runBayesianDCA`, `.runFrequentistDCA`, `.calculateComparisons`, `.calculateEVPI`, `.plotDCA`, `.plotDeltaNB`, `.plotProbability`, `.plotEVPI` |
| Number of Thresholds             | `thresholdPoints`              | Number of Thresholds                   | `netBenefitTable`, `modelResults`, `comparisonTable`, `evpiTable`, `mainPlot`, `deltaPlot`, `probPlot`, `evpiPlot` | `.run`, `.runBayesianDCA`, `.runFrequentistDCA`, `.calculateComparisons`, `.calculateEVPI`, `.plotDCA`, `.plotDeltaNB`, `.plotProbability`, `.plotEVPI` |
| Direction Indicator              | `directionIndicator`           | Direction Indicator                    | `modelResults`                      | `.runBayesianDCA`, `.runFrequentistDCA` |
| **Prevalence**                   |                                |                                        |                                     |                                      |
| Use External Prevalence          | `useExternalPrevalence`        | Use External Prevalence                | `summary`                           | `.run`                               |
| External Cases                   | `externalCases`                | External Cases                         | `summary`                           | `.run`                               |
| External Total                   | `externalTotal`                | External Total                         | `summary`                           | `.run`                               |
| **Analysis Options**             |                                |                                        |                                     |                                      |
| Bayesian Analysis                | `bayesianAnalysis`             | Bayesian Analysis                      | `summary`, `modelResults`, `comparisonTable`, `probPlot`, `evpiTable`, `evpiPlot` | `.run`, `.runBayesianDCA`, `.calculateComparisons`, `.calculateEVPI`, `.plotDCA`, `.plotProbability`, `.plotEVPI` |
| Prior Strength                   | `priorStrength`                | Prior Strength                         | (Internal)                          | `.runBayesianDCA`                    |
| Bootstrap Confidence Intervals   | `bootstrapCI`                  | Bootstrap Confidence Intervals         | `modelResults`                      | `.runFrequentistDCA`                 |
| Bootstrap Replications           | `bootstrapReps`                | Bootstrap Replications                 | (Internal)                          | `.runFrequentistDCA`                 |
| Calculate EVPI                   | `calculateEVPI`                | Calculate EVPI                         | `evpiTable`, `evpiPlot`             | `.run`, `.calculateEVPI`, `.plotEVPI` |
| Number of Posterior Draws        | `nDraws`                       | Number of Posterior Draws              | (Internal)                          | `.runBayesianDCA`                    |
| **Results Tables**               |                                |                                        |                                     |                                      |
| Analysis Summary                 | (N/A)                          | (N/A)                                  | `summary`                           | `.run`                               |
| Net Benefit Results              | (N/A)                          | (N/A)                                  | `netBenefitTable`                   | `.runBayesianDCA`, `.runFrequentistDCA` |
| Model/Test Results               | (N/A)                          | (N/A)                                  | `modelResults`                      | `.runBayesianDCA`, `.runFrequentistDCA` |
| Strategy Comparison              | (N/A)                          | (N/A)                                  | `comparisonTable`                   | `.calculateComparisons`              |
| Expected Value of Perfect Information | (N/A)                          | (N/A)                                  | `evpiTable`                         | `.calculateEVPI`                     |
| **Plots**                        |                                |                                        |                                     |                                      |
| Decision Curves                  | (N/A)                          | (N/A)                                  | `mainPlot`                          | `.plotDCA`                           |
| Net Benefit Differences          | (N/A)                          | (N/A)                                  | `deltaPlot`                         | `.plotDeltaNB`                       |
| Probability of Superiority       | (N/A)                          | (N/A)                                  | `probPlot`                          | `.plotProbability`                   |
| Expected Value of Perfect Information Plot | (N/A)                          | (N/A)                                  | `evpiPlot`                          | `.plotEVPI`                          |
