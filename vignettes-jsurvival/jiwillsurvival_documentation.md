# Will Rogers Phenomenon Survival Analysis Documentation

This document provides a comprehensive overview of the Will Rogers Phenomenon Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Will Rogers Phenomenon Survival Analysis module is a specialized tool for investigating the Will Rogers Phenomenon in survival data, where changes in staging criteria can artificially inflate survival rates in both the 'old' and 'new' stages. It provides statistical analyses and visualizations to assess this phenomenon and its impact on patient prognosis.

The module's features can be broadly categorized as follows:

*   **Core Will Rogers Analysis:** Basic statistics and visualizations to understand how patient re-classification impacts survival.
*   **Survival Curve Comparison:** Visual and statistical comparison of survival curves before and after re-classification.
*   **Prognostic Assessment:** Evaluation of prognostic factors in the context of the Will Rogers Phenomenon.
*   **Adjusted Analysis:** Options to include covariates for adjusted comparisons.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Old Staging System               | `oldStage`                     | Old Staging System                     | `willRogersOverview`                | `.calculateWillRogers`               |
| New Staging System               | `newStage`                     | New Staging System                     | `willRogersOverview`                | `.calculateWillRogers`               |
| Survival Time                    | `survivalTime`                 | Survival Time                          | `willRogersOverview`                | `.calculateWillRogers`               |
| Event                            | `event`                        | Event                                  | `willRogersOverview`                | `.calculateWillRogers`               |
| Event Level                      | `eventLevel`                   | Event Level                            | `willRogersOverview`                | `.calculateWillRogers`               |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `willRogersOverview`                | `.calculateWillRogers`               |
| **Will Rogers Analysis**         |                                |                                        |                                     |                                      |
| Show Will Rogers Analysis        | `showWillRogersAnalysis`       | Show Will Rogers Phenomenon Analysis   | `willRogersAnalysis`                | `.analyzeWillRogers`                 |
| Show Survival Curves             | `showSurvivalCurves`           | Show Survival Curves                   | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Survival Plot Type               | `survivalPlotType`             | Survival Plot Type                     | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Show Risk Tables                 | `showRiskTables`               | Show Risk Tables                       | `survivalCurves`                    | `.plotSurvivalCurves`                |
| Plot Time Range                  | `plotTimeRange`                | Plot Time Range                        | `survivalCurves`                    | `.plotSurvivalCurves`                |
| **Multifactorial Analysis**      |                                |                                        |                                     |                                      |
| Enable Multifactorial Analysis   | `enableMultifactorialAnalysis` | Enable Multifactorial Analysis         | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Continuous Covariates            | `continuousCovariates`         | Continuous Covariates                  | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Categorical Covariates           | `categoricalCovariates`        | Categorical Covariates                 | `multifactorialResults`             | `.performMultifactorialAnalysis`     |
| Show Multifactorial Tables       | `showMultifactorialTables`     | Show Multifactorial Tables             | `multifactorialResults`             | `.performMultifactorialAnalysis`     |