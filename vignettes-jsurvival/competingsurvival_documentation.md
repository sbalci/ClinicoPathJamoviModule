# Competing Survival Analysis Documentation

This document provides a comprehensive overview of the Competing Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Competing Survival Analysis module is designed to analyze time-to-event data in the presence of competing risks. It allows for the estimation of overall survival, cause-specific survival, and cumulative incidence functions, providing a comprehensive understanding of patient outcomes when multiple event types can occur.

The module's features can be broadly categorized as follows:

*   **Survival Analysis Types:** Supports overall survival, cause-specific survival, and competing risks analysis.
*   **Outcome Definition:** Flexible definition of different outcome statuses (e.g., death from disease, death from other causes, alive with/without disease).
*   **Statistical Output:** Provides hazard ratios, confidence intervals, and p-values for survival models.
*   **Cumulative Incidence:** Calculates and displays cumulative incidence functions for competing events.
*   **Visualizations:** Generates plots for competing risks to visualize the probability of each event type over time.
*   **Interpretation Guidance:** Offers clinical interpretation for different analysis types.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis Setup**          |                                |                                        |                                     |                                      |
| Explanatory Variable             | `explanatory`                  | Explanatory Variable                   | (Model input)                       | `.performAnalysis`                   |
| Overall Time                     | `overalltime`                  | Overall Time                           | (Model input)                       | `.performAnalysis`                   |
| Outcome Variable                 | `outcome`                      | Outcome                                | (Model input)                       | `.performAnalysis`                   |
| Death of Disease Level           | `dod`                          | Death of Disease                       | (Outcome mapping)                   | `.performAnalysis`                   |
| Death of Other Causes Level      | `dooc`                         | Death of Other Causes                  | (Outcome mapping)                   | `.performAnalysis`                   |
| Alive with Disease Level         | `awd`                          | Alive w Disease                        | (Outcome mapping)                   | `.performAnalysis`                   |
| Alive without Disease Level      | `awod`                         | Alive w/o Disease                      | (Outcome mapping)                   | `.performAnalysis`                   |
| Analysis Type                    | `analysistype`                 | Analysis Type                          | (Controls analysis flow)            | `.performAnalysis`                   |
| **Results Output**               |                                |                                        |                                     |                                      |
| Analysis Summary                 | (N/A)                          | Analysis Summary                       | `summary`                           | `.formatSurvivalResults`, `.formatCompetingRisksResults` |
| Survival Table                   | (N/A)                          | Survival Analysis Results              | `survivalTable`                     | `.formatSurvivalResults`, `.formatCompetingRisksResults` |
| Cumulative Incidence Function    | (N/A)                          | Cumulative Incidence Function          | `cuminc`                            | `.formatCumulativeIncidence`         |
| Competing Risks Plot             | (N/A)                          | Competing Risks Plot                   | `comprisksPlot`                     | `.plotCompetingRisks`                |
| Clinical Interpretation          | (N/A)                          | Clinical Interpretation                | `interpretation`                    | `.generateInterpretation`            |
