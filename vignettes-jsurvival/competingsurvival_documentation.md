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

## R Function Details

### Core Functions

#### `.performAnalysis()`

*   **Purpose:** This is the main function that controls the entire analysis. It reads the user's selections from the UI and calls the appropriate functions to perform the selected analysis type (Overall Survival, Cause-Specific Survival, or Competing Risks).

### Analysis-Specific Functions

#### Overall Survival

*   **Functionality:** When the user selects "Overall Survival," this function treats all death events (both disease-specific and other causes) as a single event type. It then uses the standard `survival` package to perform a Kaplan-Meier analysis and Cox proportional hazards regression.

#### Cause-Specific Survival

*   **Functionality:** When the user selects "Cause-Specific Survival," this function treats only the disease-specific deaths as events. Deaths from other causes are treated as censored observations. It then uses the standard `survival` package to perform the analysis.

#### Competing Risks

*   **Functionality:** When the user selects "Competing Risks," this function uses the `cmprsk` package to perform a competing risks analysis. It calculates the cumulative incidence function for each event type and performs a Fine-Gray subdistribution hazard regression.

### Formatting and Plotting Functions

#### `.formatSurvivalResults()`

*   **Purpose:** To format the results of the overall and cause-specific survival analyses into a user-friendly table.

#### `.formatCompetingRisksResults()`

*   **Purpose:** To format the results of the competing risks analysis, including the subdistribution hazard ratios, into a table.

#### `.formatCumulativeIncidence()`

*   **Purpose:** To format the cumulative incidence estimates at different time points into a table.

#### `.plotCompetingRisks()`

*   **Purpose:** To generate the cumulative incidence function plot, which shows the probability of each event type occurring over time.

### Helper Functions

#### `.generateInterpretation()`

*   **Purpose:** To provide a textual interpretation of the results, guiding the user on how to understand the output of the selected analysis.