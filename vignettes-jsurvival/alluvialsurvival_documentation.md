# Alluvial Survival Analysis Documentation

This document provides a comprehensive overview of the Alluvial Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Alluvial Survival Analysis module generates an alluvial plot to visualize patient treatment pathways over time, with an optional integrated survival analysis. It is designed to track patient transitions between disease stages and treatments, providing insights into the dynamics of clinical progression and treatment effectiveness.

The module's features can be broadly categorized as follows:

*   **Pathway Visualization:** Creates alluvial plots to show patient flows between different stages and treatments over multiple time points.
*   **Data Handling:** Requires longitudinal data with time points, disease stages, treatments, and patient IDs.
*   **Survival Analysis Integration:** Optionally includes survival curves based on initial disease stage and final treatment.
*   **Customization:** Options for color schemes and displaying a percentage axis.
*   **Summary Statistics:** Provides tables summarizing patient distribution by stage and treatment at each time point, and survival statistics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Pathway Visualization**        |                                |                                        |                                     |                                      |
| Time Variable                    | `timeVar`                      | Time Variable                          | `plot`                              | `.run`, `.plot`, `.validateData`, `.calculateStats`, `.prepareAlluvialData` |
| Disease Stage                    | `stageVar`                     | Disease Stage                          | `plot`                              | `.run`, `.plot`, `.validateData`, `.calculateStats`, `.prepareAlluvialData` |
| Treatment                        | `treatmentVar`                 | Treatment                              | `plot`                              | `.run`, `.plot`, `.validateData`, `.calculateStats`, `.prepareAlluvialData` |
| Patient ID                       | `patientId`                    | Patient ID                             | `plot`                              | `.run`, `.plot`, `.validateData`, `.calculateStats`, `.prepareAlluvialData` |
| Show Percentage Axis             | `showRightAxis`                | Show Percentage Axis                   | `plot`                              | `.plot`                              |
| Color Scheme                     | `colorScheme`                  | Color Scheme                           | `plot`                              | `.plot`, `.getColorScheme`           |
| **Survival Analysis Integration**|                                |                                        |                                     |                                      |
| Survival Status                  | `survivalVar`                  | Survival Status                        | `survivalStats`, `survivalPlot`     | `.run`, `.plotSurvival`, `.validateData`, `.calculateSurvivalStats`, `.prepareSurvivalData` |
| Show Survival Curve              | `showSurvival`                 | Show Survival Curve                    | `survivalPlot`                      | `.run`, `.plotSurvival`              |
| **Summary Statistics**           |                                |                                        |                                     |                                      |
| Summary Statistics Table         | `summaryTable`                 | Summary Statistics                     | `summaryTable`                      | `.run`, `.calculateStats`            |
| Survival Statistics Table        | `survivalStats`                | Survival Statistics                    | `survivalStats`                     | `.run`, `.calculateSurvivalStats`    |

## R Function Details

### Core Functions

#### `.run()`

*   **Purpose:** The main function that orchestrates the entire analysis. It calls the other functions in the correct order to validate the data, prepare it for plotting, generate the plots, and calculate the summary statistics.

#### `.validateData()`

*   **Purpose:** To ensure that the data is in the correct format for the analysis. It checks for missing values and ensures that the time, stage, treatment, and patient ID variables are of the correct type.

#### `.prepareAlluvialData()`

*   **Purpose:** To transform the raw longitudinal data into the format required for creating an alluvial plot. This involves creating a data frame where each row represents a patient at a specific time point, with their corresponding stage and treatment.

#### `.prepareSurvivalData()`

*   **Purpose:** To prepare the data for the optional survival analysis. This involves creating a survival object from the time and event variables.

### Plotting Functions

#### `.plot()`

*   **Purpose:** To generate the main alluvial plot using the `ggplot2` and `ggalluvial` packages.

#### `.plotSurvival()`

*   **Purpose:** To generate the Kaplan-Meier survival plot if the user has requested it.

### Helper Functions

#### `.calculateStats()`

*   **Purpose:** To calculate the summary statistics for the alluvial plot, such as the number of patients in each stage and treatment at each time point.

#### `.calculateSurvivalStats()`

*   **Purpose:** To calculate the summary statistics for the survival analysis, such as the median survival time.

#### `.getColorScheme()`

*   **Purpose:** To retrieve the color scheme selected by the user for the alluvial plot.