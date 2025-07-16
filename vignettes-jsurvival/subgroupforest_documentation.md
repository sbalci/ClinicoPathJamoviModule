# Subgroup Analysis Forest Plot Documentation

This document provides a comprehensive overview of the Subgroup Analysis Forest Plot module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Subgroup Analysis Forest Plot module creates forest plots to visualize treatment effects across different patient subgroups. It performs subgroup analysis for clinical trials and observational studies, calculating treatment effects within patient subgroups and testing for interactions. The module supports survival (time-to-event), binary, and continuous outcomes, providing comprehensive statistical validation and heterogeneity testing.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis Setup**          |                                |                                        |                                     |                                      |
| Outcome Variable                 | `outcome`                      | Outcome Variable                       | `plot`, `summary`, `interactions`, `overall` | `.validateInputs`, `.calculateSubgroupEffects`, `.testInteractions`, `.run` |
| Treatment Variable               | `treatment`                    | Treatment Variable                     | `plot`, `summary`, `interactions`, `overall` | `.validateInputs`, `.calculateSubgroupEffects`, `.testInteractions`, `.run` |
| Subgroup Variables               | `subgroups`                    | Subgroup Variables                     | `plot`, `summary`, `interactions`   | `.validateInputs`, `.calculateSubgroupEffects`, `.testInteractions`, `.run` |
| Time Variable (for survival)     | `time`                         | Time Variable (for survival)           | `plot`, `summary`, `interactions`, `overall` | `.validateInputs`, `.calculateSubgroupEffects`, `.testInteractions`, `.run` |
| Event Variable (for survival)    | `event`                        | Event Variable (for survival)          | `plot`, `summary`, `interactions`, `overall` | `.validateInputs`, `.calculateSubgroupEffects`, `.testInteractions`, `.run` |
| Outcome Type                     | `outcomeType`                  | Outcome Type                           | `plot`, `summary`, `interactions`, `overall` | `.validateInputs`, `.calculateSubgroupEffects`, `.testInteractions`, `.run` |
| Effect Measure                   | `effectMeasure`                | Effect Measure                         | `plot`, `summary`, `overall`        | `.validateInputs`, `.calculateSubgroupEffects`, `.run` |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `plot`, `summary`, `overall`        | `.calculateSubgroupEffects`, `.run` |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Show Overall Effect              | `showOverall`                  | Show Overall Effect                    | `overall`                           | `.run`                               |
| Test Subgroup Interactions       | `showInteraction`              | Test Subgroup Interactions             | `interactions`                      | `.run`                               |
| Sort Subgroups By                | `sortBy`                       | Sort Subgroups By                      | `summary`                           | `.run`                               |
| Show Sample Sizes                | `showSampleSizes`              | Show Sample Sizes                      | `plot`                              | `.plot`                              |
| Use Log Scale                    | `logScale`                     | Use Log Scale                          | `plot`                              | `.plot`                              |
| Null Effect Line                 | `nullLine`                     | Null Effect Line                       | `plot`                              | `.plot`                              |
