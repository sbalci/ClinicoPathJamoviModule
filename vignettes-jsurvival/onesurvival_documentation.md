# Kaplan-Meier Survival Analysis for Single Group Documentation

This document provides a comprehensive overview of the Kaplan-Meier Survival Analysis for Single Group module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs Kaplan-Meier survival analysis for a single group of subjects. It estimates survival probabilities over time and provides summary statistics, including median survival time with confidence intervals. The module generates both tabular results and Kaplan-Meier survival plots, offering a straightforward approach to visualizing and summarizing survival data for a single cohort.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Time                             | `times`                        | Time                                   | `onesurvTable1`, `onesurvPlot1`     | `.run`, `.plot`                      |
| Event indicator                  | `status`                       | Event indicator                        | `onesurvTable1`, `onesurvPlot1`     | `.run`, `.plot`                      |
| Add confidence intervals?        | `ciyn`                         | Add confidence intervals?              | `onesurvPlot1`                      | `.plot`                              |
| Time units                       | `timeunits`                    | Time units                             | `onesurvPlot1`                      | `.plot`                              |
