# Sequential Testing Analysis Documentation

This document provides a comprehensive overview of the Sequential Testing Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Sequential Testing Analysis module evaluates how diagnostic accuracy changes when applying two tests in sequence or in parallel. It compares three different testing strategies: serial positive (confirmation), serial negative (exclusion), and parallel testing. The module provides a comprehensive analysis including population flow, combined test performance metrics (sensitivity, specificity, PPV, NPV), and detailed explanations of the underlying mathematical formulas. It also includes a Fagan nomogram for visualizing changes in post-test probability.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Test Parameters**              |                                |                                        |                                     |                                      |
| Screening Test Name              | `test1_name`                   | Screening Test Name                    | `summary_table`, `individual_tests_table` | `.run`                               |
| Test 1 Sensitivity               | `test1_sens`                   | Sensitivity                            | `summary_table`, `individual_tests_table` | `.run`                               |
| Test 1 Specificity               | `test1_spec`                   | Specificity                            | `summary_table`, `individual_tests_table` | `.run`                               |
| Confirmatory Test Name           | `test2_name`                   | Confirmatory Test Name                 | `summary_table`, `individual_tests_table` | `.run`                               |
| Test 2 Sensitivity               | `test2_sens`                   | Sensitivity                            | `summary_table`, `individual_tests_table` | `.run`                               |
| Test 2 Specificity               | `test2_spec`                   | Specificity                            | `summary_table`, `individual_tests_table` | `.run`                               |
| **Strategy & Prevalence**        |                                |                                        |                                     |                                      |
| Testing Strategy                 | `strategy`                     | Testing Strategy                       | `summary_table`, `population_flow_table`, `explanation_text` | `.run`                               |
| Disease Prevalence               | `prevalence`                   | Disease Prevalence                     | `summary_table`, `population_flow_table`, `explanation_text` | `.run`                               |
| **Display Options**              |                                |                                        |                                     |                                      |
| Show Explanations                | `show_explanation`             | Show Explanations                      | `explanation_text`                  | `.run`                               |
| Show Calculation Formulas        | `show_formulas`                | Show Calculation Formulas              | `formulas_text`                     | `.run`                               |
| Show Fagan Nomogram              | `show_nomogram`                | Show Fagan Nomogram                    | `plot_nomogram`                     | `.run`, `.plot_nomogram`             |
