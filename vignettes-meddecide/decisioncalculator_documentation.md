# Medical Decision Calculator Analysis Documentation

This document provides a comprehensive overview of the Medical Decision Calculator module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Medical Decision Calculator module is a powerful tool for evaluating diagnostic test performance based on True Positives (TP), False Positives (FP), True Negatives (TN), and False Negatives (FN). It calculates a wide array of diagnostic metrics, including sensitivity, specificity, predictive values, likelihood ratios, and post-test probabilities. The module also supports confidence interval estimation and visualization through a Fagan nomogram.

The module's features can be broadly categorized as follows:

*   **Input of Test Counts:** Direct input of TP, TN, FP, and FN values.
*   **Comprehensive Metric Calculation:** Derivation of sensitivity, specificity, accuracy, prevalence, positive predictive value (PPV), negative predictive value (NPV), post-test probabilities, and likelihood ratios.
*   **Confidence Interval Estimation:** Calculation and display of 95% confidence intervals for various metrics.
*   **Fagan Nomogram Visualization:** Interactive Fagan nomogram to visualize the relationship between pre-test probability, likelihood ratios, and post-test probability.
*   **Customizable Prior Probability:** Option to use a specified prior probability (prevalence) for calculations.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Counts**                 |                                |                                        |                                     |                                      |
| True Positive (TP)               | `TP`                           | True Positive =                        | `cTable`, `nTable`, `ratioTable`    | `.run`                               |
| True Negative (TN)               | `TN`                           | True Negative =                        | `cTable`, `nTable`, `ratioTable`    | `.run`                               |
| False Positive (FP)              | `FP`                           | False Positive =                       | `cTable`, `nTable`, `ratioTable`    | `.run`                               |
| False Negative (FN)              | `FN`                           | False Negative =                       | `cTable`, `nTable`, `ratioTable`    | `.run`                               |
| **Options**                      |                                |                                        |                                     |                                      |
| Prior Probability (prevalence)   | `pp`, `pprob`                  | Prior Probability (prevalence)         | `ratioTable`, `plot1`               | `.run`                               |
| Show Footnote                    | `fnote`                        | Show Footnote                          | `nTable`, `ratioTable`              | `.run`                               |
| Show 95% CI                      | `ci`                           | Show 95% CI                            | `epirTable_ratio`, `epirTable_number` | `.run`                               |
| Fagan Nomogram                   | `fagan`                        | Fagan Nomogram                         | `plot1`                             | `.plot1`                             |
| **Results Tables**               |                                |                                        |                                     |                                      |
| Contingency Table                | (N/A)                          | (N/A)                                  | `cTable`                            | `.run`                               |
| N Counts Table                   | (N/A)                          | (N/A)                                  | `nTable`                            | `.run`                               |
| Ratio Table                      | (N/A)                          | (N/A)                                  | `ratioTable`                        | `.run`                               |
| CI Table (Ratios)                | (N/A)                          | (N/A)                                  | `epirTable_ratio`                   | `.run`                               |
| CI Table (Numbers)               | (N/A)                          | (N/A)                                  | `epirTable_number`                  | `.run`                               |
