# Analysis Without Gold Standard Documentation

This document provides a comprehensive overview of the Analysis Without Gold Standard module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs analysis of diagnostic tests without a gold standard reference. It allows users to evaluate test performance (sensitivity, specificity, predictive values) and disease prevalence using various methods, including Latent Class Analysis (LCA), composite reference, and Bayesian approaches. The module supports up to five diagnostic tests and provides bootstrap confidence intervals for robust estimation. It also includes visualizations of test agreement.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Test Input**                   |                                |                                        |                                     |                                      |
| Test 1                           | `test1`                        | Test 1                                 | `test_metrics`, `agreement_plot`, `agreement_plot2` | `.run`, `.getTestVariables`          |
| Test 1 Positive Level            | `test1Positive`                | Positive Level                         | `test_metrics`                      | `.run`                               |
| Test 2                           | `test2`                        | Test 2                                 | `test_metrics`, `agreement_plot`, `agreement_plot2` | `.run`, `.getTestVariables`          |
| Test 2 Positive Level            | `test2Positive`                | Positive Level                         | `test_metrics`                      | `.run`                               |
| Test 3                           | `test3`                        | Test 3                                 | `test_metrics`, `agreement_plot`, `agreement_plot2` | `.run`, `.getTestVariables`          |
| Test 3 Positive Level            | `test3Positive`                | Positive Level                         | `test_metrics`                      | `.run`                               |
| Test 4                           | `test4`                        | Test 4                                 | `test_metrics`, `agreement_plot`, `agreement_plot2` | `.run`, `.getTestVariables`          |
| Test 4 Positive Level            | `test4Positive`                | Positive Level                         | `test_metrics`                      | `.run`                               |
| Test 5                           | `test5`                        | Test 5                                 | `test_metrics`, `agreement_plot`, `agreement_plot2` | `.run`, `.getTestVariables`          |
| Test 5 Positive Level            | `test5Positive`                | Positive Level                         | `test_metrics`                      | `.run`                               |
| **Analysis Method**              |                                |                                        |                                     |                                      |
| Analysis Method                  | `method`                       | Analysis Method                        | `prevalence`, `test_metrics`, `model_fit` | `.run`, `.runLCA`, `.runComposite`, `.runBayesian`, `.runAllPositive`, `.runAnyPositive` |
| Bootstrap CI                     | `bootstrap`                    | Bootstrap CI                           | `prevalence`, `test_metrics`        | `.run`, `.calculateBootstrapCI`      |
| Number of Bootstrap Samples      | `nboot`                        | Number of Bootstrap Samples            | `prevalence`, `test_metrics`        | `.run`, `.calculateBootstrapCI`      |
| Alpha Level                      | `alpha`                        | Alpha Level                            | `prevalence`, `test_metrics`        | `.run`, `.calculateBootstrapCI`      |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Disease Prevalence Table         | `prevalence`                   | Disease Prevalence                     | `prevalence`                        | `.populatePrevalence`                |
| Test Performance Metrics Table   | `test_metrics`                 | Test Performance Metrics               | `test_metrics`                      | `.populateTestMetrics`               |
| Model Fit Statistics Table       | `model_fit`                    | Model Fit Statistics                   | `model_fit`                         | `.populateModelFit`                  |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Test Agreement Plot (Base R)     | `agreement_plot`               | Test Agreement Plot                    | `agreement_plot`                    | `.plot`                              |
| Test Agreement Plot (ggplot2)    | `agreement_plot2`              | Test Agreement Plot 2                  | `agreement_plot2`                   | `.plot_ggplot`                       |
