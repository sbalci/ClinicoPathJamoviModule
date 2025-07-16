# Power Analysis for Inter-rater Agreement Studies Documentation

This document provides a comprehensive overview of the Power Analysis for Inter-rater Agreement Studies module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs power analysis to determine the required sample size for detecting a specified improvement in inter-rater agreement (kappa coefficient). It helps researchers design adequately powered studies to validate training programs, standardized protocols, or other interventions aimed at improving agreement between raters. The function supports 2-5 outcome categories and 2-5 raters, using the `kappaSize` package implementation of power calculations for different scenarios.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Parameters**             |                                |                                        |                                     |                                      |
| Number of outcome level          | `outcome`                      | Number of outcome level                | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| kappa0                           | `kappa0`                       | kappa0                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| kappa1                           | `kappa1`                       | kappa1                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| Proportions of outcome level     | `props`                        | Proportions of outcome level           | `text1`, `text2`                    | `.run`, `.parseProportions`, `.calculateSampleSize`, `.generateExplanation` |
| raters                           | `raters`                       | raters                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| alpha                            | `alpha`                        | alpha                                  | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| power                            | `power`                        | power                                  | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
