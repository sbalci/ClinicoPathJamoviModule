# Odds Ratio Analysis for Binary Outcomes Documentation

This document provides a comprehensive overview of the Odds Ratio Analysis for Binary Outcomes module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs logistic regression analysis to calculate odds ratios for binary outcomes. It provides comprehensive odds ratio tables, forest plots, and optional nomogram generation for clinical prediction. The module supports both categorical and continuous explanatory variables and includes diagnostic metrics for binary predictors. It is designed for clinical research to assess the association between various factors and a binary outcome.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Explanatory Variables            | `explanatory`                  | Explanatory Variables                  | `text`, `text2`, `plot`, `nomogram`, `plot_nomogram` | `.run`, `.plot`                      |
| Outcome Variable                 | `outcome`                      | Mortality or Recurrence                | `text`, `text2`, `plot`, `nomogram`, `plot_nomogram` | `.run`, `.plot`                      |
| Positive Outcome Level           | `outcomeLevel`                 | Positive Outcome Level                 | `text2`                             | `.run`, `.calculateLikelihoodRatios` |
| **Nomogram Options**             |                                |                                        |                                     |                                      |
| Show Diagnostic Nomogram         | `showNomogram`                 | Show Diagnostic Nomogram               | `nomogram`, `plot_nomogram`         | `.run`, `.prepareRmsNomogram`, `.createNomogram`, `.plot_nomogram` |
