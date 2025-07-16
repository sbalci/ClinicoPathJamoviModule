# Screening Test Calculator Analysis Documentation

This document provides a comprehensive overview of the Screening Test Calculator module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Screening Test Calculator module calculates positive and negative predictive values for screening and diagnostic tests using Bayes' theorem. It demonstrates how disease probability changes with sequential testing and provides Fagan nomograms for clinical decision-making. This tool is particularly useful for understanding test performance in different prevalence settings, optimizing sequential testing strategies, and teaching Bayesian probability concepts in medical decision-making.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Test Parameters**              |                                |                                        |                                     |                                      |
| Sensitivity                      | `sens`                         | Sensitivity                            | `singleTestTable`, `repeatTest2Table`, `repeatTest3Table` | `.run`                               |
| Specificity                      | `spec`                         | Specificity                            | `singleTestTable`, `repeatTest2Table`, `repeatTest3Table` | `.run`                               |
| Disease Prevalence               | `prev`                         | Disease Prevalence                     | `singleTestTable`, `repeatTest2Table`, `repeatTest3Table` | `.run`                               |
| **Test Repetition**              |                                |                                        |                                     |                                      |
| Show 2x Test Repetition          | `repeat2`                      | Show 2x Test Repetition                | `repeatTest2Table`, `plot2PP`, `plot2NN` | `.run`                               |
| Show 3x Test Repetition          | `repeat3`                      | Show 3x Test Repetition                | `repeatTest3Table`, `plot3PPP`, `plot3NNN` | `.run`                               |
| **Display Options**              |                                |                                        |                                     |                                      |
| Show Footnotes                   | `fnote`                        | Show Footnotes                         | `singleTestTable`                   | `.run`                               |
| Fagan Nomogram                   | `fagan`                        | Fagan Nomogram                         | `plot1`, `plot2PP`, `plot2NN`, `plot3PPP`, `plot3NNN` | `.run`, `.plot1`, `.plot2PP`, `.plot2NN`, `.plot3PPP`, `.plot3NNN` |
