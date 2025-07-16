# Summary of Categorical Variables Analysis Documentation

This document provides a comprehensive overview of the Summary of Categorical Variables Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module generates a detailed summary of categorical variables, including counts, percentages, and missing value information. The output is presented in both textual and visual formats, making it easy to interpret the distribution of your data. It is designed to provide a quick and comprehensive overview of categorical data for clinical and research purposes.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Summary**                 |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `text`, `text1`                     | `.run`, `.catsummary`                |
