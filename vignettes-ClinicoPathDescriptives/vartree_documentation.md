# Variable Tree Analysis Documentation

This document provides a comprehensive overview of the Variable Tree Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Variable Tree Analysis module generates enhanced variable trees using the `vtree` package. It visualizes hierarchical relationships between categorical variables, displaying counts, percentages, and optional statistical summaries within nodes. The module offers various styling options, pruning controls, and an automatic interpretation feature, making it suitable for exploring complex categorical data in clinical research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Tree Generation**         |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `text1`                             | `.run`                               |
| Variable for Percentage          | `percvar`                      | Variable for Percentage                | `text1`                             | `.run`                               |
| Level for Percentage             | `percvarLevel`                 | Level                                  | `text1`                             | `.run`                               |
| Continuous Variable for Summaries| `summaryvar`                   | Continuous Variable for Summaries      | `text1`                             | `.run`                               |
| Summary Location                 | `summarylocation`              | Summary Location                       | `text1`                             | `.run`                               |
| Visual Style                     | `style`                        | Visual Style                           | `text1`                             | `.run`                               |
| Prune Below                      | `prunebelow`                   | Prune Below                            | `text1`                             | `.run`                               |
| Level 1 (Prune)                  | `pruneLevel1`                  | Level 1                                | `text1`                             | `.run`                               |
| Level 2 (Prune)                  | `pruneLevel2`                  | Level 2                                | `text1`                             | `.run`                               |
| Follow Below                     | `follow`                       | Follow Below                           | `text1`                             | `.run`                               |
| Level 1 (Follow)                 | `followLevel1`                 | Level 1                                | `text1`                             | `.run`                               |
| Level 2 (Follow)                 | `followLevel2`                 | Level 2                                | `text1`                             | `.run`                               |
| Exclude Missing (NA)             | `excl`                         | Exclude Missing (NA)                   | `text1`                             | `.run`                               |
| Valid Percentages                | `vp`                           | Valid Percentages                      | `text1`                             | `.run`                               |
| Horizontal Layout                | `horizontal`                   | Horizontal Layout                      | `text1`                             | `.run`                               |
| Sameline                         | `sline`                        | Sameline                               | `text1`                             | `.run`                               |
| Variable Names                   | `varnames`                     | Variable Names                         | `text1`                             | `.run`                               |
| Node Labels                      | `nodelabel`                    | Node Labels                            | `text1`                             | `.run`                               |
| Percentages                      | `pct`                          | Percentages                            | `text1`                             | `.run`                               |
| Counts                           | `showcount`                    | Counts                                 | `text1`                             | `.run`                               |
| Legends                          | `legend`                       | Legends                                | `text1`                             | `.run`                               |
| Pattern Tree                     | `pattern`                      | Pattern Tree                           | `text1`                             | `.run`                               |
| Sequence Tree                    | `sequence`                     | Sequence Tree                          | `text1`                             | `.run`                               |
| Pattern Table                    | `ptable`                       | Pattern Table                          | `text2`                             | `.run`                               |
| Root Title                       | `mytitle`                      | Root Title                             | `text1`                             | `.run`                               |
| Prune Small Nodes                | `useprunesmaller`              | Prune Small Nodes                      | `text1`                             | `.run`                               |
| Prune counts <                   | `prunesmaller`                 | Prune counts <                         | `text1`                             | `.run`                               |
| **Interpretation**               |                                |                                        |                                     |                                      |
| Show Interpretation              | `showInterpretation`           | Show Interpretation                    | `text1`                             | `.run`, `.generateInterpretation`    |
