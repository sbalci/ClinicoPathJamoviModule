# Table One Draft 2 Analysis Documentation

This document provides a comprehensive overview of the Table One Draft 2 Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Table One Draft 2 Analysis module generates a descriptive summary table, commonly known as a "Table One" in clinicopathological research. This enhanced version offers flexible output styles by integrating with various R packages such as `tableone`, `gtsummary`, `arsenal`, `janitor`, and introduces an advanced `pivottabler` option for highly customizable and detailed tables. It supports comprehensive data summarization and group comparisons.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Functionality**           |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `tablestyle1`, `tablestyle2`, `tablestyle3`, `tablestyle4`, `tablestyle5` | `.run`                               |
| Table Style                      | `sty`                          | Table Style                            | `tablestyle1`, `tablestyle2`, `tablestyle3`, `tablestyle4`, `tablestyle5` | `.run`                               |
| Exclude Missing (NA)             | `excl`                         | Exclude Missing (NA)                   | `tablestyle1`, `tablestyle2`, `tablestyle3`, `tablestyle4`, `tablestyle5` | `.run`                               |
| **Pivottabler Options**          |                                |                                        |                                     |                                      |
| Pivot Format Style               | `pivot_format`                 | Pivot Format Style                     | `tablestyle5`                       | `.create_pivot_tableone`             |
| Include Advanced Statistics      | `include_statistics`           | Include Advanced Statistics            | `tablestyle5`                       | `.create_pivot_tableone`             |
| Grouping Variable                | `group_var`                    | Grouping Variable                      | `tablestyle5`                       | `.create_pivot_tableone`             |
| Group Comparisons                | `group_comparisons`            | Group Comparisons                      | `tablestyle5`                       | `.create_pivot_tableone`             |
