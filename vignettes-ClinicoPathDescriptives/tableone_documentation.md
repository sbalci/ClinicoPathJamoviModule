# Table One Analysis Documentation

This document provides a comprehensive overview of the Table One Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Table One Analysis module generates a descriptive summary table, commonly known as a "Table One" in clinicopathological research. It offers flexible output styles by integrating with various R packages such as `tableone`, `gtsummary`, `arsenal`, and `janitor`, allowing users to choose the most suitable format for their needs.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Functionality**           |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `tablestyle1`, `tablestyle2`, `tablestyle3`, `tablestyle4` | `.run`                               |
| Table Style                      | `sty`                          | Table Style                            | `tablestyle1`, `tablestyle2`, `tablestyle3`, `tablestyle4` | `.run`                               |
| Exclude Missing (NA)             | `excl`                         | Exclude Missing (NA)                   | `tablestyle1`, `tablestyle2`, `tablestyle3`, `tablestyle4` | `.run`                               |
