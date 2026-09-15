# Multi-Variable Visual Quality

This module provides data quality assessment including duplicate
detection, missing value analysis, and data completeness summary
(similar to sumvar's dup() function).

## Usage

``` r
dataquality(
  data,
  vars = NULL,
  check_duplicates = FALSE,
  check_missing = FALSE,
  complete_cases_only = FALSE,
  plot_data_overview = FALSE,
  plot_missing_patterns = FALSE,
  plot_data_types = FALSE,
  missing_threshold_visual = 10,
  showSummary = TRUE,
  showRecommendations = TRUE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables to assess for data quality. At least one variable must be
  selected; the analysis does not fall back to the whole dataset.

- check_duplicates:

  If TRUE, analyzes duplicate values within each variable or across the
  entire dataset.

- check_missing:

  If TRUE, provides detailed missing value statistics and patterns.

- complete_cases_only:

  If TRUE, checks for duplicate rows across all selected variables. If
  FALSE, checks for duplicate values within each variable separately.

- plot_data_overview:

  Show data overview visualization displaying variable types and missing
  values.

- plot_missing_patterns:

  Show missing value patterns visualization.

- plot_data_types:

  Show data type detection and validation visualization.

- missing_threshold_visual:

  Threshold percentage for highlighting variables with missing values in
  visual analysis.

- showSummary:

  If TRUE, displays a concise plain-language summary of quality issues
  and overall assessment.

- showRecommendations:

  If TRUE, provides specific recommendations for addressing identified
  quality issues.

- showExplanations:

  If TRUE, includes explanations of quality metrics and statistical
  tests used in the analysis.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$text`                |     |     |     |     | a html   |
| `results$summary`             |     |     |     |     | a html   |
| `results$recommendations`     |     |     |     |     | a html   |
| `results$explanations`        |     |     |     |     | a html   |
| `results$plotDataOverview`    |     |     |     |     | an image |
| `results$plotMissingPatterns` |     |     |     |     | an image |
| `results$plotDataTypes`       |     |     |     |     | an image |
