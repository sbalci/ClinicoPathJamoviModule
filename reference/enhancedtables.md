# Enhanced Tables with gt

Creates professional, publication-ready tables using the gt package.
This module provides comprehensive table formatting with advanced
styling, grouping, summary statistics, and clinical research
optimizations. Designed for high-quality table presentation in medical
publications, reports, and presentations. Features include conditional
formatting, custom themes, statistical summaries, and multi-level
grouping.

## Usage

``` r
enhancedtables(
  data,
  vars,
  group_var,
  strata_var,
  table_type = "summary",
  stats_continuous = "mean_sd",
  stats_categorical = "n_percent",
  include_total = TRUE,
  include_pvalues = FALSE,
  test_type = "auto",
  missing_handling = "show",
  table_theme = "clinical",
  table_title = "Enhanced Data Summary",
  table_subtitle = "",
  source_note = "",
  show_footnotes = TRUE,
  decimal_places = 2,
  font_size = "normal",
  table_width = "auto",
  stripe_rows = TRUE,
  group_colors = TRUE,
  highlight_significant = TRUE,
  confidence_level = 0.95,
  show_interpretation = TRUE,
  export_format = "html"
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables to include in the enhanced table display.

- group_var:

  Variable for grouping rows and creating summary comparisons.

- strata_var:

  Additional variable for creating stratified analysis tables.

- table_type:

  Type of enhanced table to generate with appropriate clinical
  formatting.

- stats_continuous:

  Statistical summaries to display for continuous variables.

- stats_categorical:

  Statistical summaries to display for categorical variables.

- include_total:

  If TRUE, includes a total column with overall statistics.

- include_pvalues:

  If TRUE, includes statistical tests and p-values for group
  comparisons.

- test_type:

  Type of statistical tests to use for group comparisons.

- missing_handling:

  How to handle missing values in the analysis.

- table_theme:

  Visual theme for professional table appearance.

- table_title:

  Title for the enhanced table.

- table_subtitle:

  Optional subtitle for additional context.

- source_note:

  Optional source note to display below the table.

- show_footnotes:

  If TRUE, includes footnotes explaining statistical methods and
  symbols.

- decimal_places:

  Number of decimal places for numeric displays.

- font_size:

  Font size for table text.

- table_width:

  Width setting for the table layout.

- stripe_rows:

  If TRUE, applies alternating row background colors for readability.

- group_colors:

  If TRUE, applies color coding to group headers.

- highlight_significant:

  If TRUE, highlights statistically significant p-values.

- confidence_level:

  Confidence level for statistical tests and intervals.

- show_interpretation:

  If TRUE, displays clinical interpretation and usage guidelines.

- export_format:

  Additional export format for the enhanced table.

## Value

A results object containing:

|                            |     |     |     |     |        |
|----------------------------|-----|-----|-----|-----|--------|
| `results$instructions`     |     |     |     |     | a html |
| `results$enhanced_table`   |     |     |     |     | a html |
| `results$summary_stats`    |     |     |     |     | a html |
| `results$group_comparison` |     |     |     |     | a html |
| `results$interpretation`   |     |     |     |     | a html |
| `results$export_table`     |     |     |     |     | a html |
