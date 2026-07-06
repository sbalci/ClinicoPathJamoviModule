# Modern Table Formatting

Creates modern, publication-ready tables using the tinytable package.
This module provides lightweight, zero-dependency table formatting with
multiple output formats including HTML, LaTeX, PDF, and Word. Designed
to complement existing ClinicoPath table modules with modern styling,
flexible formatting, and clean aesthetics. Perfect for enhancing data
presentation in clinical research publications and reports.

## Usage

``` r
tinytable(
  data,
  vars,
  group_var,
  table_type = "summary",
  show_statistics = TRUE,
  show_counts = TRUE,
  show_missing = FALSE,
  table_theme = "clinical",
  table_title = "Data Summary Table",
  table_notes = "",
  output_format = "html",
  column_width = 0.8,
  precision_digits = 2,
  style_alternating = TRUE,
  style_borders = "all",
  font_size = "normal",
  show_interpretation = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables to include in the table display.

- group_var:

  Optional variable for grouping rows or creating summary tables.

- table_type:

  Type of table to generate with appropriate formatting.

- show_statistics:

  If TRUE, includes statistical summaries (mean, SD, etc.) for numeric
  variables.

- show_counts:

  If TRUE, shows counts and percentages for categorical variables.

- show_missing:

  If TRUE, includes information about missing values.

- table_theme:

  Visual theme for table styling and appearance.

- table_title:

  Title for the table.

- table_notes:

  Optional notes to display below the table.

- output_format:

  Primary output format for the table.

- column_width:

  Width of the table as proportion of available space.

- precision_digits:

  Number of decimal places for numeric values.

- style_alternating:

  If TRUE, applies alternating row background colors.

- style_borders:

  Border style for the table.

- font_size:

  Font size for table text.

- show_interpretation:

  If TRUE, displays interpretation and usage guidelines.

## Value

A results object containing:

|                          |     |     |     |     |        |
|--------------------------|-----|-----|-----|-----|--------|
| `results$todo`           |     |     |     |     | a html |
| `results$table`          |     |     |     |     | a html |
| `results$interpretation` |     |     |     |     | a html |
