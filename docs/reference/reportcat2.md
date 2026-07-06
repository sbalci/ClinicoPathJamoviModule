# Enhanced Frequencies

Generates a comprehensive summary of categorical variables including
frequency counts, percentages, missing value information, and optional
visual summaries. Supports multiple output formats and sorting options
for enhanced data exploration. Automatically handles edge cases like
missing values, single categories, and variable conversion.

## Usage

``` r
reportcat2(
  data,
  vars,
  sumvar_style = FALSE,
  show_proportions = TRUE,
  sort_by_frequency = FALSE
)
```

## Arguments

- data:

  The data as a data frame containing the categorical variables to be
  analyzed. The data frame should have at least one row and the
  specified variables should exist in the dataset.

- vars:

  A character vector specifying the variable names to summarize. These
  variables will be automatically converted to factors if they aren't
  already. Character, numeric, and logical variables are all acceptable
  and will be treated as categorical.

- sumvar_style:

  Logical (default: FALSE). If TRUE, provides comprehensive categorical
  analysis similar to the sumvar package's tab1() function, including
  enhanced frequency tables, percentage breakdowns, and cumulative
  statistics for each category.

- show_proportions:

  Logical (default: TRUE). If TRUE, displays cumulative counts and
  percentages alongside individual category statistics when using
  sumvar_style format. Ignored when sumvar_style is FALSE.

- sort_by_frequency:

  Logical (default: FALSE). If TRUE, categories within each variable are
  sorted by frequency (most common first) rather than alphabetically.
  This applies to both output formats.

## Value

A results object containing:

|                 |     |     |     |     |        |
|-----------------|-----|-----|-----|-----|--------|
| `results$todo`  |     |     |     |     | a html |
| `results$text`  |     |     |     |     | a html |
| `results$text1` |     |     |     |     | a html |
