# Enhanced Cross Tables (Pivot)

Enhanced cross tables using pivottabler for flexible clinical data
analysis. Provides pivot-style layouts with advanced statistics and
professional formatting. Non-breaking addition to existing crosstable
functionality.

## Usage

``` r
crosstablepivot(
  data,
  vars,
  group,
  statistics = TRUE,
  show_totals = TRUE,
  format_style = "clinical",
  export_excel = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  The variables to be placed in table rows.

- group:

  The variable used for grouping (table columns).

- statistics:

  Include count and percentage statistics.

- show_totals:

  Include row and column totals.

- format_style:

  Table formatting style.

- export_excel:

  Enable Excel export functionality.

## Value

A results object containing:

|                         |     |     |     |     |         |
|-------------------------|-----|-----|-----|-----|---------|
| `results$instructions`  |     |     |     |     | a html  |
| `results$pivot_table`   |     |     |     |     | a table |
| `results$summary_stats` |     |     |     |     | a table |
| `results$export_info`   |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$pivot_table$asDF`

`as.data.frame(results$pivot_table)`
