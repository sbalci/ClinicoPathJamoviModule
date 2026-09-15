# Recoding

Recoding

## Usage

``` r
jrecode(
  dep,
  recode_rules = "",
  else_level = "copy",
  new_var_name = "",
  show_code = TRUE,
  show_table = TRUE,
  show_levels = TRUE,
  recoded_output = FALSE
)
```

## Arguments

- dep:

  .

- recode_rules:

  One rule per line. Format: oldvalue -\> newvalue

- else_level:

  .

- new_var_name:

  Name for the recoded variable

- show_code:

  .

- show_table:

  .

- show_levels:

  .

- recoded_output:

  .

## Value

A results object containing:

|                        |     |     |     |     |                |
|------------------------|-----|-----|-----|-----|----------------|
| `results$instructions` |     |     |     |     | a html         |
| `results$levels_table` |     |     |     |     | a table        |
| `results$notices`      |     |     |     |     | a html         |
| `results$code_output`  |     |     |     |     | a preformatted |
| `results$comparison`   |     |     |     |     | a table        |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$levels_table$asDF`

`as.data.frame(results$levels_table)`
