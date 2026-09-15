# Paired Samples Contingency Tables

McNemar test

## Usage

``` r
contTablesPaired(
  data,
  rows,
  cols,
  counts = NULL,
  chiSq = TRUE,
  chiSqCorr = FALSE,
  exact = FALSE,
  pcRow = FALSE,
  pcCol = FALSE,
  formula
)
```

## Arguments

- data:

  the data as a data frame

- rows:

  the variable to use as the rows in the contingency table (not
  necessary when providing a formula, see the examples)

- cols:

  the variable to use as the columns in the contingency table (not
  necessary when providing a formula, see the examples)

- counts:

  the variable to use as the counts in the contingency table (not
  necessary when providing a formula, see the examples)

- chiSq:

  `TRUE` (default) or `FALSE`, provide X²

- chiSqCorr:

  `TRUE` or `FALSE` (default), provide X² with continuity correction

- exact:

  `TRUE` or `FALSE` (default), provide an exact log odds ratio (requires
  exact2x2 to be installed)

- pcRow:

  `TRUE` or `FALSE` (default), provide row percentages

- pcCol:

  `TRUE` or `FALSE` (default), provide column percentages

- formula:

  (optional) the formula to use, see the examples

## Value

A results object containing:

|                 |     |     |     |     |                         |
|-----------------|-----|-----|-----|-----|-------------------------|
| `results$freqs` |     |     |     |     | a proportions table     |
| `results$test`  |     |     |     |     | a table of test results |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$freqs$asDF`

`as.data.frame(results$freqs)`
