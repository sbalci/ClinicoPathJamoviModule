# Exact Tests for Small Samples

Exact Tests for Small Samples

## Usage

``` r
exacttests(
  data,
  rows,
  cols,
  counts,
  testType = "fisher",
  alternative = "two.sided",
  ciMethod = "clopper_pearson",
  ciWidth = 95,
  showCI = TRUE,
  correction = FALSE
)
```

## Arguments

- data:

  .

- rows:

  .

- cols:

  .

- counts:

  .

- testType:

  .

- alternative:

  .

- ciMethod:

  .

- ciWidth:

  .

- showCI:

  .

- correction:

  .

## Value

A results object containing:

|                        |     |     |     |     |         |
|------------------------|-----|-----|-----|-----|---------|
| `results$instructions` |     |     |     |     | a html  |
| `results$contingency`  |     |     |     |     | a table |
| `results$tests`        |     |     |     |     | a table |
| `results$note`         |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$contingency$asDF`

`as.data.frame(results$contingency)`
