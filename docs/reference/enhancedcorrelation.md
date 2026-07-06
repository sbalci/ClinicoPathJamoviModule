# Enhanced Correlation Analysis

Enhanced Correlation Analysis

## Usage

``` r
enhancedcorrelation(
  data,
  vars,
  method = "spearman",
  ci = TRUE,
  ciWidth = 95,
  sig = TRUE,
  sigLevel = 0.05,
  plotMatrix = FALSE,
  plotScatter = FALSE
)
```

## Arguments

- data:

  .

- vars:

  .

- method:

  .

- ci:

  .

- ciWidth:

  .

- sig:

  .

- sigLevel:

  .

- plotMatrix:

  .

- plotScatter:

  .

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$instructions` |     |     |     |     | a html   |
| `results$text`         |     |     |     |     | a table  |
| `results$plot`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$text$asDF`

`as.data.frame(results$text)`
