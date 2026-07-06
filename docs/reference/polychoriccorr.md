# Polychoric Correlation Analysis

Polychoric Correlation Analysis

## Usage

``` r
polychoriccorr(
  data,
  vars,
  corrType = "polychoric",
  method = "ml",
  ci = TRUE,
  ciWidth = 95,
  sig = TRUE,
  sigLevel = 0.05,
  matrixPlot = FALSE,
  showFreq = TRUE
)
```

## Arguments

- data:

  .

- vars:

  .

- corrType:

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

- matrixPlot:

  .

- showFreq:

  .

## Value

A results object containing:

|                        |     |     |     |     |                    |
|------------------------|-----|-----|-----|-----|--------------------|
| `results$instructions` |     |     |     |     | a html             |
| `results$correlations` |     |     |     |     | a table            |
| `results$frequencies`  |     |     |     |     | an array of tables |
| `results$plot`         |     |     |     |     | an image           |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$correlations$asDF`

`as.data.frame(results$correlations)`
