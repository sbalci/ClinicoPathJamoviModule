# Benford Analysis

Benford Analysis

## Usage

``` r
benford(data, var = NULL, digits = 2)
```

## Arguments

- data:

  The data as a data frame.

- var:

  a string naming the variable from `data` that contains the continuous
  values used for the report

- digits:

  Number of first digits to analyze (default: 2). Limited to 1-3 digits;
  the benford.analysis package does not provide a MAD conformity
  classification beyond 3 digits.

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$welcome`        |     |     |     |     | a html         |
| `results$explanation`    |     |     |     |     | a html         |
| `results$dataWarning`    |     |     |     |     | a html         |
| `results$summary`        |     |     |     |     | a table        |
| `results$todo`           |     |     |     |     | a html         |
| `results$text`           |     |     |     |     | a preformatted |
| `results$text2`          |     |     |     |     | a preformatted |
| `results$reportSentence` |     |     |     |     | a html         |
| `results$plot`           |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
