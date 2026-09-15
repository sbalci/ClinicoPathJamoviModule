# Parallel Coordinates Plot

Parallel Coordinates Plot

## Usage

``` r
parallelplot(
  data,
  vars,
  group = NULL,
  scaling = "std",
  alpha = 0.7,
  showMissing = FALSE,
  colorPalette = "default"
)
```

## Arguments

- data:

  .

- vars:

  Select multiple continuous variables for parallel coordinates

- group:

  Variable for coloring and grouping lines

- scaling:

  Method for scaling variables to comparable ranges

- alpha:

  Transparency level for parallel coordinate lines

- showMissing:

  Include cases with missing values

- colorPalette:

  Color scheme for grouping variable

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$instructions` |     |     |     |     | a html   |
| `results$plot`         |     |     |     |     | an image |
| `results$summary`      |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
