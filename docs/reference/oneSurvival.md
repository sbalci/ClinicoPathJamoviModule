# One Survival Outcome

One Survival Outcome

## Usage

``` r
oneSurvival(data, times, status, ciyn = FALSE, timeunits = "None")
```

## Arguments

- data:

  .

- times:

  .

- status:

  .

- ciyn:

  .

- timeunits:

  .

## Value

A results object containing:

|                         |     |     |     |     |                |
|-------------------------|-----|-----|-----|-----|----------------|
| `results$text`          |     |     |     |     | a preformatted |
| `results$onesurvTable1` |     |     |     |     | a table        |
| `results$onesurvPlot1`  |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$onesurvTable1$asDF`

`as.data.frame(results$onesurvTable1)`
