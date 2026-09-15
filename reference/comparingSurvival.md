# Comparing Survival Outcomes

Comparing Survival Outcomes

## Usage

``` r
comparingSurvival(
  data,
  times = NULL,
  status = NULL,
  groups = NULL,
  ciyn = FALSE,
  loglogyn = FALSE,
  timeunits = "None",
  pairwise = FALSE,
  pairwiseCorrection = "none",
  trendTest = FALSE,
  landmarkTime = 0,
  landmarkUnit = "same"
)
```

## Arguments

- data:

  .

- times:

  .

- status:

  .

- groups:

  .

- ciyn:

  .

- loglogyn:

  .

- timeunits:

  .

- pairwise:

  Perform pairwise log-rank comparisons between all groups

- pairwiseCorrection:

  Method for adjusting p-values for multiple comparisons

- trendTest:

  Test for trend across ordered groups (assumes groups are ordered)

- landmarkTime:

  Exclude events before this time (0 = no exclusion)

- landmarkUnit:

  Unit for landmark time

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$text`           |     |     |     |     | a preformatted |
| `results$compsurvTable1` |     |     |     |     | a table        |
| `results$compsurvTable2` |     |     |     |     | a table        |
| `results$compsurvTable3` |     |     |     |     | a table        |
| `results$plot`           |     |     |     |     | an image       |
| `results$plot2`          |     |     |     |     | an image       |
| `results$pairwiseTable`  |     |     |     |     | a table        |
| `results$trendTestTable` |     |     |     |     | a table        |
| `results$landmarkNote`   |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$compsurvTable1$asDF`

`as.data.frame(results$compsurvTable1)`
