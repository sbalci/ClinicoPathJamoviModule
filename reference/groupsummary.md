# Group and Summarize

Group data by categorical variables and calculate summary statistics

## Usage

``` r
groupsummary(
  data,
  groupVars,
  sumVars,
  statistics = list("sum", "mean", "n"),
  dateVar = NULL,
  dateFormat = "ymd",
  timeAggregation = "day",
  showMissing = FALSE,
  addPercentage = TRUE,
  sortBy = "groups"
)
```

## Arguments

- data:

  The data as a data frame.

- groupVars:

  Variables to group by (categorical or date).

- sumVars:

  Numeric variables to calculate statistics for each group.

- statistics:

  .

- dateVar:

  Select the date variable from group variables to apply date
  formatting.

- dateFormat:

  .

- timeAggregation:

  .

- showMissing:

  Include groups with missing values in the summary.

- addPercentage:

  Add percentage of total for sum values.

- sortBy:

  .

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$todo`         |     |     |     |     | a html   |
| `results$dateInfo`     |     |     |     |     | a html   |
| `results$summaryTable` |     |     |     |     | a table  |
| `results$plot`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`
