# Tools for data summary

Tools for data summary

## Usage

``` r
toolssummary(
  data,
  vars,
  excludeNA = FALSE,
  showFreq = TRUE,
  showStats = TRUE,
  useSummarytools = TRUE,
  showDfSummary = TRUE,
  showDescr = TRUE,
  groupVar,
  showCrosstabs = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables to summarize

- excludeNA:

  .

- showFreq:

  .

- showStats:

  .

- useSummarytools:

  Use summarytools package for enhanced data summaries

- showDfSummary:

  Show comprehensive data frame summary using summarytools::dfSummary

- showDescr:

  Show detailed descriptive statistics using summarytools::descr

- groupVar:

  Variable to group by for stratified summaries

- showCrosstabs:

  Show cross-tabulation tables for categorical variables

## Value

A results object containing:

|                            |     |     |     |     |         |
|----------------------------|-----|-----|-----|-----|---------|
| `results$todo`             |     |     |     |     | a html  |
| `results$summary`          |     |     |     |     | a table |
| `results$dfSummary`        |     |     |     |     | a html  |
| `results$descrStats`       |     |     |     |     | a html  |
| `results$frequencies`      |     |     |     |     | a html  |
| `results$summaryToolsFreq` |     |     |     |     | a html  |
| `results$numericStats`     |     |     |     |     | a table |
| `results$crosstabs`        |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
