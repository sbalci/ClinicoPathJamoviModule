# Synoptic Report Completeness

Audits the completeness of structured / synoptic pathology reports: the
proportion of required data elements present per report, per required
element, and by subspecialty or reporting pathologist, with an optional
completeness trend over time.

## Usage

``` r
synopticcompleteness(
  data,
  items,
  presenceRule = "nonmissing",
  presentValue = "",
  group = NULL,
  timeVar = NULL,
  completeThreshold = 100,
  showOverall = TRUE,
  showPerItem = TRUE,
  showByGroup = TRUE,
  showTrend = TRUE,
  showPlot = TRUE,
  showSummary = TRUE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  .

- items:

  The required synoptic data elements. Each is counted as present for a
  report when it is non-missing (and, for text, not blank or an explicit
  absence marker).

- presenceRule:

  How each element is judged present.

- presentValue:

  When the rule is "specified value", the value that marks an element
  present.

- group:

  Optional grouping for stratified completeness (e.g. subspecialty or
  reporting pathologist).

- timeVar:

  Optional ordered time/period variable for a completeness trend.

- completeThreshold:

  A report counts as complete when at least this percentage of elements
  are present.

- showOverall:

  .

- showPerItem:

  .

- showByGroup:

  .

- showTrend:

  .

- showPlot:

  .

- showSummary:

  .

- showExplanation:

  .

## Value

A results object containing:

|                                 |     |     |     |     |           |
|---------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                  |     |     |     |     | a html    |
| `results$overallTable`          |     |     |     |     | a table   |
| `results$perItemTable`          |     |     |     |     | a table   |
| `results$byGroupTable`          |     |     |     |     | a table   |
| `results$trendTable`            |     |     |     |     | a table   |
| `results$plot`                  |     |     |     |     | an image  |
| `results$addCompletenessToData` |     |     |     |     | an output |
| `results$summary`               |     |     |     |     | a html    |
| `results$explanation`           |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overallTable$asDF`

`as.data.frame(results$overallTable)`
