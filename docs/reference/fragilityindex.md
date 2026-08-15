# Fragility Index for Dichotomous Outcomes

Computes the Fragility Index (FI) and Fragility Quotient (FQ) for a
two-group trial with a dichotomous (binary) outcome. The Fragility Index
is the minimum number of patients whose outcome would need to change
(from non-event to event, or vice versa) to reverse the statistical
significance of the result. A small Fragility Index means a
"significant" finding hinges on only a handful of events and should be
interpreted with caution. The Fragility Quotient normalizes the index by
total sample size. For a non-significant result, the reverse fragility
index is reported: the number of outcome changes needed to reach
significance.

## Usage

``` r
fragilityindex(
  data,
  dataFormat = "summary",
  group = NULL,
  outcome = NULL,
  outcomeEvent,
  events1 = 10,
  n1 = 100,
  events2 = 25,
  n2 = 100,
  testType = "fisher",
  alpha = 0.05,
  showCounts = TRUE,
  showTrajectory = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (used only when dataFormat = "raw").

- dataFormat:

  Whether to enter summary 2x2 counts directly or supply raw group and
  outcome variables from the dataset.

- group:

  Two-level grouping variable (used when dataFormat = "raw").

- outcome:

  Two-level binary outcome variable (used when dataFormat = "raw").

- outcomeEvent:

  The level of the outcome variable that denotes the event of interest.

- events1:

  Number of events in group 1 (dataFormat = "summary").

- n1:

  Total number of subjects in group 1 (dataFormat = "summary").

- events2:

  Number of events in group 2 (dataFormat = "summary").

- n2:

  Total number of subjects in group 2 (dataFormat = "summary").

- testType:

  The test used to assess statistical significance at each step.

- alpha:

  The two-sided significance threshold.

- showCounts:

  Display the reconstructed 2x2 contingency table.

- showTrajectory:

  Display the step-by-step p-value trajectory as outcomes are reversed.

- showPlot:

  Plot the p-value against the number of outcome reversals.

- showSummary:

  Display a plain-language interpretation of the fragility index.

- showExplanation:

  Display an explanation of the fragility index methodology.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$todo`            |     |     |     |     | a html   |
| `results$countsTable`     |     |     |     |     | a table  |
| `results$mainTable`       |     |     |     |     | a table  |
| `results$trajectoryTable` |     |     |     |     | a table  |
| `results$plot`            |     |     |     |     | an image |
| `results$summary`         |     |     |     |     | a html   |
| `results$explanation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$countsTable$asDF`

`as.data.frame(results$countsTable)`

## Examples

``` r
# \donttest{
# From a 2x2 summary:
fragilityindex(
    dataFormat = "summary",
    events1 = 10, n1 = 100,
    events2 = 25, n2 = 100,
    alpha = 0.05, testType = "fisher")
#> Error in fragilityindex(dataFormat = "summary", events1 = 10, n1 = 100,     events2 = 25, n2 = 100, alpha = 0.05, testType = "fisher"): argument "outcomeEvent" is missing, with no default
# }
```
