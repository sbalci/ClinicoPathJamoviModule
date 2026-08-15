# Win Ratio for Hierarchical Composite Endpoints

Analyzes prioritized (hierarchically ordered) composite endpoints using
the win ratio, win odds, and net benefit. Every subject in the index
group is compared with every subject in the reference group. Each pair
is classified as a win, loss, or tie by examining endpoints in order of
clinical priority (e.g. death, then hospitalization, then a continuous
biomarker): the first endpoint that can distinguish the pair decides it.
The win ratio is the number of wins divided by the number of losses.
Confidence intervals use the Dong et al. (2016) analytic variance of the
log win ratio; a subject-level bootstrap option is also provided. This
complements hazard-ratio and RMST analyses when outcomes of differing
severity must be combined and ranked.

## Usage

``` r
winratio(
  data,
  group,
  refLevel,
  time1,
  status1,
  eventLevel1,
  time2 = NULL,
  status2 = NULL,
  eventLevel2,
  contEndpoint = NULL,
  contDirection = "higher",
  contTol = 0,
  conf_level = 0.95,
  ciMethod = "analytic",
  bootstrap_n = 1000,
  showWinOdds = TRUE,
  showNetBenefit = TRUE,
  showComponents = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per subject).

- group:

  Two-level grouping variable. The level NOT chosen as the reference is
  treated as the index (e.g. treatment) group; the win ratio expresses
  the index group's chance of winning relative to the reference group.

- refLevel:

  The level of the group variable to treat as the reference (control).

- time1:

  Time to the primary (highest priority) time-to-event endpoint.

- status1:

  Event indicator for the primary endpoint.

- eventLevel1:

  The level of the primary event indicator that denotes the event
  occurred.

- time2:

  Time to a secondary time-to-event endpoint, examined only when the
  primary endpoint ties a pair. Leave empty to skip.

- status2:

  Event indicator for the secondary endpoint.

- eventLevel2:

  The level of the secondary event indicator that denotes the event
  occurred.

- contEndpoint:

  A continuous endpoint used as the lowest-priority tiebreaker, examined
  only when all time-to-event endpoints tie a pair.

- contDirection:

  Whether a higher or lower value of the continuous endpoint is the
  better outcome.

- contTol:

  Minimum absolute difference on the continuous endpoint required to
  declare a win or loss; smaller differences are counted as ties.

- conf_level:

  Confidence level for interval estimates.

- ciMethod:

  Method for the win ratio confidence interval and p-value.

- bootstrap_n:

  Number of bootstrap replicates when the bootstrap CI method is
  selected.

- showWinOdds:

  Report the win odds (ties split evenly), an estimand defined even when
  there are no losses.

- showNetBenefit:

  Report the net benefit (proportion of wins minus proportion of
  losses).

- showComponents:

  Break down wins, losses and ties by the endpoint that decided each
  pair.

- showPlot:

  Display a stacked bar of the win, loss and tie proportions.

- showSummary:

  Display a plain-language summary of the results.

- showExplanation:

  Display an explanation of the win ratio methodology.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$todo`            |     |     |     |     | a html   |
| `results$mainTable`       |     |     |     |     | a table  |
| `results$countsTable`     |     |     |     |     | a table  |
| `results$componentsTable` |     |     |     |     | a table  |
| `results$plot`            |     |     |     |     | an image |
| `results$summary`         |     |     |     |     | a html   |
| `results$explanation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$mainTable$asDF`

`as.data.frame(results$mainTable)`
