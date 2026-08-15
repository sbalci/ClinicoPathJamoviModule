# Desirability of Outcome Ranking (DOOR)

Compares two groups on an ordinal Desirability of Outcome Ranking
(DOOR), a composite that ranks each patient by overall clinical
desirability combining efficacy and safety. The DOOR probability is the
chance that a randomly chosen patient from the index group has a more
desirable outcome than a randomly chosen patient from the reference
group (ties split evenly)

- equivalent to a Mann-Whitney / AUC estimand. Values above 0.5 favour
  the index group. This approach, developed for antimicrobial and
  benefit-risk trials, avoids collapsing outcomes of differing severity
  into a single binary endpoint.

## Usage

``` r
door(
  data,
  group,
  refLevel,
  doorRank,
  rankDirection = "lower",
  conf_level = 0.95,
  showDistribution = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per subject).

- group:

  Two-level group variable.

- refLevel:

  The level of the group variable to treat as the reference. The DOOR
  probability expresses the chance the other (index) group is more
  desirable.

- doorRank:

  Ordinal DOOR category for each subject. Combined with the ranking
  direction to determine which outcomes are more desirable.

- rankDirection:

  Whether a lower or higher DOOR category value represents the more
  desirable outcome.

- conf_level:

  Confidence level for the DOOR probability interval.

- showDistribution:

  Display the distribution of DOOR categories within each group.

- showPlot:

  Display a stacked bar of DOOR category proportions by group.

- showSummary:

  Display a plain-language summary of the DOOR probability.

- showExplanation:

  Display an explanation of the DOOR methodology.

## Value

A results object containing:

|                             |     |     |     |     |          |
|-----------------------------|-----|-----|-----|-----|----------|
| `results$todo`              |     |     |     |     | a html   |
| `results$mainTable`         |     |     |     |     | a table  |
| `results$distributionTable` |     |     |     |     | a table  |
| `results$plot`              |     |     |     |     | an image |
| `results$summary`           |     |     |     |     | a html   |
| `results$explanation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$mainTable$asDF`

`as.data.frame(results$mainTable)`

## Examples

``` r
# \donttest{
door(
    data = mydata,
    group = "arm",
    refLevel = "Control",
    doorRank = "door_category")
#> Error: object 'mydata' not found
# }
```
