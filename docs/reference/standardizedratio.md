# Standardized Incidence / Mortality Ratio (SIR / SMR)

Computes standardized incidence ratios (SIR) or standardized mortality
ratios (SMR) by indirect standardization. For each stratum the number of
expected events is obtained by applying reference-population rates to
the observed person-time; the SIR/SMR is the ratio of total observed to
total expected events. Exact Poisson confidence intervals and a test
against the null value of 1 are provided. This is the standard analysis
for registry, cohort, and occupational studies comparing an observed
event count with population expectation.

## Usage

``` r
standardizedratio(
  data,
  inputMode = "rate",
  observed,
  personTime = NULL,
  refRate = NULL,
  expected = NULL,
  stratum = NULL,
  ratioType = "sir",
  conf_level = 0.95,
  perStratum = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per stratum).

- inputMode:

  Whether expected events are computed from a reference rate times
  person-time, or supplied directly as a column of expected counts.

- observed:

  Observed number of events in each stratum.

- personTime:

  Person-time at risk in each stratum (used when expected events are
  computed from a reference rate).

- refRate:

  Reference-population event rate for each stratum, expressed per
  person-time unit (used with inputMode = "rate").

- expected:

  Directly supplied expected events per stratum (used with inputMode =
  "expected").

- stratum:

  Optional stratum label used to report a per-stratum SIR/SMR table.

- ratioType:

  Whether to label results as a standardized incidence or mortality
  ratio.

- conf_level:

  Confidence level for the exact Poisson interval.

- perStratum:

  Report the SIR/SMR for each stratum in addition to the overall value.

- showPlot:

  Display a forest plot of the per-stratum and overall SIR/SMR.

- showSummary:

  Display a plain-language summary of the result.

- showExplanation:

  Display an explanation of indirect standardization.

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$todo`         |     |     |     |     | a html   |
| `results$overallTable` |     |     |     |     | a table  |
| `results$stratumTable` |     |     |     |     | a table  |
| `results$plot`         |     |     |     |     | an image |
| `results$summary`      |     |     |     |     | a html   |
| `results$explanation`  |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overallTable$asDF`

`as.data.frame(results$overallTable)`

## Examples

``` r
# \donttest{
# Long data: one row per stratum with observed events, person-time,
# and a reference rate (events per person-time unit).
standardizedratio(
    data = mydata,
    observed = "obs_events",
    personTime = "person_years",
    refRate = "reference_rate")
#> Error: object 'mydata' not found
# }
```
