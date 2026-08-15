# Lymph Node Ratio

Computes the lymph node ratio (LNR = positive nodes / examined nodes)
for each patient, assesses nodal-yield adequacy against a
minimum-examined threshold, and relates LNR to a survival outcome. LNR
can be stratified either by established category thresholds or by a
data-driven optimal cutpoint chosen to maximize the log-rank separation,
and the resulting strata are compared with Kaplan-Meier / log-rank
testing. LNR is an established prognostic factor across colorectal,
gastric, breast, and head-and-neck cancers, and is more robust than
absolute node counts when nodal yield varies between specimens.

## Usage

``` r
lymphnoderatio(
  data,
  positiveNodes,
  examinedNodes,
  minYield = 12,
  stratMethod = "fixed",
  thresholds = "0.2, 0.5",
  survivalTime = NULL,
  survivalStatus = NULL,
  eventLevel,
  conf_level = 0.95,
  showRatioSummary = TRUE,
  showStrata = TRUE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per patient).

- positiveNodes:

  Number of lymph nodes containing metastatic carcinoma.

- examinedNodes:

  Total number of lymph nodes examined.

- minYield:

  Minimum examined-node count considered an adequate nodal yield (e.g.
  12 for colorectal).

- stratMethod:

  How to categorize LNR for the survival comparison.

- thresholds:

  Comma-separated LNR cut points for the established-threshold
  stratification (e.g. "0.2, 0.5" gives Low / Intermediate / High).

- survivalTime:

  Follow-up time for the survival comparison.

- survivalStatus:

  Event indicator for the survival comparison.

- eventLevel:

  Level of the status variable representing the event.

- conf_level:

  Confidence level for hazard-ratio estimates.

- showRatioSummary:

  Summary statistics of LNR and nodal yield across the cohort.

- showStrata:

  Table of LNR strata with n, events, and median survival.

- showPlot:

  Distribution of LNR (and strata) or the survival curves by stratum.

- showSummary:

  Plain-language summary.

- showExplanation:

  Explanation of LNR and its prognostic use.

## Value

A results object containing:

|                            |     |     |     |     |           |
|----------------------------|-----|-----|-----|-----|-----------|
| `results$todo`             |     |     |     |     | a html    |
| `results$ratioTable`       |     |     |     |     | a table   |
| `results$strataTable`      |     |     |     |     | a table   |
| `results$plot`             |     |     |     |     | an image  |
| `results$summary`          |     |     |     |     | a html    |
| `results$explanation`      |     |     |     |     | a html    |
| `results$addRatioToData`   |     |     |     |     | an output |
| `results$addStratumToData` |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$ratioTable$asDF`

`as.data.frame(results$ratioTable)`

## Examples

``` r
# \donttest{
lymphnoderatio(
    data = mydata,
    positiveNodes = "n_positive",
    examinedNodes = "n_examined",
    survivalTime = "months", survivalStatus = "dead")
#> Error in lymphnoderatio(data = mydata, positiveNodes = "n_positive", examinedNodes = "n_examined",     survivalTime = "months", survivalStatus = "dead"): argument "eventLevel" is missing, with no default
# }
```
