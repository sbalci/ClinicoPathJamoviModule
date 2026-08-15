# ctDNA / MRD Dynamics

Analyses circulating-tumour-DNA (ctDNA) / minimal-residual-disease (MRD)
kinetics from paired baseline and follow-up variant allele fractions
(VAF): clearance vs persistence classification, log-fold change, and MRD
status as a predictor of survival by a landmark analysis.

## Usage

``` r
ctdnadynamics(
  data,
  baselineVaf,
  followupVaf,
  detectionThreshold = 0.05,
  timeBetween = NULL,
  group = NULL,
  survivalTime = NULL,
  survivalStatus = NULL,
  eventLevel,
  conf_level = 0.95,
  showClassification = TRUE,
  showDynamics = TRUE,
  showSurvival = TRUE,
  showPlot = TRUE,
  showSummary = TRUE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  .

- baselineVaf:

  Variant allele fraction (or ctDNA concentration) at baseline /
  pre-treatment.

- followupVaf:

  Variant allele fraction (or ctDNA concentration) at the follow-up /
  MRD timepoint.

- detectionThreshold:

  VAF at or below which the follow-up sample is classed as cleared /
  MRD-negative.

- timeBetween:

  Time between the baseline and follow-up draws, used to report a
  clearance rate (change in log VAF per unit time).

- group:

  Optional grouping variable (e.g. treatment arm) for stratified
  clearance rates.

- survivalTime:

  .

- survivalStatus:

  .

- eventLevel:

  .

- conf_level:

  .

- showClassification:

  .

- showDynamics:

  .

- showSurvival:

  .

- showPlot:

  .

- showSummary:

  .

- showExplanation:

  .

## Value

A results object containing:

|                               |     |     |     |     |           |
|-------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                |     |     |     |     | a html    |
| `results$classificationTable` |     |     |     |     | a table   |
| `results$dynamicsTable`       |     |     |     |     | a table   |
| `results$survivalTable`       |     |     |     |     | a table   |
| `results$plot`                |     |     |     |     | an image  |
| `results$addStatusToData`     |     |     |     |     | an output |
| `results$summary`             |     |     |     |     | a html    |
| `results$explanation`         |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$classificationTable$asDF`

`as.data.frame(results$classificationTable)`
