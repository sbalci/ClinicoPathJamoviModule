# Multifocal / Primary-Metastasis Concordance

Assesses concordance of biomarkers or mutations across multiple foci of
a tumour, or between paired primary and metastasis samples. Reports
per-marker concordance rates, Cohen's kappa for paired (two-sample)
designs, and a case-level clonality summary.

## Usage

``` r
multifocalconcordance(
  data,
  caseId,
  focusId,
  markers,
  showPerMarker = TRUE,
  showKappa = TRUE,
  showCaseLevel = TRUE,
  showDiscordance = FALSE,
  showPlot = TRUE,
  showSummary = TRUE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  .

- caseId:

  Identifier grouping the foci / samples that belong to the same
  patient.

- focusId:

  Identifier of the focus or sample within a case (e.g. primary vs
  metastasis, or focus 1/2/3). Used to order paired comparisons.

- markers:

  One or more categorical biomarker / mutation status variables to
  assess.

- showPerMarker:

  .

- showKappa:

  .

- showCaseLevel:

  .

- showDiscordance:

  .

- showPlot:

  .

- showSummary:

  .

- showExplanation:

  .

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$todo`             |     |     |     |     | a html   |
| `results$perMarkerTable`   |     |     |     |     | a table  |
| `results$caseLevelTable`   |     |     |     |     | a table  |
| `results$discordanceTable` |     |     |     |     | a table  |
| `results$plot`             |     |     |     |     | an image |
| `results$summary`          |     |     |     |     | a html   |
| `results$explanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$perMarkerTable$asDF`

`as.data.frame(results$perMarkerTable)`
