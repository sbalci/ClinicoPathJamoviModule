# Tumor Budding (ITBCC)

Quantifies tumor budding and assigns an ITBCC (2016) grade from the
number of buds counted in a hotspot. Supports a single hotspot count,
multiple candidate fields (hotspot = the densest field), and area
normalization to the standard 0.785 mm2 field.

## Usage

``` r
tumorbudding(
  data,
  budCount,
  caseId = NULL,
  fieldArea = 0.785,
  group = NULL,
  survivalTime = NULL,
  survivalStatus = NULL,
  eventLevel,
  showGrading = TRUE,
  showPerCase = FALSE,
  showSurvival = TRUE,
  showPlot = TRUE,
  showSummary = TRUE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  .

- budCount:

  Number of tumor buds counted. With multiple fields per case (see Case
  ID), the densest field is taken as the hotspot.

- caseId:

  Optional case identifier. When several rows share a case ID (multiple
  fields), the maximum bud count is used as the hotspot for that case.

- fieldArea:

  Area of the counted field in mm2. Counts are normalized to the ITBCC
  standard 0.785 mm2 field (20x objective, 0.55 mm field diameter)
  before grading.

- group:

  .

- survivalTime:

  .

- survivalStatus:

  .

- eventLevel:

  .

- showGrading:

  .

- showPerCase:

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

|                          |     |     |     |     |           |
|--------------------------|-----|-----|-----|-----|-----------|
| `results$todo`           |     |     |     |     | a html    |
| `results$gradingTable`   |     |     |     |     | a table   |
| `results$perCaseTable`   |     |     |     |     | a table   |
| `results$survivalTable`  |     |     |     |     | a table   |
| `results$plot`           |     |     |     |     | an image  |
| `results$addGradeToData` |     |     |     |     | an output |
| `results$summary`        |     |     |     |     | a html    |
| `results$explanation`    |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$gradingTable$asDF`

`as.data.frame(results$gradingTable)`
