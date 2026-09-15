# RECIST 1.1 Multi-Lesion Aggregation

Multi-lesion RECIST 1.1 aggregation for automated calculation of target
lesion sums and best overall response. Processes individual lesion
measurements and applies RECIST 1.1 criteria (Eisenhauer et al., 2009)
for response classification.

## Usage

``` r
recist(
  data,
  patientId,
  assessmentTime,
  lesionId,
  lesionType,
  lesionDiameter,
  nonTargetStatus,
  organ,
  maxTargetLesions = 5,
  maxPerOrgan = 2,
  prThreshold = 30,
  pdThreshold = 20,
  pdAbsolute = 5,
  requireConfirmation = TRUE,
  confirmationWindow = 4,
  nadirReference = TRUE,
  sdMinDuration = 6,
  nonTargetCR = "absent, disappeared",
  nonTargetPD = "pd, progression, unequivocal",
  showLesionTable = TRUE,
  showTargetSumTable = TRUE,
  showResponseTable = TRUE,
  showBestResponse = TRUE,
  showLesionPlot = TRUE,
  showSumPlot = TRUE,
  showWaterfallPlot = TRUE,
  groupVar,
  stratifiedAnalysis = FALSE,
  exportLesionData = FALSE,
  showReference = TRUE,
  formula
)
```

## Arguments

- data:

  the data as a data frame

- patientId:

  Patient identifier variable

- assessmentTime:

  Time from baseline (weeks or months)

- lesionId:

  Unique lesion identifier

- lesionType:

  Lesion type (target, non-target, new)

- lesionDiameter:

  Lesion diameter in millimeters (for target lesions)

- nonTargetStatus:

  Non-target lesion status (present, absent, unequivocal PD)

- organ:

  Organ location (for max 2 per organ rule)

- maxTargetLesions:

  Maximum number of target lesions (default: 5 per RECIST 1.1)

- maxPerOrgan:

  Maximum target lesions per organ (default: 2 per RECIST 1.1)

- prThreshold:

  Percent decrease for partial response (default: 30 percent)

- pdThreshold:

  Percent increase for progressive disease (default: 20 percent)

- pdAbsolute:

  Absolute increase required for PD (default: 5mm)

- requireConfirmation:

  Require 2 consecutive assessments for CR/PR

- confirmationWindow:

  Minimum weeks between confirmation assessments

- nadirReference:

  Use nadir (lowest) sum as reference for PD calculation

- sdMinDuration:

  Minimum duration from baseline required to qualify for Stable Disease
  (SD)

- nonTargetCR:

  Values in non-target status variable indicating Complete Response
  (comma separated)

- nonTargetPD:

  Values in non-target status variable indicating Progressive Disease
  (comma separated)

- showLesionTable:

  Show individual lesion measurements

- showTargetSumTable:

  Show aggregated target lesion sums by assessment

- showResponseTable:

  Show RECIST response categories per assessment

- showBestResponse:

  Calculate best overall response per patient

- showLesionPlot:

  Plot individual lesion sizes over time

- showSumPlot:

  Plot target lesion sum over time

- showWaterfallPlot:

  Waterfall plot of best percent change

- groupVar:

  Grouping variable for stratified analysis

- stratifiedAnalysis:

  Perform stratified analysis by group

- exportLesionData:

  Export processed lesion-level data to CSV

- showReference:

  Display RECIST 1.1 guidelines reference

- formula:

  (optional) the formula to use, see the examples

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$runSummary`             |     |     |     |     | a html   |
| `results$instructions`           |     |     |     |     | a html   |
| `results$dataInfo`               |     |     |     |     | a table  |
| `results$lesionTable`            |     |     |     |     | a table  |
| `results$targetSumTable`         |     |     |     |     | a table  |
| `results$responseTable`          |     |     |     |     | a table  |
| `results$bestResponseTable`      |     |     |     |     | a table  |
| `results$summaryStats`           |     |     |     |     | a table  |
| `results$efficacyMetrics`        |     |     |     |     | a table  |
| `results$lesionPlot`             |     |     |     |     | an image |
| `results$sumPlot`                |     |     |     |     | an image |
| `results$waterfallPlot`          |     |     |     |     | an image |
| `results$stratifiedTable`        |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$referenceInfo`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
