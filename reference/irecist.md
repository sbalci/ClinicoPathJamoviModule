# iRECIST Analysis

Immune-related Response Evaluation Criteria in Solid Tumors (iRECIST)
analysis for assessing tumor response in immunotherapy trials.
Implements Seymour et al. (2017) guidelines including pseudoprogression
detection and confirmation requirements.

## Usage

``` r
irecist(
  data,
  patientId,
  assessmentTime,
  targetLesionSum,
  newLesions,
  nonTargetStatus,
  tumorBurden,
  confirmationWindow = 4,
  confirmationWindowMax = 12,
  baselineReference = "baseline",
  prThreshold = 30,
  pdThreshold = 20,
  pdAbsolute = 5,
  trackPseudoprogression = TRUE,
  requireConfirmation = TRUE,
  confirmationScans = 1,
  showResponseTable = TRUE,
  showBestResponse = TRUE,
  showWaterfallPlot = TRUE,
  showSwimmerPlot = TRUE,
  showSpiderPlot = FALSE,
  showTimeToCPD = TRUE,
  groupVar,
  stratifiedAnalysis = FALSE,
  nadirReference = TRUE,
  exportResults = FALSE,
  showDetailedTimeline = FALSE,
  calculateORR = TRUE,
  calculateDCR = TRUE,
  showPseudoprogressionRate = TRUE,
  showReference = TRUE,
  showSummary = TRUE,
  showGlossary = TRUE,
  showAssumptions = TRUE,
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

- targetLesionSum:

  Sum of target lesion diameters (mm)

- newLesions:

  Binary indicator for new lesions (0=no, 1=yes)

- nonTargetStatus:

  Non-target lesion status (CR, non-CR/non-PD, PD)

- tumorBurden:

  Total tumor burden measure (optional)

- confirmationWindow:

  Minimum time window for iUPD confirmation (default: 4 weeks per
  iRECIST guidelines)

- confirmationWindowMax:

  Maximum time window for iUPD confirmation (default: 12 weeks)

- baselineReference:

  Reference point for baseline measurements

- prThreshold:

  Percent decrease for partial response (default: 30 percent per RECIST)

- pdThreshold:

  Percent increase for progressive disease (default: 20 percent per
  RECIST)

- pdAbsolute:

  Absolute increase required for PD (default: 5mm per RECIST)

- trackPseudoprogression:

  Flag and track iUPD (unconfirmed progression) cases

- requireConfirmation:

  Require confirmation scan for response calls

- confirmationScans:

  Number of confirmation scans required for CR/PR

- showResponseTable:

  Show detailed response category table

- showBestResponse:

  Calculate and show best overall response per patient

- showWaterfallPlot:

  Generate waterfall plot with iRECIST colors

- showSwimmerPlot:

  Generate swimmer plot with iUPD events marked

- showSpiderPlot:

  Generate spider plot showing individual lesion trajectories

- showTimeToCPD:

  Calculate time to confirmed progressive disease (iCPD)

- groupVar:

  Grouping variable for stratified analysis (e.g., treatment arm)

- stratifiedAnalysis:

  Perform analysis stratified by grouping variable

- nadirReference:

  Use nadir (lowest) value as reference for PD calculation

- exportResults:

  Export results to CSV file

- showDetailedTimeline:

  Show detailed timeline of response assessments and transitions

- calculateORR:

  Calculate ORR (iCR + iPR)

- calculateDCR:

  Calculate DCR (iCR + iPR + iSD)

- showPseudoprogressionRate:

  Calculate rate of pseudoprogression (iUPD → non-iCPD)

- showReference:

  Display iRECIST guidelines reference (Seymour et al. 2017)

- showSummary:

  Display natural-language summary with copy-ready text

- showGlossary:

  Display glossary of iRECIST terms and definitions

- showAssumptions:

  Display analysis assumptions and data requirements

- formula:

  (optional) the formula to use, see the examples

## Value

A results object containing:

|                                  |     |     |     |     |                |
|----------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                |     |     |     |     | a preformatted |
| `results$instructions`           |     |     |     |     | a html         |
| `results$dataInfo`               |     |     |     |     | a table        |
| `results$responseTable`          |     |     |     |     | a table        |
| `results$bestResponseTable`      |     |     |     |     | a table        |
| `results$summaryStats`           |     |     |     |     | a table        |
| `results$efficacyMetrics`        |     |     |     |     | a table        |
| `results$pseudoprogressionTable` |     |     |     |     | a table        |
| `results$waterfallPlot`          |     |     |     |     | an image       |
| `results$swimmerPlot`            |     |     |     |     | an image       |
| `results$spiderPlot`             |     |     |     |     | an image       |
| `results$timeToCPDPlot`          |     |     |     |     | an image       |
| `results$timelinePlot`           |     |     |     |     | an image       |
| `results$stratifiedTable`        |     |     |     |     | a table        |
| `results$clinicalInterpretation` |     |     |     |     | a html         |
| `results$referenceInfo`          |     |     |     |     | a html         |
| `results$executiveSummary`       |     |     |     |     | a html         |
| `results$glossary`               |     |     |     |     | a html         |
| `results$assumptions`            |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
