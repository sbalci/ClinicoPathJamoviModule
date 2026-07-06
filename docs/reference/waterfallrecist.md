# RECIST v1.1 Compliant Response Analysis

Creates RECIST v1.1 compliant waterfall and spider plots for tumor
response analysis. Implements full RECIST v1.1 protocol including target
lesion summation, new lesion detection, non-target progression
assessment, and response confirmation requirements. REGULATORY-READY:
Suitable for clinical trial endpoints and regulatory submissions.

## Usage

``` r
waterfallrecist(
  data,
  patientID,
  lesionID,
  visitTime,
  lesionType = NULL,
  location = NULL,
  diameter,
  isNewLesion = NULL,
  baselineTimepoint = 0,
  confirmationInterval = 4,
  maxTargetLesions = 5,
  maxLesionsPerOrgan = 2,
  showWaterfallPlot = TRUE,
  showSpiderPlot = TRUE,
  showLesionTable = TRUE,
  showTargetSumTable = TRUE,
  showBestResponseTable = TRUE,
  showRecistComplianceReport = TRUE,
  colorScheme = "recist"
)
```

## Arguments

- data:

  The data as a data frame with LESION-LEVEL observations (one row per
  lesion per timepoint).

- patientID:

  Variable containing patient identifiers. Each patient can have
  multiple lesions tracked across timepoints.

- lesionID:

  Unique lesion identifier within each patient (e.g., L1, L2, Liver_1,
  Lung_2). Used to track individual lesions across timepoints.

- visitTime:

  Time point of measurement (e.g., weeks from baseline, days from
  treatment start). Baseline should be time = 0. Used for tracking
  lesion progression over time.

- lesionType:

  Lesion classification: Target, NonTarget, or New. Target lesions (max
  5 total, max 2 per organ) are measured and summed. NonTarget lesions
  are assessed qualitatively (present/absent/progressed).

- location:

  Anatomic site of lesion (e.g., Liver, Lung, Lymph_Node). Used to
  enforce RECIST rule: max 2 target lesions per organ.

- diameter:

  Longest diameter of target lesions in millimeters (\>=10mm for
  non-lymph nodes, \>=15mm for lymph nodes). For non-target lesions, can
  be NA (qualitative assessment only).

- isNewLesion:

  Binary indicator (0 = baseline/existing, 1 = new lesion appearing
  after baseline). ANY new lesion automatically triggers Progressive
  Disease (PD) per RECIST v1.1.

- baselineTimepoint:

  Value of visitTime representing baseline (default = 0). All lesions at
  this timepoint establish the baseline sum.

- confirmationInterval:

  Minimum time interval (weeks) for response confirmation per RECIST
  v1.1. CR and PR must be confirmed by repeat assessment \>=4 weeks
  after initial documentation.

- maxTargetLesions:

  Maximum number of target lesions per patient (RECIST v1.1 default =
  5).

- maxLesionsPerOrgan:

  Maximum target lesions per organ (RECIST v1.1 default = 2).

- showWaterfallPlot:

  Display waterfall plot showing best confirmed response for each
  patient.

- showSpiderPlot:

  Display spider plot showing target lesion sum trajectories over time.

- showLesionTable:

  Display detailed lesion-level measurements across all timepoints.

- showTargetSumTable:

  Display target lesion sums and percent changes per visit.

- showBestResponseTable:

  Display best overall response with confirmation status per patient.

- showRecistComplianceReport:

  Display audit report verifying RECIST v1.1 compliance (target lesion
  limits, confirmation, new lesions).

- colorScheme:

  Color scheme for plots.

## Value

A results object containing:

|                                              |     |     |     |     |                |
|----------------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                            |     |     |     |     | a preformatted |
| `results$lesionTable`                        |     |     |     |     | a table        |
| `results$targetSumTable`                     |     |     |     |     | a table        |
| `results$bestResponseTable`                  |     |     |     |     | a table        |
| `results$recistSummary$orrConfirmed`         |     |     |     |     | a table        |
| `results$recistSummary$dcrConfirmed`         |     |     |     |     | a table        |
| `results$recistSummary$responseDistribution` |     |     |     |     | a table        |
| `results$complianceReport`                   |     |     |     |     | a html         |
| `results$waterfallPlot`                      |     |     |     |     | an image       |
| `results$spiderPlot`                         |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$lesionTable$asDF`

`as.data.frame(results$lesionTable)`
