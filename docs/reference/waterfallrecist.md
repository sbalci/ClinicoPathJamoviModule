# Treatment Response: Lesion-Level RECIST v1.1 Algorithm

Use this when your data are lesion-level: one row per lesion per visit,
giving patient, lesion, visit time and diameter. Adding a lesion type
column (Target / Non-Target / New) and a new-lesion flag enables
new-lesion and non-target assessment; an organ column enforces the limit
of two target lesions per organ. It applies the RECIST v1.1 algorithm:
it sums target lesion diameters, measures progression against the
smallest sum recorded so far (nadir) including the 5 mm
absolute-increase rule, treats any new lesion as progression, applies
confirmation of CR and PR at 4 weeks or more, and reports best overall
response truncated at progression. This is a new implementation (version
0.0.1) that has not been checked against a reference RECIST tool or a
regulatory dataset, so it is a research tool and not a validated or
submission-ready result. Non-target progression is decided here by
lesion count rather than the radiologist's judgement of unequivocal
progression that RECIST intends. Check response assignments against the
source imaging before they are recorded or reported. For response rates
with confidence intervals, group comparison and a copy-ready summary,
the patient-level analysis has the fuller reporting.

## Usage

``` r
waterfallrecist(
  data,
  patientID = NULL,
  lesionID = NULL,
  visitTime = NULL,
  lesionType = NULL,
  location = NULL,
  diameter = NULL,
  isNewLesion = NULL,
  nonTargetResponseVar = NULL,
  targetSelectionVar = NULL,
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

- nonTargetResponseVar:

  Optional per-visit non-target lesion assessment recorded by the
  reporting radiologist. Accepted values are CR, Non-CR/Non-PD, PD and
  NE (case and punctuation are ignored, so "non-cr/non-pd", "NonCR
  NonPD" and "Non CR Non PD" all match). When supplied it OVERRIDES the
  computed non-target status for that patient and visit. RECIST v1.1
  defines non-target progression as "unequivocal progression" of
  existing non-target disease, which is a qualitative radiological
  judgement that cannot be derived from measurements. Without this
  variable the analysis falls back to a lesion-count heuristic (an
  increase of two or more non-target lesions is called progression),
  which is NOT the RECIST criterion and may both miss and over-call
  progression. Supplying this variable is the RECIST-correct route.

- targetSelectionVar:

  Optional per-lesion flag marking the lesions the reporting radiologist
  chose as target lesions (Yes/No, 1/0, TRUE/FALSE or "Target"). When
  supplied it is used verbatim and automatic selection is not applied.
  By default the analysis follows RECIST v1.1 and selects the LARGEST
  lesions within the limits (at most 5 in total, at most 2 per organ);
  the remainder are followed as non-target disease. RECIST also requires
  a target lesion to be reproducibly measurable, which is a radiological
  judgement size alone cannot establish, so use this variable whenever
  the reader's own choice differs from the largest-first default.

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
| `results$instructions`                       |     |     |     |     | a html         |
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
