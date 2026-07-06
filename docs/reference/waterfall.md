# Treatment Response Analysis

Creates waterfall and spider plots to analyze tumor response data
following RECIST criteria.

## Usage

``` r
waterfall(
  data,
  patientID,
  responseVar,
  timeVar = NULL,
  groupVar = NULL,
  inputType = "percentage",
  sortBy = "response",
  showThresholds = TRUE,
  labelOutliers = FALSE,
  showMedian = FALSE,
  showCI = FALSE,
  minResponseForLabel = 50,
  colorBy = "recist",
  colorScheme = "jamovi",
  barAlpha = 1,
  barWidth = 0.7,
  showWaterfallPlot = TRUE,
  showSpiderPlot = FALSE,
  spiderColorBy = "response",
  spiderColorScheme = "classic",
  timeUnitLabel = "generic",
  generateCopyReadyReport = FALSE,
  showClinicalSignificance = FALSE,
  showConfidenceIntervals = TRUE,
  enableGuidedMode = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- patientID:

  Variable containing patient identifiers (e.g., PT001, Patient_1,
  Study_ID). Each patient should have a unique identifier for proper
  analysis.

- responseVar:

  Response variable: either raw tumor measurements (mm, cm, sum of
  diameters) or pre-calculated percentage changes from baseline. For raw
  measurements: requires time variable with baseline at time = 0. For
  percentages: negative values = tumor shrinkage (good response),
  positive values = tumor growth (poor response). Example: -30 means 30
  percent decrease.

- timeVar:

  Time point of measurement (e.g., months from baseline, days from
  treatment start). Required for spider plot and raw measurement
  processing. Baseline should be time = 0.

- groupVar:

  Optional grouping variable for coloring bars by patient groups (e.g.,
  treatment arms, disease subtypes). When specified, overrides RECIST
  category coloring to show group-based colors.

- inputType:

  Specify data format: 'raw' for actual tumor measurements (requires
  time variable) or 'percentage' for pre-calculated percentage changes
  from baseline

- sortBy:

  Sort the waterfall plot by best response or patient ID.

- showThresholds:

  Show +20 percent and -30 percent RECIST v1.1 thresholds as dashed
  lines. Helps identify Progressive Disease (PD) and Partial Response
  (PR) cutoffs.

- labelOutliers:

  Label responses exceeding the specified threshold.

- showMedian:

  Show median response as a horizontal line.

- showCI:

  Show confidence interval around median response.

- minResponseForLabel:

  Minimum response value for labels to be displayed.

- colorBy:

  Coloring method: RECIST categories or patient groups (requires Group
  Variable).

- colorScheme:

  Color scheme for waterfall plot. 'Colorful' provides distinct colors
  for group-based coloring.

- barAlpha:

  Transparency of bars in waterfall plot.

- barWidth:

  Width of bars in waterfall plot.

- showWaterfallPlot:

  Display the waterfall plot showing best response for each patient.

- showSpiderPlot:

  Display spider plot showing response trajectories over time (requires
  time variable).

- spiderColorBy:

  Coloring method for spider plot: Response status or patient groups.
  For backward compatibility, defaults to response status coloring.

- spiderColorScheme:

  Color scheme for spider plot lines and points.

- timeUnitLabel:

  Label to use for the spider plot time axis. Does not rescale data;
  only affects axis labeling.

- generateCopyReadyReport:

  Generate publication-ready result sentences with statistical details

- showClinicalSignificance:

  Display clinical significance interpretations for ORR and DCR

- showConfidenceIntervals:

  Calculate and display exact binomial confidence intervals for ORR and
  DCR

- enableGuidedMode:

  Enable step-by-step guidance for new users

- showExplanations:

  Display comprehensive explanation of what this analysis does, when to
  use it, data requirements, and key assumptions/limitations

## Value

A results object containing:

|                                   |     |     |     |     |           |
|-----------------------------------|-----|-----|-----|-----|-----------|
| `results$guidedAnalysis`          |     |     |     |     | a html    |
| `results$todo`                    |     |     |     |     | a html    |
| `results$todo2`                   |     |     |     |     | a html    |
| `results$clinicalSummary`         |     |     |     |     | a html    |
| `results$aboutAnalysis`           |     |     |     |     | a html    |
| `results$summaryTable`            |     |     |     |     | a table   |
| `results$personTimeTable`         |     |     |     |     | a table   |
| `results$clinicalMetrics`         |     |     |     |     | a table   |
| `results$waterfallplot`           |     |     |     |     | an image  |
| `results$copyReadyReport`         |     |     |     |     | a html    |
| `results$clinicalSignificance`    |     |     |     |     | a html    |
| `results$clinicalGlossary`        |     |     |     |     | a html    |
| `results$enhancedClinicalMetrics` |     |     |     |     | a table   |
| `results$groupComparisonTable`    |     |     |     |     | a table   |
| `results$groupComparisonTest`     |     |     |     |     | a table   |
| `results$spiderplot`              |     |     |     |     | an image  |
| `results$naturalLanguageSummary`  |     |     |     |     | a html    |
| `results$explanations`            |     |     |     |     | a html    |
| `results$addResponseCategory`     |     |     |     |     | an output |
| `results$notices`                 |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`

## Details

Supports both raw tumor measurements and pre-calculated percentage
changes. Provides comprehensive response analysis including ORR, DCR,
and person-time metrics.
