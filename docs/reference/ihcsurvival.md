# IHC Survival Analysis

Survival analysis and prognostic clustering using IHC markers. Designed
for prognostic biomarker discovery and risk stratification.

## Usage

``` r
ihcsurvival(
  data,
  markers,
  survivalTime,
  survivalEvent,
  id = NULL,
  prognosticClustering = TRUE,
  multiRegionAnalysis = FALSE,
  centralRegion,
  invasiveRegion,
  riskStratification = "tertiles",
  coxRegression = TRUE,
  multivariateAdjustment,
  kaplanMeierPlots = TRUE,
  landmarkAnalysis = FALSE,
  landmarkTime = 60,
  landmarkTimePoints = "12,24,36,48,60",
  confidenceLevel = 0.95,
  fdrMethod = "BH"
)
```

## Arguments

- data:

  the data as a data frame

- markers:

  IHC marker variables for prognostic analysis

- survivalTime:

  Time to event variable (months, years, days)

- survivalEvent:

  Event indicator (1=event, 0=censored)

- id:

  Case identifier for tracking

- prognosticClustering:

  Perform clustering optimized for survival outcomes

- multiRegionAnalysis:

  Compare central vs invasive tumor regions

- centralRegion:

  IHC markers from central tumor region

- invasiveRegion:

  IHC markers from invasive tumor region

- riskStratification:

  Method for creating prognostic risk groups

- coxRegression:

  Perform Cox proportional hazards regression

- multivariateAdjustment:

  Clinical variables for multivariate adjustment

- kaplanMeierPlots:

  Generate survival curves for risk groups

- landmarkAnalysis:

  Perform landmark survival analysis at specific time points

- landmarkTime:

  Time point for landmark analysis (same units as survival time)

- landmarkTimePoints:

  Comma-separated time points for landmark survival analysis (e.g.,
  "12,24,36")

- confidenceLevel:

  Confidence level for survival estimates and hazard ratios

- fdrMethod:

  Multiple-testing adjustment applied across markers in the panel. When
  testing several IHC markers in the same cohort, raw Cox p-values
  overstate evidence; Benjamini-Hochberg controls FDR under
  independence/positive dependence, Benjamini-Yekutieli is valid under
  arbitrary dependence, Holm/Bonferroni control FWER.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`        |     |     |     |     | a html   |
| `results$assumptions`         |     |     |     |     | a html   |
| `results$reportSentence`      |     |     |     |     | a html   |
| `results$prognosticGroups`    |     |     |     |     | a table  |
| `results$coxResults`          |     |     |     |     | a table  |
| `results$multivariateResults` |     |     |     |     | a table  |
| `results$regionalComparison`  |     |     |     |     | a table  |
| `results$landmarkResults`     |     |     |     |     | a table  |
| `results$modelFit`            |     |     |     |     | a table  |
| `results$kaplanMeierPlot`     |     |     |     |     | an image |
| `results$hazardPlot`          |     |     |     |     | an image |
| `results$riskScorePlot`       |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$prognosticGroups$asDF`

`as.data.frame(results$prognosticGroups)`
