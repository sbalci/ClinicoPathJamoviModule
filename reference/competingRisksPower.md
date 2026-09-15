# Competing Risks Power Analysis (Specialized)

Power analysis and sample size calculations for competing risks studies.
Calculates power for testing differences in cumulative incidence
functions between groups using Gray's test and subdistribution hazard
models.

## Usage

``` r
competingRisksPower(
  data,
  analysisType = "power",
  alpha = 0.05,
  power = 0.8,
  totalSampleSize = 200,
  allocationRatio = "1:1",
  followUpTime = 5,
  accrualTime = 2,
  eventRate1 = 0.3,
  competingRate1 = 0.2,
  eventRate2 = 0.4,
  competingRate2 = 0.2,
  hazardRatio = 1.5,
  testType = "gray",
  distributionType = "exponential",
  shape1 = 1,
  shape2 = 1,
  numberOfSimulations = 1000,
  showSimulationDetails = FALSE,
  showEducational = TRUE,
  plotPowerCurve = TRUE,
  plotEventRates = FALSE,
  sensitivityAnalysis = FALSE,
  confidenceLevel = 0.95
)
```

## Arguments

- data:

  The data as a data frame (optional for power calculations).

- analysisType:

  Type of power analysis to perform

- alpha:

  Type I error rate for statistical testing

- power:

  Desired statistical power (for sample size calculations)

- totalSampleSize:

  Total sample size for power calculation

- allocationRatio:

  Allocation ratio between groups (e.g., "1:1", "2:1")

- followUpTime:

  Maximum follow-up time for the study

- accrualTime:

  Patient accrual/recruitment period

- eventRate1:

  Cumulative incidence rate for primary event in Group 1

- competingRate1:

  Cumulative incidence rate for competing events in Group 1

- eventRate2:

  Cumulative incidence rate for primary event in Group 2

- competingRate2:

  Cumulative incidence rate for competing events in Group 2

- hazardRatio:

  Expected subdistribution hazard ratio between groups

- testType:

  Type of statistical test for competing risks comparison

- distributionType:

  Assumed distribution for event times

- shape1:

  Shape parameter for Weibull distribution (Group 1)

- shape2:

  Shape parameter for Weibull distribution (Group 2)

- numberOfSimulations:

  Number of Monte Carlo simulations for power estimation

- showSimulationDetails:

  Display detailed simulation results and convergence diagnostics

- showEducational:

  Display educational information about competing risks power analysis

- plotPowerCurve:

  Display power curve across different effect sizes or sample sizes

- plotEventRates:

  Display visualization of cumulative incidence rates

- sensitivityAnalysis:

  Perform sensitivity analysis across different parameter values

- confidenceLevel:

  Confidence level for power estimates

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                  |     |     |     |     | a html   |
| `results$educationalInfo`       |     |     |     |     | a html   |
| `results$powerResults`          |     |     |     |     | a table  |
| `results$studyDesignTable`      |     |     |     |     | a table  |
| `results$sampleSizeBreakdown`   |     |     |     |     | a table  |
| `results$powerCurveTable`       |     |     |     |     | a table  |
| `results$sensitivityTable`      |     |     |     |     | a table  |
| `results$simulationDiagnostics` |     |     |     |     | a table  |
| `results$methodsInfo`           |     |     |     |     | a html   |
| `results$recommendationsInfo`   |     |     |     |     | a html   |
| `results$powerCurvePlot`        |     |     |     |     | an image |
| `results$eventRatesPlot`        |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$powerResults$asDF`

`as.data.frame(results$powerResults)`

## Details

This is the specialized function for competing risks power analysis. For
unified analysis across all survival study types, also consider
'Comprehensive Survival Power Analysis'.
