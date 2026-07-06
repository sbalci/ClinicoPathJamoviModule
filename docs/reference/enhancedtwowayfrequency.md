# Enhanced Two-Way Frequency Analysis with BlueSky Integration

Enhanced two-way frequency analysis toolkit with BlueSky integration for
comprehensive cross-tabulation analysis. Includes cell, row, and column
percentages, statistical tests for association, robust error handling,
and clinical interpretation guidance. Perfect for exploring
relationships between categorical variables in clinicopathological
research.

## Usage

``` r
enhancedtwowayfrequency(
  data,
  rowVar,
  colVar,
  cellPercent = TRUE,
  rowPercent = FALSE,
  colPercent = FALSE,
  showCounts = TRUE,
  showTotals = TRUE,
  chiSquareTest = TRUE,
  fisherTest = FALSE,
  continuityCorrection = TRUE,
  associationMeasures = TRUE,
  residualAnalysis = FALSE,
  expectedFrequencies = FALSE,
  minimumExpected = 5,
  robustErrorHandling = TRUE,
  showDiagnostics = FALSE,
  detailedOutput = FALSE,
  clinicalInterpretation = TRUE,
  showRecommendations = FALSE,
  bluesky_integration = TRUE,
  comprehensive_output = FALSE,
  heatmapPlot = FALSE,
  mosaicPlot = FALSE,
  percentageDisplay = "percentage"
)
```

## Arguments

- data:

  .

- rowVar:

  .

- colVar:

  .

- cellPercent:

  Display percentages of total sample (BlueSky BSkyTwoWayFrequency
  feature)

- rowPercent:

  Display row-wise percentages (sum to 100 percent across rows)

- colPercent:

  Display column-wise percentages (sum to 100 percent across columns)

- showCounts:

  Display raw frequency counts in cross-tabulation

- showTotals:

  Include row and column totals in tables

- chiSquareTest:

  Perform Pearson's chi-square test for association

- fisherTest:

  Perform Fisher's exact test (recommended for small samples)

- continuityCorrection:

  Apply continuity correction for 2x2 tables

- associationMeasures:

  Calculate Cramér's V, Phi coefficient, and other association measures

- residualAnalysis:

  Calculate and display standardized residuals for detecting patterns

- expectedFrequencies:

  Display expected frequencies under independence assumption

- minimumExpected:

  Threshold for warning about low expected frequencies

- robustErrorHandling:

  Use BlueSky-style error handling with graceful degradation

- showDiagnostics:

  Display diagnostic information about computations and assumptions

- detailedOutput:

  Include comprehensive statistical details and interpretations

- clinicalInterpretation:

  Provide clinical context and interpretation guidance

- showRecommendations:

  Provide recommendations based on analysis results

- bluesky_integration:

  Use BlueSky R statistical environment features

- comprehensive_output:

  Include comprehensive statistical details and diagnostics

- heatmapPlot:

  Create heatmap visualization of frequency table

- mosaicPlot:

  Create mosaic plot showing proportional relationships

- percentageDisplay:

  Format for displaying percentage values

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$observedFrequencies` |  |  |  |  | a table |
| `results$crossTabMatrix` |  |  |  |  | a table |
| `results$cellPercentMatrix` |  |  |  |  | a table |
| `results$rowPercentMatrix` |  |  |  |  | a table |
| `results$colPercentMatrix` |  |  |  |  | a table |
| `results$testResults` |  |  |  |  | a table |
| `results$associationMeasuresTable` |  |  |  |  | a table |
| `results$assumptionCheck` |  |  |  |  | a table |
| `results$diagnostics` |  |  |  |  | Computational diagnostics and data quality assessment |
| `results$comprehensiveAnalysisSummary` |  |  |  |  | Enhanced statistical summary with BlueSky integration |
| `results$recommendations` |  |  |  |  | a html |
| `results$clinicalInterpretationGuide` |  |  |  |  | a html |
| `results$methodsExplanation` |  |  |  |  | a html |
| `results$heatmapPlot` |  |  |  |  | an image |
| `results$mosaicPlot` |  |  |  |  | an image |
| `results$residualPlot` |  |  |  |  | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$observedFrequencies$asDF`

`as.data.frame(results$observedFrequencies)`
