# Enhanced Partial Correlation Analysis with BlueSky Integration

Enhanced partial correlation analysis toolkit with BlueSky integration
for comprehensive correlation analysis while controlling for confounding
variables. Includes partial and semi-partial correlations, robust error
handling, multiple variable support, and advanced statistical inference.
Perfect for clinicopathological research where controlling for
covariates is essential.

## Usage

``` r
partialcorrelation(
  data,
  vars,
  controls,
  method = "pearson",
  ci = TRUE,
  ciWidth = 95,
  sig = TRUE,
  sigLevel = 0.05,
  matrixPlot = FALSE,
  showZeroOrder = TRUE,
  correlationType = "partial",
  multipleComparison = "none",
  robustErrorHandling = TRUE,
  showDiagnostics = FALSE,
  detailedOutput = FALSE,
  bootstrapCI = FALSE,
  bootstrapSamples = 1000,
  showEffectSizes = TRUE,
  assumptionChecks = FALSE,
  clinicalInterpretation = TRUE,
  showRecommendations = FALSE,
  bluesky_integration = TRUE,
  comprehensive_output = FALSE
)
```

## Arguments

- data:

  .

- vars:

  .

- controls:

  .

- method:

  .

- ci:

  .

- ciWidth:

  .

- sig:

  .

- sigLevel:

  .

- matrixPlot:

  .

- showZeroOrder:

  .

- correlationType:

  Type of correlation analysis (BlueSky BSkyPartialSemiCorrelations
  feature)

- multipleComparison:

  Method for correcting p-values for multiple comparisons

- robustErrorHandling:

  Use BlueSky-style error handling with graceful degradation

- showDiagnostics:

  Display diagnostic information about computations and assumptions

- detailedOutput:

  Include comprehensive statistical details and interpretations

- bootstrapCI:

  Calculate bootstrap confidence intervals for correlations

- bootstrapSamples:

  Number of bootstrap samples for confidence intervals

- showEffectSizes:

  Include Cohen's interpretation of correlation effect sizes

- assumptionChecks:

  Perform and report assumption checks for correlation analysis

- clinicalInterpretation:

  Provide clinical context and interpretation guidance

- showRecommendations:

  Provide recommendations based on analysis results

- bluesky_integration:

  Use BlueSky R statistical environment features

- comprehensive_output:

  Include comprehensive statistical details and diagnostics

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$partialCorr` |  |  |  |  | a table |
| `results$zeroOrder` |  |  |  |  | a table |
| `results$semipartialCorr` |  |  |  |  | a table |
| `results$diagnostics` |  |  |  |  | Computational diagnostics and assumption checks |
| `results$assumptionResults` |  |  |  |  | Results of statistical assumption testing |
| `results$comprehensiveAnalysisSummary` |  |  |  |  | Enhanced statistical summary with BlueSky integration |
| `results$recommendations` |  |  |  |  | a html |
| `results$clinicalInterpretationGuide` |  |  |  |  | a html |
| `results$methodsExplanation` |  |  |  |  | a html |
| `results$plot` |  |  |  |  | an image |
| `results$partialCorrelationNetwork` |  |  |  |  | an image |
| `results$effectSizePlot` |  |  |  |  | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$partialCorr$asDF`

`as.data.frame(results$partialCorr)`
