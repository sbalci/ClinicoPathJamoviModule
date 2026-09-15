# Comprehensive Factor Analysis

Comprehensive factor analysis toolkit with BlueSky R integration.
Includes maximum likelihood factor analysis, principal axis factoring,
scree plots, multiple rotation methods, and factor score extraction.
Essential for dimensionality reduction, scale development, and
multivariate analysis in clinical and pathological research.

## Usage

``` r
factoranalysis(
  data,
  vars,
  method = "ml",
  nFactorsMethod = "kaiser",
  nFactors = 1,
  rotation = "varimax",
  scores = "none",
  saveScores = FALSE,
  scoresPrefix = "Factor",
  showUnrotated = FALSE,
  showRotated = TRUE,
  hideSmallLoadings = FALSE,
  loadingsCutoff = 0.3,
  screePlot = TRUE,
  loadingsPlot = FALSE,
  biplot = FALSE,
  priorCommunalities = "smc",
  maxIterations = 25,
  convergence = 0.001,
  adequacyTests = TRUE,
  bartlettTest = TRUE,
  kmoTest = TRUE,
  factorCorrelations = TRUE,
  communalities = TRUE,
  eigenvalues = TRUE,
  bluesky_integration = TRUE,
  comprehensive_output = FALSE,
  clinical_interpretation = TRUE,
  parallel_iterations = 100,
  parallel_percentile = 95,
  plotWidth = 600,
  plotHeight = 400
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Numeric variables for factor analysis

- method:

  Method for factor extraction

- nFactorsMethod:

  Method for determining number of factors

- nFactors:

  Number of factors to extract (when method = manual)

- rotation:

  Factor rotation method

- scores:

  Method for computing factor scores

- saveScores:

  Save factor scores to dataset

- scoresPrefix:

  Prefix for saved factor score variables

- showUnrotated:

  Display unrotated factor loadings

- showRotated:

  Display rotated factor loadings

- hideSmallLoadings:

  Hide loadings below cutoff

- loadingsCutoff:

  Cutoff for hiding small loadings

- screePlot:

  Display scree plot of eigenvalues

- loadingsPlot:

  Display factor loadings plot

- biplot:

  Display factor biplot (first 2 factors)

- priorCommunalities:

  Method for prior communality estimates

- maxIterations:

  Maximum iterations for factor extraction

- convergence:

  Convergence criterion for factor extraction

- adequacyTests:

  Perform sampling adequacy tests

- bartlettTest:

  Test if correlation matrix is identity matrix

- kmoTest:

  Measure of sampling adequacy

- factorCorrelations:

  Show correlations between factors (oblique rotation)

- communalities:

  Show initial and extracted communalities

- eigenvalues:

  Show eigenvalues and variance explained

- bluesky_integration:

  Use BlueSky R statistical environment features

- comprehensive_output:

  Include comprehensive statistical details

- clinical_interpretation:

  Provide clinical context for factor analysis results

- parallel_iterations:

  Number of iterations for parallel analysis

- parallel_percentile:

  Percentile for parallel analysis threshold

- plotWidth:

  Width of plots

- plotHeight:

  Height of plots

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$results$instructions` |  |  |  |  | a html |
| `results$results$adequacyTable` |  |  |  |  | Tests for appropriateness of factor analysis |
| `results$results$eigenvaluesTable` |  |  |  |  | Eigenvalues and percentage of variance explained |
| `results$results$parallelAnalysis` |  |  |  |  | Comparison of eigenvalues with random data |
| `results$results$communalitiesTable` |  |  |  |  | Initial and extracted communalities |
| `results$results$unrotatedLoadings` |  |  |  |  | Factor loadings before rotation |
| `results$results$rotatedLoadings` |  |  |  |  | Factor loadings after rotation |
| `results$results$factorLoadings` |  |  |  |  | Factor loadings matrix |
| `results$results$factorCorrelationsTable` |  |  |  |  | Correlations between factors (oblique rotation only) |
| `results$results$factorScoresStats` |  |  |  |  | Descriptive statistics for factor scores |
| `results$results$goodnessOfFit` |  |  |  |  | Chi-square test of model fit |
| `results$results$comprehensiveResults` |  |  |  |  | Enhanced statistical output with BlueSky integration |
| `results$results$clinicalInterpretation` |  |  |  |  | a html |
| `results$results$methodDetails` |  |  |  |  | a html |
| `results$results$screePlotImage` |  |  |  |  | Plot of eigenvalues |
| `results$results$loadingsPlotImage` |  |  |  |  | Visualization of factor loadings |
| `results$results$biplotImage` |  |  |  |  | Biplot of first two factors |
| `results$results$parallelPlot` |  |  |  |  | Comparison of actual vs random eigenvalues |
