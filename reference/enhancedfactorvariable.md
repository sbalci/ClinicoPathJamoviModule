# Enhanced Factor Variable Analysis with BlueSky Integration

Enhanced factor variable analysis toolkit with BlueSky integration for
comprehensive categorical variable profiling. Includes detailed factor
level counting, top N factor display, summary statistics, and enhanced
visualization capabilities. Perfect for exploring categorical pathology
variables, biomarker categories, and clinical classification systems
with advanced filtering and display options.

## Usage

``` r
enhancedfactorvariable(
  data,
  vars,
  showOnlyTopFactors = TRUE,
  maxTopFactors = 30,
  sortingMethod = "freq_desc",
  showDatasetOverview = TRUE,
  showNominalSummary = TRUE,
  showDetailedLevels = TRUE,
  showCombinedAnalysis = FALSE,
  includePercentages = TRUE,
  includeValidPercentages = TRUE,
  includeCumulativeStats = FALSE,
  includeMissing = TRUE,
  missingLabel = "<Missing>",
  minimumCount = 1,
  minimumPercentage = 0,
  excludeRareFactors = FALSE,
  rareFactorThreshold = 1,
  rareFactorLabel = "Other",
  factorComplexityAnalysis = FALSE,
  levelBalanceAnalysis = FALSE,
  bluesky_integration = TRUE,
  comprehensive_output = FALSE,
  clinical_interpretation = TRUE,
  createVisualizations = TRUE,
  plotTopFactorsOnly = TRUE,
  plotOrientation = "horizontal",
  plotTheme = "clinical",
  outputFormat = "standard",
  includeMethodology = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Factor variables for detailed level analysis (will be converted to
  factors if needed)

- showOnlyTopFactors:

  Display only the most frequent factor levels instead of all levels

- maxTopFactors:

  Number of top factor levels to display when limiting output

- sortingMethod:

  Method for ordering factor levels in output tables

- showDatasetOverview:

  Display dataset summary with variable counts and observations

- showNominalSummary:

  Display summary statistics for all factor variables

- showDetailedLevels:

  Display detailed counts for each factor level

- showCombinedAnalysis:

  Display all factors in a single combined table

- includePercentages:

  Calculate and display percentages for factor levels

- includeValidPercentages:

  Calculate percentages excluding missing values

- includeCumulativeStats:

  Calculate and display cumulative frequencies and percentages

- includeMissing:

  Include missing values (NA) in factor level counts

- missingLabel:

  Label to display for missing factor levels

- minimumCount:

  Hide factor levels with counts below this threshold

- minimumPercentage:

  Hide factor levels with percentages below this threshold

- excludeRareFactors:

  Group infrequent factor levels into "Other" category

- rareFactorThreshold:

  Percentage threshold below which factors are considered rare

- rareFactorLabel:

  Label for grouped rare factor levels

- factorComplexityAnalysis:

  Analyze factor complexity with entropy and diversity measures

- levelBalanceAnalysis:

  Analyze balance between factor levels with statistical measures

- bluesky_integration:

  Use BlueSky R statistical environment features and algorithms

- comprehensive_output:

  Include comprehensive statistical details and diagnostics

- clinical_interpretation:

  Provide clinical context for factor variable analysis results

- createVisualizations:

  Generate plots for factor variable analysis

- plotTopFactorsOnly:

  Limit plots to top factor levels only

- plotOrientation:

  Orientation for factor level bar plots

- plotTheme:

  Visual theme for factor analysis plots

- outputFormat:

  Output format style for tables and results

- includeMethodology:

  Include detailed methodology and references in output

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$results$instructions` |  |  |  |  | a html |
| `results$results$datasetOverview` |  |  |  |  | Summary of dataset characteristics for factor analysis |
| `results$results$nominalSummary` |  |  |  |  | Summary statistics for all factor variables |
| `results$results$detailedLevelsAnalysis` |  |  |  |  | Detailed counts and percentages for each factor level |
| `results$results$combinedFactorAnalysis` |  |  |  |  | All factor variables combined in a single analysis table |
| `results$results$complexityAnalysis` |  |  |  |  | Complexity metrics for factor variables including entropy and diversity |
| `results$results$levelBalanceAnalysis` |  |  |  |  | Balance metrics for factor level distributions |
| `results$results$comprehensiveAnalysisSummary` |  |  |  |  | Enhanced statistical summary with BlueSky integration |
| `results$results$clinicalInterpretationGuide` |  |  |  |  | a html |
| `results$results$methodsExplanation` |  |  |  |  | a html |
| `results$results$factorDistributionPlot` |  |  |  |  | Overview of factor level distributions across all variables |
| `results$results$topFactorsPlot` |  |  |  |  | Bar plot showing most frequent factor levels |
| `results$results$complexityPlot` |  |  |  |  | Visualization of factor complexity metrics |
| `results$results$balanceAnalysisPlot` |  |  |  |  | Visualization of factor level balance across variables |
