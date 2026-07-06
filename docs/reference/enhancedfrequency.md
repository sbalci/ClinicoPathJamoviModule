# Enhanced Frequency Analysis with BlueSky Integration

Enhanced frequency analysis toolkit with BlueSky integration for
comprehensive categorical data exploration. Includes flexible ordering
options (by frequency, variable value, or natural order), comprehensive
percentage calculations (valid, cumulative, missing), and enhanced
missing value handling. Perfect for exploratory data analysis,
categorical variable profiling, and data quality assessment.

## Usage

``` r
enhancedfrequency(
  data,
  vars,
  orderBy = "freq_desc",
  showDatasetOverview = TRUE,
  showVariableSummary = TRUE,
  showFrequencyTables = TRUE,
  showCombinedSummary = FALSE,
  showPercentages = TRUE,
  showValidPercentages = TRUE,
  showCumulative = TRUE,
  showValidCumulative = TRUE,
  includeMissing = TRUE,
  missingLabel = "<Missing>",
  percentageDecimals = 1,
  frequencyDecimals = 0,
  minFrequency = 0,
  maxCategories = 50,
  combineRareCategories = FALSE,
  rareCategoryLabel = "Other",
  bluesky_integration = TRUE,
  comprehensive_output = FALSE,
  clinical_interpretation = TRUE,
  dataQualityAssessment = TRUE,
  categoricalDiagnostics = FALSE,
  exportFormat = "standard",
  includeMethodology = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables for frequency analysis (will be converted to factors if
  needed)

- orderBy:

  Ordering method for frequency table categories

- showDatasetOverview:

  Display dataset summary with variable counts and observations

- showVariableSummary:

  Display summary statistics by variable

- showFrequencyTables:

  Display detailed frequency tables for each variable

- showCombinedSummary:

  Display all variables in a single summary table

- showPercentages:

  Include percentage columns in frequency tables

- showValidPercentages:

  Include valid percentages (excluding missing values)

- showCumulative:

  Include cumulative frequencies and percentages

- showValidCumulative:

  Include valid cumulative percentages (excluding missing values)

- includeMissing:

  Include missing values (NA) in frequency tables

- missingLabel:

  Label to use for missing values in output tables

- percentageDecimals:

  Number of decimal places for percentages

- frequencyDecimals:

  Number of decimal places for frequencies

- minFrequency:

  Hide categories with frequency below this threshold (0 = show all)

- maxCategories:

  Maximum number of categories to display per variable

- combineRareCategories:

  Combine categories below minimum frequency threshold into "Other"

- rareCategoryLabel:

  Label for combined rare categories

- bluesky_integration:

  Use BlueSky R statistical environment features

- comprehensive_output:

  Include comprehensive statistical details and diagnostics

- clinical_interpretation:

  Provide clinical context for frequency analysis results

- dataQualityAssessment:

  Include data quality metrics and recommendations

- categoricalDiagnostics:

  Include advanced categorical variable diagnostics

- exportFormat:

  Output format for frequency tables

- includeMethodology:

  Include detailed methodology and references in output

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$results$instructions` |  |  |  |  | a html |
| `results$results$datasetOverview` |  |  |  |  | Summary of dataset characteristics |
| `results$results$variableSummary` |  |  |  |  | Summary statistics for all selected variables |
| `results$results$combinedSummary` |  |  |  |  | All variables combined in a single summary table |
| `results$results$dataQualityReport` |  |  |  |  | Data quality metrics and recommendations |
| `results$results$categoricalDiagnostics` |  |  |  |  | Advanced diagnostics for categorical variables |
| `results$results$comprehensiveAnalysisSummary` |  |  |  |  | Enhanced statistical summary with BlueSky integration |
| `results$results$clinicalInterpretationGuide` |  |  |  |  | a html |
| `results$results$methodsExplanation` |  |  |  |  | a html |
| `results$results$frequencyDistributionPlot` |  |  |  |  | Overview of frequency distributions for all variables |
| `results$results$dataQualityPlot` |  |  |  |  | Visual assessment of data quality metrics |
| `results$results$categoryBalancePlot` |  |  |  |  | Analysis of category distribution balance |
