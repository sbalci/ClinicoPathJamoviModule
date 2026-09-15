# IHC Threshold Determination

IHC Threshold Determination

## Usage

``` r
ihcthreshold(
  data,
  sampleArea,
  threshold,
  positiveCells,
  negativeCells,
  totalCells = NULL,
  stainType = "ki67",
  tissueType = "other",
  optimizationMethod = "cv",
  showSummaryTable = TRUE,
  showThresholdComparison = TRUE,
  showOptimalThreshold = TRUE,
  showPlot = TRUE,
  plotType = "line",
  showMethodology = TRUE,
  showReferences = FALSE,
  confidenceLevel = 0.95,
  minSampleAreas = 5
)
```

## Arguments

- data:

  .

- sampleArea:

  Identifier for each sample area examined (e.g., field1, field2, etc.).
  Multiple areas should be randomly selected across the tissue.

- threshold:

  Threshold level tested (e.g., 1, 2, 3 or actual intensity values like
  100, 150, 200). Multiple thresholds should be tested to find optimal
  discrimination.

- positiveCells:

  Number of cells classified as positive at this threshold level.

- negativeCells:

  Number of cells classified as negative at this threshold level.

- totalCells:

  Total number of cells counted. If not provided, calculated as
  positive + negative.

- stainType:

  Type of immunohistochemical stain being analyzed.

- tissueType:

  .

- optimizationMethod:

  Method for selecting optimal threshold: - CV: Choose threshold with
  lowest coefficient of variation across samples - Variance: Choose
  threshold with minimum variance in positive ratio - Consistency:
  Choose threshold where positive/negative ratio is most consistent

- showSummaryTable:

  .

- showThresholdComparison:

  Compare positive ratios across thresholds and sample areas.

- showOptimalThreshold:

  Display detailed analysis of optimal threshold selection.

- showPlot:

  Visualize positive ratios across thresholds and sample areas.

- plotType:

  .

- showMethodology:

  .

- showReferences:

  .

- confidenceLevel:

  .

- minSampleAreas:

  Minimum number of sample areas needed for reliable threshold
  determination. Kayser et al. (2009) recommend at least 5 areas.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$summaryTable`        |     |     |     |     | a table  |
| `results$thresholdComparison` |     |     |     |     | a table  |
| `results$optimalThreshold`    |     |     |     |     | a table  |
| `results$plot`                |     |     |     |     | an image |
| `results$methodology`         |     |     |     |     | a html   |
| `results$references`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`
