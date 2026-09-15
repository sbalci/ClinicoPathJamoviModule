# Stereology Analysis

Stereology Analysis

## Usage

``` r
stereology(
  data,
  intersections,
  totalPoints,
  boundaryIntersections = NULL,
  objectCount = NULL,
  referenceArea,
  sectionThickness = 5,
  gridSpacing,
  tissueType = "vessels",
  groupVar = NULL,
  calculateAa = TRUE,
  calculateVv = TRUE,
  calculateBa = FALSE,
  calculateNa = FALSE,
  calculateSv = FALSE,
  showConfidenceIntervals = TRUE,
  bootstrapIterations = 1000,
  showGroupComparison = FALSE,
  showDescriptives = TRUE,
  showPlot = TRUE,
  plotType = "bars",
  showMethodology = TRUE,
  showReferences = FALSE
)
```

## Arguments

- data:

  .

- intersections:

  Number of grid intersections (hits) with the structure of interest
  (e.g., vessels, nuclei, fibrosis areas).

- totalPoints:

  Total number of grid points or test lines used in the measurement.

- boundaryIntersections:

  Number of grid line intersections with object boundaries (for boundary
  density calculation).

- objectCount:

  Number of discrete objects counted (for numerical density
  calculation).

- referenceArea:

  Total reference area examined (in square micrometers). Typically
  calculated as: image width × height.

- sectionThickness:

  Histological section thickness in micrometers. Required for volume
  density calculations. Default: 5 μm.

- gridSpacing:

  Distance between grid lines in micrometers. Used for length
  calculations in boundary/surface density.

- tissueType:

  Type of tissue structure being quantified. This helps provide
  context-specific interpretation.

- groupVar:

  Optional grouping variable to compare stereology parameters across
  groups (e.g., diagnosis, treatment, grade).

- calculateAa:

  Area density: proportion of reference area occupied by structure.
  Formula: Aa = intersection count / total points.

- calculateVv:

  Volume density: proportion of reference volume occupied by structure.
  In isotropic sections, Vv = Aa.

- calculateBa:

  Boundary density: boundary length per unit area. Requires boundary
  intersection data.

- calculateNa:

  Numerical density: number of objects per unit area. Requires object
  count data.

- calculateSv:

  Surface density: surface area per unit volume. Formula: Sv = 2 × Ba
  (for isotropic sections).

- showConfidenceIntervals:

  Calculate and display 95 percent confidence intervals for
  stereological estimates using bootstrap methods.

- bootstrapIterations:

  Number of bootstrap iterations for confidence interval estimation.

- showGroupComparison:

  Compare stereological parameters across groups (requires grouping
  variable).

- showDescriptives:

  .

- showPlot:

  Display bar chart of stereological parameters with confidence
  intervals.

- plotType:

  .

- showMethodology:

  Display detailed explanation of stereological methods, formulas, and
  interpretation guidelines.

- showReferences:

  Display literature references for stereological methods.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$descriptives`    |     |     |     |     | a table  |
| `results$stereologyTable` |     |     |     |     | a table  |
| `results$groupComparison` |     |     |     |     | a table  |
| `results$groupTests`      |     |     |     |     | a table  |
| `results$plot`            |     |     |     |     | an image |
| `results$methodology`     |     |     |     |     | a html   |
| `results$references`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptives$asDF`

`as.data.frame(results$descriptives)`
