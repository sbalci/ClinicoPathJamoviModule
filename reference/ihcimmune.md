# IHC Immune Analysis

Tumor-infiltrating lymphocyte (TIL) analysis and immune microenvironment
characterization using IHC markers. Specialized for immuno-oncology
applications.

## Usage

``` r
ihcimmune(
  data,
  immune_markers,
  checkpoint_markers,
  id = NULL,
  tilAnalysis = TRUE,
  tilMethod = "comprehensive",
  immuneContexture = TRUE,
  spatialAnalysis = FALSE,
  x_coordinate,
  y_coordinate,
  tumorRegion,
  hotspotAnalysis = FALSE,
  diversityMetrics = TRUE,
  checkpointScore = FALSE,
  pd1Cutoff = 1,
  pdl1Cutoff = 1,
  cd3Threshold = 10,
  cd8Threshold = 5,
  immuneScoreThreshold = 10
)
```

## Arguments

- data:

  the data as a data frame

- immune_markers:

  Immune cell markers (CD3, CD4, CD8, CD20, etc.)

- checkpoint_markers:

  Immune checkpoint markers (PD-1, PD-L1, CTLA-4, etc.)

- id:

  Case identifier for tracking

- tilAnalysis:

  Perform specialized tumor-infiltrating lymphocyte analysis

- tilMethod:

  Method for TIL analysis and interpretation

- immuneContexture:

  Calculate immune contexture score and classification

- spatialAnalysis:

  Analyze spatial distribution of immune cells (requires coordinate
  data)

- x_coordinate:

  X spatial coordinates for proximity analysis

- y_coordinate:

  Y spatial coordinates for proximity analysis

- tumorRegion:

  Tumor region classification (center, invasive margin, stroma)

- hotspotAnalysis:

  Identify immune cell hotspots and cold regions

- diversityMetrics:

  Calculate Shannon and Simpson diversity indices for immune populations

- checkpointScore:

  Calculate composite score for checkpoint inhibitor therapy prediction

- pd1Cutoff:

  Threshold for PD-1 positivity assessment

- pdl1Cutoff:

  Threshold for PD-L1 positivity assessment

- cd3Threshold:

  Threshold for CD3+ TIL categorization (Low vs Intermediate)

- cd8Threshold:

  Threshold for CD8+ TIL categorization (Low vs Intermediate)

- immuneScoreThreshold:

  Threshold for overall immune infiltration categorization

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$clinicalContext`    |     |     |     |     | a html   |
| `results$tilSummary`         |     |     |     |     | a table  |
| `results$immuneProfile`      |     |     |     |     | a table  |
| `results$checkpointAnalysis` |     |     |     |     | a table  |
| `results$immuneContexture`   |     |     |     |     | a table  |
| `results$spatialMetrics`     |     |     |     |     | a table  |
| `results$hotspotAnalysis`    |     |     |     |     | a table  |
| `results$diversityResults`   |     |     |     |     | a table  |
| `results$tilMethodResults`   |     |     |     |     | a table  |
| `results$immunePlot`         |     |     |     |     | an image |
| `results$tilPlot`            |     |     |     |     | an image |
| `results$contextureHeatmap`  |     |     |     |     | an image |
| `results$spatialPlot`        |     |     |     |     | an image |
| `results$checkpointPlot`     |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$tilSummary$asDF`

`as.data.frame(results$tilSummary)`
