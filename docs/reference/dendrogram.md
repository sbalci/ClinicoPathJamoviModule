# Dendrogram

Dendrogram

## Usage

``` r
dendrogram(
  data,
  vars,
  clusterMethod = "ward.D2",
  distanceMethod = "euclidean",
  standardize = TRUE,
  showLabels = TRUE,
  colorGroups = FALSE,
  group = NULL,
  plotHeight = 600,
  plotWidth = 800,
  plotType = "linear",
  edgeType = "diagonal",
  colorScheme = "default",
  highlightClusters = FALSE,
  nClusters = 3,
  maxLabels = 50,
  showRowDendro = TRUE,
  showColDendro = TRUE,
  heatmapScale = "row",
  heatmapPalette = "bluered",
  showCellBorders = FALSE
)
```

## Arguments

- data:

  .

- vars:

  .

- clusterMethod:

  .

- distanceMethod:

  .

- standardize:

  .

- showLabels:

  .

- colorGroups:

  .

- group:

  .

- plotHeight:

  .

- plotWidth:

  .

- plotType:

  .

- edgeType:

  .

- colorScheme:

  .

- highlightClusters:

  .

- nClusters:

  .

- maxLabels:

  .

- showRowDendro:

  .

- showColDendro:

  .

- heatmapScale:

  .

- heatmapPalette:

  .

- showCellBorders:

  .

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$notices`        |     |     |     |     | a preformatted |
| `results$welcome`        |     |     |     |     | a html         |
| `results$plot`           |     |     |     |     | an image       |
| `results$clusterInfo`    |     |     |     |     | a html         |
| `results$summary`        |     |     |     |     | a table        |
| `results$clusterSummary` |     |     |     |     | a table        |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
