# Clinical Heatmap

Clinical Heatmap

## Usage

``` r
clinicalheatmap(
  data,
  rowVar,
  colVar,
  valueVar,
  annotationCols = NULL,
  annotationRows = NULL,
  scaleMethod = "none",
  clusterRows = FALSE,
  clusterCols = FALSE,
  colorPalette = "viridis",
  showRownames = FALSE,
  showColnames = FALSE,
  naHandling = "exclude",
  showDataSummary = FALSE,
  showInterpretation = FALSE,
  showSummary = TRUE,
  showWorkflow = FALSE,
  plotWidth = 800,
  plotHeight = 600,
  clusterDistanceRows = "euclidean",
  clusterMethodRows = "complete",
  clusterDistanceCols = "euclidean",
  clusterMethodCols = "complete",
  annotationType = "tile",
  splitRows = 1,
  splitCols = 1,
  findOptimalK = FALSE,
  kMin = 2,
  kMax = 8,
  exportRowClusters = FALSE,
  exportColClusters = FALSE,
  rowClusterPrefix = "RowCluster",
  colClusterPrefix = "ColCluster",
  survivalAnalysis = FALSE,
  survivalTime = NULL,
  survivalEvent = NULL,
  survivalEventLevel,
  clusterComparison = FALSE,
  comparisonVars = NULL
)
```

## Arguments

- data:

  The dataset as a data frame containing the variables for analysis.

- rowVar:

  A string naming the variable from `data` that will define the heatmap
  rows (e.g., patient IDs, gene names).

- colVar:

  A string naming the variable from `data` that will define the heatmap
  columns (e.g., biomarkers, time points).

- valueVar:

  A string naming the numeric variable from `data` that will be
  visualized in the heatmap.

- annotationCols:

  Optional variables to use as column annotations (e.g., treatment
  groups, clinical stages).

- annotationRows:

  Optional variables to use as row annotations (e.g., patient
  characteristics, gene families).

- scaleMethod:

  Method for scaling the data values before visualization.

- clusterRows:

  Whether to perform hierarchical clustering on rows.

- clusterCols:

  Whether to perform hierarchical clustering on columns.

- colorPalette:

  Color palette for the heatmap visualization.

- showRownames:

  Whether to display row names in the heatmap.

- showColnames:

  Whether to display column names in the heatmap.

- naHandling:

  How to handle missing values in the data.

- showDataSummary:

  Display a summary of the data structure and values.

- showInterpretation:

  Display interpretation guidelines and clinical context.

- showSummary:

  Display a plain-language summary suitable for clinical documentation
  and reports.

- showWorkflow:

  Display step-by-step clinical workflow guidance for using heatmaps in
  practice.

- plotWidth:

  Width of heatmap plot in pixels.

- plotHeight:

  Height of heatmap plot in pixels.

- clusterDistanceRows:

  Distance metric for row clustering.

- clusterMethodRows:

  Agglomeration method for row clustering.

- clusterDistanceCols:

  Distance metric for column clustering.

- clusterMethodCols:

  Agglomeration method for column clustering.

- annotationType:

  Type of annotation visualization for tidyHeatmap.

- splitRows:

  Number of row groups to split based on dendrogram branches.

- splitCols:

  Number of column groups to split based on dendrogram branches.

- findOptimalK:

  Automatically determine optimal number of clusters using elbow method
  and silhouette analysis.

- kMin:

  Minimum number of clusters to evaluate when searching for the
  optimal K. Must be at least 2 and no greater than `kMax`.

- kMax:

  Maximum number of clusters to evaluate when searching for the
  optimal K. Must be at least `kMin` and no greater than 20.

- exportRowClusters:

  Add row cluster assignments as a new column in the dataset.

- exportColClusters:

  Add column cluster assignments as a new column in the dataset.

- rowClusterPrefix:

  Name for the exported row cluster assignment column.

- colClusterPrefix:

  Name for the exported column cluster assignment column.

- survivalAnalysis:

  Perform survival analysis comparing row clusters (requires survival
  data).

- survivalTime:

  .

- survivalEvent:

  .

- survivalEventLevel:

  .

- clusterComparison:

  Compare clinical/molecular characteristics across discovered clusters.

- comparisonVars:

  .

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$todo` |  |  |  |  | a html |
| `results$aboutAnalysis` |  |  |  |  | a html |
| `results$dataSummary$dataStructure` |  |  |  |  | a table |
| `results$dataSummary$valueSummary` |  |  |  |  | a table |
| `results$dataSummary$missingData` |  |  |  |  | a table |
| `results$heatmap` |  |  |  |  | an image |
| `results$annotations$columnAnnotations` |  |  |  |  | a html |
| `results$annotations$rowAnnotations` |  |  |  |  | a html |
| `results$interpretation` |  |  |  |  | a html |
| `results$clinicalSummary` |  |  |  |  | a html |
| `results$reportSentences` |  |  |  |  | a html |
| `results$plainSummary` |  |  |  |  | a html |
| `results$workflow` |  |  |  |  | a html |
| `results$assumptions` |  |  |  |  | a html |
| `results$optimalKAnalysis$elbowPlot` |  |  |  |  | an image |
| `results$optimalKAnalysis$silhouettePlot` |  |  |  |  | an image |
| `results$optimalKAnalysis$optimalKTable` |  |  |  |  | a table |
| `results$clusterAssignments$rowClusterTable` |  |  |  |  | a table |
| `results$clusterAssignments$colClusterTable` |  |  |  |  | a table |
| `results$clusterSurvival$kmPlot` |  |  |  |  | an image |
| `results$clusterSurvival$survivalTable` |  |  |  |  | a table |
| `results$clusterSurvival$logRankTest` |  |  |  |  | a table |
| `results$clusterCharacteristics$characteristicTable` |  |  |  |  | a table |
| `results$clusterCharacteristics$clusterInterpretation` |  |  |  |  | a html |
