# Correlation Network Analysis

Comprehensive network analysis of correlation structures using multiple
algorithms including Gaussian Graphical Models (GGM), LASSO regularized
networks, and partial correlations. Provides network metrics, community
detection, interactive visualizations, and network comparison tests.
Suitable for understanding complex relationships between variables
through network topology.

## Usage

``` r
correlationnetwork(
  data,
  vars,
  group = NULL,
  networkMethod = "correlation",
  correlationMethod = "pearson",
  threshold = 0.1,
  significanceThreshold = 0.05,
  onlySignificant = FALSE,
  centralityMeasures = TRUE,
  communityDetection = TRUE,
  communityMethod = "walktrap",
  networkComparison = FALSE,
  plots = TRUE,
  layoutAlgorithm = "spring",
  nodeSize = "degree",
  edgeWidth = "weight",
  colorNodes = TRUE,
  interactive = FALSE,
  report = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  A vector of strings naming the variables to include in the network
  analysis.

- group:

  Variable to split the analysis by for network comparison.

- networkMethod:

  Method for constructing the network: 'correlation' (default),
  'partial', 'ggm', or 'lasso'.

- correlationMethod:

  The correlation method to use: 'pearson' (default), 'spearman', or
  'kendall'.

- threshold:

  Minimum absolute correlation value to include as edge in network
  (default: 0.1).

- significanceThreshold:

  Alpha level for edge significance testing (default: 0.05).

- onlySignificant:

  FALSE (default) or TRUE, show only statistically significant edges.

- centralityMeasures:

  TRUE (default) or FALSE, calculate node centrality measures.

- communityDetection:

  TRUE (default) or FALSE, perform community detection analysis.

- communityMethod:

  Method for community detection: 'walktrap' (default), 'louvain',
  'leiden', 'spinglass', or 'betweenness'.

- networkComparison:

  FALSE (default) or TRUE, perform network comparison tests when
  grouping variable is provided.

- plots:

  TRUE (default) or FALSE, generate network visualization plots.

- layoutAlgorithm:

  Layout algorithm for network visualization: 'spring' (default),
  'circle', 'nicely', 'kk', or 'lgl'.

- nodeSize:

  Method for sizing nodes: 'constant', 'degree' (default),
  'betweenness', 'closeness', or 'eigenvector'.

- edgeWidth:

  Method for edge width: 'weight' (default) or 'constant'.

- colorNodes:

  TRUE (default) or FALSE, color nodes by detected communities.

- interactive:

  FALSE (default) or TRUE, generate interactive network plots.

- report:

  TRUE (default) or FALSE, provide natural language interpretation of
  network analysis.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$networkSummary` |  |  |  |  | overview of network structure and properties |
| `results$adjacencyMatrix` |  |  |  |  | network adjacency matrix showing edge weights |
| `results$centralityMeasures` |  |  |  |  | node centrality metrics for network analysis |
| `results$communityStructure` |  |  |  |  | community detection results and modularity |
| `results$communityStats` |  |  |  |  | summary statistics for detected communities |
| `results$networkComparison` |  |  |  |  | comparison of networks across groups |
| `results$report` |  |  |  |  | a html |
| `results$networkPlot` |  |  |  |  | an image |
| `results$interactiveNetworkPlot` |  |  |  |  | a html |
| `results$centralityPlot` |  |  |  |  | an image |
| `results$communityPlot` |  |  |  |  | an image |
| `results$adjacencyHeatmap` |  |  |  |  | an image |
| `results$networkMetricsPlot` |  |  |  |  | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$networkSummary$asDF`

`as.data.frame(results$networkSummary)`

## Examples

``` r
# Basic correlation network analysis
correlationnetwork(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", "MeasurementC")
)
#> Error in initialize(...): unused arguments (width = 900, height = 700, renderFun = ".interactiveNetworkPlot")

# With LASSO regularization and community detection
correlationnetwork(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", "MeasurementC"),
    networkMethod = "lasso",
    communityDetection = TRUE,
    centralityMeasures = TRUE
)
#> Error in initialize(...): unused arguments (width = 900, height = 700, renderFun = ".interactiveNetworkPlot")
```
