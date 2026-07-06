# Patient Similarity Clustering

Visualizes patient similarity using dimensionality reduction techniques
(PCA, t-SNE, UMAP, MDS). Projects high-dimensional patient data into 2D
or 3D space to reveal natural patient groupings and subpopulations.
Inspired by Orange Data Mining's interactive projection widgets, adapted
for jamovi with comprehensive cluster analysis and statistical
validation.

## Usage

``` r
patientsimilarity(
  data,
  vars = NULL,
  seed = 123,
  method = "tsne",
  dimensions = "2",
  colorBy = NULL,
  perplexity = 30,
  iterations = 1000,
  umapNeighbors = 15,
  umapMinDist = 0.1,
  performClustering = FALSE,
  clusterMethod = "kmeans",
  nClusters = 3,
  dbscan_eps = 0.5,
  dbscan_minpts = 5,
  showClusterStats = FALSE,
  survivalAnalysis = FALSE,
  survivalTime = NULL,
  survivalEvent = NULL,
  survivalEventLevel,
  scaleVars = FALSE,
  removeOutliers = FALSE,
  showLoadings = FALSE,
  show3DPlot = FALSE
)
```

## Arguments

- data:

  The dataset to be analyzed, provided as a data frame.

- vars:

  Continuous variables to use for calculating patient similarity. These
  will be used to compute distances between patients. Categorical
  variables should be converted to numeric or one-hot encoded.

- seed:

  Random seed for reproducibility of t-SNE, UMAP, and clustering
  results.

- method:

  Method for dimensionality reduction: - PCA: Linear method, preserves
  global structure - t-SNE: Non-linear, excellent for visualization,
  preserves local structure - UMAP: Non-linear, preserves both local and
  global structure, faster than t-SNE - MDS: Classical method, preserves
  pairwise distances

- dimensions:

  Number of dimensions for projection. 2D is easier to interpret, 3D can
  reveal additional structure.

- colorBy:

  Variable to use for coloring points. Typically an outcome variable
  (e.g., disease status, survival, response) to see if it corresponds to
  natural patient groupings.

- perplexity:

  Perplexity parameter for t-SNE. Roughly corresponds to the number of
  nearest neighbors considered. Typical values: 5-50. Higher values
  preserve more global structure.

- iterations:

  Number of iterations for t-SNE optimization. More iterations improve
  convergence but take longer.

- umapNeighbors:

  Number of nearest neighbors for UMAP. Controls local vs global
  structure. Smaller values preserve local structure, larger values
  preserve global.

- umapMinDist:

  Minimum distance between points in UMAP. Controls how tightly points
  are packed. Smaller values create tighter clusters.

- performClustering:

  Automatically identify patient clusters using k-means or hierarchical
  clustering on the reduced-dimension space.

- clusterMethod:

  Method for clustering patients in the reduced space.

- nClusters:

  Number of clusters for k-means or hierarchical clustering. For DBSCAN,
  this is ignored.

- dbscan_eps:

  The radius of the neighborhood around a point (eps) for DBSCAN.

- dbscan_minpts:

  The minimum number of points required to form a dense region (MinPts)
  for DBSCAN.

- showClusterStats:

  Display summary statistics for each cluster including size,
  characteristics, and outcome distribution.

- survivalAnalysis:

  If survival data is available, compare survival across discovered
  clusters. Useful for identifying prognostic patient subtypes.

- survivalTime:

  Time to event or censoring for survival analysis.

- survivalEvent:

  Event indicator (1=event, 0=censored).

- survivalEventLevel:

  Level indicating the event occurred.

- scaleVars:

  Standardize variables to mean=0, sd=1 before analysis. Recommended
  when variables have different scales.

- removeOutliers:

  Remove outliers before analysis using IQR method. May improve
  visualization quality.

- showLoadings:

  Show how original variables contribute to each dimension. Only
  available for PCA and MDS.

- show3DPlot:

  Generate interactive 3D plot using plotly (if dimensions=3).

## Value

A results object containing:

|                                  |     |     |     |     |                |
|----------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                |     |     |     |     | a preformatted |
| `results$instructions`           |     |     |     |     | a html         |
| `results$summaryText`            |     |     |     |     | a preformatted |
| `results$projectionPlot`         |     |     |     |     | an image       |
| `results$projection3D`           |     |     |     |     | an image       |
| `results$varianceTable`          |     |     |     |     | a table        |
| `results$loadingsTable`          |     |     |     |     | a table        |
| `results$clusterHeading`         |     |     |     |     | a html         |
| `results$clusterSummary`         |     |     |     |     | a table        |
| `results$clusterCharacteristics` |     |     |     |     | a table        |
| `results$clusterOutcomes`        |     |     |     |     | a table        |
| `results$clusterQuality`         |     |     |     |     | a table        |
| `results$survivalHeading`        |     |     |     |     | a html         |
| `results$survivalTable`          |     |     |     |     | a table        |
| `results$survivalPlot`           |     |     |     |     | an image       |
| `results$survivalComparison`     |     |     |     |     | a table        |
| `results$exportClusters`         |     |     |     |     | an output      |
| `results$exportCoordinates`      |     |     |     |     | an output      |
| `results$interpretation`         |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$varianceTable$asDF`

`as.data.frame(results$varianceTable)`
