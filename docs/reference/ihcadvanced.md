# Advanced IHC Clustering Analysis

Advanced clustering analysis with marker optimization and validation.
Includes optimal K selection, PCA analysis, and iterative marker
selection.

## Usage

``` r
ihcadvanced(
  data,
  markers,
  id = NULL,
  optimalKMethod = "silhouette",
  kRange = "2:8",
  iterativeClustering = FALSE,
  pcaAnalysis = TRUE,
  consensusClustering = FALSE,
  nBootstrap = 100,
  clusterValidation = TRUE,
  showAdvancedPlots = TRUE,
  parallelProcessing = FALSE,
  randomSeed = 123
)
```

## Arguments

- data:

  the data as a data frame

- markers:

  Select IHC marker variables (e.g., ER, PR, HER2, Ki67, CD markers).
  Can be categorical scores (0/1+/2+/3+) or continuous values (H-scores,
  percentages).

- id:

  Case identifier for tracking

- optimalKMethod:

  Method for automatically determining optimal number of clusters

- kRange:

  Range of K values to test (e.g., "2:8" or "2,3,4,5")

- iterativeClustering:

  Perform iterative optimization to select most informative markers

- pcaAnalysis:

  Perform PCA for dimensionality reduction and visualization

- consensusClustering:

  Use bootstrap consensus clustering for stable results

- nBootstrap:

  Number of bootstrap iterations for consensus clustering

- clusterValidation:

  Perform multiple validation metrics (silhouette, connectivity, Dunn
  index)

- showAdvancedPlots:

  Display PCA, silhouette, and validation plots

- parallelProcessing:

  Enable parallel processing for computationally intensive tasks

- randomSeed:

  Set random seed for reproducible results

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$assumptions`        |     |     |     |     | a html   |
| `results$optimalKResults`    |     |     |     |     | a table  |
| `results$markerOptimization` |     |     |     |     | a table  |
| `results$pcaResults`         |     |     |     |     | a table  |
| `results$pcaLoadings`        |     |     |     |     | a table  |
| `results$validationMetrics`  |     |     |     |     | a table  |
| `results$consensusResults`   |     |     |     |     | a table  |
| `results$optimalKPlot`       |     |     |     |     | an image |
| `results$pcaPlot`            |     |     |     |     | an image |
| `results$silhouettePlot`     |     |     |     |     | an image |
| `results$validationPlot`     |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$optimalKResults$asDF`

`as.data.frame(results$optimalKResults)`
