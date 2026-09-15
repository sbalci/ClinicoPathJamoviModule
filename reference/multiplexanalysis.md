# Multiplex Immunofluorescence Analysis

Multiplex Immunofluorescence Analysis

## Usage

``` r
multiplexanalysis(
  data,
  marker_vars,
  x_coord = NULL,
  y_coord = NULL,
  cell_type_var = NULL,
  sample_id_var = NULL,
  group_var = NULL,
  analysis_focus = "comprehensive",
  correlation_method = "pearson",
  positivity_cutpoint = 0.1,
  perform_clustering = TRUE,
  n_clusters = 0,
  perform_pca = TRUE,
  show_loadings = TRUE,
  immune_contexture = FALSE,
  diversity_metrics = TRUE,
  spatial_analysis = FALSE,
  proximity_threshold = 20,
  show_plots = TRUE,
  show_clustering_plots = TRUE,
  biomarker_panel = "custom",
  normalization_method = "zscore",
  multiple_testing = "fdr",
  confidence_level = 0.95
)
```

## Arguments

- data:

  the data as a data frame

- marker_vars:

  expression levels or intensities for multiple biomarkers

- x_coord:

  X spatial coordinates for proximity analysis

- y_coord:

  Y spatial coordinates for proximity analysis

- cell_type_var:

  pre-defined cell type classifications

- sample_id_var:

  unique identifier for samples/regions

- group_var:

  grouping variable for comparative analysis

- analysis_focus:

  primary focus of multiplex analysis

- correlation_method:

  correlation method for co-expression analysis

- positivity_cutpoint:

  threshold for defining positive marker expression

- perform_clustering:

  identify cell populations through clustering

- n_clusters:

  number of clusters (0 = auto-determine)

- perform_pca:

  dimensionality reduction and visualization

- show_loadings:

  display loading vectors on PCA plot

- immune_contexture:

  calculate immunoscore and T-cell infiltration metrics

- diversity_metrics:

  Shannon and Simpson diversity for cellular composition

- spatial_analysis:

  cell-cell interaction analysis (requires coordinates)

- proximity_threshold:

  distance threshold for defining cell proximity

- show_plots:

  display correlation, PCA, and expression plots

- show_clustering_plots:

  display cluster analysis visualizations

- biomarker_panel:

  type of biomarker panel for optimized analysis

- normalization_method:

  normalization method for marker expressions

- multiple_testing:

  correction for multiple comparisons

- confidence_level:

  confidence level for statistical intervals

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$interpretation` |  |  |  |  | a html |
| `results$expressiontable` |  |  |  |  | Descriptive statistics for each biomarker |
| `results$correlationtable` |  |  |  |  | Pairwise correlations between biomarkers |
| `results$clusteringtable` |  |  |  |  | Identified cell clusters and phenotype suggestions |
| `results$pcatable` |  |  |  |  | PCA results and variance explained |
| `results$correlationplot` |  |  |  |  | Heatmap showing pairwise marker correlations |
| `results$heatmapplot` |  |  |  |  | Comprehensive marker expression patterns |
| `results$pcaplot` |  |  |  |  | PCA visualization with optional loading vectors |
| `results$clusterplot` |  |  |  |  | Visualization of identified cell populations |
| `results$spatialanalysis$proximityresults` |  |  |  |  | Distance-based interaction analysis |
| `results$spatialanalysis$spatialplot` |  |  |  |  | an image |
| `results$immunecontexture$immunescores` |  |  |  |  | T-cell infiltration and immune context metrics |
| `results$immunecontexture$immuneplot` |  |  |  |  | an image |
| `results$diversityanalysis$diversitytable` |  |  |  |  | Shannon, Simpson, and evenness indices |
| `results$qualitycontrol$qcmetrics` |  |  |  |  | Missing data, outliers, and technical quality |
| `results$qualitycontrol$batcheffects` |  |  |  |  | Analysis of systematic technical variation |
| `results$advancedanalysis$networkanalysis` |  |  |  |  | Network-based marker interaction analysis |
| `results$advancedanalysis$pathwayenrichment` |  |  |  |  | Biological pathway associations |
| `results$clinicalreport` |  |  |  |  | Copy-ready clinical interpretation and summary |
| `results$exportdata` |  |  |  |  | Formatted data for external analysis |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$expressiontable$asDF`

`as.data.frame(results$expressiontable)`
