# Spatial Autocorrelation Analysis (Moran's I, Geary's C)

Comprehensive spatial autocorrelation analysis for tissue pattern
analysis and digital pathology. Implements Moran's I, Geary's C, and
local indicators of spatial association (LISA) for detecting spatial
clustering and patterns in histopathological data. Essential for
analyzing spatial dependencies in tissue architecture and cellular
distributions.

## Usage

``` r
spatialautocorrelation(
  data,
  measurement,
  x_coordinate,
  y_coordinate,
  region_id,
  time_point,
  autocorr_method = "morans_i",
  spatial_weights = "k_nearest",
  distance_threshold = 50,
  k_neighbors = 8,
  bandwidth = 25,
  significance_test = "permutation_test",
  permutations = 999,
  confidence_level = 0.95,
  local_analysis = TRUE,
  cluster_detection = TRUE,
  hotspot_analysis = TRUE,
  spatial_regimes = FALSE,
  temporal_analysis = FALSE,
  multivariate_analysis = FALSE,
  robustness_check = TRUE,
  edge_effects = TRUE,
  clinical_interpretation = TRUE,
  spatial_plots = TRUE,
  autocorr_plots = TRUE,
  lisa_plots = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- measurement:

  Continuous measurement for spatial autocorrelation analysis

- x_coordinate:

  X spatial coordinate for location data

- y_coordinate:

  Y spatial coordinate for location data

- region_id:

  Region or tissue identifier for stratified analysis

- time_point:

  Time point identifier for temporal spatial analysis

- autocorr_method:

  Spatial autocorrelation method to compute

- spatial_weights:

  Method for constructing spatial weights matrix

- distance_threshold:

  Distance threshold for spatial neighbor definition

- k_neighbors:

  Number of nearest neighbors for spatial weights

- bandwidth:

  Bandwidth for Gaussian kernel weights

- significance_test:

  Method for statistical significance testing

- permutations:

  Number of permutations for significance testing

- confidence_level:

  Confidence level for statistical inference

- local_analysis:

  Compute local indicators of spatial association

- cluster_detection:

  Detect and characterize spatial clusters

- hotspot_analysis:

  Identify spatial hot spots and cold spots

- spatial_regimes:

  Identify spatially distinct regimes or patterns

- temporal_analysis:

  Analyze changes in spatial autocorrelation over time

- multivariate_analysis:

  Analyze spatial relationships between multiple variables

- robustness_check:

  Test robustness to different spatial weight specifications

- edge_effects:

  Apply corrections for edge effects in spatial analysis

- clinical_interpretation:

  Provide clinical interpretation of spatial patterns

- spatial_plots:

  Generate spatial pattern visualization maps

- autocorr_plots:

  Create spatial autocorrelation function plots

- lisa_plots:

  Generate LISA cluster and significance maps

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$dataInfo`               |     |     |     |     | a table  |
| `results$globalAutocorrelation`  |     |     |     |     | a table  |
| `results$localIndicators`        |     |     |     |     | a table  |
| `results$spatialClusters`        |     |     |     |     | a table  |
| `results$hotspotAnalysis`        |     |     |     |     | a table  |
| `results$spatialRegimes`         |     |     |     |     | a table  |
| `results$robustnessAnalysis`     |     |     |     |     | a table  |
| `results$temporalAnalysis`       |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a table  |
| `results$spatialMap`             |     |     |     |     | an image |
| `results$autocorrPlot`           |     |     |     |     | an image |
| `results$lisaMap`                |     |     |     |     | an image |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
