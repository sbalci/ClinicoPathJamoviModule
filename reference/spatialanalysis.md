# Spatial Statistics from Coordinates

Spatial Statistics from Coordinates

## Usage

``` r
spatialanalysis(
  data,
  coords_x,
  coords_y,
  cell_types,
  groups,
  roi_id,
  perform_ripley = TRUE,
  perform_nnd = TRUE,
  perform_hotspot = TRUE,
  perform_interaction = TRUE,
  show_plots = TRUE,
  analysis_scope = "comprehensive",
  distance_method = "euclidean",
  correction_method = "both",
  significance_level = 0.05,
  min_points = 10
)
```

## Arguments

- data:

  the data as a data frame

- coords_x:

  X coordinate variable (numeric). Must contain spatial position data
  for each observation.

- coords_y:

  Y coordinate variable (numeric). Must contain spatial position data
  for each observation.

- cell_types:

  Optional cell type variable for multi-type spatial analysis. Should be
  a factor or character variable identifying different cell populations.

- groups:

  Optional grouping variable for comparing spatial patterns between
  conditions (e.g., treatment vs control, tumor vs normal).

- roi_id:

  Optional ROI identifier for analyzing multiple tissue regions
  separately.

- perform_ripley:

  Perform Ripley's K-function analysis to detect spatial clustering or
  dispersion patterns.

- perform_nnd:

  Calculate nearest neighbor distances and perform Clark-Evans test for
  spatial randomness.

- perform_hotspot:

  Detect spatial hotspots using kernel density estimation and
  statistical thresholds.

- perform_interaction:

  Perform multi-type spatial interaction analysis when cell types are
  specified.

- show_plots:

  Create spatial distribution plots showing coordinate data with
  optional cell type coloring.

- analysis_scope:

  Analysis scope: basic (core statistics), comprehensive (all methods),
  or clinical (pathology-focused interpretation).

- distance_method:

  Distance calculation method for spatial analysis.

- correction_method:

  Edge correction method for handling boundary effects in spatial
  analysis.

- significance_level:

  Alpha level for statistical significance testing.

- min_points:

  Minimum number of complete coordinate pairs required to perform
  spatial analysis.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$text` |  |  |  |  | a html |
| `results$copysummary` |  |  |  |  | Plain-language summary of spatial analysis results ready for copy-paste |
| `results$summary` |  |  |  |  | Basic spatial metrics including point counts, density, and study area |
| `results$ripley` |  |  |  |  | Spatial clustering analysis at multiple distance scales |
| `results$nearestneighbor` |  |  |  |  | Nearest neighbor distances and Clark-Evans randomness test |
| `results$hotspots` |  |  |  |  | Spatial hotspot analysis using kernel density estimation |
| `results$interaction` |  |  |  |  | Cross-type spatial relationships and interaction patterns |
| `results$spatialplot` |  |  |  |  | Visualization of spatial coordinate data with optional cell type coloring |
| `results$interpretation` |  |  |  |  | Clinical significance and pathology applications of spatial patterns |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
