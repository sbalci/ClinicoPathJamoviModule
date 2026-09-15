# River Plots & Alluvial Diagrams

Creates comprehensive river plots, alluvial diagrams, and stream charts
to visualize flows, transitions, and temporal changes in categorical
data. Supports individual entity tracking, aggregate trends, and
multi-stage pathways. Perfect for patient journeys, treatment
progressions, categorical changes over time, and flow analysis.

## Usage

``` r
riverplot(
  data,
  id = NULL,
  time = NULL,
  strata = list(),
  weight = NULL,
  plotType = "alluvial",
  fillType = "first",
  dataFormat = "auto",
  sortStreams = TRUE,
  labelNodes = TRUE,
  curveType = "cubic",
  showCounts = FALSE,
  showPercentages = FALSE,
  showLegend = TRUE,
  legendPosition = "right",
  colorScheme = "default",
  customColors = "",
  nodeWidth = 0.1,
  nodeGap = 0.05,
  flowAlpha = 0.7,
  mytitle = "",
  xtitle = "",
  ytitle = "",
  originaltheme = FALSE,
  fontSize = 12,
  plotWidth = 10,
  plotHeight = 8,
  enableDiagnostics = FALSE,
  edgeGradient = TRUE,
  nodeStyle = "regular",
  edgeStyle = "gradient",
  gravity = "center",
  addMidPoints = FALSE,
  reorderEdges = FALSE,
  curveGranularity = 100,
  backgroundLabels = FALSE,
  exportRiverplotObject = FALSE,
  clinicalPreset = "none",
  smart_detection = TRUE,
  enhanced_validation = TRUE,
  multi_format_support = FALSE,
  adaptive_styling = TRUE,
  quality_optimization = FALSE,
  cross_reference_mode = FALSE,
  high_contrast_mode = FALSE,
  large_dataset_mode = FALSE,
  enable_caching = TRUE,
  detailed_progress = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- id:

  Optional identifier for tracking individual entities (patients, cases,
  customers) through transitions. Required for individual-level flow
  tracking. Leave empty for aggregate flow visualization.

- time:

  Variable representing time points or sequential stages (visit dates,
  follow-up periods, sequential stages). Required for longitudinal data
  with single strata variable. Leave empty when using wide format data
  with multiple strata variables.

- strata:

  Variables containing the categories that flow between stages. For
  longitudinal data: single variable with categories
  (treatment_response, disease_stage). For wide format: multiple
  variables representing sequential stages (baseline_status,
  month3_status, month6_status). Each should be a factor with meaningful
  category names.

- weight:

  Optional numerical variable to determine flow width proportional to
  values. Examples: patient count, cost, severity score, revenue. If not
  provided, flows represent equal counts/frequencies. Use for
  emphasizing quantitative importance of different pathways.

- plotType:

  Type of flow visualization. 'alluvial' creates flowing streams with
  curved connections, ideal for transitions. 'sankey' creates directed
  flow diagrams with straighter connections, good for process flows.
  'stream' shows aggregate category trends over time. 'flow' tracks
  individual entities through time periods.

- fillType:

  Determines how colors are assigned to flows. 'first' colors flows
  based on initial category (tracking origins). 'last' colors by final
  category (tracking destinations). 'frequency' uses flow volume for
  colors (emphasizes major pathways).

- dataFormat:

  Format of input data. 'long' format has rows for each time point with
  single strata variable. 'wide' format has multiple strata variables as
  columns representing different stages. 'auto' detects format based on
  variables provided.

- sortStreams:

  Sort alluvial streams by frequency/size. Larger flows are positioned
  more prominently, making major pathways easier to identify.
  Recommended for highlighting dominant patterns.

- labelNodes:

  Display category labels on nodes (vertical bars). Labels identify what
  each segment represents. Recommended unless space is limited or labels
  overlap.

- curveType:

  Shape of connecting curves between stages. 'cardinal' creates smooth
  flowing curves (most aesthetic). 'sin' creates sinusoidal curves (CRAN
  riverplot style). 'riverplot' uses the advanced curve generation from
  CRAN riverplot package.

- showCounts:

  Display numerical counts or values on nodes. Shows exact frequencies
  or sums for each category at each stage. Useful for precise
  interpretation but may clutter visualization with many categories.

- showPercentages:

  Display percentages on nodes or flows. Shows relative proportions
  within each stage or flow. Complement to absolute counts for better
  interpretation.

- showLegend:

  Display color legend explaining category mappings. Recommended when
  colors represent meaningful categories. Can be hidden to save space if
  category labels are clearly visible.

- legendPosition:

  Position of the color legend relative to the plot.

- colorScheme:

  Color palette for categories. Choose based on data type and aesthetic
  preference.

- customColors:

  Comma-separated hex colors for custom color scheme (e.g.,
  "#1f77b4,#ff7f0e,#2ca02c"). Only used when Color Scheme is set to
  'Custom'.

- nodeWidth:

  Width of nodes (vertical bars) as proportion of plot width. Adjust for
  better balance between nodes and flows.

- nodeGap:

  Gap between categories within each node as proportion of node height.
  Smaller values create more compact nodes.

- flowAlpha:

  Transparency of flow connections (0 = fully transparent, 1 = opaque).
  Lower values help see overlapping flows.

- mytitle:

  Main title for the plot. Leave empty for no title.

- xtitle:

  Label for x-axis (time/stage axis). Leave empty for default label.

- ytitle:

  Label for y-axis (frequency/count axis). Leave empty for default
  label.

- originaltheme:

  Use original ggalluvial package theme instead of minimal theme. May
  provide better default styling for some plot types.

- fontSize:

  Base font size for labels and text in the plot.

- plotWidth:

  Width of the plot in inches for export.

- plotHeight:

  Height of the plot in inches for export.

- enableDiagnostics:

  Display diagnostic information about data processing, flow
  calculations, and potential issues. Useful for troubleshooting complex
  datasets.

- edgeGradient:

  Apply color gradients to edges for smoother visual transitions. Based
  on CRAN riverplot package gradient capabilities.

- nodeStyle:

  Visual style for nodes. 'regular' shows filled rectangles, 'point'
  shows small circles, 'invisible' hides nodes but maintains
  connections.

- edgeStyle:

  Visual style for edges. 'gradient' creates smooth color transitions,
  'sin' varies edge thickness sinusoidally for aesthetic effect.

- gravity:

  Vertical alignment strategy for nodes within each stage. Affects how
  nodes are positioned relative to each other.

- addMidPoints:

  Add intermediate invisible nodes to create smoother edge paths. Helps
  reduce edge crossings and improve visual clarity (CRAN riverplot
  feature).

- reorderEdges:

  Automatically attempt to disentangle and optimize edge routing to
  reduce crossings. Uses algorithms from CRAN riverplot package.

- curveGranularity:

  Number of points used to generate smooth curves. Higher values create
  smoother curves but increase rendering time.

- backgroundLabels:

  Add background boxes to labels for better readability, similar to CRAN
  riverplot's bglabel functionality.

- exportRiverplotObject:

  Create and display a riverplot-compatible object structure for use
  with the CRAN riverplot package.

- clinicalPreset:

  Apply predefined configurations optimized for common clinical
  scenarios. Each preset automatically configures plot type, styling,
  and display options for the selected clinical use case.

- smart_detection:

  Automatically detect optimal data format and analysis approach based
  on provided variables.

- enhanced_validation:

  Apply comprehensive data validation with user-friendly error messages
  and suggestions.

- multi_format_support:

  Enable experimental support for additional data formats
  (source-target, multi-node).

- adaptive_styling:

  Automatically adapt plot styling based on data characteristics and
  size.

- quality_optimization:

  Apply advanced algorithms to optimize flow layout, reduce crossings,
  and improve readability.

- cross_reference_mode:

  Show suggestions for when alluvial function might be more appropriate
  for the data.

- high_contrast_mode:

  Enable high contrast colors and larger fonts for better accessibility
  and readability.

- large_dataset_mode:

  Enable memory-efficient processing for datasets with \>10,000
  observations. May reduce rendering detail for performance.

- enable_caching:

  Cache computation results to improve performance with repeated
  analysis of the same data.

- detailed_progress:

  Show detailed progress information during long-running operations.
  Useful for very large datasets.

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                        |     |     |     |     | a html   |
| `results$summary`                     |     |     |     |     | a html   |
| `results$reportSentence`              |     |     |     |     | a html   |
| `results$about_analysis`              |     |     |     |     | a html   |
| `results$plot`                        |     |     |     |     | an image |
| `results$diagnostics`                 |     |     |     |     | a html   |
| `results$flowTable`                   |     |     |     |     | a table  |
| `results$stageTable`                  |     |     |     |     | a table  |
| `results$transitionMatrix`            |     |     |     |     | a table  |
| `results$riverplotObject`             |     |     |     |     | a html   |
| `results$riverplotCode`               |     |     |     |     | a html   |
| `results$validation_report`           |     |     |     |     | a html   |
| `results$cross_reference_suggestions` |     |     |     |     | a html   |
| `results$optimization_report`         |     |     |     |     | a html   |
| `results$clinical_insights`           |     |     |     |     | a html   |
| `results$enhanced_caveats`            |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$flowTable$asDF`

`as.data.frame(results$flowTable)`
