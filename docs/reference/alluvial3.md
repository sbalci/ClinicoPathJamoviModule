# Alluvial & Sankey Diagrams

Alluvial & Sankey Diagrams

## Usage

``` r
alluvial3(
  data,
  vars,
  condensationvar,
  excl = FALSE,
  marg = FALSE,
  fill = "first_variable",
  bin = "default",
  orient = "vert",
  usetitle = FALSE,
  mytitle = "Alluvial Plot",
  maxvars = 8,
  custombinlabels = "",
  time,
  id,
  weight,
  sankeyStyle = FALSE,
  curveType = "cubic",
  engine = "easyalluvial",
  labelNodes = TRUE,
  showCounts = FALSE,
  verbose = TRUE,
  diagram_type = "alluvial",
  value_var,
  source_var,
  target_var,
  node_vars,
  grouping_var,
  time_var,
  node_width = 0.5,
  node_spacing = 0.1,
  edge_alpha = 0.6,
  color_palette = "default",
  show_labels = TRUE,
  label_size = 10,
  iterations = 32,
  show_values = TRUE,
  value_format = "raw",
  sort_nodes = "original",
  flow_direction = "left_right",
  plot_title = "",
  plot_subtitle = "",
  theme_style = "default",
  show_statistics = FALSE,
  show_interpretation = TRUE,
  output_format = "plot_only",
  clinical_mode = FALSE,
  clinical_preset = "none",
  enhanced_gradients = FALSE,
  flow_optimization = FALSE,
  advanced_diagnostics = FALSE,
  export_compatibility = "standard"
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  a string naming the variables from `data` that contains the values
  used for the Alluvial Diagram.

- condensationvar:

  The primary variable to be used for condensation.

- excl:

  Exclude missing values from the analysis.

- marg:

  Include marginal plots.

- fill:

  A list for the argument fill for selecting the variable to be
  represented by color. Default is 'first_variable'.

- bin:

  labels for the bins from low to high

- orient:

  Orientation of the plot. Default is 'vertical'.

- usetitle:

  Use a custom title for the plot.

- mytitle:

  Title for the plot.

- maxvars:

  Maximum number of variables to include in the alluvial plot.

- custombinlabels:

  Custom labels for bins, separated by commas (e.g., "Low,Medium,High").
  Leave empty to use bin option defaults.

- time:

  Optional time or sequence variable for time-based flows.

- id:

  Optional ID variable for tracking individual entities.

- weight:

  Optional weight variable for flow thickness.

- sankeyStyle:

  Apply Sankey diagram styling (narrow nodes, sigmoid curves).

- curveType:

  Curve style for flows. Use sigmoid for Sankey-style appearance.

- engine:

  Choose plotting engine: easyalluvial for automatic plots, ggalluvial
  for manual control.

- labelNodes:

  Show basic node labels. This is the primary label control that works
  with all plot engines.

- showCounts:

  Display count values on nodes.

- verbose:

  Show detailed information about the plot generation process.

- diagram_type:

  Type of flow diagram to generate.

- value_var:

  Variable containing flow values/weights for source-target format.

- source_var:

  Variable defining source nodes for source-target format.

- target_var:

  Variable defining target nodes for source-target format.

- node_vars:

  Variables for multi-level node definitions in source-target format.

- grouping_var:

  Variable for grouping flows.

- time_var:

  Variable for temporal flow analysis in source-target format.

- node_width:

  Width of nodes in the diagram.

- node_spacing:

  Spacing between nodes.

- edge_alpha:

  Transparency level for edges.

- color_palette:

  Color palette for the diagram.

- show_labels:

  Enable enhanced label styling with custom fonts and positioning.
  Complements basic labeling.

- label_size:

  Size of node labels.

- iterations:

  Number of iterations for layout optimization.

- show_values:

  Display flow values on diagram.

- value_format:

  Format for displaying flow values.

- sort_nodes:

  Method for sorting nodes.

- flow_direction:

  Direction of flow in the diagram.

- plot_title:

  Advanced plot title (overrides basic title when set).

- plot_subtitle:

  Subtitle for the plot.

- theme_style:

  Theme style for the plot.

- show_statistics:

  Display statistical summary of flows.

- show_interpretation:

  Display interpretation of the diagram.

- output_format:

  Format for output display.

- clinical_mode:

  Enable clinical-specific features and interpretations for medical data
  analysis.

- clinical_preset:

  Apply predefined settings optimized for common clinical scenarios.

- enhanced_gradients:

  Apply sophisticated color gradients to flow edges for better visual
  appeal.

- flow_optimization:

  Automatically optimize flow paths to reduce crossings and improve
  readability.

- advanced_diagnostics:

  Display detailed flow analysis, transition patterns, and data quality
  metrics.

- export_compatibility:

  Choose output format compatibility for integration with other tools.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$naturalSummary`      |     |     |     |     | a html   |
| `results$reportSentence`      |     |     |     |     | a html   |
| `results$caveats`             |     |     |     |     | a html   |
| `results$plot`                |     |     |     |     | an image |
| `results$plot2`               |     |     |     |     | an image |
| `results$datatab`             |     |     |     |     | a table  |
| `results$stats`               |     |     |     |     | a table  |
| `results$interpretation`      |     |     |     |     | a html   |
| `results$clinical_summary`    |     |     |     |     | a html   |
| `results$diagnostics_report`  |     |     |     |     | a html   |
| `results$flow_metrics`        |     |     |     |     | a table  |
| `results$transition_analysis` |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$datatab$asDF`

`as.data.frame(results$datatab)`
