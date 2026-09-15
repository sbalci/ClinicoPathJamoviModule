# Alluvial Diagrams

Alluvial Diagrams

## Usage

``` r
alluvial(
  data,
  vars,
  condensationvar = NULL,
  excl = FALSE,
  marg = FALSE,
  fill = "first_variable",
  fillGgalluvial = NULL,
  bin = "default",
  orient = "vert",
  usetitle = FALSE,
  mytitle = "Alluvial Plot",
  maxvars = 8,
  custombinlabels = "",
  colorPalette = "default",
  showCounts = FALSE,
  themeStyle = "default",
  enhancedGradients = FALSE,
  plotSubtitle = "",
  weight = NULL,
  sankeyStyle = FALSE,
  curveType = "cubic",
  flowDirection = "left_right",
  engine = "easyalluvial",
  labelNodes = FALSE
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

- fillGgalluvial:

  A string naming the categorical variable from `data` that will be used
  to color the flows in the ggalluvial plot.

- bin:

  Display labels for bin categories. Note: This controls label text
  only, not the binning method. easyalluvial uses its own internal
  binning algorithm. For custom binning, create categorized variables
  before analysis.

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
  The number of labels determines the number of bins. Leave empty to use
  the selected bin-label method.

- colorPalette:

  Color palette for the diagram flows.

- showCounts:

  Display count values on nodes.

- themeStyle:

  Theme style for the plot background and elements.

- enhancedGradients:

  Apply sophisticated color gradients to flow edges for better visual
  appeal.

- plotSubtitle:

  Subtitle for the plot (shown below the title).

- weight:

  Optional weight variable for flow thickness. If not provided, counts
  are used.

- sankeyStyle:

  Apply Sankey diagram styling (narrow nodes, sigmoid curves).

- curveType:

  Curve style for flows. Requires ggalluvial engine.

- flowDirection:

  Direction of flow in the diagram.

- engine:

  Choose plotting engine: easyalluvial for automatic plots, ggalluvial
  for manual control with curve types and Sankey styling.

- labelNodes:

  Show node labels (for ggalluvial engine).

## Value

A results object containing:

|                               |     |     |     |     |                |
|-------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`             |     |     |     |     | a preformatted |
| `results$todo`                |     |     |     |     | a html         |
| `results$dataWarning`         |     |     |     |     | a html         |
| `results$plot`                |     |     |     |     | an image       |
| `results$condensationWarning` |     |     |     |     | a html         |
| `results$plot2`               |     |     |     |     | an image       |
