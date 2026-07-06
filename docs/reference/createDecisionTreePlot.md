# Create Decision Tree Plot

Generates the main decision tree visualization using network layout
algorithms.

## Usage

``` r
createDecisionTreePlot(
  treeData,
  layout = "horizontal",
  colorScheme = "medical",
  nodeShapes = TRUE,
  showProbabilities = TRUE,
  showCosts = TRUE,
  showUtilities = TRUE
)
```

## Arguments

- treeData:

  List containing nodes and edges data

- layout:

  Character string specifying layout type ("horizontal", "vertical",
  "radial")

- colorScheme:

  Character string for color theme

- nodeShapes:

  Logical, whether to use different shapes for node types

- showProbabilities:

  Logical, whether to display probabilities on branches

- showCosts:

  Logical, whether to display costs

- showUtilities:

  Logical, whether to display utilities

## Value

ggplot2 object representing the decision tree
