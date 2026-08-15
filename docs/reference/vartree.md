# Variable Tree

Enhanced function for generating comprehensive tree summaries of
variables. Supports current CRAN vtree package with advanced styling,
statistical summaries, and interpretation features. Consolidates
functionality from legacy versions with modern vtree capabilities.

## Usage

``` r
vartree(
  data,
  vars,
  percvar = NULL,
  percvarLevel,
  summaryvar = NULL,
  summarylocation = "leafonly",
  style = "default",
  prunebelow = NULL,
  pruneLevel1,
  pruneLevel2,
  follow = NULL,
  followLevel1,
  followLevel2,
  excl = FALSE,
  vp = TRUE,
  horizontal = FALSE,
  sline = TRUE,
  varnames = FALSE,
  nodelabel = TRUE,
  pct = FALSE,
  showcount = TRUE,
  legend = FALSE,
  pattern = FALSE,
  sequence = FALSE,
  ptable = FALSE,
  mytitle = "",
  useprunesmaller = FALSE,
  prunesmaller = 5,
  showInterpretation = TRUE,
  maxwidth = 600
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Categorical variables for tree construction.

- percvar:

  Variable for percentage calculations.

- percvarLevel:

  Specific level for percentage calculations.

- summaryvar:

  Continuous variable for statistical summaries (mean, SD) in tree
  nodes.

- summarylocation:

  Where to display statistical summaries in the tree.

- style:

  Visual style preset: default (colored), clean (minimal colors),
  minimal (simplified layout).

- prunebelow:

  Variable for conditional pruning of tree branches.

- pruneLevel1:

  First level for pruning condition.

- pruneLevel2:

  Second level for pruning condition.

- follow:

  Variable for conditional following of tree branches.

- followLevel1:

  First level for follow condition.

- followLevel2:

  Second level for follow condition.

- excl:

  Exclude rows with missing values from analysis.

- vp:

  Calculate percentages based on valid (non-missing) values.

- horizontal:

  Display tree in horizontal orientation.

- sline:

  Display variable names and values on same line.

- varnames:

  Show variable names in tree nodes.

- nodelabel:

  Show labels for tree nodes.

- pct:

  Display percentages in tree nodes.

- showcount:

  Display counts in tree nodes.

- legend:

  Show legend for tree visualization.

- pattern:

  Generate pattern-based tree analysis.

- sequence:

  Generate sequence-based tree analysis.

- ptable:

  Display pattern table alongside tree.

- mytitle:

  Custom title for the tree root.

- useprunesmaller:

  Enable pruning of nodes with small counts.

- prunesmaller:

  Minimum count threshold for node pruning.

- showInterpretation:

  Generate and display automatic interpretation of tree results.

- maxwidth:

  Maximum width for tree display in pixels.

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$notices`        |     |     |     |     | a preformatted |
| `results$todo`           |     |     |     |     | a html         |
| `results$text1`          |     |     |     |     | a html         |
| `results$text2`          |     |     |     |     | a preformatted |
| `results$reportSentence` |     |     |     |     | a preformatted |
| `results$interpretation` |     |     |     |     | a html         |
