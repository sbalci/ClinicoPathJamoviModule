# Venn Diagram

Venn Diagram

## Usage

``` r
venn(
  data,
  var1 = NULL,
  var1true,
  var2 = NULL,
  var2true,
  var3 = NULL,
  var3true,
  var4 = NULL,
  var4true,
  var5 = NULL,
  var5true,
  var6 = NULL,
  var6true,
  var7 = NULL,
  var7true,
  show_upsetR = FALSE,
  show_complexUpset = FALSE,
  show_ggvenn = TRUE,
  show_ggVennDiagram = FALSE,
  sortBy = "freq",
  minSize = 0,
  showAnnotations = FALSE,
  explanatory = FALSE,
  aboutAnalysis = FALSE,
  clinicalSummary = FALSE,
  reportSentences = FALSE,
  assumptions = FALSE,
  shapeType = "auto",
  regionLabels = "count",
  labelGeometry = "label",
  labelPrecisionDigits = 1,
  setNameSize = 5,
  labelSize = 4,
  edgeSize = 1,
  edgeColor = "black",
  edgeLineType = "solid",
  edgeAlpha = 1,
  fillAlpha = 0.5,
  showSetLabels = TRUE,
  setLabelColor = "black",
  fillColorMapping = TRUE,
  colorPalette = "default",
  showSetCalculations = FALSE,
  calculateOverlap = FALSE,
  calculateDiscern = FALSE,
  calculateUnite = FALSE,
  showMembershipTable = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- data:

  The dataset as a data frame containing the variables for analysis.

- var1:

  A string naming the primary variable from `data` for the diagram.

- var1true:

  The level in `var1` that represents the positive condition.

- var2:

  A string naming the secondary variable from `data` used in the
  diagram.

- var2true:

  The level in `var2` that represents the positive condition.

- var3:

  An optional variable from `data` to include in the diagram.

- var3true:

  The level in `var3` that represents the positive condition.

- var4:

  An optional variable from `data` for additional overlap analysis.

- var4true:

  The level in `var4` that represents the positive condition.

- var5:

  Fifth variable for 5-set Venn diagrams (automatically switches to
  advanced engine).

- var5true:

  The level in `var5` that represents the positive condition.

- var6:

  Sixth variable for 6-set Venn diagrams (uses advanced engine
  automatically).

- var6true:

  The level in `var6` that represents the positive condition.

- var7:

  Seventh variable for 7-set Venn diagrams (uses advanced engine
  automatically).

- var7true:

  The level in `var7` that represents the positive condition.

- show_upsetR:

  .

- show_complexUpset:

  .

- show_ggvenn:

  .

- show_ggVennDiagram:

  .

- sortBy:

  How to sort the intersections in the UpSet plot. 'None' (unsorted)
  applies to the ComplexUpset engine only; the UpSetR engine always
  orders intersections by frequency.

- minSize:

  Minimum size of intersections to display. Applies to the ComplexUpset
  engine only; UpSetR does not filter intersections by minimum size.

- showAnnotations:

  Add percentage labels to intersection sizes in ComplexUpset plots or
  enhanced text scaling in UpSetR plots.

- explanatory:

  Add detailed explanatory footnotes with interpretation guidance for
  Venn diagrams and set operations.

- aboutAnalysis:

  Display detailed information about Venn diagram analysis and
  interpretation.

- clinicalSummary:

  Display clinical interpretation of overlap patterns and key findings.

- reportSentences:

  Generate copy-ready sentences for clinical reports and publications.

- assumptions:

  Display interpretation guidelines and statistical assumptions.

- shapeType:

  Shape type for ggVennDiagram. Auto selects optimal shape based on
  number of sets.

- regionLabels:

  What to display in Venn diagram regions (available for advanced
  diagrams).

- labelGeometry:

  Whether to use label geometry (with background) or text geometry for
  region labels.

- labelPrecisionDigits:

  Number of decimal places for percentage labels (available for advanced
  diagrams).

- setNameSize:

  Font size for set names (available for advanced diagrams).

- labelSize:

  Font size for region labels (available for advanced diagrams).

- edgeSize:

  Width of set boundary lines (available for advanced diagrams).

- edgeColor:

  Color of set boundary lines (available for advanced diagrams).

- edgeLineType:

  Line type for set boundaries in ggVennDiagram.

- edgeAlpha:

  Transparency level for set boundary lines (0=transparent, 1=opaque).

- fillAlpha:

  Transparency level for region fills (0=transparent, 1=opaque).

- showSetLabels:

  Whether to display set names on the diagram.

- setLabelColor:

  Color for set name labels.

- fillColorMapping:

  Map fill colors to intersection sizes (available for advanced
  diagrams).

- colorPalette:

  Color palette for Venn diagram regions (available for advanced
  diagrams).

- showSetCalculations:

  Display detailed set calculations including overlaps, unique members,
  and unions.

- calculateOverlap:

  Calculate and display overlap intersections between sets.

- calculateDiscern:

  Calculate and display unique members for each set (discern operation).

- calculateUnite:

  Calculate and display union of all sets.

- showMembershipTable:

  Display detailed membership table showing which items belong to which
  sets.

- showGlossary:

  Display glossary of statistical terms and clinical interpretations.

## Value

A results object containing:

|                              |     |     |     |     |           |
|------------------------------|-----|-----|-----|-----|-----------|
| `results$welcome`            |     |     |     |     | a html    |
| `results$todo`               |     |     |     |     | a html    |
| `results$summary`            |     |     |     |     | a table   |
| `results$validationErrors`   |     |     |     |     | a html    |
| `results$validationWarnings` |     |     |     |     | a html    |
| `results$analysisInfo`       |     |     |     |     | a html    |
| `results$plotGgvenn`         |     |     |     |     | an image  |
| `results$plotGgVennDiagram`  |     |     |     |     | an image  |
| `results$plotUpsetR`         |     |     |     |     | an image  |
| `results$plotComplexUpset`   |     |     |     |     | an image  |
| `results$aboutAnalysis`      |     |     |     |     | a html    |
| `results$clinicalSummary`    |     |     |     |     | a html    |
| `results$reportSentences`    |     |     |     |     | a html    |
| `results$assumptions`        |     |     |     |     | a html    |
| `results$setCalculations`    |     |     |     |     | a html    |
| `results$membershipTable`    |     |     |     |     | a table   |
| `results$membershipGroups`   |     |     |     |     | an output |
| `results$glossary`           |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
