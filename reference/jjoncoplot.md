# Genomic Landscape Visualization

Creates oncoplots (mutation landscapes) to visualize genomic alterations
across genes and samples with optional clinical annotations.

## Usage

``` r
jjoncoplot(
  data,
  sampleVar,
  geneVars,
  clinicalVars = NULL,
  mutationTypeVar = NULL,
  clinicalPreset = "custom",
  plotType = "oncoplot",
  topn = 10,
  genesToInclude = "",
  genesToIgnore = "",
  maxSamples = 50,
  sortBy = "hierarchical",
  colorScheme = "default",
  showMutationLoad = FALSE,
  showGeneFreq = FALSE,
  drawMarginalPlots = FALSE,
  showTMB = FALSE,
  showTMBTable = TRUE,
  log10TransformTMB = TRUE,
  showClinicalAnnotation = FALSE,
  plotWidth = 800,
  plotHeight = 600,
  fontSize = 10,
  showLegend = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- sampleVar:

  Variable containing sample identifiers.

- geneVars:

  Variables representing genes or mutations (0/1 or categorical values).

- clinicalVars:

  Optional clinical variables for annotation (e.g., age, stage,
  treatment).

- mutationTypeVar:

  Variable containing mutation type information (e.g., SNV, CNV,
  Fusion).

- clinicalPreset:

  Predefined clinical analysis configurations optimized for specific
  research goals.

- plotType:

  Type of genomic visualization to generate.

- topn:

  Number of most frequently mutated genes to display.

- genesToInclude:

  Comma-separated list of specific genes to include (overrides topn).

- genesToIgnore:

  Comma-separated list of genes to exclude from analysis.

- maxSamples:

  Maximum number of samples to display.

- sortBy:

  Method for sorting samples in the plot.

- colorScheme:

  Color scheme for the oncoplot visualization.

- showMutationLoad:

  Display mutation load bar plot alongside oncoplot.

- showGeneFreq:

  Display gene mutation frequency alongside oncoplot.

- drawMarginalPlots:

  Integrate gene frequency and TMB plots as margins of the main
  oncoplot.

- showTMB:

  Display tumor mutation burden (TMB) bar plot.

- showTMBTable:

  Show per-sample mutation burden table alongside the plot.

- log10TransformTMB:

  Apply log10 transformation to TMB values for better visualization.

- showClinicalAnnotation:

  Display clinical annotations as heatmap.

- plotWidth:

  Width of the plot in pixels.

- plotHeight:

  Height of the plot in pixels.

- fontSize:

  Font size for plot labels.

- showLegend:

  Display plot legend.

## Value

A results object containing:

|                               |     |     |     |     |                |
|-------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`             |     |     |     |     | a preformatted |
| `results$instructions`        |     |     |     |     | a html         |
| `results$setupGuidance`       |     |     |     |     | a html         |
| `results$main`                |     |     |     |     | an image       |
| `results$mutationSummary`     |     |     |     |     | a table        |
| `results$sampleSummary`       |     |     |     |     | a table        |
| `results$tmbTable`            |     |     |     |     | a table        |
| `results$clinicalSummary`     |     |     |     |     | a table        |
| `results$cooccurrence`        |     |     |     |     | a table        |
| `results$plotInfo`            |     |     |     |     | a table        |
| `results$clinicalSummaryText` |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$mutationSummary$asDF`

`as.data.frame(results$mutationSummary)`
