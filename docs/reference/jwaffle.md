# Waffle Charts

'Creates waffle charts to visualize categorical distributions and
proportions using colored squares. Ideal for showing parts-of-whole
relationships in clinical data such as disease subtypes, treatment
outcomes, or risk category distributions. Each square represents a
proportion of the total sample, making it easy to see relative
frequencies across categories.'

## Usage

``` r
jwaffle(
  data,
  counts = NULL,
  groups,
  facet = NULL,
  rows = 5,
  flip = FALSE,
  color_palette = "default",
  show_legend = FALSE,
  mytitle = "",
  legendtitle = "",
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- counts:

  Optional numeric weight variable if cases have different weights.
  Leave empty to count each row equally (recommended for most clinical
  data). Only needed if your data is pre-aggregated or weighted.

- groups:

  The categorical grouping variable for the waffle squares. Each
  category will be displayed as proportional colored squares in a 10x10
  grid. Examples: Tumor grade (G1/G2/G3), Treatment outcome
  (Complete/Partial/No response), Risk categories (Low/Medium/High).

- facet:

  Optional variable to split the waffle chart by another categorical
  variable to compare distributions across subgroups (e.g., by treatment
  arm, patient cohort, or time period). Creates separate waffle charts
  for each level.

- rows:

  Number of rows in the waffle chart

- flip:

  Whether to flip the orientation of the waffle chart

- color_palette:

  Color scheme for the waffle squares

- show_legend:

  Whether to display the legend

- mytitle:

  Custom title for the plot

- legendtitle:

  Custom title for the legend

- showSummaries:

  Generate natural language summary of waffle chart results including
  proportions, dominant categories, and clinical interpretation.

- showExplanations:

  Show detailed methodology explanations about waffle charts, when to
  use them, and how to interpret the results in clinical contexts.

## Value

A results object containing:

|                             |     |     |     |     |                |
|-----------------------------|-----|-----|-----|-----|----------------|
| `results$notices`           |     |     |     |     | a preformatted |
| `results$todo`              |     |     |     |     | a html         |
| `results$warnings`          |     |     |     |     | a html         |
| `results$analysisSummary`   |     |     |     |     | a html         |
| `results$plot`              |     |     |     |     | an image       |
| `results$methodExplanation` |     |     |     |     | a html         |
