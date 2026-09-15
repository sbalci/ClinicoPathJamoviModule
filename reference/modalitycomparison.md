# Modality Comparison Analysis

Modality Comparison Analysis for digital pathology validation studies.
Compare agreement between different diagnostic modalities (e.g., glass
slides vs digital images) with specialized metrics for pathology
applications including discordance pattern analysis and HER2 scoring
validation.

## Usage

``` r
modalitycomparison(
  data,
  modality1_var,
  modality2_var,
  case_id = NULL,
  modality1_name = "Modality 1",
  modality2_name = "Modality 2",
  show_discordance_analysis = TRUE,
  score_categories = "auto",
  show_contingency_table = TRUE,
  calculate_weighted_kappa = FALSE,
  confidence_intervals = TRUE,
  directional_analysis = TRUE,
  low_end_focus = FALSE,
  show_plots = TRUE
)
```

## Arguments

- data:

  The data as a data frame with paired observations from two modalities.

- modality1_var:

  Variable containing scores/ratings from the first modality (e.g.,
  glass slides).

- modality2_var:

  Variable containing scores/ratings from the second modality (e.g.,
  digital images).

- case_id:

  Optional case identifier variable for tracking individual cases.

- modality1_name:

  Descriptive name for the first modality (e.g., "Glass Slides").

- modality2_name:

  Descriptive name for the second modality (e.g., "Digital Images").

- show_discordance_analysis:

  Perform detailed analysis of discordance patterns between modalities.

- score_categories:

  Category system for score interpretation and analysis.

- show_contingency_table:

  Display detailed cross-tabulation of scores between modalities.

- calculate_weighted_kappa:

  Calculate weighted kappa for ordinal scores (requires ordered
  categories).

- confidence_intervals:

  Calculate and display 95 percent confidence intervals for agreement
  statistics.

- directional_analysis:

  Analyze systematic bias in one direction (e.g., higher scores on
  digital vs glass).

- low_end_focus:

  Perform specialized analysis focusing on agreement at low expression
  levels (relevant for HER2-low/null distinction).

- show_plots:

  Generate scatter plots and agreement visualization.

## Value

A results object containing:

|                                |     |     |     |     |          |
|--------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                 |     |     |     |     | a html   |
| `results$overviewTable`        |     |     |     |     | a table  |
| `results$agreementTable`       |     |     |     |     | a table  |
| `results$contingencyTable`     |     |     |     |     | a table  |
| `results$discordanceTable`     |     |     |     |     | a table  |
| `results$directionalBiasTable` |     |     |     |     | a table  |
| `results$lowEndAnalysisTable`  |     |     |     |     | a table  |
| `results$caseDetailTable`      |     |     |     |     | a table  |
| `results$agreementPlot`        |     |     |     |     | an image |
| `results$discordancePlot`      |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overviewTable$asDF`

`as.data.frame(results$overviewTable)`
