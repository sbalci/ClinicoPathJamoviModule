# Diagnostic Test Meta-Analysis for Pathology

Comprehensive meta-analysis of diagnostic test accuracy studies designed
for pathology research. Performs bivariate random-effects modeling,
HSROC analysis, meta-regression, and publication bias assessment for AI
algorithm validation and biomarker diagnostic accuracy synthesis.

## Usage

``` r
diagnosticmeta(
  data,
  study,
  true_positives,
  false_positives,
  false_negatives,
  true_negatives,
  covariate = NULL,
  bivariate_analysis = TRUE,
  hsroc_analysis = FALSE,
  meta_regression = FALSE,
  heterogeneity_analysis = FALSE,
  publication_bias = FALSE,
  confidence_level = 95,
  method = "reml",
  zero_cell_correction = "none",
  forest_plot = FALSE,
  sroc_plot = FALSE,
  funnel_plot = FALSE,
  show_individual_studies = FALSE,
  show_interpretation = FALSE,
  show_methodology = FALSE,
  show_analysis_summary = FALSE,
  color_palette = "standard",
  show_plot_explanations = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- study:

  Variable containing unique study identifiers

- true_positives:

  Number of true positive results in each study

- false_positives:

  Number of false positive results in each study

- false_negatives:

  Number of false negative results in each study

- true_negatives:

  Number of true negative results in each study

- covariate:

  Optional covariate for meta-regression analysis

- bivariate_analysis:

  Perform bivariate random-effects meta-analysis

- hsroc_analysis:

  Perform hierarchical summary ROC (HSROC) analysis

- meta_regression:

  Perform meta-regression with specified covariate

- heterogeneity_analysis:

  Perform heterogeneity analysis including I-squared and Q statistics

- publication_bias:

  Assess publication bias using Deeks' funnel plot test

- confidence_level:

  Confidence level for meta-analysis results

- method:

  Method for meta-analysis estimation (Note DerSimonian-Laird is not
  appropriate for bivariate diagnostic meta-analysis)

- zero_cell_correction:

  Method for handling zero cells in 2x2 tables. 'none' relies on
  bivariate model (recommended). 'constant' adds 0.5 to all cells of
  affected studies. 'treatment_arm' adds correction only to zero cells.
  'empirical' uses study-specific corrections.

- forest_plot:

  Generate forest plot for sensitivity and specificity

- sroc_plot:

  Generate summary receiver operating characteristic plot

- funnel_plot:

  Generate funnel plot for publication bias assessment

- show_individual_studies:

  Display results for individual studies in summary tables

- show_interpretation:

  Display clinical interpretation guidelines and recommendations

- show_methodology:

  Display detailed methodology and statistical approach information

- show_analysis_summary:

  Display natural language summary of analysis results

- color_palette:

  Color palette for all plots - choose color-blind safe options for
  accessibility

- show_plot_explanations:

  Display detailed explanations for all plots including interpretation
  guidance

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$welcome`                |     |     |     |     | a html   |
| `results$instructions`           |     |     |     |     | a html   |
| `results$summary`                |     |     |     |     | a html   |
| `results$about`                  |     |     |     |     | a html   |
| `results$bivariateresults`       |     |     |     |     | a table  |
| `results$hsrocresults`           |     |     |     |     | a table  |
| `results$heterogeneity`          |     |     |     |     | a table  |
| `results$metaregression`         |     |     |     |     | a table  |
| `results$publicationbias`        |     |     |     |     | a table  |
| `results$individualstudies`      |     |     |     |     | a table  |
| `results$forestplot`             |     |     |     |     | an image |
| `results$srocplot`               |     |     |     |     | an image |
| `results$funnelplot`             |     |     |     |     | an image |
| `results$interpretation`         |     |     |     |     | a html   |
| `results$forestplot_explanation` |     |     |     |     | a html   |
| `results$srocplot_explanation`   |     |     |     |     | a html   |
| `results$funnelplot_explanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$bivariateresults$asDF`

`as.data.frame(results$bivariateresults)`
