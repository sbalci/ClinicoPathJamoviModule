# Pathology Agreement Analysis

Pathology Agreement Analysis

## Usage

``` r
pathologyagreement(
  data,
  dep1 = NULL,
  dep2 = NULL,
  additional_methods = list(),
  clinical_preset = "general",
  bootstrap_n = 1000,
  conf_level = 0.95,
  show_plots = TRUE,
  icc_type = "consistency",
  correlation_method = "both",
  ba_method = "standard",
  missing_data = "listwise",
  show_interpretation = TRUE,
  show_summary = FALSE,
  show_explanations = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- dep1:

  a vector of numbers (the first measurement method)

- dep2:

  a vector of numbers (the second measurement method)

- additional_methods:

  optional additional measurement methods for multi-method analysis
  (enables 3+ method comparison)

- clinical_preset:

  clinical analysis preset optimized for specific pathology scenarios

- bootstrap_n:

  number of bootstrap replicates for confidence intervals

- conf_level:

  confidence level for intervals (default 0.95 for 95 percent CI)

- show_plots:

  show scatter plot and Bland-Altman plot

- icc_type:

  type of intraclass correlation coefficient to calculate

- correlation_method:

  correlation method(s) to calculate

- ba_method:

  Method for calculating limits of agreement in Bland-Altman analysis.
  Standard uses fixed limits (mean +/- 1.96\*SD). Regression-based
  limits account for proportional bias where disagreement varies with
  magnitude.

- missing_data:

  method for handling missing values

- show_interpretation:

  provide clinical interpretation of agreement statistics

- show_summary:

  generate a plain-language summary paragraph suitable for copy-pasting
  into reports

- show_explanations:

  display educational content explaining when to use this analysis,
  assumptions, and interpretation

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$warnings` |  |  |  |  | Important notices, warnings, and error messages |
| `results$interpretation` |  |  |  |  | a html |
| `results$summary` |  |  |  |  | One-paragraph summary of results in plain language |
| `results$explanations` |  |  |  |  | Educational guide on agreement analysis |
| `results$agreementtable` |  |  |  |  | ICC, CCC, and Bland-Altman metrics |
| `results$correlationtable` |  |  |  |  | Pearson and Spearman correlation coefficients |
| `results$scatterplot` |  |  |  |  | Scatter plot with line of perfect agreement |
| `results$blandaltmanplot` |  |  |  |  | Bland-Altman plot showing bias and limits of agreement |
| `results$multimethod_summary` |  |  |  |  | Summary of multi-method analysis results |
| `results$correlationmatrix` |  |  |  |  | Correlation matrix for all methods |
| `results$overall_icc_table` |  |  |  |  | ICC analysis for all methods combined |
| `results$report_sentences` |  |  |  |  | Pre-formatted sentences for manuscripts and reports |
| `results$statistical_glossary` |  |  |  |  | Definitions of statistical terms with clinical interpretations |
| `results$icc_selection_guide` |  |  |  |  | When to use Consistency vs Absolute Agreement |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$agreementtable$asDF`

`as.data.frame(results$agreementtable)`
