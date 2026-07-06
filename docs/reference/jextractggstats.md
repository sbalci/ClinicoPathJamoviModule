# Statistical Data Extraction from ggstatsplot

Statistical Data Extraction from ggstatsplot

## Usage

``` r
jextractggstats(
  data,
  dep_var,
  group_var,
  test_value = 0,
  analysis_type = "between_stats",
  extract_components = "all",
  statistical_test = "parametric",
  effect_size_type = "eta",
  pairwise_comparisons = FALSE,
  pairwise_correction = "holm",
  conf_level = 0.95,
  bf_prior = 0.707,
  centrality_plotting = TRUE,
  outlier_tagging = FALSE,
  output_format = "table",
  include_plot_data = TRUE,
  include_model_data = TRUE,
  detailed_results = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  R object to use

- dep_var:

  Dependent variable for analysis

- group_var:

  Grouping variable for comparisons

- test_value:

  Reference value for one-sample test

- analysis_type:

  Type of ggstatsplot analysis to perform

- extract_components:

  Which statistical components to extract

- statistical_test:

  Type of statistical test to use

- effect_size_type:

  Type of effect size to calculate

- pairwise_comparisons:

  Perform pairwise comparisons for multiple groups

- pairwise_correction:

  Multiple comparison correction method

- conf_level:

  Confidence level for intervals

- bf_prior:

  Prior width for Bayesian analysis

- centrality_plotting:

  Display measures of central tendency

- outlier_tagging:

  Identify and tag outliers

- output_format:

  Format for extracted data

- include_plot_data:

  Include underlying plot data

- include_model_data:

  Include statistical model information

- detailed_results:

  Include detailed statistical output

- show_interpretation:

  Display result interpretation

## Value

A results object containing:

|                               |     |     |     |     |        |
|-------------------------------|-----|-----|-----|-----|--------|
| `results$instructions`        |     |     |     |     | a html |
| `results$extracted_data`      |     |     |     |     | a html |
| `results$statistical_summary` |     |     |     |     | a html |
| `results$interpretation`      |     |     |     |     | a html |
