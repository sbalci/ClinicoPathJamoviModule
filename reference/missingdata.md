# Missing Data Analysis and Imputation

Comprehensive missing data analysis and multiple imputation using mice
and ggmice packages. Analyze missing data patterns, perform multiple
imputation by chained equations (MICE), and visualize imputation
results. Includes diagnostic plots for convergence assessment,
comparison of observed vs imputed data, and quality evaluation. Perfect
for clinical research data preprocessing where missing data is common
and proper handling is critical for valid statistical inference.

## Usage

``` r
missingdata(
  data,
  analysis_vars,
  analysis_type = "pattern",
  n_imputations = 5,
  max_iterations = 5,
  imputation_method = "auto",
  seed_value = 123,
  convergence_check = TRUE,
  show_pattern_plot = TRUE,
  show_pattern_table = TRUE,
  show_correlation_plot = TRUE,
  show_flux_plot = TRUE,
  show_trace_plot = TRUE,
  show_density_plot = TRUE,
  show_stripplot = TRUE,
  show_scatterplot = FALSE,
  show_imputation_summary = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- analysis_vars:

  Variables to include in the missing data analysis and imputation. Can
  include numeric, factor, and ID variables.

- analysis_type:

  Type of missing data analysis to perform. Pattern analysis explores
  missing data structure, imputation performs MICE, complete does both.

- n_imputations:

  Number of imputed datasets to generate using MICE algorithm. Typically
  5-20 imputations are sufficient for most applications.

- max_iterations:

  Maximum number of MICE iterations per imputation. Default is 5,
  increase if convergence issues occur.

- imputation_method:

  Default imputation method. Auto-select chooses appropriate methods
  based on variable types. PMM is robust for continuous variables.

- seed_value:

  Random seed for reproducible imputation results.

- convergence_check:

  Perform convergence diagnostics to ensure MICE algorithm stability.

- show_pattern_plot:

  Display visual representation of missing data patterns.

- show_pattern_table:

  Display tabular summary of missing data patterns.

- show_correlation_plot:

  Display correlation matrix between incomplete variables.

- show_flux_plot:

  Display influx and outflux of missing data patterns.

- show_trace_plot:

  Display convergence trace plots for imputation diagnostics.

- show_density_plot:

  Compare distributions of observed vs imputed data.

- show_stripplot:

  Display strip plots comparing observed and imputed values.

- show_scatterplot:

  Display scatter plots of observed vs imputed data relationships.

- show_imputation_summary:

  Display summary statistics and information about the imputation
  process.

- show_interpretation:

  Display guidance on missing data analysis and imputation best
  practices for clinical research.

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$todo`               |     |     |     |     | a html   |
| `results$pattern_plot`       |     |     |     |     | an image |
| `results$pattern_table`      |     |     |     |     | a html   |
| `results$correlation_plot`   |     |     |     |     | an image |
| `results$flux_plot`          |     |     |     |     | an image |
| `results$trace_plot`         |     |     |     |     | an image |
| `results$density_plot`       |     |     |     |     | an image |
| `results$stripplot`          |     |     |     |     | an image |
| `results$scatterplot`        |     |     |     |     | an image |
| `results$imputation_summary` |     |     |     |     | a html   |
| `results$interpretation`     |     |     |     |     | a html   |
