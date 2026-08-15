# Automated Exploratory Data Analysis

Automated Exploratory Data Analysis using DataExplorer package
integration. This module provides comprehensive automated EDA
capabilities including dataset overview, missing value analysis,
correlation matrices, PCA visualization, and automated reporting. Based
on autoEDA research from R Journal 2019 (Staniak & Biecek).

## Usage

``` r
autoeda(
  data,
  vars,
  analysis_type = "overview",
  target_var = NULL,
  include_plots = TRUE,
  missing_threshold = 5,
  correlation_method = "pearson",
  pca_components = 5,
  plot_theme = "clinical",
  output_format = "combined",
  eda_engine = "dataexplorer",
  advanced_options = FALSE,
  categorical_limit = 15,
  generate_report = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables to include in the automated exploratory data analysis. All
  variable types are supported.

- analysis_type:

  Choose the type of automated EDA analysis to perform.

- target_var:

  Optional target variable for supervised EDA analysis. Used for target
  vs predictors analysis.

- include_plots:

  Include automated plots and visualizations in the output.

- missing_threshold:

  Threshold percentage for highlighting variables with missing values.

- correlation_method:

  Method for correlation analysis.

- pca_components:

  Number of principal components to display in PCA analysis.

- plot_theme:

  Visual theme for automated plots.

- output_format:

  Format for the automated EDA output.

- eda_engine:

  Choose the exploratory data analysis engine. DataExplorer provides
  comprehensive automated reporting. ggEDA provides enhanced
  visualizations for clinical research.

- advanced_options:

  Enable advanced features and detailed analysis.

- categorical_limit:

  Maximum number of levels for categorical variables to include in
  analysis.

- generate_report:

  Generate a comprehensive automated EDA report with all analyses.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$todo` |  |  |  |  | Welcome message and instructions |
| `results$overview` |  |  |  |  | Comprehensive dataset introduction and summary |
| `results$missing_analysis` |  |  |  |  | Missing value patterns and recommendations |
| `results$distributions` |  |  |  |  | Univariate distribution analysis and plots |
| `results$correlation_analysis` |  |  |  |  | Correlation matrices and relationship analysis |
| `results$pca_analysis` |  |  |  |  | PCA results and component visualization |
| `results$target_analysis` |  |  |  |  | Target vs predictors relationship analysis |
| `results$comprehensive_report` |  |  |  |  | Complete automated exploratory data analysis report |
| `results$plots` |  |  |  |  | Collection of automated plots and charts |
| `results$recommendations` |  |  |  |  | Automated recommendations for further analysis |
