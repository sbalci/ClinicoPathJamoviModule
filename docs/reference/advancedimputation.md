# Advanced Multiple Imputation & Sensitivity Analysis

Advanced multiple imputation by chained equations (MICE) with
comprehensive sensitivity analysis for clinical research applications.
Includes nested imputation for multilevel data, missing not at random
(MNAR) imputation methods, and comprehensive sensitivity testing for
missing data assumptions. Essential for regulatory-compliant clinical
research where missing data handling must be thoroughly documented and
validated.

## Usage

``` r
advancedimputation(
  data,
  imputation_vars,
  auxiliary_vars,
  cluster_var,
  id_var,
  n_imputations = 10,
  n_iterations = 10,
  convergence_check = TRUE,
  imputation_method = "pmm",
  categorical_method = "logreg",
  mnar_methods = FALSE,
  mnar_type = "delta_adjustment",
  delta_values = "0",
  sensitivity_analysis = TRUE,
  sensitivity_methods = "all",
  multilevel_imputation = FALSE,
  level1_vars,
  level2_vars,
  min_bucket_size = 5,
  exclude_vars,
  passive_imputation,
  ridge_penalty = 1e-05,
  remove_collinear = TRUE,
  collinearity_threshold = 0.95,
  diagnostic_plots = TRUE,
  imputation_quality = TRUE,
  cross_validation = FALSE,
  ampute_test = FALSE,
  save_imputations = FALSE,
  pool_results = TRUE,
  show_detailed_output = TRUE,
  regulatory_report = FALSE,
  random_seed = 123
)
```

## Arguments

- data:

  the data as a data frame

- imputation_vars:

  Variables with missing values to be imputed

- auxiliary_vars:

  Complete or mostly complete variables to assist imputation

- cluster_var:

  Clustering variable for nested/multilevel imputation (e.g., study
  site, patient ID)

- id_var:

  Subject identifier for tracking observations across imputations

- n_imputations:

  Number of imputed datasets to create (recommended 5-20 for analysis,
  100+ for final results)

- n_iterations:

  Number of MICE iterations per imputation (increase for convergence)

- convergence_check:

  Monitor and assess MICE convergence with diagnostic plots and
  statistics

- imputation_method:

  Primary method for continuous variables

- categorical_method:

  Method for categorical variables

- mnar_methods:

  Include Missing Not At Random imputation methods

- mnar_type:

  Type of MNAR imputation approach

- delta_values:

  Comma-separated delta values for MNAR sensitivity analysis (e.g., "0,
  -0.5, -1")

- sensitivity_analysis:

  Perform extensive sensitivity analysis for missing data assumptions

- sensitivity_methods:

  Type of sensitivity analysis to perform

- multilevel_imputation:

  Use multilevel imputation for nested/clustered data

- level1_vars:

  Individual-level variables for multilevel imputation

- level2_vars:

  Cluster-level variables for multilevel imputation

- min_bucket_size:

  Minimum number of donors for PMM (prevents poor matching)

- exclude_vars:

  Variables to exclude from imputation model (but keep in dataset)

- passive_imputation:

  R expressions for passive imputation (e.g., "bmi ~
  I(weight/height^2)")

- ridge_penalty:

  Ridge penalty for numerical stability in regression imputation

- remove_collinear:

  Automatically remove highly collinear variables from imputation model

- collinearity_threshold:

  Correlation threshold for removing collinear variables

- diagnostic_plots:

  Generate convergence and diagnostic plots

- imputation_quality:

  Assess quality of imputations vs observed data

- cross_validation:

  Perform cross-validation of imputation methods

- ampute_test:

  Test imputation performance using artificial missingness

- save_imputations:

  Save completed imputed datasets for further analysis

- pool_results:

  Pool results across imputations using Rubin's rules

- show_detailed_output:

  Show detailed imputation summaries and diagnostics

- regulatory_report:

  Generate regulatory-compliant missing data analysis report

- random_seed:

  Set random seed for reproducible results

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$summary`                 |     |     |     |     | a table  |
| `results$imputationMethodSummary` |     |     |     |     | a table  |
| `results$pooledResults`           |     |     |     |     | a table  |
| `results$diagnosticPlot`          |     |     |     |     | an image |
| `results$methodExplanation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
