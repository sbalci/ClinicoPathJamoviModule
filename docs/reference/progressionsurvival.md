# Progression-Free Survival Analysis

Specialized progression-free survival (PFS) analysis designed for
oncology research. Provides comprehensive analysis of
time-to-progression endpoints with competing risks considerations,
landmark analysis, and treatment effect estimation. Includes analysis of
progression patterns, censoring mechanisms, and clinical interpretation
specific to cancer clinical trials and observational studies.

## Usage

``` r
progressionsurvival(
  data,
  time_var,
  progression_var,
  death_var,
  treatment_var,
  stratification_vars,
  patient_id,
  baseline_vars,
  analysis_type = "standard_pfs",
  progression_definition = "recist",
  censoring_mechanism = "administrative",
  survival_method = "kaplan_meier",
  comparison_test = "logrank",
  regression_model = "cox_ph",
  landmark_times = "6,12,18,24",
  rmst_tau = 24,
  confidence_level = 0.95,
  alpha_spending = "obrien_fleming",
  kaplan_meier_curves = TRUE,
  cumulative_incidence = FALSE,
  landmark_analysis_plot = FALSE,
  progression_patterns = FALSE,
  treatment_effect_estimation = TRUE,
  subgroup_analysis = FALSE,
  biomarker_interaction = FALSE,
  sensitivity_analysis = FALSE,
  interim_monitoring = FALSE,
  progression_velocity = FALSE,
  clinical_significance = TRUE,
  regulatory_endpoints = FALSE,
  health_economic_outcomes = FALSE,
  plot_theme = "publication",
  risk_tables = TRUE,
  median_survival_lines = TRUE,
  confidence_bands = TRUE,
  generate_report = FALSE,
  regulatory_tables = FALSE,
  consort_diagram = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- time_var:

  Time to progression or censoring (PFS time)

- progression_var:

  Progression indicator (1=progression, 0=censored)

- death_var:

  Death indicator (1=death, 0=alive) for competing risks analysis

- treatment_var:

  Treatment or intervention variable

- stratification_vars:

  Variables for stratified analysis (e.g., stage, histology, biomarkers)

- patient_id:

  Patient identifier for longitudinal data

- baseline_vars:

  Baseline covariates for adjusted analysis

- analysis_type:

  Type of progression-free survival analysis

- progression_definition:

  Definition of disease progression used

- censoring_mechanism:

  Primary censoring mechanism in the data

- survival_method:

  Method for survival function estimation

- comparison_test:

  Statistical test for group comparisons

- regression_model:

  Regression model for treatment effect estimation

- landmark_times:

  Landmark time points in months (comma-separated)

- rmst_tau:

  Restriction time for RMST analysis (months)

- confidence_level:

  Confidence level for intervals

- alpha_spending:

  Alpha spending function for interim analyses

- kaplan_meier_curves:

  Generate Kaplan-Meier survival curves

- cumulative_incidence:

  Calculate cumulative incidence functions (competing risks)

- landmark_analysis_plot:

  Generate landmark analysis visualizations

- progression_patterns:

  Analyze patterns and timing of disease progression

- treatment_effect_estimation:

  Estimate treatment effects with confidence intervals

- subgroup_analysis:

  Perform pre-specified subgroup analyses

- biomarker_interaction:

  Test for biomarker-treatment interactions

- sensitivity_analysis:

  Perform sensitivity analyses for assumptions

- interim_monitoring:

  Analysis for interim monitoring and early stopping

- progression_velocity:

  Analyze velocity and acceleration of disease progression

- clinical_significance:

  Assess clinical significance of findings

- regulatory_endpoints:

  Analysis aligned with regulatory requirements (FDA/EMA)

- health_economic_outcomes:

  Calculate health economic outcome measures

- plot_theme:

  Visual theme for plots

- risk_tables:

  Include risk tables below survival curves

- median_survival_lines:

  Add median survival reference lines

- confidence_bands:

  Show confidence bands on survival curves

- generate_report:

  Generate comprehensive clinical analysis report

- regulatory_tables:

  Generate regulatory submission-ready tables

- consort_diagram:

  Generate CONSORT-style patient flow diagram

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                |     |     |     |     | a html   |
| `results$pfs_summary`                 |     |     |     |     | a table  |
| `results$survival_estimates`          |     |     |     |     | a table  |
| `results$treatment_effects`           |     |     |     |     | a table  |
| `results$competing_risks_analysis`    |     |     |     |     | a table  |
| `results$landmark_results`            |     |     |     |     | a table  |
| `results$subgroup_analysis_table`     |     |     |     |     | a table  |
| `results$progression_patterns`        |     |     |     |     | a table  |
| `results$sensitivity_analysis_table`  |     |     |     |     | a table  |
| `results$biomarker_interaction_table` |     |     |     |     | a table  |
| `results$interim_monitoring_table`    |     |     |     |     | a table  |
| `results$clinical_significance_table` |     |     |     |     | a table  |
| `results$km_plot`                     |     |     |     |     | an image |
| `results$cif_plot`                    |     |     |     |     | an image |
| `results$landmark_plot`               |     |     |     |     | an image |
| `results$forest_plot`                 |     |     |     |     | an image |
| `results$progression_pattern_plot`    |     |     |     |     | an image |
| `results$consort_flow`                |     |     |     |     | an image |
| `results$clinical_interpretation`     |     |     |     |     | a html   |
| `results$regulatory_summary`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$pfs_summary$asDF`

`as.data.frame(results$pfs_summary)`
