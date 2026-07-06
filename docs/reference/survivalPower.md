# Survival Power Analysis

Power analysis and sample size calculation for survival studies.
User-friendly interface for survival power calculations with multiple
statistical methods.

## Usage

``` r
survivalPower(
  clinical_preset = "custom",
  analysis_type = "sample_size",
  test_type = "log_rank",
  study_design = "two_arm_parallel",
  primary_endpoint = "overall_survival",
  effect_size_type = "hazard_ratio",
  effect_size = 0.75,
  alpha_level = 0.05,
  power_level = 0.8,
  allocation_ratio = 1,
  sample_size_input = 200,
  control_median_survival = 12,
  survival_distribution = "exponential",
  weibull_shape = 1,
  accrual_period = 24,
  follow_up_period = 12,
  accrual_pattern = "uniform",
  dropout_rate = 0.05,
  ni_margin = 1.25,
  ni_type = "relative_margin",
  competing_risk_rate = 0.1,
  competing_risk_hr = 1,
  rmst_tau = 36,
  rmst_difference = 3,
  snp_maf = 0.3,
  genetic_model = "additive",
  number_of_arms = 3,
  multiple_comparisons = "dunnett",
  interim_analyses = 0,
  alpha_spending = "none",
  stratification_factors = 0,
  cluster_size = 50,
  icc = 0.05,
  sensitivity_analysis = FALSE,
  run_simulation_validation = FALSE,
  simulation_runs = 10000,
  show_summary = FALSE,
  show_explanations = FALSE,
  show_glossary = FALSE,
  guided_mode = FALSE
)
```

## Arguments

- clinical_preset:

  Pre-configured parameter sets for common clinical trial designs.
  Selecting a preset will automatically populate appropriate values.

- analysis_type:

  Type of power analysis to perform

- test_type:

  Type of statistical test for power calculation

- study_design:

  Overall study design type

- primary_endpoint:

  Primary survival endpoint

- effect_size_type:

  Type of effect size specification

- effect_size:

  Expected effect size (HR, median ratio, or difference)

- alpha_level:

  Significance level (two-sided)

- power_level:

  Desired statistical power

- allocation_ratio:

  Ratio of control to experimental group sizes

- sample_size_input:

  Total sample size to use when calculating power, effect size, or
  duration

- control_median_survival:

  Expected median survival in control group

- survival_distribution:

  Assumed survival distribution

- weibull_shape:

  Shape parameter for Weibull distribution

- accrual_period:

  Patient recruitment period

- follow_up_period:

  Additional follow-up after recruitment ends

- accrual_pattern:

  Pattern of patient accrual over time

- dropout_rate:

  Annual rate of loss to follow-up

- ni_margin:

  Non-inferiority margin (hazard ratio scale)

- ni_type:

  Type of non-inferiority margin

- competing_risk_rate:

  Annual rate of competing risk events

- competing_risk_hr:

  Treatment effect on competing risks

- rmst_tau:

  Restriction time for RMST analysis

- rmst_difference:

  Expected difference in restricted mean survival time

- snp_maf:

  Minor allele frequency for SNP analysis

- genetic_model:

  Genetic inheritance model

- number_of_arms:

  Total number of treatment arms (including control)

- multiple_comparisons:

  Method for multiple comparisons adjustment

- interim_analyses:

  Number of planned interim analyses

- alpha_spending:

  Alpha spending function for interim analyses

- stratification_factors:

  Number of stratification factors

- cluster_size:

  Average cluster size for cluster randomized trials

- icc:

  ICC for cluster randomized trials

- sensitivity_analysis:

  Perform sensitivity analysis across parameter ranges

- run_simulation_validation:

  Validate analytical power calculations using Monte Carlo simulation.
  Currently validated for exponential log-rank settings only. May take
  10-60 seconds depending on simulation_runs setting.

- simulation_runs:

  Number of simulation runs for complex calculations

- show_summary:

  Display plain-language summary of results

- show_explanations:

  Display educational notes and guidance

- show_glossary:

  Display glossary of statistical terms

- guided_mode:

  Step-by-step guided analysis workflow

## Value

A results object containing:

|                                       |     |     |     |     |                |
|---------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                     |     |     |     |     | a preformatted |
| `results$instructions`                |     |     |     |     | a html         |
| `results$power_summary`               |     |     |     |     | a table        |
| `results$simulation_validation_table` |     |     |     |     | a table        |
| `results$sample_size_results`         |     |     |     |     | a table        |
| `results$power_results`               |     |     |     |     | a table        |
| `results$effect_size_results`         |     |     |     |     | a table        |
| `results$study_duration_results`      |     |     |     |     | a table        |
| `results$assumptions_table`           |     |     |     |     | a table        |
| `results$competing_risks_table`       |     |     |     |     | a table        |
| `results$non_inferiority_table`       |     |     |     |     | a table        |
| `results$rmst_analysis_table`         |     |     |     |     | a table        |
| `results$snp_analysis_table`          |     |     |     |     | a table        |
| `results$multi_arm_table`             |     |     |     |     | a table        |
| `results$interim_analysis_table`      |     |     |     |     | a table        |
| `results$sensitivity_analysis_table`  |     |     |     |     | a table        |
| `results$regulatory_table`            |     |     |     |     | a table        |
| `results$power_curve_plot`            |     |     |     |     | an image       |
| `results$sample_size_plot`            |     |     |     |     | an image       |
| `results$survival_curves_plot`        |     |     |     |     | an image       |
| `results$accrual_timeline_plot`       |     |     |     |     | an image       |
| `results$sensitivity_plot`            |     |     |     |     | an image       |
| `results$clinical_interpretation`     |     |     |     |     | a html         |
| `results$natural_language_summary`    |     |     |     |     | a html         |
| `results$educational_explanations`    |     |     |     |     | a html         |
| `results$statistical_glossary`        |     |     |     |     | a html         |
| `results$guided_workflow`             |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$power_summary$asDF`

`as.data.frame(results$power_summary)`

## Details

This function provides a streamlined approach to survival power
analysis. For specialized needs, consider the other power analysis
functions: Classical, Competing Risks, Advanced, or Comprehensive.
