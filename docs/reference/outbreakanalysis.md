# Outbreak Analysis & Epidemiological Investigation

Comprehensive outbreak analysis and epidemiological investigation for
infectious disease surveillance and public health research. Analyzes
tabular outbreak data to identify attack rates, risk factors, epidemic
curves, case-control associations, and spatial-temporal patterns.
Essential for outbreak investigation, surveillance analysis, and
epidemiological research with robust statistical methods for public
health decision-making and intervention planning.

## Usage

``` r
outbreakanalysis(
  data,
  case_status,
  exposure_vars,
  date_onset,
  location_var,
  person_id,
  age_var,
  sex_var,
  additional_vars,
  outbreak_type = "unknown",
  analysis_type = "combined",
  epidemic_curve_unit = "auto",
  attack_rate_analysis = TRUE,
  risk_factor_analysis = TRUE,
  dose_response_analysis = FALSE,
  temporal_analysis = TRUE,
  spatial_analysis = FALSE,
  statistical_tests = "all_tests",
  confidence_level = 0.95,
  multiple_testing_correction = "holm",
  stratified_analysis = FALSE,
  stratification_vars,
  case_definition = TRUE,
  incubation_period = FALSE,
  exposure_date_var,
  epidemic_curve_plot = TRUE,
  attack_rate_plot = TRUE,
  risk_factor_plot = TRUE,
  spatial_plot = FALSE,
  power_calculation = FALSE,
  sample_size_calculation = FALSE,
  sensitivity_analysis = FALSE,
  comprehensive_report = TRUE,
  public_health_summary = TRUE,
  surveillance_indicators = FALSE,
  data_quality_assessment = TRUE,
  strobe_compliance = TRUE,
  export_for_epiinfo = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- case_status:

  Binary variable indicating case (1) vs control (0) or case vs non-case

- exposure_vars:

  Variables representing potential exposures or risk factors

- date_onset:

  Date of symptom onset or case identification

- location_var:

  Geographic location or administrative unit

- person_id:

  Unique identifier for each individual

- age_var:

  Age of individuals for demographic analysis

- sex_var:

  Sex/gender variable for demographic analysis

- additional_vars:

  Additional variables for descriptive analysis

- outbreak_type:

  Type of outbreak pattern expected

- analysis_type:

  Primary analytical approach for outbreak investigation

- epidemic_curve_unit:

  Time unit for epidemic curve construction

- attack_rate_analysis:

  Calculate attack rates and 95 percent confidence intervals

- risk_factor_analysis:

  Perform risk factor analysis with odds ratios and relative risks

- dose_response_analysis:

  Analyze dose-response relationships for quantitative exposures

- temporal_analysis:

  Analyze temporal patterns and epidemic curve characteristics

- spatial_analysis:

  Perform spatial analysis and clustering detection

- statistical_tests:

  Statistical tests for association analysis

- confidence_level:

  Confidence level for confidence intervals

- multiple_testing_correction:

  Multiple testing correction method

- stratified_analysis:

  Perform stratified analysis by demographic variables

- stratification_vars:

  Variables for stratified analysis

- case_definition:

  Analyze case definition sensitivity and specificity

- incubation_period:

  Calculate incubation period statistics

- exposure_date_var:

  Date of exposure for incubation period calculation

- epidemic_curve_plot:

  Generate epidemic curve visualization

- attack_rate_plot:

  Generate attack rate comparison plots

- risk_factor_plot:

  Generate forest plot of risk factors

- spatial_plot:

  Generate spatial distribution maps

- power_calculation:

  Calculate statistical power for detected associations

- sample_size_calculation:

  Provide sample size recommendations for future studies

- sensitivity_analysis:

  Perform sensitivity analysis for case definitions

- comprehensive_report:

  Generate comprehensive outbreak investigation report

- public_health_summary:

  Generate public health summary with recommendations

- surveillance_indicators:

  Calculate surveillance system quality indicators

- data_quality_assessment:

  Assess data completeness and quality issues

- strobe_compliance:

  Ensure STROBE guideline compliance for epidemiological reporting

- export_for_epiinfo:

  Export results in EpiInfo compatible format

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$outbreak_overview`           |     |     |     |     | a table  |
| `results$descriptive_summary`         |     |     |     |     | a table  |
| `results$risk_factor_analysis`        |     |     |     |     | a table  |
| `results$temporal_analysis`           |     |     |     |     | a table  |
| `results$epidemic_curve_stats`        |     |     |     |     | a table  |
| `results$spatial_analysis_results`    |     |     |     |     | a table  |
| `results$dose_response_analysis`      |     |     |     |     | a table  |
| `results$stratified_analysis_results` |     |     |     |     | a table  |
| `results$incubation_period_stats`     |     |     |     |     | a table  |
| `results$case_definition_analysis`    |     |     |     |     | a table  |
| `results$data_quality_report`         |     |     |     |     | a table  |
| `results$power_analysis_results`      |     |     |     |     | a table  |
| `results$surveillance_indicators`     |     |     |     |     | a table  |
| `results$epidemic_curve_plot`         |     |     |     |     | an image |
| `results$attack_rate_plot`            |     |     |     |     | an image |
| `results$risk_factor_forest_plot`     |     |     |     |     | an image |
| `results$spatial_distribution_plot`   |     |     |     |     | an image |
| `results$dose_response_plot`          |     |     |     |     | an image |
| `results$outbreak_timeline_plot`      |     |     |     |     | an image |
| `results$comprehensive_report`        |     |     |     |     | a html   |
| `results$public_health_summary`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$outbreak_overview$asDF`

`as.data.frame(results$outbreak_overview)`
