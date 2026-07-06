# Screening Program Evaluation & Performance Analysis

Comprehensive evaluation of screening programs and diagnostic test
performance from tabular screening data. Analyzes screening outcomes,
program effectiveness, population coverage, diagnostic accuracy, and
cost-effectiveness. Essential for public health screening program
assessment, quality improvement, and evidence-based screening policy
development using standardized epidemiological methods and performance
indicators.

## Usage

``` r
screeningevaluation(
  data,
  screening_result,
  disease_status,
  participant_id,
  age_var,
  sex_var,
  screening_date,
  screening_round,
  risk_factors,
  location_var,
  screening_type = "population_based",
  target_disease = "cancer",
  screening_test_type = "laboratory",
  diagnostic_accuracy = TRUE,
  program_coverage = TRUE,
  detection_rates = TRUE,
  interval_cancer_analysis = FALSE,
  lead_time_analysis = FALSE,
  length_bias_analysis = FALSE,
  age_stratified_analysis = TRUE,
  age_group_breaks = "40,50,60,70,80",
  risk_stratified_analysis = FALSE,
  geographic_analysis = FALSE,
  calculate_predictive_values = TRUE,
  likelihood_ratios = TRUE,
  roc_analysis = FALSE,
  optimal_cutpoint = FALSE,
  quality_indicators = TRUE,
  participation_rates = TRUE,
  recall_rates = TRUE,
  completion_rates = TRUE,
  cost_effectiveness = FALSE,
  cost_per_case_detected = FALSE,
  screening_cost_var,
  followup_cost_var,
  time_to_diagnosis = FALSE,
  diagnosis_date_var,
  survival_analysis = FALSE,
  survival_time_var,
  death_indicator,
  overdiagnosis_analysis = FALSE,
  false_positive_impact = FALSE,
  screening_adherence = FALSE,
  performance_plots = TRUE,
  coverage_plots = TRUE,
  trend_analysis_plots = FALSE,
  comprehensive_report = TRUE,
  quality_assurance_report = TRUE,
  public_health_indicators = TRUE,
  international_standards = TRUE,
  export_for_registry = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- screening_result:

  Binary screening test result (positive/negative)

- disease_status:

  True disease status from gold standard or follow-up

- participant_id:

  Unique identifier for screening participants

- age_var:

  Age of screening participants

- sex_var:

  Sex/gender of screening participants

- screening_date:

  Date of screening test

- screening_round:

  Screening round number (for repeated screening)

- risk_factors:

  Risk factor variables for stratified analysis

- location_var:

  Geographic location or screening site

- screening_type:

  Type of screening program

- target_disease:

  Target disease or condition being screened

- screening_test_type:

  Type of screening test used

- diagnostic_accuracy:

  Calculate diagnostic accuracy measures from tabular data

- program_coverage:

  Analyze screening program coverage and participation

- detection_rates:

  Calculate disease detection rates by subgroups

- interval_cancer_analysis:

  Analyze interval cancers (for cancer screening programs)

- lead_time_analysis:

  Estimate lead time and lead time bias

- length_bias_analysis:

  Assess potential length bias in screening

- age_stratified_analysis:

  Perform analysis stratified by age groups

- age_group_breaks:

  Age boundaries for stratified analysis (comma-separated)

- risk_stratified_analysis:

  Perform analysis stratified by risk factors

- geographic_analysis:

  Analyze screening performance by geographic location

- calculate_predictive_values:

  Calculate positive and negative predictive values

- likelihood_ratios:

  Calculate likelihood ratios and post-test probabilities

- roc_analysis:

  Perform ROC curve analysis (for continuous test results)

- optimal_cutpoint:

  Determine optimal cutpoint for continuous screening tests

- quality_indicators:

  Calculate standard screening quality indicators

- participation_rates:

  Analyze participation rates and demographic factors

- recall_rates:

  Calculate recall rates for further investigation

- completion_rates:

  Analyze completion of recommended follow-up

- cost_effectiveness:

  Basic cost-effectiveness analysis

- cost_per_case_detected:

  Calculate cost per case detected

- screening_cost_var:

  Variable containing screening costs per individual

- followup_cost_var:

  Variable containing follow-up diagnostic costs

- time_to_diagnosis:

  Analyze time from screening to diagnosis

- diagnosis_date_var:

  Date of disease diagnosis

- survival_analysis:

  Compare survival between screen-detected and symptomatic cases

- survival_time_var:

  Time to death or last follow-up

- death_indicator:

  Death indicator (0=alive, 1=dead)

- overdiagnosis_analysis:

  Assess potential overdiagnosis in screening program

- false_positive_impact:

  Analyze impact of false positive results

- screening_adherence:

  Analyze adherence to screening recommendations

- performance_plots:

  Generate diagnostic performance plots

- coverage_plots:

  Generate coverage and participation plots

- trend_analysis_plots:

  Generate time trend analysis plots

- comprehensive_report:

  Generate comprehensive screening evaluation report

- quality_assurance_report:

  Generate quality assurance and improvement report

- public_health_indicators:

  Generate public health screening indicators report

- international_standards:

  Assess compliance with international screening standards

- export_for_registry:

  Export results in cancer registry compatible format

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$screening_overview`          |     |     |     |     | a table  |
| `results$diagnostic_accuracy_summary` |     |     |     |     | a table  |
| `results$program_coverage`            |     |     |     |     | a table  |
| `results$detection_rates`             |     |     |     |     | a table  |
| `results$age_stratified_results`      |     |     |     |     | a table  |
| `results$quality_indicators`          |     |     |     |     | a table  |
| `results$recall_analysis`             |     |     |     |     | a table  |
| `results$interval_cancer_results`     |     |     |     |     | a table  |
| `results$cost_effectiveness_summary`  |     |     |     |     | a table  |
| `results$geographic_analysis`         |     |     |     |     | a table  |
| `results$time_trends`                 |     |     |     |     | a table  |
| `results$overdiagnosis_assessment`    |     |     |     |     | a table  |
| `results$survival_comparison`         |     |     |     |     | a table  |
| `results$adherence_analysis`          |     |     |     |     | a table  |
| `results$diagnostic_performance_plot` |     |     |     |     | an image |
| `results$coverage_participation_plot` |     |     |     |     | an image |
| `results$detection_rate_plot`         |     |     |     |     | an image |
| `results$age_stratified_plot`         |     |     |     |     | an image |
| `results$time_trend_plot`             |     |     |     |     | an image |
| `results$roc_curve_plot`              |     |     |     |     | an image |
| `results$cost_effectiveness_plot`     |     |     |     |     | an image |
| `results$comprehensive_report`        |     |     |     |     | a html   |
| `results$quality_assurance_report`    |     |     |     |     | a html   |
| `results$public_health_report`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$screening_overview$asDF`

`as.data.frame(results$screening_overview)`
