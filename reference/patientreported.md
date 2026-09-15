# Patient-Reported Outcomes & Quality of Life Analysis

Comprehensive analysis of Patient-Reported Outcomes (PRO) and Quality of
Life (QoL) data for clinical research. Includes psychometric validation,
score calculation, change analysis, and clinical interpretation.
Supports standardized instruments (SF-36, EORTC QLQ-C30, FACT-G, etc.)
and custom questionnaires with missing data handling and longitudinal
analysis. Essential for patient-centered outcomes research and clinical
trials.

## Usage

``` r
patientreported(
  data,
  scale_items,
  patient_id,
  time_var,
  group_var,
  demographic_vars,
  scale_type = "generic_qol",
  instrument_name = "custom",
  scoring_method = "sum_score",
  reverse_coded_items,
  response_scale_min = 1,
  response_scale_max = 5,
  reliability_analysis = TRUE,
  validity_analysis = TRUE,
  factor_analysis = FALSE,
  irt_analysis = FALSE,
  dimensionality_test = TRUE,
  measurement_invariance = FALSE,
  missing_data_method = "pro_rata_scoring",
  min_items_required = 2,
  missing_threshold = 0.5,
  clinical_interpretation = TRUE,
  normative_comparison = FALSE,
  minimal_important_difference = TRUE,
  mid_value = 5,
  ceiling_floor_effects = TRUE,
  longitudinal_analysis = FALSE,
  change_analysis = "simple_change",
  trajectory_analysis = FALSE,
  time_to_deterioration = FALSE,
  group_comparisons = FALSE,
  comparison_method = "t_test",
  effect_size_analysis = TRUE,
  multiple_comparisons = "fdr",
  responder_analysis = FALSE,
  responder_threshold = 10,
  anchor_based_analysis = FALSE,
  anchor_variables,
  distribution_based_analysis = TRUE,
  data_quality_assessment = TRUE,
  response_patterns = TRUE,
  acquiescence_analysis = FALSE,
  detailed_output = TRUE,
  summary_report = TRUE,
  individual_profiles = FALSE,
  save_scores = FALSE,
  regulatory_documentation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- scale_items:

  Items/questions that comprise the PRO scale or questionnaire

- patient_id:

  Patient identifier for longitudinal analysis

- time_var:

  Time point variable for longitudinal PRO analysis (visit, week, etc.)

- group_var:

  Grouping variable for between-group PRO comparisons (treatment,
  disease stage, etc.)

- demographic_vars:

  Demographic variables for subgroup analysis and validation

- scale_type:

  Type of PRO scale being analyzed

- instrument_name:

  Standardized PRO instrument being used

- scoring_method:

  Method for calculating PRO scale scores

- reverse_coded_items:

  Items that need to be reverse-coded before scoring

- response_scale_min:

  Minimum value of the response scale (e.g., 1 for 1-5 Likert scale)

- response_scale_max:

  Maximum value of the response scale (e.g., 5 for 1-5 Likert scale)

- reliability_analysis:

  Perform reliability analysis (Cronbach's alpha, item-total
  correlations)

- validity_analysis:

  Perform validity analysis (construct validity, concurrent validity)

- factor_analysis:

  Perform exploratory and confirmatory factor analysis

- irt_analysis:

  Perform IRT analysis for item characteristics and scale properties

- dimensionality_test:

  Test scale dimensionality and factor structure

- measurement_invariance:

  Test measurement invariance across groups and time points

- missing_data_method:

  Method for handling missing item responses

- min_items_required:

  Minimum number of non-missing items required for scale scoring

- missing_threshold:

  Maximum proportion of missing items allowed for scoring (50 percent =
  0.5)

- clinical_interpretation:

  Provide clinical interpretation of PRO scores

- normative_comparison:

  Compare scores to published normative data

- minimal_important_difference:

  Analyze changes relative to minimal important difference (MID)

- mid_value:

  Minimal important difference value for clinical significance

- ceiling_floor_effects:

  Analyze ceiling and floor effects in PRO responses

- longitudinal_analysis:

  Perform longitudinal PRO analysis

- change_analysis:

  Method for analyzing PRO changes over time

- trajectory_analysis:

  Analyze PRO trajectories and patterns over time

- time_to_deterioration:

  Analyze time to meaningful PRO deterioration

- group_comparisons:

  Compare PRO scores between groups

- comparison_method:

  Statistical method for group comparisons

- effect_size_analysis:

  Calculate effect sizes for group differences

- multiple_comparisons:

  Method for multiple comparisons correction

- responder_analysis:

  Analyze proportion of patients with meaningful improvements

- responder_threshold:

  Threshold for defining treatment responders

- anchor_based_analysis:

  Use anchor variables to interpret PRO changes

- anchor_variables:

  External anchor variables for interpreting PRO changes

- distribution_based_analysis:

  Use distribution-based methods for interpreting changes

- data_quality_assessment:

  Assess PRO data quality and completion patterns

- response_patterns:

  Analyze response patterns and potential response bias

- acquiescence_analysis:

  Analyze acquiescence and extreme response styles

- detailed_output:

  Include detailed psychometric and clinical analysis results

- summary_report:

  Generate comprehensive PRO analysis summary report

- individual_profiles:

  Generate individual patient PRO profiles

- save_scores:

  Save calculated PRO scores to dataset

- regulatory_documentation:

  Include documentation for regulatory submissions

## Value

A results object containing:

|                                                |     |     |     |     |          |
|------------------------------------------------|-----|-----|-----|-----|----------|
| `results$scale_overview`                       |     |     |     |     | a table  |
| `results$data_summary`                         |     |     |     |     | a table  |
| `results$item_statistics`                      |     |     |     |     | a table  |
| `results$item_correlations`                    |     |     |     |     | a table  |
| `results$reliability_results`                  |     |     |     |     | a table  |
| `results$item_reliability`                     |     |     |     |     | a table  |
| `results$validity_results`                     |     |     |     |     | a table  |
| `results$factor_analysis_results`              |     |     |     |     | a table  |
| `results$factor_loadings`                      |     |     |     |     | a table  |
| `results$scale_scores`                         |     |     |     |     | a table  |
| `results$score_distribution`                   |     |     |     |     | a table  |
| `results$group_comparison_results`             |     |     |     |     | a table  |
| `results$group_descriptives`                   |     |     |     |     | a table  |
| `results$longitudinal_results`                 |     |     |     |     | a table  |
| `results$change_analysis_results`              |     |     |     |     | a table  |
| `results$clinical_interpretation_results`      |     |     |     |     | a table  |
| `results$minimal_important_difference_results` |     |     |     |     | a table  |
| `results$data_quality_results`                 |     |     |     |     | a table  |
| `results$missing_data_patterns`                |     |     |     |     | a table  |
| `results$response_patterns`                    |     |     |     |     | a table  |
| `results$responder_analysis_results`           |     |     |     |     | a table  |
| `results$regulatory_summary`                   |     |     |     |     | a table  |
| `results$item_distribution_plot`               |     |     |     |     | an image |
| `results$scale_distribution_plot`              |     |     |     |     | an image |
| `results$reliability_plot`                     |     |     |     |     | an image |
| `results$factor_plot`                          |     |     |     |     | an image |
| `results$group_comparison_plot`                |     |     |     |     | an image |
| `results$longitudinal_plot`                    |     |     |     |     | an image |
| `results$change_plot`                          |     |     |     |     | an image |
| `results$responder_plot`                       |     |     |     |     | an image |
| `results$missing_data_plot`                    |     |     |     |     | an image |
| `results$correlation_heatmap`                  |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$scale_overview$asDF`

`as.data.frame(results$scale_overview)`
