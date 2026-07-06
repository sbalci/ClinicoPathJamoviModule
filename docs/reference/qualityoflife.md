# Quality of Life Analysis & Patient-Centered Outcomes

Specialized Quality of Life (QoL) analysis for clinical research and
patient care. Comprehensive assessment of health-related quality of life
using standardized instruments (SF-36, EORTC QLQ-C30, FACT, EQ-5D) with
domain-specific scoring, clinical interpretation, and longitudinal
change analysis. Includes utilities calculation, preference-based
measures, and health economic outcomes. Essential for clinical trials,
patient-centered care, and health technology assessment.

## Usage

``` r
qualityoflife(
  data,
  physical_function_items,
  role_physical_items,
  bodily_pain_items,
  general_health_items,
  vitality_items,
  social_function_items,
  role_emotional_items,
  mental_health_items,
  symptom_items,
  functional_items,
  global_qol_items,
  patient_id,
  time_var,
  group_var,
  clinical_vars,
  instrument_type = "sf36",
  scoring_algorithm = "standard",
  reference_population = "general_us",
  missing_domain_threshold = 0.5,
  imputation_method = "person_mean",
  quality_control = TRUE,
  response_pattern_analysis = TRUE,
  clinical_interpretation = TRUE,
  minimally_important_difference = TRUE,
  mid_values = "",
  ceiling_floor_analysis = TRUE,
  health_utilities = FALSE,
  utility_algorithm = "sf6d",
  longitudinal_analysis = FALSE,
  change_analysis = "simple_change",
  baseline_adjustment = TRUE,
  time_to_deterioration = FALSE,
  group_comparisons = FALSE,
  comparison_domains = "all_domains",
  statistical_test = "t_test",
  multiple_testing_correction = "fdr",
  domain_correlations = TRUE,
  clinical_correlations = FALSE,
  predictive_modeling = FALSE,
  factor_analysis = FALSE,
  clustering_analysis = FALSE,
  health_economics = FALSE,
  qaly_calculation = FALSE,
  cost_effectiveness = FALSE,
  cost_variables,
  comprehensive_report = TRUE,
  domain_profiles = FALSE,
  population_norms_comparison = TRUE,
  clinical_cutoffs = TRUE,
  save_domain_scores = FALSE,
  save_utility_scores = FALSE,
  regulatory_documentation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- physical_function_items:

  Items measuring physical functioning domain

- role_physical_items:

  Items measuring role limitations due to physical problems

- bodily_pain_items:

  Items measuring bodily pain domain

- general_health_items:

  Items measuring general health perceptions

- vitality_items:

  Items measuring vitality and energy levels

- social_function_items:

  Items measuring social functioning domain

- role_emotional_items:

  Items measuring role limitations due to emotional problems

- mental_health_items:

  Items measuring mental health domain

- symptom_items:

  Items measuring disease-specific symptoms

- functional_items:

  Additional functional assessment items

- global_qol_items:

  Global quality of life assessment items

- patient_id:

  Patient identifier for longitudinal analysis

- time_var:

  Time point variable (visit, week, month)

- group_var:

  Grouping variable for comparisons (treatment, disease stage)

- clinical_vars:

  Clinical variables for correlation and validation

- instrument_type:

  Type of QoL instrument being analyzed

- scoring_algorithm:

  Scoring algorithm for QoL domains

- reference_population:

  Reference population for norm-based scoring

- missing_domain_threshold:

  Maximum missing proportion for domain scoring (50 percent = 0.5)

- imputation_method:

  Method for handling missing QoL data

- quality_control:

  Perform QoL data quality control checks

- response_pattern_analysis:

  Analyze response patterns and validity

- clinical_interpretation:

  Provide clinical interpretation of QoL scores

- minimally_important_difference:

  Apply minimally important difference thresholds

- mid_values:

  Custom MID values for domains (comma-separated, e.g., "5,10,3")

- ceiling_floor_analysis:

  Analyze ceiling and floor effects by domain

- health_utilities:

  Calculate health utility values for health economics

- utility_algorithm:

  Algorithm for health utility calculation

- longitudinal_analysis:

  Perform longitudinal QoL analysis

- change_analysis:

  Method for analyzing QoL changes over time

- baseline_adjustment:

  Adjust for baseline QoL differences in change analysis

- time_to_deterioration:

  Analyze time to meaningful QoL deterioration

- group_comparisons:

  Compare QoL between groups

- comparison_domains:

  QoL domains to include in group comparisons

- statistical_test:

  Statistical test for group comparisons

- multiple_testing_correction:

  Multiple testing correction method

- domain_correlations:

  Analyze correlations between QoL domains

- clinical_correlations:

  Correlate QoL domains with clinical variables

- predictive_modeling:

  Build predictive models for QoL outcomes

- factor_analysis:

  Perform factor analysis of QoL domains

- clustering_analysis:

  Cluster patients based on QoL profiles

- health_economics:

  Calculate health economic outcomes

- qaly_calculation:

  Calculate Quality-Adjusted Life Years

- cost_effectiveness:

  Perform cost-effectiveness analysis (requires cost data)

- cost_variables:

  Cost variables for health economic analysis

- comprehensive_report:

  Generate comprehensive QoL analysis report

- domain_profiles:

  Generate individual patient QoL domain profiles

- population_norms_comparison:

  Compare scores to population normative data

- clinical_cutoffs:

  Apply clinical cutoffs and severity classifications

- save_domain_scores:

  Save calculated domain scores to dataset

- save_utility_scores:

  Save calculated utility scores to dataset

- regulatory_documentation:

  Include regulatory compliance documentation

## Value

A results object containing:

|                                           |     |     |     |     |          |
|-------------------------------------------|-----|-----|-----|-----|----------|
| `results$qol_overview`                    |     |     |     |     | a table  |
| `results$domain_configuration`            |     |     |     |     | a table  |
| `results$domain_scores`                   |     |     |     |     | a table  |
| `results$summary_scores`                  |     |     |     |     | a table  |
| `results$ceiling_floor_effects`           |     |     |     |     | a table  |
| `results$population_norms`                |     |     |     |     | a table  |
| `results$group_comparison_domains`        |     |     |     |     | a table  |
| `results$group_descriptives`              |     |     |     |     | a table  |
| `results$longitudinal_summary`            |     |     |     |     | a table  |
| `results$change_analysis_results`         |     |     |     |     | a table  |
| `results$health_utilities_summary`        |     |     |     |     | a table  |
| `results$qaly_results`                    |     |     |     |     | a table  |
| `results$domain_correlations`             |     |     |     |     | a table  |
| `results$clinical_correlations`           |     |     |     |     | a table  |
| `results$quality_control_results`         |     |     |     |     | a table  |
| `results$response_patterns`               |     |     |     |     | a table  |
| `results$mid_analysis`                    |     |     |     |     | a table  |
| `results$clinical_interpretation_summary` |     |     |     |     | a table  |
| `results$severity_classification`         |     |     |     |     | a table  |
| `results$health_economics_summary`        |     |     |     |     | a table  |
| `results$cost_effectiveness_results`      |     |     |     |     | a table  |
| `results$regulatory_summary`              |     |     |     |     | a table  |
| `results$domain_scores_plot`              |     |     |     |     | an image |
| `results$summary_scores_plot`             |     |     |     |     | an image |
| `results$population_comparison_plot`      |     |     |     |     | an image |
| `results$group_comparison_plot`           |     |     |     |     | an image |
| `results$longitudinal_trajectory_plot`    |     |     |     |     | an image |
| `results$change_waterfall_plot`           |     |     |     |     | an image |
| `results$domain_correlation_heatmap`      |     |     |     |     | an image |
| `results$utilities_distribution_plot`     |     |     |     |     | an image |
| `results$ceiling_floor_plot`              |     |     |     |     | an image |
| `results$radar_plot`                      |     |     |     |     | an image |
| `results$missing_data_pattern_plot`       |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$qol_overview$asDF`

`as.data.frame(results$qol_overview)`
