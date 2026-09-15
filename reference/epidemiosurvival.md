# Epidemiological Survival Analysis

Epidemiological survival analysis for population-based studies and
observational research. Provides specialized methods for cohort survival
analysis, case-cohort designs, and population attributable risk
calculation. Includes adjustment for sampling schemes, competing
mortality, and population stratification. Designed for epidemiological
research including cancer registry studies, cohort studies, and
population surveillance.

## Usage

``` r
epidemiosurvival(
  data,
  time_var,
  event_var,
  exposure_var,
  age_var,
  calendar_time,
  population_weights,
  subcohort_indicator,
  stratification_vars,
  competing_events,
  analysis_type = "cohort_survival",
  cohort_design = "prospective",
  sampling_method = "simple_random",
  survival_method = "kaplan_meier",
  regression_method = "cox_robust",
  age_standardization = "none",
  reference_population = "study_population",
  age_groups = "0-49,50-59,60-69,70+",
  calendar_periods = "1990-1999,2000-2009,2010-2019",
  subcohort_size = 1000,
  case_cohort_method = "prentice",
  par_method = "levin",
  confidence_level = 0.95,
  cohort_life_tables = TRUE,
  competing_risks_analysis = FALSE,
  age_period_cohort_effects = FALSE,
  population_impact_measures = FALSE,
  survival_disparities = FALSE,
  trend_analysis = FALSE,
  excess_mortality = FALSE,
  standardized_mortality_ratio = FALSE,
  survival_curves = TRUE,
  cumulative_incidence_plots = FALSE,
  age_specific_rates = FALSE,
  trend_plots = FALSE,
  forest_plots = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- time_var:

  Follow-up time (years, months, or days)

- event_var:

  Event indicator (1=event, 0=censored)

- exposure_var:

  Primary exposure of interest for epidemiological analysis

- age_var:

  Age at study entry or baseline (years)

- calendar_time:

  Calendar year or period of study entry

- population_weights:

  Sampling or population weights for complex survey designs

- subcohort_indicator:

  Subcohort membership indicator for case-cohort designs

- stratification_vars:

  Variables for stratified analysis (e.g., sex, region, socioeconomic
  status)

- competing_events:

  Competing event indicators for competing mortality analysis

- analysis_type:

  Type of epidemiological survival analysis

- cohort_design:

  Type of cohort study design

- sampling_method:

  Sampling method used in the study design

- survival_method:

  Method for survival function estimation

- regression_method:

  Regression method for hazard estimation

- age_standardization:

  Method for age standardization

- reference_population:

  Reference population for standardization (study_population, external,
  or custom)

- age_groups:

  Age group categories for stratified analysis (comma-separated)

- calendar_periods:

  Calendar period categories for period analysis

- subcohort_size:

  Size of subcohort for case-cohort analysis

- case_cohort_method:

  Estimation method for case-cohort designs

- par_method:

  Method for population attributable risk calculation

- confidence_level:

  Confidence level for intervals

- cohort_life_tables:

  Generate cohort life tables

- competing_risks_analysis:

  Analyze competing mortality risks

- age_period_cohort_effects:

  Estimate age, period, and cohort effects

- population_impact_measures:

  Calculate population impact measures (PAR, PAF, NNT)

- survival_disparities:

  Analyze survival disparities by demographic factors

- trend_analysis:

  Analyze temporal trends in survival

- excess_mortality:

  Calculate excess mortality compared to general population

- standardized_mortality_ratio:

  Calculate SMR with confidence intervals

- survival_curves:

  Generate survival curves by exposure groups

- cumulative_incidence_plots:

  Plot cumulative incidence functions

- age_specific_rates:

  Plot age-specific mortality/incidence rates

- trend_plots:

  Plot survival trends over calendar time

- forest_plots:

  Generate forest plots for subgroup analyses

## Value

A results object containing:

|                                          |     |     |     |     |          |
|------------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                   |     |     |     |     | a html   |
| `results$cohort_summary`                 |     |     |     |     | a table  |
| `results$survival_estimates`             |     |     |     |     | a table  |
| `results$hazard_ratios`                  |     |     |     |     | a table  |
| `results$population_attributable_risk`   |     |     |     |     | a table  |
| `results$age_standardized_rates`         |     |     |     |     | a table  |
| `results$life_table`                     |     |     |     |     | a table  |
| `results$case_cohort_results`            |     |     |     |     | a table  |
| `results$competing_mortality`            |     |     |     |     | a table  |
| `results$apc_effects`                    |     |     |     |     | a table  |
| `results$survival_disparities_table`     |     |     |     |     | a table  |
| `results$trend_analysis_table`           |     |     |     |     | a table  |
| `results$excess_mortality_table`         |     |     |     |     | a table  |
| `results$survival_curves_plot`           |     |     |     |     | an image |
| `results$cumulative_incidence_plot`      |     |     |     |     | an image |
| `results$age_specific_rates_plot`        |     |     |     |     | an image |
| `results$trend_plot`                     |     |     |     |     | an image |
| `results$forest_plot`                    |     |     |     |     | an image |
| `results$epidemiological_interpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$cohort_summary$asDF`

`as.data.frame(results$cohort_summary)`
