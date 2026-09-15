# Comprehensive Survival Power Analysis (Expert)

Comprehensive power analysis and sample size calculation for survival
studies and clinical trials. Unifies all survival power methods:
standard log-rank tests, Cox regression, competing risks, RMST-based
analyses, non-inferiority trials, and specialized methods. Supports both
simple and complex study designs with accrual periods, variable
follow-up, and dropout considerations. Essential for study planning,
grant applications, and regulatory submissions.

## Usage

``` r
comprehensiveSurvivalPower(
  data,
  method_category = "standard",
  calculation_type = "sample_size",
  statistical_method = "log_rank_schoenfeld",
  study_design = "simple",
  alpha = 0.05,
  power = 0.8,
  sample_size = 200,
  allocation_ratio = 1,
  hazard_ratio = 0.75,
  median_survival_control = 12,
  median_survival_treatment = 16,
  accrual_period = 24,
  follow_up_period = 36,
  dropout_rate = 0.05,
  event_rate = 0.6,
  competing_event_rate = 0.2,
  cumulative_incidence_control = 0.4,
  cumulative_incidence_treatment = 0.3,
  rmst_timepoint = 24,
  non_inferiority_margin = 1.25,
  superiority_test = FALSE,
  snp_maf = 0.2,
  genetic_model = "additive",
  number_of_looks = 1,
  spending_function = "lan_demets_obf",
  show_detailed_output = TRUE,
  show_sensitivity_analysis = FALSE,
  show_power_curves = FALSE,
  export_study_design = FALSE,
  genetic_effect_size = 1.5,
  cure_rate_control = 0,
  cure_rate_treatment = 0,
  survival_distribution = "exponential",
  accrual_distribution = "uniform",
  interim_futility_boundary = 0.2,
  binding_futility = FALSE,
  number_of_covariates = 2,
  covariate_correlation = 0,
  interaction_effect_size = 1,
  covariate_distribution = "normal",
  adjust_for_confounders = TRUE,
  show_method_comparison = FALSE
)
```

## Arguments

- data:

  The data as a data frame (optional for power calculations).

- method_category:

  Choose the category of survival analysis method. Standard methods
  include log-rank tests and Cox regression. Competing risks methods
  analyze multiple event types. Advanced methods include RMST,
  non-inferiority, and specialized approaches.

- calculation_type:

  Select what parameter to calculate. Most commonly used are sample size
  (given desired power) and power (given available sample size).

- statistical_method:

  Choose the specific statistical method. Available options depend on
  the selected method category. Schoenfeld method is most common for
  log-rank tests.

- study_design:

  Simple design assumes fixed follow-up time. Complex design includes
  accrual period and variable follow-up. Sequential designs allow
  interim analyses.

- alpha:

  Type I error rate. Standard values: 0.05 (two-sided) or 0.025
  (one-sided). For non-inferiority trials, consider 0.025.

- power:

  Statistical power (probability of detecting true effect). Standard
  values: 0.80 (80 percent) or 0.90 (90 percent). Higher power requires
  larger sample sizes.

- sample_size:

  Total number of subjects in the study. Used when calculating power or
  detectable effect size from a fixed sample size.

- allocation_ratio:

  Ratio of control to treatment group sizes. 1.0 = equal allocation
  (1:1), 2.0 = twice as many controls (2:1). Equal allocation is most
  efficient.

- hazard_ratio:

  Expected hazard ratio. Values \< 1 indicate treatment benefit. Common
  values: 0.75 (25 percent reduction), 0.67 (33 percent reduction), 0.50
  (50 percent reduction).

- median_survival_control:

  Expected median survival time in control group. Units should match
  your study timeline (months, years, etc.).

- median_survival_treatment:

  Expected median survival time in treatment group. Should be longer
  than control group for beneficial treatments.

- accrual_period:

  Time period for patient recruitment (months/years). Longer accrual
  periods allow for smaller sample sizes but extend study duration.

- follow_up_period:

  Additional follow-up time after accrual completion. Total study
  duration = accrual period + follow-up period.

- dropout_rate:

  Annual rate of loss to follow-up (0.05 = 5 percent per year). Higher
  dropout rates require larger sample sizes to maintain power.

- event_rate:

  Proportion of subjects expected to experience the event during study
  period. Used for simple design calculations.

- competing_event_rate:

  Proportion experiencing competing events (for competing risks
  analysis). Should be less than primary event rate.

- cumulative_incidence_control:

  Expected cumulative incidence of primary event in control group (for
  competing risks analysis).

- cumulative_incidence_treatment:

  Expected cumulative incidence of primary event in treatment group (for
  competing risks analysis).

- rmst_timepoint:

  Time point for restricted mean survival time analysis. Should be
  clinically meaningful and within study follow-up period.

- non_inferiority_margin:

  Non-inferiority margin for hazard ratio. 1.25 means treatment is
  non-inferior if HR \< 1.25 (25 percent increase in hazard is
  acceptable).

- superiority_test:

  After establishing non-inferiority, test for superiority. Requires
  hierarchical testing approach to control Type I error.

- snp_maf:

  Minor allele frequency for SNP-based survival analysis. Common range:
  0.05-0.45.

- genetic_model:

  Genetic inheritance model for SNP analysis. Additive model is most
  common.

- number_of_looks:

  Number of planned interim analyses for sequential designs. More looks
  require larger sample sizes to maintain overall Type I error.

- spending_function:

  Method for spending Type I error across interim analyses.
  O'Brien-Fleming type preserves most alpha for final analysis.

- show_detailed_output:

  Display detailed calculation steps and intermediate results.

- show_sensitivity_analysis:

  Perform sensitivity analysis varying key parameters around their
  specified values (+/- 10 percent, 20 percent).

- show_power_curves:

  Generate plots showing power vs sample size, effect size, or other
  parameters.

- export_study_design:

  Generate a comprehensive study design document suitable for protocols
  and grant applications.

- genetic_effect_size:

  Genetic effect size for SNP-based survival analysis. Represents the
  hazard ratio per copy of the risk allele.

- cure_rate_control:

  Proportion of subjects in control group who will never experience the
  event (cure fraction). 0 = no cure, standard survival model.

- cure_rate_treatment:

  Proportion of subjects in treatment group who will never experience
  the event (cure fraction). Usually higher than control group in
  effective treatments.

- survival_distribution:

  Assumed underlying survival distribution. Exponential assumes constant
  hazard; Weibull allows increasing/decreasing hazard; Piecewise allows
  complex patterns.

- accrual_distribution:

  Pattern of patient enrollment over time. Uniform = constant rate;
  Increasing/Decreasing = linear change; Exponential = curved pattern.

- interim_futility_boundary:

  Futility boundary for interim analyses in sequential designs.
  Probability threshold below which study may be stopped for futility.

- binding_futility:

  Whether futility boundaries are binding (must stop if crossed) or
  non-binding (advisory only).

- number_of_covariates:

  Number of covariates to include in the Cox regression model. More
  covariates generally require larger sample sizes.

- covariate_correlation:

  Correlation between covariates in the model. Higher correlation
  reduces effective sample size and increases required sample size.

- interaction_effect_size:

  Effect size for interaction between main exposure and other
  covariates. Used when testing interaction effects.

- covariate_distribution:

  Distribution type for covariates in the model. Affects power
  calculations for epidemiological studies.

- adjust_for_confounders:

  Whether to account for confounding variables in power calculations.
  Generally recommended for epidemiological studies.

- show_method_comparison:

  Compare results across multiple power calculation methods to assess
  robustness of sample size estimates.

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$powerResults`           |     |     |     |     | a table  |
| `results$methodDetails`          |     |     |     |     | a table  |
| `results$studyDesign`            |     |     |     |     | a html   |
| `results$assumptions`            |     |     |     |     | a table  |
| `results$sensitivityTable`       |     |     |     |     | a table  |
| `results$powerCurvePlot`         |     |     |     |     | an image |
| `results$effectSizePlot`         |     |     |     |     | an image |
| `results$methodComparison`       |     |     |     |     | a table  |
| `results$competingRisksDetails`  |     |     |     |     | a table  |
| `results$sequentialDesign`       |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$protocolSummary`        |     |     |     |     | a html   |
| `results$regulatoryNotes`        |     |     |     |     | a html   |
| `results$technicalDetails`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$powerResults$asDF`

`as.data.frame(results$powerResults)`
