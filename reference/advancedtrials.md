# Advanced Clinical Trial Methods

Advanced clinical trial methodologies including group sequential
designs, adaptive trials, and interim monitoring. Provides sophisticated
statistical methods for non-inferiority trials, futility analysis, and
early stopping rules. Implements cutting-edge designs such as seamless
Phase II/III, platform trials, and master protocol studies. Designed for
clinical trialists requiring state-of-the-art methodology and regulatory
compliance.

## Usage

``` r
advancedtrials(
  data,
  time_var,
  event_var,
  treatment_var,
  biomarker_var,
  interim_time_var,
  stratification_vars,
  design_type = "group_sequential",
  primary_endpoint = "overall_survival",
  statistical_test = "log_rank",
  alpha_spending = "obrien_fleming",
  beta_spending = "obrien_fleming",
  number_of_looks = 3,
  information_fractions = "0.5,0.75,1.0",
  efficacy_boundaries = "auto",
  adaptation_type = "sample_size_reestimation",
  adaptation_timing = 50,
  conditional_power_threshold = 0.3,
  ni_margin = 1.25,
  ni_margin_type = "hazard_ratio",
  overall_alpha = 0.025,
  overall_power = 0.8,
  expected_hr = 0.75,
  accrual_rate = 25,
  dropout_rate = 0.05,
  number_of_arms = 2,
  control_arm_sharing = TRUE,
  interim_arm_addition = FALSE,
  interim_arm_dropping = FALSE,
  biomarker_strategy = "all_comers",
  biomarker_prevalence = 0.3,
  futility_analysis = TRUE,
  stochastic_curtailment = FALSE,
  predictive_power_threshold = 0.1,
  interim_monitoring_plan = TRUE,
  operating_characteristics = TRUE,
  boundary_plots = TRUE,
  conditional_power_analysis = FALSE,
  predictive_power_analysis = FALSE,
  bias_adjustment = FALSE,
  multiplicity_adjustment = FALSE,
  simulation_runs = 10000,
  seed_value = 12345
)
```

## Arguments

- data:

  the data as a data frame

- time_var:

  Time to primary endpoint (survival, progression, response)

- event_var:

  Event indicator (1=event, 0=censored)

- treatment_var:

  Treatment arm assignment (experimental vs control)

- biomarker_var:

  Biomarker for enrichment or stratification strategies

- interim_time_var:

  Calendar time of interim analysis

- stratification_vars:

  Variables for stratified randomization

- design_type:

  Advanced clinical trial design type

- primary_endpoint:

  Primary efficacy endpoint

- statistical_test:

  Statistical test for primary analysis

- alpha_spending:

  Alpha spending function for efficacy boundaries

- beta_spending:

  Beta spending function for futility boundaries

- number_of_looks:

  Total number of planned interim looks

- information_fractions:

  Information fractions for each analysis (comma-separated)

- efficacy_boundaries:

  Custom efficacy boundaries or 'auto' for spending function

- adaptation_type:

  Type of mid-trial adaptation

- adaptation_timing:

  Percentage of planned events for adaptation decision

- conditional_power_threshold:

  Threshold for sample size re-estimation

- ni_margin:

  Non-inferiority margin (hazard ratio scale)

- ni_margin_type:

  Scale for non-inferiority margin

- overall_alpha:

  One-sided Type I error rate

- overall_power:

  Overall study power

- expected_hr:

  Expected treatment effect (hazard ratio)

- accrual_rate:

  Expected patient accrual rate

- dropout_rate:

  Annual patient dropout rate

- number_of_arms:

  Total number of treatment arms

- control_arm_sharing:

  Share control arm across comparisons

- interim_arm_addition:

  Allow adding arms during trial

- interim_arm_dropping:

  Allow dropping arms for futility

- biomarker_strategy:

  Biomarker-driven design strategy

- biomarker_prevalence:

  Prevalence of positive biomarker

- futility_analysis:

  Include formal futility testing

- stochastic_curtailment:

  Use stochastic curtailment approach

- predictive_power_threshold:

  Threshold for futility based on predictive power

- interim_monitoring_plan:

  Generate detailed interim monitoring guidelines

- operating_characteristics:

  Calculate design operating characteristics via simulation

- boundary_plots:

  Generate efficacy and futility boundary plots

- conditional_power_analysis:

  Calculate conditional power at interim analyses

- predictive_power_analysis:

  Calculate predictive power for study completion

- bias_adjustment:

  Apply bias adjustment methods for adaptive designs

- multiplicity_adjustment:

  Apply multiplicity adjustment for multiple arms/endpoints

- simulation_runs:

  Number of simulations for operating characteristics

- seed_value:

  Random seed for reproducible simulations

## Value

A results object containing:

|                                          |     |     |     |     |          |
|------------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                   |     |     |     |     | a html   |
| `results$trial_design_summary`           |     |     |     |     | a table  |
| `results$group_sequential_boundaries`    |     |     |     |     | a table  |
| `results$adaptive_design_parameters`     |     |     |     |     | a table  |
| `results$platform_trial_design`          |     |     |     |     | a table  |
| `results$biomarker_strategy_results`     |     |     |     |     | a table  |
| `results$operating_characteristics`      |     |     |     |     | a table  |
| `results$interim_monitoring_guidelines`  |     |     |     |     | a table  |
| `results$sample_size_calculations`       |     |     |     |     | a table  |
| `results$futility_analysis_results`      |     |     |     |     | a table  |
| `results$multiplicity_adjustment`        |     |     |     |     | a table  |
| `results$regulatory_compliance`          |     |     |     |     | a table  |
| `results$boundary_plot`                  |     |     |     |     | an image |
| `results$operating_characteristics_plot` |     |     |     |     | an image |
| `results$conditional_power_plot`         |     |     |     |     | an image |
| `results$sample_size_sensitivity_plot`   |     |     |     |     | an image |
| `results$platform_trial_flowchart`       |     |     |     |     | an image |
| `results$design_recommendations`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$trial_design_summary$asDF`

`as.data.frame(results$trial_design_summary)`
