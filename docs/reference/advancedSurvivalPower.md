# Advanced Survival Power Analysis (Advanced)

Basic power analysis for survival studies with simple and complex design
options. This helps researchers design adequately powered studies to
detect meaningful differences in survival between groups.

## Usage

``` r
advancedSurvivalPower(
  data,
  calc_type = "sample_size",
  study_design = "simple",
  hazard_ratio = 0.7,
  power = 0.8,
  alpha = 0.05,
  sample_size = 200,
  allocation_ratio = 1,
  prob_event = 0.5,
  accrual_time = 1,
  follow_up_time = 3,
  median_survival = 5,
  loss_followup = 0.05
)
```

## Arguments

- data:

  The data as a data frame. Note that power analysis typically doesn't
  require actual data - it's used for study planning.

- calc_type:

  Select what to calculate. 'Power' calculates statistical power given
  sample size and hazard ratio. 'Sample Size' determines required sample
  size for desired power and hazard ratio. 'Hazard Ratio' calculates the
  minimum detectable effect size given sample size and power.

- study_design:

  Select the study design complexity. 'Simple' assumes a fixed follow-up
  period for all subjects. 'Complex' allows for accrual period and
  variable follow-up times.

- hazard_ratio:

  The hazard ratio to detect. Values \< 1 indicate protective effects
  (treatment better than control); values \> 1 indicate harmful effects
  (control better than treatment).

- power:

  The probability of detecting an effect if one exists (1 minus the Type
  II error rate). Conventional values are 0.8 or 0.9.

- alpha:

  The Type I error rate (probability of falsely rejecting the null
  hypothesis). Conventional value is 0.05.

- sample_size:

  The total number of subjects across all groups. For sample size
  calculation, this is a starting value for the search algorithm.

- allocation_ratio:

  The ratio of control group size to treatment group size. 1 indicates
  equal allocation. Values \> 1 mean more subjects in the control group;
  values \< 1 mean more in the treatment group.

- prob_event:

  The overall probability of observing the event (e.g., death) during
  the study period. This affects the number of events observed, which is
  crucial for power.

- accrual_time:

  The period over which participants are recruited, in years. Only used
  for complex designs.

- follow_up_time:

  The additional follow-up period after accrual ends, in years. Only
  used for complex designs.

- median_survival:

  The median survival time in the control group, in years. Used to
  estimate the baseline hazard rate. Only used for complex designs.

- loss_followup:

  The annual rate of loss to follow-up (attrition). Only used for
  complex designs.

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$message`             |     |     |     |     | a html   |
| `results$power_result`        |     |     |     |     | a html   |
| `results$sample_size_result`  |     |     |     |     | a html   |
| `results$hazard_ratio_result` |     |     |     |     | a html   |
| `results$power_plot`          |     |     |     |     | an image |

## Details

This is the straightforward power analysis function. For more advanced
methods including competing risks, RMST analysis, and additional
statistical tests, also consider other specialized power functions.
