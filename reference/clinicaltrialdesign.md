# Clinical Trial Design & Power Analysis

Comprehensive power analysis and sample size calculation for clinical
trial design. Covers the most common clinical trial types with
appropriate statistical tests, effect size calculations, and regulatory
considerations for clinical research.

## Usage

``` r
clinicaltrialdesign(
  data,
  trial_type = "superiority",
  outcome_type = "continuous",
  test_type = "two_sample_ttest",
  calculation_type = "sample_size",
  alpha = 0.05,
  power = 0.8,
  sample_size = 100,
  effect_size = 0.5,
  mean_difference = 1,
  common_sd = 2,
  proportion1 = 0.3,
  proportion2 = 0.5,
  allocation_ratio = 1,
  two_sided = TRUE,
  continuity_correction = TRUE,
  margin = 0.1,
  margin_type = "absolute",
  dropout_rate = 10,
  interim_analyses = 0,
  multiple_comparisons = "none",
  show_assumptions = TRUE,
  show_interpretation = TRUE,
  show_sensitivity = TRUE,
  show_plots = TRUE,
  regulatory_context = "ich"
)
```

## Arguments

- data:

  the data as a data frame (optional for power calculations)

- trial_type:

  Type of clinical trial design for appropriate power calculations

- outcome_type:

  Type of primary outcome variable determining appropriate statistical
  test

- test_type:

  Statistical test appropriate for the outcome type and study design

- calculation_type:

  What to calculate - power, sample size, or detectable effect size

- alpha:

  Significance level (typically 0.05 for superiority, 0.025 for
  non-inferiority)

- power:

  Desired statistical power (typically 0.80 or 0.90)

- sample_size:

  Total sample size for power calculation

- effect_size:

  Standardized effect size (Cohen's d for continuous outcomes)

- mean_difference:

  Expected difference in means between groups

- common_sd:

  Pooled standard deviation for continuous outcomes

- proportion1:

  Expected proportion in control/reference group

- proportion2:

  Expected proportion in treatment/experimental group

- allocation_ratio:

  Ratio of treatment to control group sizes (1 = equal allocation)

- two_sided:

  Use two-sided statistical test (recommended for most trials)

- continuity_correction:

  Apply continuity correction for proportion tests

- margin:

  Non-inferiority or equivalence margin (absolute difference)

- margin_type:

  Type of margin for non-inferiority/equivalence testing

- dropout_rate:

  Expected dropout/loss to follow-up rate for sample size inflation

- interim_analyses:

  Number of planned interim analyses (affects alpha spending)

- multiple_comparisons:

  Adjustment for multiple testing (when applicable)

- show_assumptions:

  Display assumptions for the selected statistical test

- show_interpretation:

  Include clinical interpretation and regulatory considerations

- show_sensitivity:

  Perform sensitivity analysis across parameter ranges

- show_plots:

  Generate power/sample size relationship plots

- regulatory_context:

  Regulatory context for power analysis considerations

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Instructions for clinical trial design and power analysis |
| `results$design_summary` |  |  |  |  | Summary of trial design parameters and recommendations |
| `results$power_results` |  |  |  |  | Primary power analysis calculations and results |
| `results$sample_size_breakdown` |  |  |  |  | Detailed sample size calculations with adjustments |
| `results$effect_size_analysis` |  |  |  |  | Effect size calculations and clinical significance assessment |
| `results$assumptions_check` |  |  |  |  | Key assumptions for the selected statistical test |
| `results$sensitivity_analysis` |  |  |  |  | Power/sample size sensitivity across parameter ranges |
| `results$regulatory_considerations` |  |  |  |  | Regulatory guidance and compliance considerations |
| `results$power_curve` |  |  |  |  | Power curves showing relationship between sample size and statistical power |
| `results$effect_size_plot` |  |  |  |  | Effect size distribution and clinical significance thresholds |
| `results$sample_size_plot` |  |  |  |  | Sample size requirements across different effect sizes and power levels |
| `results$clinical_interpretation` |  |  |  |  | Clinical context, interpretation guidelines, and next steps |
| `results$study_protocol_template` |  |  |  |  | Template sections for study protocol statistical analysis plan |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$design_summary$asDF`

`as.data.frame(results$design_summary)`

## Details

Supports randomized controlled trials (RCTs), equivalence trials,
non-inferiority trials, superiority trials, and observational studies
with proper power calculations.
