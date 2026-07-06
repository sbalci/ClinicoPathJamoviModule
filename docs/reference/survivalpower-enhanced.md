# Enhanced Survival Analysis Power & Sample Size

Comprehensive power analysis and sample size calculation for survival
studies using Lachin-Foulkes and Schoenfeld methods from the gsDesign
package. Enhanced with visualizations, export functionality, and
clinical examples.

## Usage

``` r
survivalpower(
  calculation_type = c("sample_size", "power", "events", "hazard_ratio"),
  method = c("lachin_foulkes", "schoenfeld"), 
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  hazard_ratio = 0.6,
  study_duration = 24,
  accrual_duration = 12,
  alpha = 0.025,
  beta = 0.1,
  power = 0.9,
  allocation_ratio = 1,
  sample_size_input = 100,
  events_input = 50,
  sided = c("1", "2"),
  entry_type = c("unif", "expo"),
  gamma = 0,
  dropout_rate = 0,
  show_summary = TRUE,
  show_formulas = FALSE,
  show_interpretation = TRUE,
  show_power_plot = FALSE,
  show_timeline_plot = FALSE,
  power_plot_range = "auto",
  export_results = FALSE,
  export_power_curve = FALSE
)
```

## Arguments

- calculation_type:

  Type of calculation: "sample_size", "power", "events", or
  "hazard_ratio"

- method:

  Calculation method: "lachin_foulkes" (full study design) or
  "schoenfeld" (events-based)

- hazard_control:

  Event hazard rate for control group (events per time unit)

- hazard_treatment:

  Event hazard rate for treatment group (events per time unit)

- hazard_ratio:

  Hazard ratio (treatment/control) for Schoenfeld method

- study_duration:

  Maximum study duration (months or years)

- accrual_duration:

  Patient accrual (recruitment) duration

- alpha:

  Type I error rate (significance level)

- beta:

  Type II error rate (1 - power)

- power:

  Statistical power (1 - beta)

- allocation_ratio:

  Randomization ratio (treatment:control)

- sample_size_input:

  Sample size for power calculation

- events_input:

  Number of events for power calculation

- sided:

  Test type: "1" for one-sided, "2" for two-sided

- entry_type:

  Patient entry pattern: "unif" for uniform, "expo" for exponential

- gamma:

  Rate parameter for exponential entry

- dropout_rate:

  Equal dropout hazard rate for both groups

- show_summary:

  Whether to display comprehensive study design summary

- show_formulas:

  Whether to display mathematical formulas

- show_interpretation:

  Whether to include clinical interpretation

- show_power_plot:

  Whether to display power curve visualization

- show_timeline_plot:

  Whether to display study timeline (Lachin-Foulkes only)

- power_plot_range:

  Sample size range for power plots ("auto" or "min,max")

- export_results:

  Whether to export detailed results

- export_power_curve:

  Whether to export power curve data

## Details

This enhanced version of survivalpower includes several major
improvements:

**Calculation Methods:**

- **Lachin-Foulkes Method:** Full study design approach accounting for
  accrual duration, follow-up period, dropout rates, and patient entry
  patterns

- **Schoenfeld Method:** Events-based approximation using asymptotic
  normal distribution of log-rank statistic for quick estimates

**Calculation Types:**

- **Sample Size:** Calculate required sample size for desired power

- **Power Analysis:** Calculate power given sample size and effect size

- **Events Calculation:** Determine number of events needed

- **Hazard Ratio Detection:** Minimum detectable effect size

**Enhanced Visualizations:**

- **Power Curves:** Interactive plots showing power vs sample size
  relationships

- **Study Timeline:** Visual representation of accrual and follow-up
  periods

- **Effect Detection:** Visualization of detectable effect sizes

**Export Functionality:**

- **Results Export:** Comprehensive results suitable for protocol
  documents

- **Power Curve Export:** Data points for external analysis and custom
  plotting

- **CSV/Excel Compatible:** Ready for regulatory submissions

**Clinical Examples:** See `survivalpower_scenarios` dataset for
pre-configured clinical trial scenarios including oncology, cardiology,
neurology, and rare disease studies.

## Value

A survivalpowerResults object containing:

- **power_results:** HTML-formatted comprehensive results

- **formulas:** Mathematical formulas and references (optional)

- **interpretation:** Clinical interpretation and recommendations
  (optional)

- **power_plot:** Power curve visualization (optional)

- **timeline_plot:** Study timeline visualization (optional)

- **exported_results:** Detailed results data frame (optional)

- **exported_power_curve:** Power curve data points (optional)

- **export_summary:** Export summary and usage instructions (optional)

## References

- Lachin JM, Foulkes MA (1986). Evaluation of sample size and power for
  analyses of survival with allowance for nonuniform patient entry,
  losses to follow-up, noncompliance, and stratification. Biometrics
  42:507-519.

- Schoenfeld D (1981). The asymptotic properties of nonparametric tests
  for comparing survival distributions. Biometrika 68:316-319.

- Jennison C, Turnbull BW (2000). Group Sequential Methods with
  Applications to Clinical Trials. Chapman & Hall/CRC.

- Anderson KM, Seibold H (2019). gsDesign: An R Package for Group
  Sequential Design. R package version 3.1.1.

## See also

[`survival`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/survival.md),
[`survivalcont`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/survivalcont.md),
`powersurvival`, `survivalpower_scenarios`

## Author

ClinicoPath Development Team

## Examples

``` r
# Example 1: Oncology Phase III Trial - Sample Size Calculation
oncology_trial <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,      # ~12 month median survival
  hazard_treatment = 0.042,    # ~24 month median survival
  study_duration = 36,         # 3 year study
  accrual_duration = 24,       # 2 year accrual
  alpha = 0.025,              # One-sided 2.5%
  beta = 0.1,                 # 90% power
  show_interpretation = TRUE
)
#> Error in survivalpower(calculation_type = "sample_size", method = "lachin_foulkes",     hazard_control = 0.083, hazard_treatment = 0.042, study_duration = 36,     accrual_duration = 24, alpha = 0.025, beta = 0.1, show_interpretation = TRUE): could not find function "survivalpower"

# Example 2: Cardiology Trial - Events-Based Quick Calculation
cardio_trial <- survivalpower(
  calculation_type = "events",
  method = "schoenfeld",
  hazard_ratio = 0.75,        # 25% risk reduction
  alpha = 0.025,
  beta = 0.2,                 # 80% power
  show_summary = TRUE
)
#> Error in survivalpower(calculation_type = "events", method = "schoenfeld",     hazard_ratio = 0.75, alpha = 0.025, beta = 0.2, show_summary = TRUE): could not find function "survivalpower"

# Example 3: Power Analysis with Visualization
power_analysis <- survivalpower(
  calculation_type = "power",
  method = "schoenfeld", 
  hazard_ratio = 0.6,
  events_input = 150,
  show_power_plot = TRUE,     # Show power curve
  power_plot_range = "50,400"
)
#> Error in survivalpower(calculation_type = "power", method = "schoenfeld",     hazard_ratio = 0.6, events_input = 150, show_power_plot = TRUE,     power_plot_range = "50,400"): could not find function "survivalpower"

# Example 4: Complex Study Design with Timeline
complex_study <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.05,
  hazard_treatment = 0.03,
  study_duration = 48,
  accrual_duration = 30,
  entry_type = "expo",        # Exponential accrual
  gamma = 1,
  dropout_rate = 0.02,
  allocation_ratio = 2,       # 2:1 randomization
  show_timeline_plot = TRUE,  # Show study phases
  show_formulas = TRUE
)
#> Error in survivalpower(calculation_type = "sample_size", method = "lachin_foulkes",     hazard_control = 0.05, hazard_treatment = 0.03, study_duration = 48,     accrual_duration = 30, entry_type = "expo", gamma = 1, dropout_rate = 0.02,     allocation_ratio = 2, show_timeline_plot = TRUE, show_formulas = TRUE): could not find function "survivalpower"

# Example 5: Export for External Analysis
export_analysis <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  study_duration = 36,
  accrual_duration = 24,
  export_results = TRUE,      # Export main results
  export_power_curve = TRUE,  # Export power curve
  power_plot_range = "200,600"
)
#> Error in survivalpower(calculation_type = "sample_size", method = "lachin_foulkes",     hazard_control = 0.083, hazard_treatment = 0.042, study_duration = 36,     accrual_duration = 24, export_results = TRUE, export_power_curve = TRUE,     power_plot_range = "200,600"): could not find function "survivalpower"

# Example 6: Using Pre-configured Scenarios
data(survivalpower_scenarios)
oncology_scenario <- survivalpower_scenarios[1, ]  # Oncology Phase III

scenario_analysis <- survivalpower(
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = oncology_scenario$Control_Hazard_Rate,
  hazard_treatment = oncology_scenario$Treatment_Hazard_Rate,
  study_duration = oncology_scenario$Study_Duration_Months,
  accrual_duration = oncology_scenario$Accrual_Duration_Months,
  alpha = oncology_scenario$Alpha_Level,
  power = oncology_scenario$Target_Power,
  allocation_ratio = oncology_scenario$Allocation_Ratio,
  dropout_rate = oncology_scenario$Dropout_Rate,
  show_interpretation = TRUE
)
#> Error in survivalpower(calculation_type = "sample_size", method = "lachin_foulkes",     hazard_control = oncology_scenario$Control_Hazard_Rate, hazard_treatment = oncology_scenario$Treatment_Hazard_Rate,     study_duration = oncology_scenario$Study_Duration_Months,     accrual_duration = oncology_scenario$Accrual_Duration_Months,     alpha = oncology_scenario$Alpha_Level, power = oncology_scenario$Target_Power,     allocation_ratio = oncology_scenario$Allocation_Ratio, dropout_rate = oncology_scenario$Dropout_Rate,     show_interpretation = TRUE): could not find function "survivalpower"
```
