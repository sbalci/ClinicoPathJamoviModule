# Demonstration Scenarios for Sequential Testing Analysis

Parameter sets used to demonstrate the
[`sequentialtests()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/sequentialtests.md)
analysis. Each row is one worked scenario: the sensitivity, specificity
and (where present) unit cost of a screening test and a confirmatory
test, the disease prevalence to assume, and the testing strategy to
apply. They are inputs to the calculator, not patient-level data —
[`sequentialtests()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/sequentialtests.md)
takes no variables.

## Usage

``` r
sequentialtests_cancer

sequentialtests_cost_comparison

sequentialtests_covid

sequentialtests_emergency

sequentialtests_extreme

sequentialtests_infectious

sequentialtests_preset_examples

sequentialtests_prevalence_sensitivity

sequentialtests_reference

sequentialtests_strategy_comparison

sequentialtests_teaching
```

## Format

- sequentialtests_cancer:

  Cancer screening scenarios. 6 rows, 12 columns.

- sequentialtests_cost_comparison:

  Scenarios contrasting protocol cost. 9 rows, 12 columns.

- sequentialtests_covid:

  Respiratory-virus screening across community, hospital and outbreak
  prevalences. 12 rows, 12 columns.

- sequentialtests_emergency:

  Emergency-department rule-out scenarios. 6 rows, 12 columns.

- sequentialtests_extreme:

  Boundary cases (near-perfect and near-useless tests, very low and very
  high prevalence) for exercising edge behaviour. 8 rows, 8 columns.

- sequentialtests_infectious:

  Infectious-disease screening scenarios. 6 rows, 12 columns.

- sequentialtests_preset_examples:

  One row per Clinical Preset offered by the analysis. 7 rows, 12
  columns.

- sequentialtests_prevalence_sensitivity:

  One test pair held fixed while prevalence is varied, to show how
  predictive values move. 9 rows, 9 columns.

- sequentialtests_reference:

  Assorted reference test pairs. 10 rows, 12 columns.

- sequentialtests_strategy_comparison:

  The same test pairs under all three strategies, for side-by-side
  comparison. 15 rows, 11 columns.

- sequentialtests_teaching:

  Simple round-numbered scenarios for teaching. 8 rows, 7 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 6
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 9
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 12
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 6
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 8
rows and 8 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 6
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 7
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 9
rows and 9 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 10
rows and 12 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 15
rows and 11 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 8
rows and 7 columns.

## Details

Note that `"serial_negative"` and `"parallel"` are the same rule — a
subject is positive if either test is positive — and so give identical
sensitivity, specificity, PPV and NPV. They differ only in how many
second tests are performed, which is what the cost columns are for.

## These numbers are for demonstration only

**The figures in these datasets are illustrative. They are not
clinically accurate and must not be used to design a testing protocol or
to advise on a patient.**

They are rounded, approximate values chosen to make the behaviour of
each strategy easy to see — how confirming positives trades sensitivity
for specificity, how retesting negatives does the reverse, and how
prevalence drives predictive value. Specifically:

- They are **not taken from any particular published study**, and carry
  no citation, confidence interval, or population definition.

- Real test performance varies substantially with assay, manufacturer,
  specimen type, operator, disease stage, and time since exposure. A
  single sensitivity figure hides all of that.

- The prevalences are illustrative settings, not the prevalence in your
  population, which is the input that moves predictive values most.

- The costs are round numbers in unspecified units, not any real tariff
  or reimbursement rate.

- Test names such as `"RT-PCR"` or `"Mammography"` label the scenario.
  They do not assert that the accompanying numbers describe that test as
  actually performed anywhere.

Before drawing any clinical conclusion, replace every value with an
estimate from your own setting, or from a source you have read and
judged applicable to your population. The same warning applies to the
Clinical Preset control inside the analysis, which loads equivalent
demonstration values.

## Columns

- scenario:

  Label for the clinical setting being illustrated.

- strategy:

  Which strategy to apply: `"serial_positive"` (confirm the positives),
  `"serial_negative"` (retest the negatives), or `"parallel"` (test
  everyone with both).

- test1_name, test2_name:

  Labels for the screening and confirmatory tests.

- test1_sens, test1_spec, test2_sens, test2_spec:

  Assumed accuracy, as proportions.

- test1_cost, test2_cost:

  Assumed unit cost, in unspecified units. Present in most but not all
  of these datasets.

- prevalence:

  Assumed disease prevalence in the population tested, as a proportion.

## See also

[`sequentialtests()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/sequentialtests.md)

## Examples

``` r
# Run one scenario through the analysis
data(sequentialtests_covid)
row <- sequentialtests_covid[1, ]
sequentialtests(
    test1_name = row$test1_name, test1_sens = row$test1_sens, test1_spec = row$test1_spec,
    test2_name = row$test2_name, test2_sens = row$test2_sens, test2_spec = row$test2_spec,
    prevalence = row$prevalence, strategy = row$strategy
)
#> 
#>  SEQUENTIAL TESTING ANALYSIS
#> Analysis Complete
#> Sequential testing analysis completed: Serial Testing (Test positives) strategy with prevalence 2.0%, combined sensitivity 80.8%, combined specificity 100.0%.
#> 
#> Independence Assumption
#> Combined metrics assume conditional independence between tests. If tests are correlated (similar biology/technology), combined performance may be overestimated.
#>  Summary of Testing Strategy                                                                                                                                                                          
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Testing Strategy                   Disease Prevalence    First Test       Second Test    Combined Sensitivity    Combined Specificity    Combined PPV    Combined NPV    Number Needed to Screen   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Serial Testing (Test positives)               2.00000    Rapid Antigen    RT-PCR                     80.75000                99.95000        97.05529        99.60849                         62   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. Sensitivity, specificity and prevalence are treated as exact. These combined figures therefore carry <i>no</i> confidence interval and do not reflect sampling uncertainty in the values
#>    entered — published test performance and local prevalence both vary.
#>    Note. Combined figures assume the two tests are <i>conditionally independent</i> — that, among people with the same disease status, one test's result says nothing about the other's. Tests
#>    measuring related biology usually violate this, and the combined sensitivity and specificity above are then too optimistic.
#> 
#> 
#>  Individual Test Performance                                                                                 
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                 Sensitivity    Specificity    PPV          NPV          Positive LR    Negative LR   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Rapid Antigen           85.00000       95.00000     25.75758     99.67880       17.00000     0.15789474   
#>    RT-PCR                  95.00000       99.00000     65.97222     99.89703       95.00000     0.05050505   
#>    Combined Strategy       80.75000       99.95000     97.05529     99.60849     1615.00000     0.19259630   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Population Flow Analysis                                                                                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Testing Stage              Total Subjects    Disease Positive    Disease Negative    Test Positive    Test Negative    True Positives    False Positives    False Negatives    True Negatives   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Initial Population               1000.000            20.00000            980.0000                                                                                                               
#>    After First Test                 1000.000            20.00000            980.0000         66.00000         934.0000          17.00000         49.0000000           3.000000          931.0000   
#>    After Combined Strategy          1000.000            20.00000            980.0000         16.64000         983.3600          16.15000          0.4900000           3.850000          979.5100   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div class='jmv-guidance'
#>  style='background-color:#f8f9fa;padding:15px;border-radius:6px;margin-top:10px;'>
#> 
#>  Clinical Decision Making Guide
#> 
#>  When to Use Each Strategy:
#> 
#>  Serial Positive (Confirmation): Use when false positives are costly or
#>  harmful. First test should be sensitive, second test should be
#>  specific.Serial Negative (Exclusion): Use when false negatives are
#>  dangerous. First test should be specific, second test should be
#>  sensitive.Parallel Testing: Use when rapid diagnosis is critical and
#>  both tests can be performed simultaneously.
#> 
#>  Clinical Examples:
#> 
#>  HIV screening (ELISA → Western Blot)Cancer screening (Imaging →
#>  Biopsy)COVID-19 (Rapid Antigen → PCR)

# Serial-negative and parallel testing are the same rule, so they agree exactly
data(sequentialtests_strategy_comparison)
head(sequentialtests_strategy_comparison)
#> # A tibble: 6 × 11
#>   scenario_id scenario_name strategy test1_sens test1_spec test1_cost test2_sens
#>         <int> <chr>         <chr>         <dbl>      <dbl>      <dbl>      <dbl>
#> 1           1 Screening     serial_…       0.9        0.85         50       0.85
#> 2           1 Screening     serial_…       0.9        0.85         50       0.85
#> 3           1 Screening     parallel       0.9        0.85         50       0.85
#> 4           2 Diagnosis     serial_…       0.85       0.88        100       0.9 
#> 5           2 Diagnosis     serial_…       0.85       0.88        100       0.9 
#> 6           2 Diagnosis     parallel       0.85       0.88        100       0.9 
#> # ℹ 4 more variables: test2_spec <dbl>, test2_cost <dbl>, prevalence <dbl>,
#> #   population_size <dbl>
```
