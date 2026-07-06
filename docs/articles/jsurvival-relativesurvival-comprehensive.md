# Relative Survival Analysis - Comprehensive Guide

> **Note:** The
> [`relativesurvival()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/relativesurvival.md)
> function is designed for use within **jamovi’s GUI**. The code
> examples below show the R syntax for reference. To run interactively,
> use
> [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
> and call the wrapper function directly.

## Relative Survival Analysis

### Overview

Relative survival analysis compares observed survival in a patient
cohort to the survival expected in a matched general population. Rather
than requiring (often inaccurate) cause-of-death data, it estimates
disease-specific mortality indirectly through the ratio of observed to
expected survival. This approach is the standard in population-based
cancer epidemiology and is used by EUROCARE, CONCORD, and national
cancer registries worldwide.

The `relativesurvival` module provides:

- **Four estimation methods**: Pohar-Perme (recommended), Ederer I,
  Ederer II, Hakulinen
- **15 population rate tables**: 4 from the `relsurv` package (US,
  Minnesota, France, Slovenia) and 10 WHO-based tables (Turkey, Germany,
  UK, Italy, Japan, Spain, Brazil, South Korea, China, India), plus a
  custom option
- **Net survival** and **excess mortality** estimation
- **Crude probability of death** decomposition (disease vs. other
  causes)
- **ICSS age standardization** for international comparisons
- **Period analysis** to track survival trends over diagnosis years
- **Regression models**: additive excess hazard, multiplicative, and
  flexible parametric (via `rstpm2`)
- **Four publication-ready plots**: observed, expected, relative
  survival, and excess mortality

------------------------------------------------------------------------

### Datasets

| Dataset | N | Events | Key Features |
|----|----|----|----|
| `relativesurvival_test` | 200 | ~116 deaths | 4 cancer sites (Colon, Breast, Lung, Prostate), stages I-IV, ages 30-90, diagnosis years 2000-2015, covariates (comorbidity, tumor_size) |

``` r

data(relativesurvival_test)
str(relativesurvival_test)
#> 'data.frame':    200 obs. of  11 variables:
#>  $ patient_id      : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ followup_years  : num  3.59 10 3.39 1.94 3.56 10 2.39 3.37 3.02 7.03 ...
#>  $ vital_status    : int  1 0 1 1 1 0 1 1 1 1 ...
#>  $ age_at_diagnosis: num  81 58 69 73 70 64 83 64 89 64 ...
#>  $ sex             : Factor w/ 2 levels "female","male": 2 2 1 2 1 2 1 2 2 2 ...
#>  $ diagnosis_year  : int  2007 2003 2005 2009 2011 2010 2003 2007 2008 2014 ...
#>  $ cancer_site     : Factor w/ 4 levels "Breast","Colon",..: 4 4 2 3 3 2 3 2 2 3 ...
#>  $ stage           : Ord.factor w/ 4 levels "I"<"II"<"III"<..: 1 2 1 3 3 2 3 4 2 1 ...
#>  $ grade           : Factor w/ 3 levels "Moderately differentiated",..: 1 1 2 1 3 3 3 2 1 2 ...
#>  $ comorbidity     : int  0 0 0 0 2 2 2 2 2 3 ...
#>  $ tumor_size      : num  3.6 2.5 1.6 4 4.2 5.8 4.1 3.3 8.2 2.3 ...
```

``` r

summary(relativesurvival_test[, c("followup_years", "vital_status",
                                   "age_at_diagnosis", "sex", "diagnosis_year")])
#>  followup_years    vital_status  age_at_diagnosis     sex      diagnosis_year
#>  Min.   : 0.080   Min.   :0.00   Min.   :30.0     female: 82   Min.   :2000  
#>  1st Qu.: 1.518   1st Qu.:0.00   1st Qu.:58.0     male  :118   1st Qu.:2004  
#>  Median : 3.460   Median :1.00   Median :65.0                  Median :2008  
#>  Mean   : 4.754   Mean   :0.58   Mean   :64.6                  Mean   :2008  
#>  3rd Qu.: 8.590   3rd Qu.:1.00   3rd Qu.:73.0                  3rd Qu.:2012  
#>  Max.   :10.000   Max.   :1.00   Max.   :90.0                  Max.   :2015
```

------------------------------------------------------------------------

### 1. Basic Analysis

The minimum call requires five variables: follow-up time, vital status,
age at diagnosis, sex, and calendar year of diagnosis. By default, the
module uses the **Pohar-Perme** estimator and the **US population** rate
table.

This produces the main survival table (observed, expected, relative
survival at 1, 3, 5, and 10 years), net survival table, excess mortality
rates, crude probability of death, and all four plots.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### What the outputs mean

- **Survival Estimates by Time**: Observed (Kaplan-Meier), expected
  (population), and relative (net) survival at each requested timepoint.
- **Net Survival Estimates**: The probability of surviving the disease
  if other causes of death were removed. Values near 1.0 mean the
  disease contributes little to mortality.
- **Excess Mortality Rates**: The additional hazard attributable to the
  disease in each year interval. Higher values indicate more
  disease-specific mortality.
- **Crude Probability of Death**: Decomposes total mortality into
  disease-related and other-cause components, accounting for competing
  risks.
- **Clinical Interpretation**: Automated 5-year prognosis grading
  (Excellent \>90%, Good \>70%, Fair \>50%, Poor \<50%).

------------------------------------------------------------------------

### 2. Estimation Methods

The module supports four estimation methods, each differing in how
expected survival is calculated. The **Pohar-Perme** method (default) is
the only unbiased estimator of net survival and is recommended by
international guidelines.

#### 2a. Pohar-Perme (default, recommended)

Pohar-Perme uses inverse-probability-of-censoring weighting (IPCW) to
produce unbiased net survival estimates. This is the EUROCARE/CONCORD
standard.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  method = "poharperme"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 2b. Ederer II

Ederer II updates the expected survival at each event time. It was the
traditional standard but is now known to be biased for net survival.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  method = "ederer2"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 2c. Ederer I

Ederer I calculates expected survival based on the full cohort
demographics at baseline, without updating. It tends to overestimate
expected survival at later times.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  method = "ederer1"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 2d. Hakulinen

Hakulinen weights expected survival by the censoring distribution,
making it a compromise between Ederer I and Ederer II.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  method = "hakulinen"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 3. Rate Tables

Population rate tables are the source of expected survival. The choice
of rate table should match the country and calendar period of the
patient cohort.

#### 3a. US Population (default)

The `survexp.us` table from the `survival` package, repackaged by
`relsurv`.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  ratetable = "us"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 3b. Minnesota Population

A state-level rate table useful for regional cancer registries.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  ratetable = "mn"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 3c. French Population

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  ratetable = "fr"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 3d. Slovenian Population

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  ratetable = "slovenia"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 3e. WHO-Based Tables (Turkey example)

WHO-based tables use Global Health Observatory life table data bundled
with this module. They cover Turkey, Germany, UK, Italy, Japan, Spain,
Brazil, South Korea, China, and India. If the table is not available,
the module falls back to the US table with a notice.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  ratetable = "turkey"
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 4. Net Survival & Excess Mortality

By default, both **net survival** (`net_survival = TRUE`) and **excess
mortality** (`excess_mortality = TRUE`) are calculated. You can toggle
each independently.

#### 4a. Net Survival Only

Net survival is the probability of surviving the disease in a
hypothetical world where the disease is the only possible cause of
death.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  net_survival = TRUE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 4b. Excess Mortality Only

Excess mortality is the additional hazard due to the disease, computed
as `-log(S_net(t) / S_net(t-1))` for each yearly interval.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  net_survival = FALSE,
  excess_mortality = TRUE,
  crude_probability = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 5. Crude Probabilities

Crude probabilities decompose total mortality into disease-specific and
other-cause components, using the method of Cronin and Feuer
(implemented in
[`relsurv::cmp.rel`](https://rdrr.io/pkg/relsurv/man/cmp.rel.html)).
This accounts for competing risks.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = TRUE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 6. Age Standardization

Age-standardized relative survival uses **ICSS (International Cancer
Survival Standard)** weights to make survival estimates comparable
across populations with different age structures.

ICSS weight groups: - 0-44 years: 0.07 - 45-54 years: 0.12 - 55-64
years: 0.23 - 65-74 years: 0.29 - 75+ years: 0.29

The module calculates Pohar-Perme net survival within each age group,
then produces a weighted average.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  age_standardized = TRUE,
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 7. Period Analysis

Period analysis estimates the most recent survival experience by
restricting follow-up to a specific calendar window. Unlike cohort
analysis (which follows patients from diagnosis), period analysis
captures the latest treatment effects.

#### 7a. Default Period Analysis

Without a cohort definition, the module creates 5-year diagnosis periods
automatically from the data range and computes 5-year relative survival
for each period.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  period_analysis = TRUE,
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 7b. With Cohort Definition

The `cohort_year` option restricts the analysis to patients diagnosed
within a specific year range. The format is “start-end” (e.g.,
“2005-2015”).

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  period_analysis = TRUE,
  cohort_year = "2005-2015",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 8. Regression Models

When additional covariates are provided, the module can fit three types
of regression models to assess the effect of prognostic factors on
excess mortality.

#### 8a. Additive Excess Hazard Model

The additive model
([`relsurv::rsadd`](https://rdrr.io/pkg/relsurv/man/rsadd.html)) assumes
that excess hazard is additive to the expected hazard. This is the
classic Esteve model.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  covariates = "cancer_site",
  regression_model = "additive",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error:
#> ! 'survexp.us' is not an exported object from 'namespace:relsurv'
```

#### 8b. Multiplicative Model

The multiplicative model
([`relsurv::rsmul`](https://rdrr.io/pkg/relsurv/man/rsmul.html)) assumes
that excess hazard is proportional (multiplicative) to baseline excess
hazard.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  covariates = "cancer_site",
  regression_model = "multiplicative",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error:
#> ! 'survexp.us' is not an exported object from 'namespace:relsurv'
```

#### 8c. Flexible Parametric Model

The flexible parametric model
([`rstpm2::stpm2`](https://rdrr.io/pkg/rstpm2/man/gsm.html)) uses
restricted cubic splines to model the baseline excess hazard. The
`spline_df` option controls the degrees of freedom (complexity) of the
spline.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  covariates = "cancer_site",
  regression_model = "flexible",
  spline_df = 3,
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error:
#> ! 'survexp.us' is not an exported object from 'namespace:relsurv'
```

#### 8d. Multiple Covariates

You can include multiple covariates (continuous and/or categorical).

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  covariates = c("cancer_site", "stage"),
  regression_model = "additive",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error:
#> ! 'survexp.us' is not an exported object from 'namespace:relsurv'
```

------------------------------------------------------------------------

### 9. Time Scales

The module accepts follow-up time in years (default), months, or days.
The `time_scale` option tells the module how to interpret the time
variable; it is internally converted to years (and then to days for the
`relsurv` rate table).

#### 9a. Time in Months

``` r

# Create a copy with follow-up in months
test_months <- relativesurvival_test
test_months$followup_months <- test_months$followup_years * 12

relativesurvival(
  data = test_months,
  time = "followup_months",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  time_scale = "months",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 9b. Time in Days

``` r

# Create a copy with follow-up in days
test_days <- relativesurvival_test
test_days$followup_days <- test_days$followup_years * 365.25

relativesurvival(
  data = test_days,
  time = "followup_days",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  time_scale = "days",
  net_survival = FALSE,
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 10. Custom Timepoints & Confidence Levels

#### 10a. Custom Timepoints

The `timepoints` option accepts a comma-separated string of time values
(in years, regardless of the `time_scale` setting). These determine at
which follow-up times the survival estimates are reported.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  timepoints = "0.5, 1, 2, 3, 5, 7",
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 10b. Custom Confidence Level

The `confidence_level` option controls the width of all confidence
intervals in the analysis (default 0.95). Here we use 90%.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  confidence_level = 0.90,
  timepoints = "1, 3, 5",
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

------------------------------------------------------------------------

### 11. Edge Cases

#### 11a. Small Sample Warning

The module requires at least 30 complete cases and at least 10 events.
With 10-19 events, a strong warning is displayed; with 20-49, a moderate
warning.

``` r

# Subset to a small sample with few events
small_data <- relativesurvival_test[1:40, ]
relativesurvival(
  data = small_data,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  excess_mortality = FALSE,
  crude_probability = FALSE,
  plot_observed = FALSE,
  plot_expected = FALSE,
  plot_relative = FALSE,
  plot_excess = FALSE
)
#> Error in `relativesurvival()`:
#> ! argument "covariates" is missing, with no default
```

#### 11b. Full Analysis with All Options Enabled

This demonstrates the maximum output: all analysis options, all plots,
period analysis with cohort definition, age standardization, and an
additive regression model.

``` r

relativesurvival(
  data = relativesurvival_test,
  time = "followup_years",
  status = "vital_status",
  age = "age_at_diagnosis",
  sex = "sex",
  year = "diagnosis_year",
  covariates = c("cancer_site", "stage"),
  ratetable = "us",
  method = "poharperme",
  time_scale = "years",
  net_survival = TRUE,
  excess_mortality = TRUE,
  crude_probability = TRUE,
  age_standardized = TRUE,
  period_analysis = TRUE,
  cohort_year = "2000-2015",
  regression_model = "additive",
  plot_observed = TRUE,
  plot_expected = TRUE,
  plot_relative = TRUE,
  plot_excess = TRUE,
  confidence_level = 0.95,
  timepoints = "1, 3, 5, 10"
)
#> Error:
#> ! 'survexp.us' is not an exported object from 'namespace:relsurv'
```

------------------------------------------------------------------------

### Complete Option Reference

| \# | Option | Type | Default | Range/Choices | Section Demonstrated |
|----|----|----|----|----|----|
| 1 | `time` | Variable (numeric) | \- | continuous | 1\. Basic Analysis |
| 2 | `status` | Variable (factor/numeric) | \- | binary 0/1 | 1\. Basic Analysis |
| 3 | `age` | Variable (numeric) | \- | 0-120 | 1\. Basic Analysis |
| 4 | `sex` | Variable (factor) | \- | male/female mappings | 1\. Basic Analysis |
| 5 | `year` | Variable (numeric) | \- | 1900-2100 | 1\. Basic Analysis |
| 6 | `covariates` | Variables (numeric/factor) | \- | any | 8\. Regression Models |
| 7 | `ratetable` | List | `us` | us/mn/fr/slovenia/turkey/germany/uk/italy/japan/spain/brazil/south_korea/china/india/custom | 3\. Rate Tables |
| 8 | `method` | List | `poharperme` | poharperme/ederer1/ederer2/hakulinen | 2\. Estimation Methods |
| 9 | `time_scale` | List | `years` | years/months/days | 9\. Time Scales |
| 10 | `net_survival` | Bool | `true` | \- | 4\. Net Survival |
| 11 | `excess_mortality` | Bool | `true` | \- | 4\. Excess Mortality |
| 12 | `crude_probability` | Bool | `true` | \- | 5\. Crude Probabilities |
| 13 | `age_standardized` | Bool | `false` | \- | 6\. Age Standardization |
| 14 | `period_analysis` | Bool | `false` | \- | 7\. Period Analysis |
| 15 | `cohort_year` | String | `""` | e.g., “2010-2015” | 7b. Cohort Definition |
| 16 | `regression_model` | List | `none` | none/additive/multiplicative/flexible | 8\. Regression Models |
| 17 | `spline_df` | Integer | `4` | 1-10 | 8c. Flexible Parametric |
| 18 | `plot_observed` | Bool | `true` | \- | 1\. Basic Analysis |
| 19 | `plot_expected` | Bool | `true` | \- | 1\. Basic Analysis |
| 20 | `plot_relative` | Bool | `true` | \- | 1\. Basic Analysis |
| 21 | `plot_excess` | Bool | `true` | \- | 4b. Excess Mortality |
| 22 | `confidence_level` | Number | `0.95` | 0.50-0.99 | 10b. Custom Confidence |

All 22 `.a.yaml` options are covered in the examples above.

------------------------------------------------------------------------

### References

1.  Pohar Perme M, Stare J, Esteve J. On Estimation in Relative
    Survival. *Biometrics*, 2012;68:113-120.
2.  Ederer F, Axtell LM, Cutler SJ. The Relative Survival Rate: A
    Statistical Methodology. *NCI Monograph*, 1961;6:101-121.
3.  Hakulinen T. Cancer Survival Corrected for Heterogeneity in Patient
    Withdrawal. *Biometrics*, 1982;38:933-942.
4.  Corazziari I, Quinn M, Capocaccia R. Standard cancer patient
    population for age standardising survival ratios. *Eur J Cancer*,
    2004;40:2307-2316.
5.  Dickman PW, Coviello E. Estimating and Modeling Relative Survival.
    *The Stata Journal*, 2015;15(1):186-215.
6.  Lambert PC, Royston P. Further Development of Flexible Parametric
    Models for Survival Analysis. *The Stata Journal*,
    2009;9(2):265-290.
7.  Cronin KA, Feuer EJ. Cumulative Cause-Specific Mortality for Cancer
    Patients in the Presence of Other Causes: A Crude Analogue of
    Relative Survival. *Statistics in Medicine*, 2000;19:1729-1740.

------------------------------------------------------------------------

*This vignette is part of the
[jsurvival](https://www.serdarbalci.com/jsurvival/) module of the
ClinicoPath jamovi package.*
