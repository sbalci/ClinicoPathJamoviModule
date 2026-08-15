# Test Datasets for Survival Continuous Variable Function

Test Datasets for Survival Continuous Variable Function

## Usage

``` r
survivalcont_test

survivalcont_ki67

survivalcont_psa

survivalcont_hemoglobin

survivalcont_tumorsize

survivalcont_age

survivalcont_compete

survivalcont_dates

survivalcont_landmark

survivalcont_multicut

survivalcont_expression

survivalcont_small

survivalcont_nocutoff

survivalcont_extreme

survivalcont_missing

survivalcont_constant

survivalcont_large
```

## Format

### `survivalcont_test`

Main test dataset (150 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time in months

- outcome:

  Outcome status (Alive/Dead)

- biomarker:

  Continuous biomarker (mean=100, SD=25)

- age:

  Patient age

- sex:

  Patient sex

### `survivalcont_ki67`

Ki67 proliferation index data (140 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- ki67_percent:

  Ki67 percentage (0-100)

- tumor_grade:

  Tumor grade

- stage:

  Disease stage

### `survivalcont_psa`

PSA level data (130 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- psa_level:

  PSA level (ng/mL)

- gleason_score:

  Gleason score (6-10)

- treatment:

  Treatment type

### `survivalcont_hemoglobin`

Hemoglobin level data (120 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- hemoglobin_gL:

  Hemoglobin level (g/L)

- performance_status:

  ECOG performance status

### `survivalcont_tumorsize`

Tumor size data (135 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- tumor_size_cm:

  Tumor size in cm

- lymph_nodes:

  Lymph node status

- histology:

  Histological type

### `survivalcont_age`

Age as continuous predictor (145 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- age_years:

  Patient age in years

- comorbidity_index:

  Comorbidity index

### `survivalcont_compete`

Competing risks data (110 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome (Alive_NED/Alive_Disease/Dead_Disease/Dead_Other)

- biomarker_score:

  Continuous biomarker score

- risk_category:

  Risk category

### `survivalcont_dates`

Date-based data (100 observations):

- patient_id:

  Patient identifier

- diagnosis_date:

  Diagnosis date (YYYY-MM-DD)

- followup_date:

  Follow-up date (YYYY-MM-DD)

- outcome:

  Outcome status

- continuous_marker:

  Continuous marker

### `survivalcont_landmark`

Landmark analysis data (125 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- early_response_score:

  Early response score

- baseline_score:

  Baseline score

### `survivalcont_multicut`

Multiple cutoff optimization data (150 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- risk_score:

  Risk score with clear stratification

### `survivalcont_expression`

Gene expression data (115 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- gene_expression:

  Gene expression level

- mutation_status:

  Mutation status

### `survivalcont_small`

Small dataset (20 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- continuous_var:

  Continuous variable

### `survivalcont_nocutoff`

No clear cutoff data (80 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- random_marker:

  Random marker (no survival correlation)

### `survivalcont_extreme`

Extreme values data (70 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- extreme_values:

  Values with outliers

### `survivalcont_missing`

Data with missing values (90 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time (with NAs)

- outcome:

  Outcome status (with NAs)

- biomarker:

  Biomarker (with NAs)

- covariate:

  Categorical covariate

### `survivalcont_constant`

Constant variable data (50 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- constant_marker:

  Constant marker (no variation)

### `survivalcont_large`

Large dataset for performance testing (500 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- biomarker:

  Continuous biomarker

- age:

  Patient age

- stage:

  Disease stage

- grade:

  Tumor grade

- sex:

  Patient sex

- site:

  Study site

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 140
rows and 6 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 130
rows and 6 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 120
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 135
rows and 6 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 145
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 110
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 100
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 125
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 150
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 115
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 20
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 80
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 70
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 90
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 50
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 500
rows and 9 columns.

## Source

Generated using data-raw/survivalcont_test_data.R (seed = 42)

## See also

[`survivalcont`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/survivalcont.md)

## Examples

``` r
data(survivalcont_test)
if (FALSE) { # \dontrun{
survivalcont(
  data = survivalcont_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  contexpl = "biomarker",
  findcut = TRUE,
  sc = TRUE
)
} # }
```
