# Test Datasets for Single Arm Survival Function

Test Datasets for Single Arm Survival Function

## Usage

``` r
singlearm_test

singlearm_dates

singlearm_compete

singlearm_causespecific

singlearm_landmark

singlearm_dmy

singlearm_mdy

singlearm_datetime

singlearm_longfu

singlearm_shortfu

singlearm_persontime

singlearm_small

singlearm_censored

singlearm_allevents

singlearm_early

singlearm_missing

singlearm_zerotime

singlearm_large
```

## Format

### `singlearm_test`

Main test dataset for overall survival (150 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time in months

- outcome:

  Outcome status (Alive/Dead)

- age:

  Patient age

- stage:

  Disease stage (I/II/III/IV)

- sex:

  Patient sex

### `singlearm_dates`

Date-based survival data in YMD format (120 observations):

- patient_id:

  Patient identifier

- diagnosis_date:

  Diagnosis date (YYYY-MM-DD)

- followup_date:

  Follow-up date (YYYY-MM-DD)

- outcome:

  Outcome status (Alive/Dead)

- treatment:

  Treatment type

### `singlearm_compete`

Competing risks data (100 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time in months

- outcome:

  Outcome (Alive_NED/Alive_Disease/Dead_Disease/Dead_Other)

- biomarker:

  Biomarker status

- grade:

  Tumor grade

### `singlearm_causespecific`

Cause-specific survival data (110 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time in months

- outcome:

  Outcome (Alive/Dead_Cancer/Dead_Cardiac/Dead_Other)

- risk_score:

  Risk score

- comorbidity:

  Comorbidity status

### `singlearm_landmark`

Landmark analysis data (130 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time in months

- outcome:

  Outcome status (Alive/Dead)

- treatment_response:

  Response (CR/PR/SD/PD)

- early_toxicity:

  Early toxicity status

### `singlearm_dmy`

Date-based data in DMY format (90 observations):

- patient_id:

  Patient identifier

- diagnosis_date:

  Diagnosis date (DD-MM-YYYY)

- followup_date:

  Follow-up date (DD-MM-YYYY)

- outcome:

  Outcome status

### `singlearm_mdy`

Date-based data in MDY format (85 observations):

- patient_id:

  Patient identifier

- diagnosis_date:

  Diagnosis date (MM/DD/YYYY)

- followup_date:

  Follow-up date (MM/DD/YYYY)

- outcome:

  Outcome status

### `singlearm_datetime`

Date-time data with HMS (75 observations):

- patient_id:

  Patient identifier

- diagnosis_datetime:

  Diagnosis datetime

- followup_datetime:

  Follow-up datetime

- outcome:

  Outcome status

- emergency_admission:

  Emergency admission status

### `singlearm_longfu`

Long follow-up data (95 observations):

- patient_id:

  Patient identifier

- time_months:

  Long-term survival time

- outcome:

  Outcome status

- enrollment_year:

  Year of enrollment

- clinical_trial:

  Trial or registry

### `singlearm_shortfu`

Short follow-up data (80 observations):

- patient_id:

  Patient identifier

- time_months:

  Short-term survival time

- outcome:

  Outcome status

- disease:

  Disease type

### `singlearm_persontime`

Person-time analysis data (140 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- age_group:

  Age group

- exposure:

  Exposure level

### `singlearm_small`

Small dataset (15 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

### `singlearm_censored`

All censored data (30 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome (all Alive)

### `singlearm_allevents`

All events data (25 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome (all Dead)

### `singlearm_early`

Very early events data (40 observations):

- patient_id:

  Patient identifier

- time_months:

  Short survival time

- outcome:

  Outcome status

- rapid_progression:

  Rapid progression status

### `singlearm_missing`

Data with missing values (60 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time (with NAs)

- outcome:

  Outcome status (with NAs)

- covariate1:

  Continuous covariate

- covariate2:

  Categorical covariate

### `singlearm_zerotime`

Data with zero time values (35 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time (including zeros)

- outcome:

  Outcome status

### `singlearm_large`

Large dataset for performance testing (500 observations):

- patient_id:

  Patient identifier

- time_months:

  Survival time

- outcome:

  Outcome status

- age:

  Patient age

- stage:

  Disease stage

- grade:

  Tumor grade

- sex:

  Patient sex

- biomarker:

  Biomarker value

- treatment:

  Treatment type

- site:

  Study site

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 120
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 100
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 110
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 130
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 90
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 85
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 75
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 95
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 80
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 140
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 15
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 30
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 25
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 40
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 60
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 35
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 500
rows and 10 columns.

## Source

Generated using data-raw/singlearm_test_data.R (seed = 42)

## See also

[`singlearm`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/singlearm.md)

## Examples

``` r
data(singlearm_test)
if (FALSE) { # \dontrun{
singlearm(
  data = singlearm_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  sc = TRUE
)
} # }
```
