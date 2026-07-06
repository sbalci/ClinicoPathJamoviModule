# Test Datasets for Outcome Organizer Function

Test Datasets for Outcome Organizer Function

## Usage

``` r
outcomeorganizer_os

outcomeorganizer_compete

outcomeorganizer_pfs

outcomeorganizer_rfs

outcomeorganizer_causespecific

outcomeorganizer_multistate

outcomeorganizer_dfs

outcomeorganizer_small

outcomeorganizer_censored

outcomeorganizer_allevents
```

## Format

### `outcomeorganizer_os`

Overall Survival dataset (150 observations):

- patient_id:

  Patient identifier

- vital_status:

  Vital status (Alive/Dead)

- time_months:

  Follow-up time in months

- age:

  Patient age

- stage:

  Disease stage (I/II/III/IV)

### `outcomeorganizer_compete`

Competing Risks dataset (120 observations):

- patient_id:

  Patient identifier

- outcome_status:

  Outcome (Alive_NED/Alive_Disease/Dead_Disease/Dead_Other)

- time:

  Follow-up time

- treatment:

  Treatment type

### `outcomeorganizer_pfs`

Progression-Free Survival dataset (100 observations):

- patient_id:

  Patient identifier

- vital_status:

  Vital status (Alive/Dead)

- progression:

  Progression status (No/Yes)

- time_months:

  Follow-up time in months

- biomarker:

  Biomarker status

### `outcomeorganizer_rfs`

Recurrence-Free Survival dataset (110 observations):

- patient_id:

  Patient identifier

- vital:

  Vital status (Alive/Dead)

- recurrence:

  Recurrence status (No/Yes)

- fu_time:

  Follow-up time

- grade:

  Tumor grade

### `outcomeorganizer_causespecific`

Cause-Specific Survival dataset (90 observations):

- patient_id:

  Patient identifier

- death_status:

  Death status (Alive/Dead_Cancer/Dead_Cardiac/Dead_Other)

- time:

  Follow-up time

- risk_score:

  Risk score

### `outcomeorganizer_multistate`

Multistate Model dataset (80 observations):

- patient_id:

  Patient identifier

- current_state:

  Current state (Disease_Free/Local_Recurrence/Metastatic/Dead)

- time:

  Follow-up time

### `outcomeorganizer_dfs`

Disease-Free Survival dataset (95 observations):

- patient_id:

  Patient identifier

- status:

  Status (NED/Disease/Dead)

- time_years:

  Follow-up time in years

- surgery_type:

  Surgery type

### `outcomeorganizer_small`

Small dataset (15 observations) for edge case testing:

- id:

  Patient identifier

- outcome:

  Outcome (Event/NoEvent)

- time:

  Follow-up time

### `outcomeorganizer_censored`

All censored dataset (30 observations):

- id:

  Patient identifier

- status:

  Status (all Alive)

- time:

  Follow-up time

### `outcomeorganizer_allevents`

All events dataset (25 observations):

- id:

  Patient identifier

- status:

  Status (all Dead)

- time:

  Follow-up time

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 120
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 100
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 110
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 90
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 80
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 95
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 15
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 30
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 25
rows and 3 columns.

## Source

Generated using data-raw/outcomeorganizer_test_data.R (seed = 42)

## See also

[`outcomeorganizer`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/outcomeorganizer.md)

## Examples

``` r
data(outcomeorganizer_os)
if (FALSE) { # \dontrun{
outcomeorganizer(
  data = outcomeorganizer_os,
  outcome = "vital_status",
  time = "time_months",
  dead = "Dead",
  alive = "Alive"
)
} # }
```
