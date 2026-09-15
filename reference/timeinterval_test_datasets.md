# Test Datasets for Time Interval Function

A collection of test datasets for the timeinterval function, which
calculates time intervals between dates for survival analysis and
person-time calculations.

## Usage

``` r
timeinterval_test

timeinterval_ymd

timeinterval_dmy

timeinterval_mdy

timeinterval_ymdhms

timeinterval_landmark

timeinterval_quality

timeinterval_extreme

timeinterval_small

timeinterval_large

timeinterval_trial

timeinterval_shortterm

timeinterval_longterm

timeinterval_sameday

timeinterval_negative

timeinterval_allmissing

timeinterval_mixedformat
```

## Format

### `timeinterval_test`

Main test dataset with diagnosis and follow-up dates (100 patients):

- patient_id:

  Character. Patient identifier

- diagnosis_date:

  Character. Start date (YYYY-MM-DD format)

- followup_date:

  Character. End date (YYYY-MM-DD format)

- age:

  Numeric. Patient age

- sex:

  Character. Male/Female

- treatment:

  Character. Treatment type

- stage:

  Character. Disease stage (I-IV)

- event_occurred:

  Integer. Event indicator (0/1)

### `timeinterval_ymd`

Standard YMD format (50 observations):

- id:

  Character. Identifier

- start_date:

  Character. Start date (YYYY-MM-DD)

- end_date:

  Character. End date (YYYY-MM-DD)

### `timeinterval_dmy`

European DMY format (50 observations):

- id:

  Character. Identifier

- start_date:

  Character. Start date (DD-MM-YYYY)

- end_date:

  Character. End date (DD-MM-YYYY)

### `timeinterval_mdy`

US MDY format (50 observations):

- id:

  Character. Identifier

- start_date:

  Character. Start date (MM-DD-YYYY)

- end_date:

  Character. End date (MM-DD-YYYY)

### `timeinterval_ymdhms`

DateTime format with hours/minutes/seconds (50 observations):

- id:

  Character. Identifier

- start_datetime:

  Character. Start datetime

- end_datetime:

  Character. End datetime

### `timeinterval_landmark`

Data for landmark analysis (80 patients):

- patient_id:

  Character. Patient identifier

- enrollment_date:

  Character. Study enrollment date

- last_contact:

  Character. Last follow-up contact

- treatment_arm:

  Character. Treatment group

- biomarker_positive:

  Character. Biomarker status

### `timeinterval_quality`

Dataset with quality issues (60 observations, ~10% missing, some
negative intervals)

### `timeinterval_extreme`

Dataset with extreme values (40 observations: very short, normal, very
long durations)

### `timeinterval_small`

Minimal dataset (5 observations)

### `timeinterval_large`

Large dataset for performance testing (500 observations)

### `timeinterval_trial`

Realistic clinical trial simulation (150 patients):

- patient_id:

  Character. Trial patient ID

- enrollment_date:

  Character. Enrollment date

- followup_date:

  Character. Last follow-up

- treatment_arm:

  Character. Trial arm

- site:

  Character. Study site

- age:

  Numeric. Patient age

- ecog_ps:

  Integer. ECOG performance status

- progression:

  Integer. Disease progression (0/1)

- death:

  Integer. Death event (0/1)

### Other Datasets

- `timeinterval_shortterm`: Hospital stays (1-14 days)

- `timeinterval_longterm`: Long-term study (5-15 years)

- `timeinterval_sameday`: Zero-interval (same start and end)

- `timeinterval_negative`: Negative intervals (end before start)

- `timeinterval_allmissing`: All missing dates

- `timeinterval_mixedformat`: Inconsistent date formats

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 50
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 50
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 50
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 50
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 80
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 60
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 40
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 5
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 500
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 150
rows and 9 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 30
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 40
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 10
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 15
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 10
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 20
rows and 3 columns.

## Source

Generated using data-raw/timeinterval_test_data.R (seed = 42)

## Supported Date Formats

- auto: Automatic detection

- ymd: YYYY-MM-DD (ISO 8601)

- dmy: DD-MM-YYYY (European)

- mdy: MM-DD-YYYY (US)

- ymdhms: YYYY-MM-DD HH:MM:SS

## Time Units

- days: Day-level precision

- weeks: 7-day periods

- months: 30.44-day months (standardized) or calendar months

- years: 365.25-day years (standardized) or calendar years

## See also

[`timeinterval`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeinterval.md)
for the time interval calculation function

## Examples

``` r
# Load main test dataset
data(timeinterval_test)
head(timeinterval_test)
#> # A tibble: 6 × 8
#>   patient_id diagnosis_date followup_date   age sex    treatment    stage
#>   <chr>      <chr>          <chr>         <dbl> <chr>  <chr>        <chr>
#> 1 PT001      2020-02-18     2022-11-12       59 Male   Radiation    IV   
#> 2 PT002      2020-11-16     2021-10-22       58 Male   Chemotherapy IV   
#> 3 PT003      2020-06-01     2022-12-29       49 Female Radiation    I    
#> 4 PT004      2020-03-14     2021-07-30       61 Male   Chemotherapy I    
#> 5 PT005      2020-08-15     2022-12-03       44 Male   Radiation    III  
#> 6 PT006      2020-05-25     2020-08-04       61 Female Combined     III  
#> # ℹ 1 more variable: event_occurred <int>

# Basic time interval calculation
if (FALSE) { # \dontrun{
timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  time_format = "ymd",
  output_unit = "months"
)
} # }

# Landmark analysis
if (FALSE) { # \dontrun{
data(timeinterval_landmark)
timeinterval(
  data = timeinterval_landmark,
  dx_date = "enrollment_date",
  fu_date = "last_contact",
  use_landmark = TRUE,
  landmark_time = 6,
  output_unit = "months"
)
} # }
```
