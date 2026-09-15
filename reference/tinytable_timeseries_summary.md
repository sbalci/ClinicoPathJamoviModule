# Time Series Summary Test Dataset

Longitudinal dataset with repeated measures per subject across multiple
timepoints. Designed to test time-based table formatting, longitudinal
data presentation, and outcome tracking over time with realistic dropout
patterns.

## Usage

``` r
tinytable_timeseries_summary
```

## Format

A data frame with 150 observations and 10 variables:

- subject_id:

  Character. Subject identifier (TS_01 to TS_30)

- timepoint:

  Factor. Assessment timepoint ("T1" to "T5")

- months_from_baseline:

  Integer. Time since baseline (0, 3, 6, 12, 24 months)

- assessment_date:

  Date. Date of assessment

- primary_outcome:

  Numeric. Primary outcome score (0-100) with time-dependent missing

- secondary_outcome_1:

  Numeric. Secondary outcome correlated with primary

- secondary_outcome_2:

  Numeric. Independent secondary outcome

- response_status:

  Factor. Treatment response ("Responder", "Non-responder")

- compliance_pct:

  Numeric. Treatment compliance percentage (75-100%)

- dose_adjustment:

  Factor. Dose modification ("None", "Increase", "Decrease", "Hold")

## Source

Simulated data generated using create_tinytable_test_data.R

## Details

This dataset represents a longitudinal clinical study with 30 subjects
followed over 5 timepoints (24 months). It includes realistic patterns
of improvement, dropout, and missing data that increase over time.

**Key Features:**

- 30 subjects with 5 timepoints each (150 total observations)

- Time-dependent outcome improvement pattern

- Realistic dropout patterns (increasing missing data over time)

- Compliance and dose adjustment tracking

- Response status categorization

- Multiple correlated outcomes

**Recommended TinyTable Usage:**

- Table Type: "Grouped Summary" by timepoint

- Grouping Variable: timepoint or response_status

- Variables: primary_outcome, secondary_outcome_1, compliance_pct

- Themes: "Clinical" for longitudinal study reporting

## See also

[`tinytable`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable.md),
[`tinytable_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(tinytable_timeseries_summary)

# Outcomes by timepoint
result <- tinytable(
  data = tinytable_timeseries_summary,
  vars = c("primary_outcome", "secondary_outcome_1", "compliance_pct"),
  group_var = "timepoint",
  table_type = "grouped",
  show_missing = TRUE
)

# Response analysis
result_response <- tinytable(
  data = tinytable_timeseries_summary,
  vars = c("primary_outcome", "secondary_outcome_2"),
  group_var = "response_status",
  table_type = "descriptive"
)
} # }
```
