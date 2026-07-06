# Time Series Data for Longitudinal Analysis

Longitudinal dataset with repeated measures per subject across multiple
timepoints, realistic dropout patterns, and outcome tracking. Designed
to test time-based summaries, longitudinal data presentation, missing
data patterns over time, and grouped analysis capabilities using
summarytools enhanced features.

## Usage

``` r
toolssummary_timeseries_data
```

## Format

A data frame with 200 observations and 12 variables:

- subject_id:

  Character. Subject identifier (TS_001 to TS_040)

- timepoint:

  Factor. Assessment timepoint ("T1", "T2", "T3", "T4", "T5")

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

- compliance_percent:

  Numeric. Treatment compliance percentage (75-100%)

- dose_level:

  Ordered Factor. Dose level ("Low" \< "Medium" \< "High")

- adverse_events:

  Integer. Count of adverse events (0-5)

- biomarker_level:

  Numeric. Biomarker measurement (log-normal distribution)

## Source

Simulated data generated using create_toolssummary_test_data.R

## Details

This dataset represents a longitudinal clinical study with 40 subjects
followed over 5 timepoints (24 months total). It includes realistic
patterns of outcome changes, dropout over time, and missing data that
increase with follow-up duration, making it ideal for testing
summarytools longitudinal capabilities.

**Key Features:**

- 40 subjects with up to 5 timepoints each (200 total observations)

- Time-dependent outcome patterns and trends

- Realistic dropout patterns (increasing missing data over time)

- Multiple correlated and independent outcomes

- Compliance and adverse event tracking

- Response status categorization

**summarytools Integration Testing:**

- **dfSummary**: Longitudinal data overview with temporal patterns

- **freq**: Time-based frequency analysis and response categorization

- **descr**: Outcome statistics across timepoints with trend assessment

- **ctable**: Cross-tabulations by timepoint and response status

**Recommended Usage Scenarios:**

- Longitudinal outcome analysis by timepoint

- Dropout pattern assessment over time

- Treatment response analysis

- Compliance and safety monitoring

## See also

[`toolssummary`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary.md),
[`toolssummary_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toolssummary_timeseries_data)

# Longitudinal outcomes analysis
result <- toolssummary(
  data = toolssummary_timeseries_data,
  vars = c("primary_outcome", "secondary_outcome_1", "compliance_percent"),
  useSummarytools = TRUE,
  showDescr = TRUE,
  showDfSummary = TRUE
)

# Analysis by timepoint
result_time <- toolssummary(
  data = toolssummary_timeseries_data,
  vars = c("primary_outcome", "response_status", "adverse_events"),
  groupVar = "timepoint",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
} # }
```
