# Edge Cases and Quality Testing Dataset

Specialized dataset containing various edge cases, outliers, and quality
issues designed to test the robustness of time-dependent ROC analysis
implementations. Includes extreme values, missing data, and challenging
scenarios.

## Usage

``` r
timeroc_edge_cases
```

## Format

A data frame with 150 observations and 7 variables:

- id:

  Character. Unique identifier (EC_001 to EC_150)

- age:

  Integer. Patient age

- scenario:

  Integer. Test scenario type (1-5)

- biomarker:

  Numeric. Biomarker value with various data quality issues

- time_months:

  Numeric. Follow-up time in months

- event_status:

  Integer. Event indicator with missing values

- test_timepoints:

  Character. Suggested timepoints for testing

## Source

Simulated data generated using create_timeroc_test_data.R

## Details

This dataset is designed to stress-test time-dependent ROC
implementations with various edge cases and data quality issues:

**Scenario Types:**

- **Scenario 1**: Normal case (baseline comparison)

- **Scenario 2**: Very high biomarker values with short survival

- **Scenario 3**: Very low biomarker values with long survival

- **Scenario 4**: Extreme outliers (values up to 1000x normal)

- **Scenario 5**: Very long follow-up times (rare events)

**Quality Issues:**

- ~5% missing biomarker values

- Extreme outliers requiring robust handling

- Wide range of follow-up times

- 64/150 events (42.7% event rate)

- Various suggested timepoint specifications

**Recommended TimeROC Parameters:**

- Timepoints: 6, 12, 18 months (test robustness)

- Marker: biomarker (with missing values and outliers)

- Event: event_status

- Time: time_months

## See also

[`timeroc`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc.md),
[`timeroc_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(timeroc_edge_cases)

# Test robustness with edge cases
edge_roc <- timeroc(
  data = timeroc_edge_cases,
  elapsedtime = "time_months",
  outcome = "event_status",
  marker = "biomarker",
  timepoints = "6, 12, 18"
)

# Examine data quality
summary(timeroc_edge_cases$biomarker)  # Check for outliers
table(timeroc_edge_cases$scenario)     # Distribution of scenarios
sum(is.na(timeroc_edge_cases$biomarker))  # Missing values
} # }
```
