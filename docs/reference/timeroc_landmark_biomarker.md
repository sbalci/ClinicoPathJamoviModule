# Landmark Biomarker Analysis Test Dataset

Specialized dataset for landmark time-dependent ROC analysis where
biomarker values change over time. All patients survive to a landmark
time (6 months) and subsequent analysis is conditional on landmark
survival.

## Usage

``` r
timeroc_landmark_biomarker
```

## Format

A data frame with 200 observations and 10 variables:

- patient_id:

  Character. Unique patient identifier (LM_001 to LM_200)

- age:

  Integer. Patient age

- baseline_biomarker:

  Numeric. Biomarker level at study entry

- month6_biomarker:

  Numeric. Biomarker level at 6-month landmark

- biomarker_change:

  Numeric. Log-ratio change from baseline to 6 months

- total_follow_up_months:

  Numeric. Total follow-up time from baseline (6-60 months)

- landmark_eligible:

  Logical. All TRUE (all patients survived to landmark)

- post_landmark_event:

  Integer. Event after landmark time (1 = event, 0 = censored)

- response_status:

  Character. Treatment response ("Complete", "Partial", "Stable",
  "Progressive")

- treatment_arm:

  Character. Treatment assignment ("Experimental", "Standard")

## Source

Simulated data generated using create_timeroc_test_data.R

## Details

This dataset supports landmark analysis methodology where time-dependent
biomarker values are assessed at a fixed landmark time (6 months) and
used to predict subsequent events. This approach is common in oncology
and transplantation studies.

**Key Features:**

- All patients survive to landmark time (6 months)

- Time-varying biomarker measurements

- Biomarker change calculations (log-ratio)

- Treatment response classifications

- 48/200 post-landmark events (24% event rate)

- Randomized treatment arms

**Recommended TimeROC Parameters:**

- Timepoints: 12, 24, 36 months (from baseline)

- Markers: month6_biomarker, biomarker_change

- Event: post_landmark_event

- Time: total_follow_up_months

## See also

[`timeroc`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc.md),
[`timeroc_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(timeroc_landmark_biomarker)

# Landmark analysis using 6-month biomarker value
landmark_roc <- timeroc(
  data = timeroc_landmark_biomarker,
  elapsedtime = "total_follow_up_months",
  outcome = "post_landmark_event", 
  marker = "month6_biomarker",
  timepoints = "12, 24, 36"
)

# Analysis of biomarker change
change_roc <- timeroc(
  data = timeroc_landmark_biomarker,
  elapsedtime = "total_follow_up_months",
  outcome = "post_landmark_event",
  marker = "biomarker_change", 
  timepoints = "12, 24, 36"
)
} # }
```
