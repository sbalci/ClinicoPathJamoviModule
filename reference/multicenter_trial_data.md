# Multi-center Clinical Trial Data

Simulated dataset for a multi-center randomized controlled trial with
differential retention across sites. Designed for testing complex flow
scenarios with site-level variability in the `consortdiagram` function.

## Usage

``` r
multicenter_trial_data
```

## Format

A data frame with 600 rows and 10 columns:

- participant_id:

  Unique participant identifier (MC-00001 to MC-00600)

- site:

  Study site (Site A/B/C/D)

- age:

  Age in years (mean=65, sd=10)

- sex:

  Sex (Male/Female)

- screening_failure:

  Screening exclusion reasons (inclusion/exclusion criteria, lab values)

- enrollment_issue:

  Enrollment exclusion reasons (consent, travel distance)

- arm:

  Randomized treatment arm (Experimental/Control)

- not_received:

  Allocation exclusion reasons (intervention unavailable, deterioration)

- followup_loss_reason:

  Follow-up loss reasons (lost, withdrew, site closure)

- analysis_issue:

  Analysis exclusion reasons (missing endpoint)

## Source

Generated using `data-raw/create_clinical_trial_flow_data.R` (seed:
20251005)

## Details

This dataset simulates a realistic multi-center trial with:

- 600 participants assessed across 4 sites

- 25 percent screening failure rate (higher than single-center for
  realism)

- 5 percent enrollment exclusion rate

- 1:1 randomization to Experimental vs Control

- 3 percent allocation exclusion rate

- 15 percent follow-up loss rate (varies by site)

- 2 percent analysis exclusion rate

- Final retention: 57.7 percent (346 participants analyzed)

- Site-specific retention rates: 52.5 percent to 60.1 percent

The dataset demonstrates realistic site variability in retention rates,
which is common in multi-center trials due to differences in site
infrastructure, patient populations, and study management.

## Usage

This dataset demonstrates:

- Multi-site trial flow visualization

- Site-level retention variability

- Complex exclusion patterns

- Higher attrition rates typical of multi-center studies

## See also

[`clinical_trial_consort_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/clinical_trial_consort_data.md),
[`observational_study_flow_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/observational_study_flow_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load data
data(multicenter_trial_data)

# Site-specific retention
library(dplyr)
multicenter_trial_data |>
  group_by(site) |>
  summarise(
    total = n(),
    analyzed = sum(is.na(screening_failure) &
                   is.na(enrollment_issue) &
                   is.na(not_received) &
                   is.na(followup_loss_reason) &
                   is.na(analysis_issue)),
    retention = analyzed / total * 100
  )
} # }
```
