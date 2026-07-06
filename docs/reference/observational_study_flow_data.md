# Observational Study Flow Data

Simulated dataset for a prospective cohort study (non-randomized
observational design) with multiple exclusion stages. Designed for
testing the `studydiagram` function and demonstrating participant flow
in observational research.

## Usage

``` r
observational_study_flow_data
```

## Format

A data frame with 350 rows and 8 columns:

- participant_id:

  Unique participant identifier (OBS-0001 to OBS-0350)

- enrollment_date:

  Date of enrollment (2023-01-01 onwards)

- age:

  Age in years (mean=58, sd=15)

- diagnosis:

  Disease stage (Stage I/II/III/IV)

- initial_exclusion:

  Initial screening exclusion reasons

- consent_status:

  Consent/enrollment exclusion reasons

- followup_status:

  Follow-up completion status and reasons for loss

- in_final_analysis:

  Logical flag indicating inclusion in final analysis

- study_group:

  Observational cohort assignment (Surgery/Radiation/Combination)

## Source

Generated using `data-raw/create_clinical_trial_flow_data.R` (seed:
20251005)

## Details

This dataset simulates a realistic observational cohort study with:

- 350 participants initially enrolled

- 15 percent initial exclusion rate (incomplete records, duplicates)

- 8 percent consent exclusion rate (declined, unable to contact)

- 10 percent follow-up loss rate (lost, moved, death, withdrew)

- Final retention: 70.6 percent (247 participants in final analysis)

Unlike the RCT dataset (`clinical_trial_consort_data`), this represents
observational allocation to treatment groups (not randomized).

## Usage

This dataset demonstrates:

- Non-randomized study flow visualization

- Observational cohort allocation

- Temporal enrollment patterns (enrollment_date)

- Multiple exclusion pathways without randomization

## See also

[`clinical_trial_consort_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/clinical_trial_consort_data.md),
[`multicenter_trial_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/multicenter_trial_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load data
data(observational_study_flow_data)

# View structure
str(observational_study_flow_data)

# Enrollment over time
table(format(observational_study_flow_data$enrollment_date, "%Y-%m"))

# Final analysis by study group
table(observational_study_flow_data$study_group,
      observational_study_flow_data$in_final_analysis)
} # }
```
