# Clinical Trial CONSORT Flow Data

Realistic simulated dataset for a Phase III randomized controlled trial
(RCT) with multiple exclusion stages. Designed for testing the
`consortdiagram` function and demonstrating CONSORT 2010 compliant flow
diagrams.

## Usage

``` r
clinical_trial_consort_data
```

## Format

A data frame with 500 rows and 17 columns:

- participant_id:

  Unique participant identifier (PT-0001 to PT-0500)

- age:

  Age in years (mean=62, sd=12)

- sex:

  Sex (Male/Female)

- age_exclusion:

  Screening exclusion reason: age criteria violations

- performance_exclusion:

  Screening exclusion reason: ECOG performance status or organ function
  issues

- comorbidity_exclusion:

  Screening exclusion reason: severe comorbidity or prior malignancy

- consent_exclusion:

  Enrollment exclusion reason: consent-related issues

- eligibility_exclusion:

  Enrollment exclusion reason: eligibility re-assessment failures

- treatment_arm:

  Randomized treatment assignment (Treatment A/Treatment B)

- allocation_exclusion:

  Post-randomization exclusion: intervention not received

- followup_loss:

  Follow-up exclusion reason: lost to follow-up or moved

- followup_death:

  Follow-up exclusion reason: death or withdrawal

- analysis_exclusion:

  Analysis exclusion reason: missing data or protocol violations

- outcome_response:

  Treatment outcome (Complete/Partial Response, Stable/Progressive
  Disease)

## Source

Generated using `data-raw/create_clinical_trial_flow_data.R` (seed:
20251005)

## Details

This dataset simulates a realistic Phase III clinical trial with:

- 500 participants initially assessed for eligibility

- 20 percent screening exclusion rate (age, performance, comorbidity)

- 10 percent enrollment exclusion rate (consent, eligibility)

- 1:1 randomization to Treatment A vs Treatment B

- 5 percent allocation exclusion rate (intervention not received)

- 12 percent follow-up loss rate (lost to follow-up, death, withdrawal)

- 3 percent analysis exclusion rate (missing data, protocol violations)

- Final retention: 58.4 percent (292 participants analyzed)

Exclusion variables use NA for participants who continued to the next
stage. Non-NA values indicate the specific exclusion reason at that
stage.

## Usage

This dataset demonstrates all features of the `consortdiagram` function:

- Multiple exclusion stages (screening, enrollment, allocation,
  follow-up, analysis)

- Multiple exclusion reasons per stage

- Randomized treatment arms

- Realistic retention rates

- Detailed exclusion reasons for CONSORT compliance

## References

Schulz KF, Altman DG, Moher D. (2010). CONSORT 2010 Statement: updated
guidelines for reporting parallel group randomised trials. BMJ,
340:c332.

Ots R. (2024). CONSORT diagrams in R using patient-level data.
<https://www.riinu.me/2024/02/consort/>

## See also

[`observational_study_flow_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/observational_study_flow_data.md),
[`multicenter_trial_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/multicenter_trial_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load data
data(clinical_trial_consort_data)

# View structure
str(clinical_trial_consort_data)

# Summary statistics
summary(clinical_trial_consort_data)

# Count participants at each stage
n_assessed <- nrow(clinical_trial_consort_data)
n_screening_excl <- sum(!is.na(clinical_trial_consort_data$age_exclusion) |
                        !is.na(clinical_trial_consort_data$performance_exclusion) |
                        !is.na(clinical_trial_consort_data$comorbidity_exclusion))
n_randomized <- sum(!is.na(clinical_trial_consort_data$treatment_arm))
n_analyzed <- sum(!is.na(clinical_trial_consort_data$outcome_response))

cat("Assessed:", n_assessed, "\n")
cat("Screening excluded:", n_screening_excl, "\n")
cat("Randomized:", n_randomized, "\n")
cat("Analyzed:", n_analyzed, "\n")
} # }
```
