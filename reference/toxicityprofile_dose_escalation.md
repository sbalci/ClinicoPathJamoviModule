# Dose Escalation Study Safety Dataset

Simulated dose escalation study adverse event dataset featuring
dose-dependent toxicity patterns, escalating severity with increasing
dose levels, and dose-limiting toxicity assessment. Designed to test
dose-response safety analysis, maximum tolerated dose determination, and
phase I trial safety monitoring.

## Usage

``` r
toxicityprofile_dose_escalation
```

## Format

A data frame with 603 adverse events from 148 patients and 8 variables:

- patient_id:

  Character. Unique patient identifier (DSE_001 to DSE_150)

- treatment_group:

  Factor. Dose level ("Dose Level 1" to "Dose Level 5")

- adverse_event:

  Factor. General adverse events (20 events across all systems)

- toxicity_grade:

  Integer. CTCAE grade with dose-dependent severity escalation

- system_organ_class:

  Factor. Simplified SOC categorization

- time_to_event:

  Integer. Days to AE onset (typically early in dose escalation)

- patient_age:

  Integer. Patient age at enrollment

- patient_sex:

  Factor. Patient sex ("Male", "Female")

## Source

Simulated data generated using create_toxicityprofile_test_data.R

## Details

This dataset simulates a dose escalation study with realistic
dose-dependent toxicity patterns, increasing incidence and severity at
higher dose levels, and appropriate distributions for dose-limiting
toxicity assessment. It reflects typical phase I dose escalation trial
safety patterns.

**Clinical Context:**

- Five-level dose escalation study (3+3 or similar design)

- 150 patients distributed across dose levels

- Dose-dependent incidence and severity patterns

- Early toxicity assessment (first cycle focus)

- DLT evaluation timeframe (typically 28 days)

**Dose-Response Characteristics:**

- Increasing incidence rates with dose escalation

- Higher grade toxicities at elevated dose levels

- Realistic dose level patient distributions

- Early onset appropriate for DLT assessment

- General toxicity profile applicable across therapeutic areas

**Adverse Events Pattern:**

- **Constitutional**: Fatigue, Nausea, Vomiting, Decreased appetite

- **Gastrointestinal**: Diarrhea, Constipation, Abdominal pain,
  Dyspepsia

- **Neurologic**: Headache, Dizziness, Peripheral neuropathy, Muscle
  weakness

- **Psychiatric**: Insomnia, Anxiety

- **Dermatologic**: Rash, Pruritus, Dry mouth

- **Musculoskeletal**: Arthralgia, Back pain

- **Cardiovascular**: Hypertension

**Key Features:**

- Clear dose-response relationship in toxicity

- Escalating severity with increasing dose

- Appropriate for DLT assessment

- Realistic phase I trial patient numbers

- Early onset patterns for cycle 1 evaluation

**Recommended Analysis Scenarios:**

- Dose-response toxicity analysis

- Maximum tolerated dose determination

- Dose-limiting toxicity identification

- Safety run-in analysis

- Dose modification recommendations

- Phase I safety summary

## See also

[`toxicityprofile`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile.md),
[`toxicityprofile_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toxicityprofile_dose_escalation)

# Dose-response analysis
result <- toxicityprofile(
    data = toxicityprofile_dose_escalation,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    plotType = "stacked_bar",
    sortBy = "high_grade"
)

# High-grade toxicity by dose level
result_dlts <- toxicityprofile(
    data = toxicityprofile_dose_escalation,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    showHighGradeOnly = TRUE,
    groupComparison = FALSE
)

# Dose level comparison
result_comparison <- toxicityprofile(
    data = toxicityprofile_dose_escalation,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    plotType = "heatmap"
)
} # }
```
