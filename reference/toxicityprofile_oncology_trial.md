# Oncology Clinical Trial Safety Dataset

Simulated comprehensive oncology clinical trial adverse event dataset
with three treatment arms, realistic CTCAE grading, and time-to-event
information. Designed to test standard oncology safety analysis,
treatment comparisons, regulatory reporting, and comprehensive toxicity
profiling typical in phase II/III oncology trials.

## Usage

``` r
toxicityprofile_oncology_trial
```

## Format

A data frame with 2,061 adverse events from 250 patients and 9
variables:

- patient_id:

  Character. Unique patient identifier (PT_001 to PT_250)

- treatment_group:

  Factor. Treatment assignment ("Control", "Treatment A", "Treatment B")

- adverse_event:

  Factor. CTCAE adverse event term (22 unique oncology-specific events)

- toxicity_grade:

  Integer. CTCAE toxicity grade (1-5 scale)

- system_organ_class:

  Factor. MedDRA System Organ Class categorization

- time_to_event:

  Integer. Days from treatment initiation to AE onset (1-365 days)

- event_date:

  Date. Calculated date of adverse event occurrence

- patient_age:

  Integer. Patient age at enrollment (range varies by realistic
  distribution)

- patient_sex:

  Factor. Patient sex ("Male", "Female")

## Source

Simulated data generated using create_toxicityprofile_test_data.R

## Details

This dataset simulates a comprehensive oncology clinical trial with
realistic adverse event patterns, CTCAE grading distributions, and
treatment-specific toxicity profiles. It includes common oncology
adverse events such as hematologic toxicities, gastrointestinal effects,
constitutional symptoms, and treatment-specific events.

**Clinical Context:**

- Multi-arm oncology trial with control and two experimental treatments

- 250 patients with realistic demographic distribution

- 22 common oncology adverse events with expected frequency patterns

- CTCAE v5.0 compatible grading system

- Realistic time-to-event distributions for safety monitoring

**Adverse Events Included:**

- **Hematologic**: Anemia, Neutropenia, Thrombocytopenia

- **Gastrointestinal**: Nausea, Vomiting, Diarrhea, Constipation,
  Mucositis

- **Constitutional**: Fatigue, Fever, Decreased appetite

- **Neurologic**: Peripheral neuropathy

- **Dermatologic**: Alopecia, Rash

- **Laboratory**: Elevated ALT/AST, Hyponatremia

- **Serious Events**: Pneumonitis, Infection, Dehydration

- **Cardiovascular**: Hypertension, Proteinuria

**Statistical Features:**

- Treatment-specific incidence patterns with realistic effect sizes

- Grade distributions appropriate for each adverse event type

- Time-to-event patterns reflecting clinical experience

- Balanced patient demographics across treatment arms

**Recommended Analysis Scenarios:**

- Standard safety tabulations and listings

- Treatment group comparisons with statistical testing

- Time-to-event analysis and cumulative incidence

- System organ class summaries for regulatory reporting

- High-grade (\>=3) adverse event analysis

- Dose-limiting toxicity assessment

## See also

[`toxicityprofile`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile.md),
[`toxicityprofile_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toxicityprofile_oncology_trial)

# Basic toxicity profile analysis
result <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    plotType = "stacked_bar",
    showHighGradeOnly = FALSE
)

# Treatment comparison analysis
result_comparison <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    systemOrganClass = "system_organ_class",
    groupComparison = TRUE,
    showConfidenceIntervals = TRUE
)

# Time-to-event analysis
result_time <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    timeToEvent = "time_to_event",
    plotType = "time_to_event",
    cumulativeIncidence = TRUE
)
} # }
```
