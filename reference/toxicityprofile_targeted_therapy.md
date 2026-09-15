# Targeted Therapy Safety Profile Dataset

Simulated targeted therapy clinical trial adverse event dataset
featuring characteristic targeted therapy toxicities, early onset
patterns, and class-specific effects. Designed to test targeted therapy
safety analysis, mechanism-based toxicity profiling, and kinase
inhibitor safety monitoring typical in precision oncology.

## Usage

``` r
toxicityprofile_targeted_therapy
```

## Format

A data frame with 1,142 adverse events from 200 patients and 8
variables:

- patient_id:

  Character. Unique patient identifier (TRG_001 to TRG_200)

- treatment_group:

  Factor. Treatment regimen ("Monotherapy", "Combination")

- adverse_event:

  Factor. Targeted therapy-specific adverse events (24 unique events)

- toxicity_grade:

  Integer. CTCAE grade with targeted therapy-specific patterns

- system_organ_class:

  Factor. MedDRA SOC emphasizing targeted therapy effects

- time_to_event:

  Integer. Days to AE onset (typically early, 1-365 days)

- patient_age:

  Integer. Patient age at enrollment

- patient_sex:

  Factor. Patient sex ("Male", "Female")

## Source

Simulated data generated using create_toxicityprofile_test_data.R

## Details

This dataset simulates targeted therapy safety data with characteristic
class effects, early onset patterns, and mechanism-based toxicity
profiles. It reflects real-world targeted therapy safety experience
including kinase inhibitor effects, EGFR inhibitor toxicities, and
angiogenesis inhibitor-related events.

**Clinical Context:**

- Targeted therapy trial comparing monotherapy vs combination

- 200 patients with targeted therapy-specific toxicity focus

- 24 mechanism-based adverse events

- Early onset patterns typical of targeted agents

- Class-specific toxicity distributions

**Targeted Therapy-Specific Adverse Events:**

- **Gastrointestinal**: Diarrhea, Mucositis, Nausea, Vomiting

- **Dermatologic**: Rash, Acneiform rash, Paronychia, Dry skin,
  Hand-foot syndrome

- **Constitutional**: Fatigue, Decreased appetite

- **Cardiovascular**: Hypertension, QT prolongation

- **Vascular**: Bleeding, Thrombosis, Proteinuria

- **Hepatic**: Elevated transaminases, Hyperbilirubinemia

- **Pulmonary**: Pneumonitis, Interstitial lung disease

- **Other**: Peripheral edema, Pleural effusion, Muscle spasms,
  Arthralgia

**Key Features:**

- Early onset patterns characteristic of targeted agents

- Higher frequency of dermatologic and GI toxicities

- Cardiovascular effects typical of angiogenesis inhibitors

- Appropriate grade distributions for each toxicity class

- Realistic incidence rates based on clinical experience

**Recommended Analysis Scenarios:**

- Targeted therapy toxicity profiling

- Class effect identification and analysis

- Early vs late onset toxicity patterns

- Dermatologic toxicity monitoring

- Cardiovascular safety assessment

- Dose modification analysis

## See also

[`toxicityprofile`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile.md),
[`toxicityprofile_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toxicityprofile_targeted_therapy)

# Targeted therapy toxicity profile
result <- toxicityprofile(
    data = toxicityprofile_targeted_therapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    plotType = "stacked_bar",
    sortBy = "frequency"
)

# Dermatologic toxicity focus
dermatologic_data <- subset(
    toxicityprofile_targeted_therapy,
    system_organ_class == "Skin and subcutaneous tissue disorders"
)
result_derm <- toxicityprofile(
    data = dermatologic_data,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "dot_plot",
    showConfidenceIntervals = TRUE
)

# Early onset pattern analysis
result_timing <- toxicityprofile(
    data = toxicityprofile_targeted_therapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    timeToEvent = "time_to_event",
    plotType = "time_to_event"
)
} # }
```
