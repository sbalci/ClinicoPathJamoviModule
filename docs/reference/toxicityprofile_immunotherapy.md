# Immunotherapy Safety Profile Dataset

Simulated immunotherapy clinical trial adverse event dataset featuring
immune-related adverse events (irAEs), monotherapy vs combination
therapy comparison, and characteristic delayed-onset toxicity patterns.
Designed to test immunotherapy-specific safety analysis, immune-related
AE monitoring, and regulatory reporting for checkpoint inhibitors.

## Usage

``` r
toxicityprofile_immunotherapy
```

## Format

A data frame with 541 adverse events from 171 patients and 8 variables:

- patient_id:

  Character. Unique patient identifier (IMM_001 to IMM_180)

- treatment_group:

  Factor. Treatment regimen ("Monotherapy", "Combination")

- adverse_event:

  Factor. Immune-related adverse events (20 unique irAEs)

- toxicity_grade:

  Integer. CTCAE toxicity grade with irAE-specific distributions

- system_organ_class:

  Factor. MedDRA SOC with emphasis on immune-related systems

- time_to_event:

  Integer. Days to irAE onset (typically delayed, 7-365 days)

- patient_age:

  Integer. Patient age at enrollment

- patient_sex:

  Factor. Patient sex ("Male", "Female")

## Source

Simulated data generated using create_toxicityprofile_test_data.R

## Details

This dataset simulates immunotherapy safety data with characteristic
immune-related adverse events, delayed onset patterns, and combination
vs monotherapy toxicity profiles. It reflects real-world immunotherapy
safety experience including checkpoint inhibitor toxicities and immune
system activation effects.

**Clinical Context:**

- Immunotherapy trial comparing monotherapy vs combination treatment

- 180 patients with immune-related adverse event focus

- 20 specific immune-related adverse events

- Delayed onset patterns characteristic of immunotherapy

- Grade distributions reflecting irAE severity patterns

**Immune-Related Adverse Events:**

- **Gastrointestinal**: Diarrhea, Colitis

- **Pulmonary**: Pneumonitis

- **Hepatic**: Hepatitis

- **Endocrine**: Thyroiditis, Adrenal insufficiency, Diabetes mellitus,
  Hypophysitis

- **Dermatologic**: Rash, Pruritus, Vitiligo, Severe skin reactions

- **Musculoskeletal**: Arthralgia, Myalgia

- **Renal**: Nephritis

- **Neurologic**: Neurological toxicity

- **Cardiac**: Myocarditis (rare but serious)

- **Ocular**: Uveitis

**Key Features:**

- Delayed onset patterns typical of immune activation

- Higher incidence in combination therapy vs monotherapy

- Appropriate grade distributions for each irAE type

- Serious events (myocarditis, severe colitis) at realistic low
  frequencies

- System organ class categorization for regulatory reporting

**Recommended Analysis Scenarios:**

- Immune-related adverse event profiling

- Monotherapy vs combination safety comparison

- Time-to-onset analysis for delayed toxicities

- Endocrine toxicity monitoring

- Serious irAE identification and reporting

- Safety stopping rule evaluation

## See also

[`toxicityprofile`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile.md),
[`toxicityprofile_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toxicityprofile_immunotherapy)

# Immune-related AE analysis
result <- toxicityprofile(
    data = toxicityprofile_immunotherapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    systemOrganClass = "system_organ_class",
    plotType = "heatmap"
)

# High-grade irAE analysis
result_serious <- toxicityprofile(
    data = toxicityprofile_immunotherapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    showHighGradeOnly = TRUE,
    minIncidence = 1
)

# Time-to-onset analysis
result_timing <- toxicityprofile(
    data = toxicityprofile_immunotherapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    timeToEvent = "time_to_event",
    plotType = "time_to_event"
)
} # }
```
