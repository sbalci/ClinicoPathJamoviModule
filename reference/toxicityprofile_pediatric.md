# Pediatric Safety Study Dataset

Simulated pediatric clinical trial adverse event dataset featuring
age-appropriate adverse events, milder severity profiles, and
pediatric-specific safety considerations. Designed to test pediatric
safety analysis, age-specific toxicity profiling, and regulatory
pediatric safety reporting requirements.

## Usage

``` r
toxicityprofile_pediatric
```

## Format

A data frame with 387 adverse events from 114 patients and 8 variables:

- patient_id:

  Character. Unique patient identifier (PED_001 to PED_120)

- treatment_group:

  Factor. Treatment assignment ("Active", "Placebo")

- adverse_event:

  Factor. Pediatric-appropriate adverse events (18 unique events)

- toxicity_grade:

  Integer. CTCAE grade with pediatric-appropriate severity (1-3 focus)

- system_organ_class:

  Factor. Simplified SOC for pediatric context

- time_to_event:

  Integer. Days to AE onset (shorter timeframe, 1-90 days)

- patient_age:

  Integer. Pediatric age range (6-17 years)

- patient_sex:

  Factor. Patient sex ("Male", "Female")

## Source

Simulated data generated using create_toxicityprofile_test_data.R

## Details

This dataset simulates pediatric clinical trial safety data with
age-appropriate adverse events, typically milder severity profiles, and
pediatric-specific considerations such as growth, development, and
behavioral effects. It reflects pediatric safety monitoring requirements
and regulatory considerations.

**Clinical Context:**

- Pediatric clinical trial with active treatment vs placebo

- 120 patients aged 6-17 years

- Age-appropriate adverse event profile

- Milder severity patterns typical in pediatric populations

- Shorter assessment timeframe appropriate for pediatric studies

**Pediatric-Specific Adverse Events:**

- **Constitutional**: Fatigue, Fever, Decreased appetite, Weight loss

- **Gastrointestinal**: Nausea, Vomiting, Diarrhea, Abdominal pain

- **Neurologic**: Headache

- **Behavioral**: Irritability, Sleep disturbance, Mood changes,
  Attention difficulties, Hyperactivity

- **Respiratory**: Upper respiratory infection, Cough

- **Dermatologic**: Rash

- **Local**: Injection site reaction

**Key Features:**

- Age-appropriate adverse event selection

- Predominantly grade 1-2 events (milder severity)

- Behavioral and developmental considerations

- Realistic pediatric study size and design

- Appropriate timeframe for pediatric assessment

- Placebo-controlled design typical in pediatric trials

**Recommended Analysis Scenarios:**

- Pediatric safety profiling

- Active vs placebo safety comparison

- Age-stratified safety analysis

- Behavioral toxicity assessment

- Growth and development monitoring

- Pediatric regulatory safety reporting

## See also

[`toxicityprofile`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile.md),
[`toxicityprofile_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toxicityprofile_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toxicityprofile_pediatric)

# Pediatric safety profile
result <- toxicityprofile(
    data = toxicityprofile_pediatric,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    plotType = "dot_plot",
    showConfidenceIntervals = TRUE
)

# Behavioral toxicity focus
behavioral_events <- c(
    "Irritability", "Sleep disturbance", "Mood changes",
    "Attention difficulties", "Hyperactivity"
)
behavioral_data <- subset(
    toxicityprofile_pediatric,
    adverse_event %in% behavioral_events
)
result_behavioral <- toxicityprofile(
    data = behavioral_data,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    groupComparison = TRUE
)

# Age-stratified analysis
adolescent_data <- subset(toxicityprofile_pediatric, patient_age >= 13)
result_adolescent <- toxicityprofile(
    data = adolescent_data,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "stacked_bar"
)
} # }
```
