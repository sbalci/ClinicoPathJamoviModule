# Cancer Biomarker Time-Dependent ROC Test Dataset

Simulated dataset representing a cancer biomarker study with tumor
marker measurements and survival outcomes. Designed to test basic
time-dependent ROC functionality with realistic cancer progression
patterns.

## Usage

``` r
timeroc_cancer_biomarker
```

## Format

A data frame with 300 observations and 9 variables:

- patient_id:

  Character. Unique patient identifier (CA_001 to CA_300)

- age:

  Integer. Patient age at diagnosis (30-90 years)

- sex:

  Character. Patient sex ("Male", "Female")

- cancer_stage:

  Character. Cancer stage at diagnosis ("I", "II", "III", "IV")

- tumor_biomarker:

  Numeric. Continuous tumor biomarker level (higher = worse prognosis)

- follow_up_months:

  Numeric. Follow-up time in months (0-60)

- death_event:

  Integer. Death indicator (1 = death, 0 = censored)

- treatment_type:

  Character. Treatment received ("Surgery", "Surgery+Chemo",
  "Surgery+Radio", "Palliative")

- hospital_center:

  Character. Treatment center ("Center_A" to "Center_E")

## Source

Simulated data generated using create_timeroc_test_data.R

## Details

This dataset simulates a cohort study following cancer patients for up
to 60 months. The tumor biomarker shows realistic stage-dependent
distributions with higher values in advanced stages. Survival times are
generated based on biomarker level, age, and cancer stage using
exponential survival models.

**Key Features:**

- Realistic biomarker-outcome associations

- Stage-stratified survival patterns

- Administrative censoring at 60 months

- Random dropout patterns

- 245/300 events (81.7% event rate)

**Recommended TimeROC Parameters:**

- Timepoints: 12, 36, 60 months

- Marker: tumor_biomarker

- Event: death_event

- Time: follow_up_months

## See also

[`timeroc`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc.md),
[`timeroc_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(timeroc_cancer_biomarker)

# Basic time-dependent ROC analysis
result <- timeroc(
  data = timeroc_cancer_biomarker,
  elapsedtime = "follow_up_months",
  outcome = "death_event", 
  marker = "tumor_biomarker",
  timepoints = "12, 36, 60"
)

# View AUC results
result$aucTable$asDF
} # }
```
