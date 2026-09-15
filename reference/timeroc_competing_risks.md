# Competing Risks Analysis Test Dataset

Simulated study with competing risks where patients can experience
either disease-specific death or death from other causes. Designed to
test time-dependent ROC analysis in competing risks settings.

## Usage

``` r
timeroc_competing_risks
```

## Format

A data frame with 180 observations and 10 variables:

- patient_id:

  Character. Unique patient identifier (CR_001 to CR_180)

- age:

  Integer. Patient age

- comorbidity_score:

  Integer. Comorbidity burden score (Poisson distributed)

- disease_biomarker:

  Numeric. Disease-specific biomarker

- time_to_event_months:

  Numeric. Time to first event (0-48 months)

- event_type:

  Factor. Type of event ("Censored", "Disease_Death", "Other_Death")

- disease_death:

  Integer. Disease-specific death indicator (1 = disease death, 0 =
  other)

- any_death:

  Integer. Any death indicator (1 = any death, 0 = censored)

- performance_status:

  Integer. Performance status (0-2)

- prior_treatment:

  Integer. Prior treatment indicator (1 = yes, 0 = no)

## Source

Simulated data generated using create_timeroc_test_data.R

## Details

This dataset models a clinical scenario where patients face competing
risks of disease-specific mortality versus other-cause mortality. The
disease biomarker specifically predicts disease-related death, while age
and comorbidity predict competing mortality.

**Competing Events:**

- **Disease Death**: Primary outcome of interest

- **Other Death**: Competing risk (age/comorbidity related)

- **Censored**: Administrative censoring at 48 months

**Key Features:**

- True competing risks structure

- Biomarker specific to disease outcome

- Comorbidity effects on competing risk

- 64/180 disease-specific deaths (35.6% disease event rate)

- Multi-factorial risk structure

**Recommended TimeROC Parameters:**

- Timepoints: 6, 24, 48 months

- Marker: disease_biomarker

- Event: disease_death (focus on disease-specific outcome)

- Time: time_to_event_months

## See also

[`timeroc`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc.md),
[`timeroc_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(timeroc_competing_risks)

# Time-dependent ROC for disease-specific mortality
disease_roc <- timeroc(
  data = timeroc_competing_risks,
  elapsedtime = "time_to_event_months",
  outcome = "disease_death",
  marker = "disease_biomarker", 
  timepoints = "6, 24, 48"
)

# Compare to any-death outcome
any_death_roc <- timeroc(
  data = timeroc_competing_risks,
  elapsedtime = "time_to_event_months",
  outcome = "any_death",
  marker = "disease_biomarker",
  timepoints = "6, 24, 48"
)

# Examine competing risks structure
table(timeroc_competing_risks$event_type)
} # }
```
