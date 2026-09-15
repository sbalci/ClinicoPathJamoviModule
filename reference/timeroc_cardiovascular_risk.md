# Cardiovascular Risk Prediction Test Dataset

Simulated cardiovascular risk prediction study with multiple biomarkers
and clinical risk factors. Designed to test time-dependent ROC analysis
in the context of cardiovascular event prediction.

## Usage

``` r
timeroc_cardiovascular_risk
```

## Format

A data frame with 400 observations and 12 variables:

- patient_id:

  Character. Unique patient identifier (CV_0001 to CV_0400)

- age:

  Integer. Patient age (30-85 years)

- sex:

  Character. Patient sex ("Male", "Female")

- diabetes:

  Integer. Diabetes status (1 = yes, 0 = no)

- hypertension:

  Integer. Hypertension status (1 = yes, 0 = no)

- smoking_status:

  Integer. Smoking status (1 = current smoker, 0 = not)

- troponin_level:

  Numeric. Troponin biomarker level (log-normal distribution)

- crp_level:

  Numeric. C-reactive protein level (log-normal distribution)

- risk_score:

  Numeric. Composite cardiovascular risk score

- follow_up_months:

  Numeric. Follow-up time in months (0-36)

- cv_event:

  Integer. Cardiovascular event indicator (1 = event, 0 = censored)

- event_type:

  Character. Type of cardiovascular event ("MI", "Stroke", "CHF",
  "Death", "None")

- study_site:

  Character. Study recruitment site ("Site_A" to "Site_D")

## Source

Simulated data generated using create_timeroc_test_data.R

## Details

This dataset represents a prospective cardiovascular risk study with
36-month follow-up. The dataset includes traditional risk factors (age,
sex, diabetes, hypertension, smoking) and novel biomarkers (troponin,
CRP). The composite risk score combines all risk factors.

**Key Features:**

- Multiple biomarker comparison capability

- Traditional + novel risk factors

- Realistic biomarker distributions (log-normal)

- 375/400 events (93.8% event rate)

- Multi-site study design

**Recommended TimeROC Parameters:**

- Timepoints: 6, 18, 36 months

- Markers: troponin_level, crp_level, risk_score

- Event: cv_event

- Time: follow_up_months

## See also

[`timeroc`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc.md),
[`timeroc_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(timeroc_cardiovascular_risk)

# Compare multiple biomarkers
troponin_roc <- timeroc(
  data = timeroc_cardiovascular_risk,
  elapsedtime = "follow_up_months",
  outcome = "cv_event",
  marker = "troponin_level",
  timepoints = "6, 18, 36"
)

risk_score_roc <- timeroc(
  data = timeroc_cardiovascular_risk,
  elapsedtime = "follow_up_months", 
  outcome = "cv_event",
  marker = "risk_score",
  timepoints = "6, 18, 36"
)
} # }
```
