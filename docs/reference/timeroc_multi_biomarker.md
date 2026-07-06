# Multi-Biomarker Comparison Test Dataset

Specialized dataset with three biomarkers of varying predictive
performance designed to test comparative time-dependent ROC analysis.
Each biomarker has different signal-to-noise ratios representing
excellent, good, and fair predictive ability.

## Usage

``` r
timeroc_multi_biomarker
```

## Format

A data frame with 250 observations and 10 variables:

- subject_id:

  Character. Unique subject identifier (MB_001 to MB_250)

- age_years:

  Integer. Subject age in years

- gender:

  Character. Subject gender ("M", "F")

- biomarker_alpha:

  Numeric. Excellent predictor biomarker (expected AUC ~0.85)

- biomarker_beta:

  Numeric. Good predictor biomarker (expected AUC ~0.75)

- biomarker_gamma:

  Numeric. Fair predictor biomarker (expected AUC ~0.65)

- composite_score:

  Numeric. Weighted combination of alpha and beta biomarkers

- follow_up_months:

  Numeric. Follow-up time in months (0-48)

- primary_event:

  Integer. Primary endpoint event (1 = event, 0 = censored)

- cohort:

  Character. Study cohort ("Training", "Validation")

- enrollment_year:

  Integer. Year of enrollment (2018-2022)

## Source

Simulated data generated using create_timeroc_test_data.R

## Details

This dataset is specifically designed to test biomarker ranking and
comparison functionality in time-dependent ROC analysis. The three
biomarkers have systematically different predictive abilities:

- **biomarker_alpha**: Strong signal (high correlation with outcome)

- **biomarker_beta**: Moderate signal (medium correlation with outcome)

- **biomarker_gamma**: Weak signal (low correlation with outcome)

- **composite_score**: Combined alpha + beta for testing multi-marker
  models

**Key Features:**

- Controlled predictive performance differences

- Training/validation split capability

- 249/250 events (99.6% event rate)

- Multi-year enrollment period

- Realistic biomarker scales and distributions

**Recommended TimeROC Parameters:**

- Timepoints: 6, 12, 18 months

- Markers: biomarker_alpha, biomarker_beta, biomarker_gamma,
  composite_score

- Event: primary_event

- Time: follow_up_months

## See also

[`timeroc`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc.md),
[`timeroc_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/timeroc_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(timeroc_multi_biomarker)

# Compare all biomarkers
markers <- c("biomarker_alpha", "biomarker_beta", "biomarker_gamma")
results <- list()

for(marker in markers) {
  results[[marker]] <- timeroc(
    data = timeroc_multi_biomarker,
    elapsedtime = "follow_up_months",
    outcome = "primary_event",
    marker = marker,
    timepoints = "6, 12, 18"
  )
}

# Extract AUC values for comparison
sapply(results, function(x) x$aucTable$asDF$auc)
} # }
```
